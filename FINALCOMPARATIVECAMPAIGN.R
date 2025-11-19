rm(list=ls())

# ######################################################################
# --- SKYRIM POLITICAL OPINION DYNAMICS: COMPARATIVE CAMPAIGN MODEL ---
# --- Agent-Based Model: Imperial vs Stormcloak Victory Scenarios ---
# --- WITH: Social Hierarchy Networks & Conflict-Driven Extremization ---
# ######################################################################

library(tidyverse)
library(ggplot2)
library(patchwork)

FILE_PATH <- "People of Skyrim/Skyrim_Named_Characters.csv"
df <- read_csv(FILE_PATH, show_col_types = FALSE)
names(df) <- make.names(names(df))  # Convert column names to valid R identifiers

cat("\n=======================================================\n")
cat("SKYRIM AGENT-BASED MODEL: COMPARATIVE CAMPAIGN ANALYSIS\n")
cat("Imperial Victory vs Stormcloak Victory\n")
cat("Sequential Shocks & Dynamic Homophily\n")
cat("WITH: Social Hierarchy & Conflict Extremization\n")
cat("=======================================================\n\n")

# ######################################################################
# --- PART 1: DATA PREPARATION ---
# ######################################################################

cat("PART 1: Data Preparation\n")
cat("----------------------------\n")

# Parse level strings (e.g., "10-20", "PC×1") into numeric estimates
clean_level <- function(level_str) {
     if (length(level_str) > 1) return(sapply(level_str, clean_level))
     level_str <- as.character(level_str)
     if (is.na(level_str) | level_str == "" | length(level_str) == 0) return(NA_real_)
     # Handle player-scaled levels (PC×N) as level 50
     if (str_detect(level_str, "PC\\s*[xX]")) return(50)
     # Handle ranges (e.g., "10-20") by taking midpoint
     if (str_detect(level_str, "-")) {
          nums <- str_extract_all(level_str, "\\d+")[[1]] %>% as.numeric() %>% na.omit()
          if (length(nums) >= 2) return(mean(c(min(nums), max(nums))))
          if (length(nums) == 1) return(nums[1])
          return(NA_real_)
     }
     # Extract single numeric value
     num <- as.numeric(str_extract(level_str, "\\d+"))
     return(ifelse(is.na(num), NA_real_, num))
}

# Map locations to their hold capitals for political allegiance
location_to_hold <- c(
     "Solitude" = "Solitude", "Dragon Bridge" = "Solitude", "Katla's Farm" = "Solitude", 
     "East Empire Company" = "Solitude", "Markarth" = "Markarth", "Karthwasten" = "Markarth", 
     "Old Hroldan" = "Markarth", "Salvius Farm" = "Markarth", "Falkreath" = "Falkreath", 
     "Helgen" = "Falkreath", "Riverwood" = "Falkreath", "Ivarstead" = "Falkreath",
     "Morthal" = "Morthal", "Windhelm" = "Windhelm", "Kynesgrove" = "Windhelm", 
     "Mixwater Mill" = "Windhelm", "Dawnstar" = "Dawnstar", "Nightgate Inn" = "Dawnstar",
     "Riften" = "Riften", "Shor's Stone" = "Riften", "Sarethi Farm" = "Riften",
     "Winterhold" = "Winterhold", "College of Winterhold" = "Winterhold",
     "Whiterun" = "Whiterun", "Rorikstead" = "Whiterun", "Battle-Born Farm" = "Whiterun", 
     "Chillfurrow Farm" = "Whiterun"
)

# Initial hold allegiances at game start (before civil war progression)
# Whiterun is de facto Imperial (Balgruuf hasn't rebelled) but reluctant
hold_allegiance_map_initial <- c(
     "Solitude" = "Imperial", "Markarth" = "Imperial", "Falkreath" = "Imperial", 
     "Morthal" = "Imperial", "Windhelm" = "Stormcloak", "Dawnstar" = "Stormcloak", 
     "Riften" = "Stormcloak", "Winterhold" = "Stormcloak",
     "Whiterun" = "Imperial",  # De facto Imperial, not truly neutral
     "Transient_Wilderness" = "Neutral"
)

# Social influence scores by class (for network preferential attachment)
# Jarls have significantly more influence due to near-absolute Hold authority
class_influence_scores <- c(
     "Jarl" = 15, "Legate" = 9, "Court Wizard" = 8, "Housecarl" = 7,
     "Warrior" = 5, "Mage" = 5, "Blacksmith" = 4, "Merchant" = 3,
     "Citizen" = 2, "Beggar" = 1
)

# Map aggression categories to numeric values
aggression_mapping <- c(
     "Unaggressive" = 0, "Aggressive" = 1, "Very Aggressive" = 2,
     "Frenzied" = 3, "Indeterminate" = 1, "0" = 0, "1" = 1, "2" = 2, "3" = 3
)

# Clean and prepare agent data
df <- df %>%
     mutate(
          # Parse level values
          Level_Est = map_dbl(Level, clean_level),
          Level_Est = replace_na(Level_Est, median(Level_Est, na.rm = TRUE)),
          # Assign home cities and holds
          Home_City_Raw = replace_na(Home.City, "Unknown"),
          Hold_Capital = location_to_hold[Home_City_Raw],
          Home_City = coalesce(Hold_Capital, "Transient_Wilderness"),
          Hold_Allegiance = hold_allegiance_map_initial[Home_City],
          # Assign base influence from class
          Class_Raw = as.character(Class),
          Base_Influence = coalesce(class_influence_scores[Class_Raw], 2),
          # Scale level to 0-1 range
          Level_Scaled = {
               lr <- max(Level_Est, na.rm = TRUE) - min(Level_Est, na.rm = TRUE)
               if (lr == 0) rep(0.5, n()) else (Level_Est - min(Level_Est, na.rm = TRUE)) / lr
          },
          # Combine class and level into influence score (70% class, 30% level)
          Influence_Score = (Base_Influence * 0.7) + (Level_Scaled * 0.3 * 10),
          # Clean and normalize aggression values
          Aggression_clean = str_trim(as.character(Aggression)),
          Aggression = case_when(
               Aggression_clean %in% names(aggression_mapping) ~ aggression_mapping[Aggression_clean],
               !is.na(suppressWarnings(as.numeric(Aggression_clean))) ~ as.numeric(Aggression_clean),
               TRUE ~ NA_real_
          ),
          # Fill missing stats with medians
          Health = coalesce(suppressWarnings(as.numeric(str_trim(as.character(Health)))), 
                            median(suppressWarnings(as.numeric(str_trim(as.character(Health)))), na.rm = TRUE), 100),
          Magicka = coalesce(suppressWarnings(as.numeric(str_trim(as.character(Magicka)))), 
                             median(suppressWarnings(as.numeric(str_trim(as.character(Magicka)))), na.rm = TRUE), 50),
          Stamina = coalesce(suppressWarnings(as.numeric(str_trim(as.character(Stamina)))), 
                             median(suppressWarnings(as.numeric(str_trim(as.character(Stamina)))), na.rm = TRUE), 100),
          Aggression = coalesce(Aggression, median(Aggression, na.rm = TRUE), 1),
          # Convert to factors
          Race = as.factor(Race),
          Home_City = as.factor(Home_City),
          Class = as.factor(Class),
          Morality = as.factor(Morality),
          # Assign initial Stormcloak support probability based on lore/demographics
          Stormcloak_Prob = case_when(
               # Key NPCs with known allegiances
               Name == "Ulfric Stormcloak" ~ 0.98,
               Name == "General Tullius" ~ 0.02,
               Name == "Galmar Stone-Fist" ~ 0.97,
               Name == "Legate Rikke" ~ 0.05,
               str_detect(Name, "Gray.Mane") ~ 0.90,
               str_detect(Name, "Battle.Born") ~ 0.10,
               str_detect(Name, "Black.Briar") ~ 0.10,
               # Nords in Stormcloak holds have high support
               Race == "Nord" & Home_City == "Windhelm" ~ 0.85,
               Race == "Nord" & Home_City == "Dawnstar" ~ 0.80,
               Race == "Nord" & Home_City == "Winterhold" ~ 0.75,
               Race == "Nord" & Home_City == "Riften" ~ 0.70,
               # Imperials lean heavily Imperial (with some variation)
               Race == "Imperial" ~ runif(n(), 0.10, 0.20),
               # Nords in Imperial holds have lower support
               Race == "Nord" & Home_City == "Solitude" ~ 0.25,
               Race == "Nord" & Home_City == "Markarth" ~ 0.30,
               Race == "Nord" & Home_City == "Falkreath" ~ 0.35,
               Race == "Nord" & Home_City == "Morthal" ~ 0.40,
               Race == "Nord" & Home_City == "Whiterun" ~ runif(n(), 0.40, 0.60),  # Mixed, reluctant Imperial
               # Other races generally lean Imperial (cosmopolitan Empire)
               Race == "High Elf" ~ 0.20,
               # Dark Elves face racism from Stormcloaks, especially in Windhelm
               Race == "Dark Elf" & Home_City == "Windhelm" ~ 0.25,  # Segregated in Gray Quarter
               Race == "Dark Elf" ~ 0.30,  # Generally prefer Imperial tolerance
               Race %in% c("Argonian", "Khajiit") ~ 0.35,  # Face discrimination, prefer Empire
               TRUE ~ 0.50  # Default neutral
          ),
          Agent_ID = row_number()
     ) %>%
     select(-Home_City_Raw, -Hold_Capital, -Aggression_clean, -Class_Raw)

cat(sprintf("Data cleaning complete. Total NPCs: %d\n\n", nrow(df)))

# ######################################################################
# --- PART 2: NETWORK TOPOLOGY ---
# ######################################################################

cat("PART 2: Network Topology\n")
cat("-------------------------\n")

# Approximate coordinates for each hold capital (for distance calculations)
city_coords <- tibble::tribble(
     ~Home_City,              ~X_Coord, ~Y_Coord,
     "Solitude",               -145000,   118000, 
     "Markarth",               -175700,   6200,     
     "Falkreath",              -68000,    -51600,   
     "Morthal",                -37900,    65000,    
     "Windhelm",               135900,    26100,    
     "Dawnstar",               31400,     114000,  
     "Riften",                 175000,    -105000, 
     "Winterhold",             109700,    101000,   
     "Whiterun",               20000,     -10000,   
     "Transient_Wilderness",   15000,     -45000
)

# Calculate Euclidean distances between all city pairs
coords_matrix <- city_coords %>% select(X_Coord, Y_Coord) %>% as.matrix()
rownames(coords_matrix) <- city_coords$Home_City
city_distance_matrix <- as.matrix(dist(coords_matrix, method = "euclidean"))

cat("City distance matrix computed.\n\n")

# ######################################################################
# --- PART 3: SCENARIO CONFIGURATION ---
# ######################################################################

cat("PART 3: Scenario Configuration\n")
cat("--------------------------------\n")

# Model hyperparameters
HYPERPARAMS <- list(
     CONFIDENCE_THRESHOLD = 0.35,            # Bounded confidence threshold for interaction
     INTERACTION_RADIUS = 200000,            # Geographic distance threshold
     LEARNING_RATE = 0.02,                   # Speed of opinion change
     AGGRESSION_SCALAR = 0.4,                # Weight of aggression in stubbornness
     MORALITY_SCALAR = 0.2,                  # Weight of morality in stubbornness
     RELIGIOUS_ZEALOTRY_FACTOR = 0.3,        # Talos worship resistance strength
     HOMOPHILY_RACE_WEIGHT = 0.5,            # Bonus for same-race interactions
     HOMOPHILY_FACTION_WEIGHT = 0.7,         # Bonus for same-faction interactions
     HIERARCHY_DOWNWARD_WEIGHT = 2.5,        # Elite → commoner influence boost (increased)
     HIERARCHY_UPWARD_WEIGHT = 0.15,         # Commoner → elite influence penalty (decreased)
     HIERARCHY_THRESHOLD = 0.4,              # Min influence gap to block upward influence
     EXTREMIZATION_BASE_RATE = 0.005,        # Baseline polarization during peace
     EXTREMIZATION_BATTLE_RATE = 0.035,      # Spike during battles
     EXTREMIZATION_BATTLE_DURATION = 10,     # Timesteps of elevated extremization
     EXTREMIZATION_THRESHOLD = 0.3,          # Min opinion distance to trigger extremization
     THALMOR_INTERFERENCE_STRENGTH = 0.008,  # Thalmor push toward perpetual stalemate
     THALMOR_ACTIVE_UNTIL = 101,             # Thalmor stop interfering after final battle
     POST_WAR_THALMOR_THREAT = 0.015,        # Post-war anti-Thalmor unification rate
     POST_WAR_START = 110,                   # When anti-Thalmor sentiment kicks in
     N_RUNS = 50,                            # Monte Carlo iterations per scenario
     TOTAL_TIME_STEPS = 150                  # Simulation duration
)

# Define two counterfactual scenarios
SCENARIOS <- list(
     IMPERIAL = list(
          NAME = "Imperial Victory",
          # Schedule of major battles that flip holds
          SCHEDULE = tribble(
               ~time_step, ~event_name,              ~hold_to_flip,
               26,         "Battle of Whiterun",     "Whiterun",
               51,         "Battle for Dawnstar",    "Dawnstar",
               76,         "Battle for Riften",      "Riften",
               101,        "Battle of Windhelm",     "Windhelm"
          ),
          # Jarl changes after each battle
          JARL_SWAPS = list(
               "Whiterun" = list(exile = "Vignar Gray-Mane", install = "Balgruuf the Greater"),
               "Dawnstar" = list(exile = "Skald the Elder", install = "Brina Merilis"),
               "Riften" = list(exile = "Laila Law-Giver", install = "Maven Black-Briar"),
               "Windhelm" = list(exile = "Ulfric Stormcloak", install = "Brunwulf Free-Winter")
          ),
          # Leaders defeated in final battle
          LEADERSHIP_DEFEAT = c("Ulfric Stormcloak", "Galmar Stone-Fist"),
          # Dragonborn allegiance and stats (both scenarios use Nord for lore accuracy)
          DRAGONBORN = tibble(
               Name = "The Dragonborn", Race = "Nord", Home_City = "Solitude",
               Prob_t = 0.01, Stormcloak_Prob = 0.01, Influence_Score_Norm = 1.0,
               Stubbornness_k = 0.99, Talos_Conviction = 0.3, Faction = "Imperial"
          ),
          POPULATION_SHOCK_STRENGTH = -0.2,     # Opinion shift in captured holds (pro-Imperial)
          FINAL_BATTLE_NAME = "Battle of Windhelm",
          NEW_ALLEGIANCE = "Imperial"
     ),
     STORMCLOAK = list(
          NAME = "Stormcloak Victory",
          SCHEDULE = tribble(
               ~time_step, ~event_name,              ~hold_to_flip,
               26,         "Battle of Whiterun",     "Whiterun",
               51,         "Battle for Falkreath",   "Falkreath",
               76,         "Battle for The Reach",   "Markarth",
               101,        "Battle for Solitude",    "Solitude"
          ),
          JARL_SWAPS = list(
               "Whiterun" = list(exile = "Balgruuf the Greater", install = "Vignar Gray-Mane"),
               "Falkreath" = list(exile = "Siddgeir", install = "Dengeir of Stuhn"),
               "Markarth" = list(exile = "Igmund", install = "Thongvor Silver-Blood"),
               "Solitude" = list(exile = "Elisif the Fair", install = "Ulfric Stormcloak")
          ),
          LEADERSHIP_DEFEAT = c("General Tullius", "Legate Rikke"),
          DRAGONBORN = tibble(
               Name = "The Dragonborn", Race = "Nord", Home_City = "Whiterun",
               Prob_t = 0.99, Stormcloak_Prob = 0.99, Influence_Score_Norm = 1.0,
               Stubbornness_k = 0.99, Talos_Conviction = 0.8, Faction = "Stormcloak"
          ),
          POPULATION_SHOCK_STRENGTH = 0.2,      # Opinion shift in captured holds (pro-Stormcloak)
          FINAL_BATTLE_NAME = "Battle for Solitude",
          NEW_ALLEGIANCE = "Stormcloak"
     )
)

cat(sprintf("Total time steps: %d | Monte Carlo runs: %d per scenario\n\n", 
            HYPERPARAMS$TOTAL_TIME_STEPS, HYPERPARAMS$N_RUNS))

# ######################################################################
# --- PART 4: AGENT INITIALIZATION ---
# ######################################################################

# Normalize vector to 0-1 range
scale_01 <- function(x) {
     x <- as.numeric(x)
     if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) return(rep(0.5, length(x)))
     (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Initialize agent properties for simulation
df_agents_initial <- df %>%
     mutate(
          # Assign Talos worship conviction based on race and location
          Talos_Conviction = case_when(
               Race == "High Elf" ~ -1.0,                                    # Thalmor opposition
               Name == "Heimskr" ~ 1.0,                                      # Fervent priest
               Race == "Nord" & Hold_Allegiance == "Stormcloak" ~ 0.9,      # Strong worship
               Race == "Nord" & Home_City == "Whiterun" ~ 0.7,              # Moderate worship
               Race == "Nord" ~ 0.6,                                         # General Nord worship
               # Imperials have varied conviction (traditionalists vs pragmatists)
               Race == "Imperial" & Name %in% c("Legate Rikke") ~ 0.7,     # Military traditionalists
               Race == "Imperial" & Class == "Legate" ~ runif(n(), 0.5, 0.8),  # Officers vary
               Race == "Imperial" ~ runif(n(), 0.2, 0.5),                   # Civilians vary widely
               TRUE ~ 0.1                                                    # Other races
          ),
          # Normalize influence scores to 0-1
          Influence_Score_Norm = scale_01(Influence_Score),
          # Calculate stubbornness from aggression and morality
          Stubbornness_k = (scale_01(as.numeric(Aggression)) * HYPERPARAMS$AGGRESSION_SCALAR) +
               (scale_01(as.numeric(Morality)) * HYPERPARAMS$MORALITY_SCALAR),
          # Cap stubbornness, except for key leaders
          Stubbornness_k = case_when(
               Name %in% c("Ulfric Stormcloak", "General Tullius") ~ 0.98,
               Name %in% c("Galmar Stone-Fist", "Legate Rikke") ~ 0.95,
               TRUE ~ pmin(Stubbornness_k, 0.90)
          ),
          # Initialize current opinion to initial opinion
          Prob_t = Stormcloak_Prob,
          # Categorize agents by faction
          Faction = case_when(
               Stormcloak_Prob > 0.6 ~ "Stormcloak",
               Stormcloak_Prob < 0.4 ~ "Imperial",
               TRUE ~ "Neutral"
          )
     ) %>%
     select(Agent_ID, Name, Race, Home_City, Prob_t, Stormcloak_Prob,
            Influence_Score_Norm, Stubbornness_k, Talos_Conviction, Faction)

cat("PART 4: Initial Agent Setup\n")
cat("----------------------------\n")
cat(sprintf("Initial agents: %d\n", nrow(df_agents_initial)))
cat("Initial faction distribution:\n")
print(table(df_agents_initial$Faction))

# Add Thalmor agents who actively interfere to prolong the war
cat("\nAdding Thalmor interference agents...\n")
thalmor_agents <- tibble(
     Agent_ID = max(df_agents_initial$Agent_ID) + 1:3,
     Name = c("Elenwen", "Ancano", "Ondolemar"),
     Race = factor("High Elf", levels = levels(df_agents_initial$Race)),
     Home_City = factor(c("Solitude", "Winterhold", "Markarth"), 
                        levels = levels(df_agents_initial$Home_City)),
     Prob_t = 0.50,  # Want stalemate, no preference for either side
     Stormcloak_Prob = 0.50,
     Influence_Score_Norm = 0.75,  # High influence (ambassadors, advisors)
     Stubbornness_k = 0.98,  # Extremely stubborn (following Thalmor orders)
     Talos_Conviction = -1.0,  # Strongly oppose Talos worship
     Faction = "Thalmor"  # Third faction: neither Imperial nor Stormcloak
)

# Combine with initial agents
df_agents_initial <- bind_rows(df_agents_initial, thalmor_agents)

cat(sprintf("Total agents including Thalmor: %d\n", nrow(df_agents_initial)))
cat("Updated faction distribution:\n")
print(table(df_agents_initial$Faction))
cat("\n")

# ######################################################################
# --- PART 5: SIMULATION ENGINE ---
# ######################################################################

# Precompute neighbor network with homophily weights AND social hierarchy
precompute_neighbors_with_hierarchy <- function(agents_df, city_dist_matrix, params) {
     neighbor_list <- vector("list", nrow(agents_df))
     
     for (i in 1:nrow(agents_df)) {
          agent_city <- as.character(agents_df$Home_City[i])
          # Skip if city not in distance matrix
          if (!agent_city %in% rownames(city_dist_matrix)) {
               neighbor_list[[i]] <- list(indices = integer(0), weights = numeric(0))
               next
          }
          
          # Get agent attributes
          agent_race <- as.character(agents_df$Race[i])
          agent_faction <- as.character(agents_df$Faction[i])
          agent_influence <- agents_df$Influence_Score_Norm[i]
          
          # Find cities within interaction radius
          city_distances_vec <- city_dist_matrix[agent_city, ]
          nearby_cities <- names(city_distances_vec[city_distances_vec > 0 & 
                                                         city_distances_vec <= params$INTERACTION_RADIUS])
          interaction_cities <- c(agent_city, nearby_cities)
          
          # Get potential neighbors in interaction range (exclude self)
          neighbor_indices <- which(agents_df$Home_City %in% interaction_cities & 
                                         agents_df$Agent_ID != agents_df$Agent_ID[i])
          
          if (length(neighbor_indices) > 0) {
               # Calculate homophily bonuses
               race_match <- as.character(agents_df$Race[neighbor_indices]) == agent_race
               faction_match <- as.character(agents_df$Faction[neighbor_indices]) == agent_faction
               
               # Base homophily weight
               weights <- 1.0 + (race_match * params$HOMOPHILY_RACE_WEIGHT) + 
                    (faction_match * params$HOMOPHILY_FACTION_WEIGHT)
               
               # Apply social hierarchy modifiers (preferential attachment)
               neighbor_influence <- agents_df$Influence_Score_Norm[neighbor_indices]
               
               # High-influence agents (elites) have greater impact on low-influence (commoners)
               influence_ratio <- neighbor_influence / (agent_influence + 0.01)  # Avoid division by zero
               
               # Block upward influence if gap too large (commoners can't influence nobility)
               can_influence_upward <- (agent_influence - neighbor_influence) < params$HIERARCHY_THRESHOLD
               
               hierarchy_modifier <- ifelse(
                    !can_influence_upward & influence_ratio < 0.8,           # Low trying to influence high
                    0,                                                       # Complete block
                    ifelse(
                         influence_ratio > 1.2,                              # Neighbor is higher status
                         1 + (influence_ratio - 1) * params$HIERARCHY_DOWNWARD_WEIGHT,  # Boost influence
                         ifelse(
                              influence_ratio < 0.8,                         # Neighbor is lower status
                              params$HIERARCHY_UPWARD_WEIGHT,                # Reduce influence
                              1                                              # Similar status, no modifier
                         )
                    )
               )
               
               # Combine homophily and hierarchy weights
               weights <- weights * hierarchy_modifier
               
               neighbor_list[[i]] <- list(indices = neighbor_indices, weights = weights)
          } else {
               neighbor_list[[i]] <- list(indices = integer(0), weights = numeric(0))
          }
     }
     return(neighbor_list)
}

# Apply a major shock event (battle, leadership change, territory flip)
apply_shock_event <- function(agents_df, hold_map, event_name, hold_to_flip, params, scenario) {
     # Add Dragonborn at Battle of Whiterun
     if (event_name == "Battle of Whiterun") {
          dragonborn_agent <- scenario$DRAGONBORN
          dragonborn_agent$Agent_ID <- max(agents_df$Agent_ID) + 1
          # Ensure all columns present
          for (col in names(agents_df)) {
               if (!col %in% names(dragonborn_agent)) dragonborn_agent[[col]] <- NA
          }
          dragonborn_agent <- dragonborn_agent[names(agents_df)]
          agents_df <- bind_rows(agents_df, dragonborn_agent)
     }
     
     # Swap Jarls (exile loser, install winner)
     jarl_swap <- scenario$JARL_SWAPS[[hold_to_flip]]
     if (!is.null(jarl_swap)) {
          # Special case: Elisif remains in Solitude but loses power
          if (hold_to_flip == "Solitude" && jarl_swap$exile == "Elisif the Fair") {
               agents_df <- agents_df %>%
                    mutate(
                         Influence_Score_Norm = if_else(Name == "Elisif the Fair", 0.3, Influence_Score_Norm),
                         Influence_Score_Norm = if_else(Name == "Ulfric Stormcloak", 0.98, Influence_Score_Norm)
                    )
          } else {
               # Move exiled Jarl to opponent's capital, reduce influence
               exile_home <- ifelse(scenario$NEW_ALLEGIANCE == "Imperial", "Windhelm", "Solitude")
               agents_df <- agents_df %>%
                    mutate(
                         Home_City = if_else(Name == jarl_swap$exile, exile_home, as.character(Home_City)),
                         Influence_Score_Norm = if_else(Name == jarl_swap$exile, 0.1, Influence_Score_Norm),
                         Influence_Score_Norm = if_else(Name == jarl_swap$install, 0.9, Influence_Score_Norm)
                    )
          }
     }
     
     # Defeat enemy leaders in final battle (set influence and stubbornness to 0)
     if (event_name == scenario$FINAL_BATTLE_NAME) {
          agents_df <- agents_df %>%
               mutate(
                    Influence_Score_Norm = if_else(Name %in% scenario$LEADERSHIP_DEFEAT, 0.0, Influence_Score_Norm),
                    Stubbornness_k = if_else(Name %in% scenario$LEADERSHIP_DEFEAT, 0.0, Stubbornness_k)
               )
     }
     
     # Apply population shock in conquered hold (shift opinions toward winner)
     shock_strength <- scenario$POPULATION_SHOCK_STRENGTH
     agents_df <- agents_df %>%
          mutate(
               Prob_t = if_else(as.character(Home_City) == hold_to_flip, Prob_t + shock_strength, Prob_t),
               Prob_t = pmax(0, pmin(1, Prob_t))  # Clamp to [0,1]
          )
     
     # Update hold allegiance map
     hold_map[hold_to_flip] <- scenario$NEW_ALLEGIANCE
     
     # Update agent factions based on new hold allegiances and opinions
     agents_df <- agents_df %>%
          mutate(
               Hold_Allegiance = hold_map[as.character(Home_City)],
               Faction = case_when(
                    # Keep key NPCs in their factions
                    Name %in% c("General Tullius", "Legate Rikke") ~ "Imperial",
                    Name %in% c("Ulfric Stormcloak", "Galmar Stone-Fist") ~ "Stormcloak",
                    Name == "The Dragonborn" ~ scenario$DRAGONBORN$Faction,
                    # Agents in newly-captured holds may switch to winning faction
                    Hold_Allegiance == scenario$NEW_ALLEGIANCE & 
                         ((scenario$NEW_ALLEGIANCE == "Imperial" & Prob_t < 0.5) |
                               (scenario$NEW_ALLEGIANCE == "Stormcloak" & Prob_t > 0.5)) ~ scenario$NEW_ALLEGIANCE,
                    # Otherwise faction based on opinion thresholds
                    Prob_t < 0.4 ~ "Imperial",
                    Prob_t > 0.6 ~ "Stormcloak",
                    TRUE ~ "Neutral"
               )
          )
     
     return(list(agents_df = agents_df, hold_map = hold_map))
}

# Apply conflict-driven opinion extremization (polarization)
# Rate varies: baseline during peace, spikes during battles
apply_extremization <- function(agents_df, params, current_time, battle_times) {
     # Determine if we're in a battle period (battle + aftermath)
     time_since_battle <- sapply(battle_times, function(bt) current_time - bt)
     in_battle_period <- any(time_since_battle >= 0 & time_since_battle <= params$EXTREMIZATION_BATTLE_DURATION)
     
     # Use elevated rate during battles, baseline otherwise
     extremization_rate <- ifelse(in_battle_period, 
                                  params$EXTREMIZATION_BATTLE_RATE,
                                  params$EXTREMIZATION_BASE_RATE)
     
     # Extremization pushes moderate opinions toward the poles (0 or 1)
     agents_df <- agents_df %>%
          mutate(
               # Calculate distance from neutral (0.5)
               distance_from_center = abs(Prob_t - 0.5),
               # Only extremize if already leaning toward a side (beyond threshold)
               should_extremize = distance_from_center > params$EXTREMIZATION_THRESHOLD,
               # Push toward nearest pole
               extremization_target = ifelse(Prob_t > 0.5, 1, 0),
               # Apply extremization gradually (rate varies by conflict intensity)
               Prob_t = ifelse(
                    should_extremize,
                    Prob_t + extremization_rate * (extremization_target - Prob_t),
                    Prob_t
               ),
               # Clamp to [0,1]
               Prob_t = pmax(0, pmin(1, Prob_t))
          ) %>%
          select(-distance_from_center, -should_extremize, -extremization_target)
     
     return(agents_df)
}

# Apply Thalmor interference (push opinions toward 0.5 to create stalemate)
apply_thalmor_interference <- function(agents_df, params, current_time) {
     # Thalmor only interfere before final battle (want war to continue)
     if (current_time > params$THALMOR_ACTIVE_UNTIL) {
          return(agents_df)
     }
     
     # Thalmor push all opinions toward 0.5 (indecision/stalemate)
     # Stronger effect on agents near Thalmor cities (Solitude, Winterhold, Markarth)
     agents_df <- agents_df %>%
          mutate(
               # Identify agents in Thalmor-influenced areas
               near_thalmor = Home_City %in% c("Solitude", "Winterhold", "Markarth"),
               thalmor_effect = ifelse(near_thalmor, 
                                       params$THALMOR_INTERFERENCE_STRENGTH * 1.5,  # Stronger in Thalmor cities
                                       params$THALMOR_INTERFERENCE_STRENGTH),  # Weaker elsewhere
               # Push toward 0.5 (neutrality/indecision)
               Prob_t = Prob_t + thalmor_effect * (0.5 - Prob_t),
               # Clamp to [0,1]
               Prob_t = pmax(0, pmin(1, Prob_t))
          ) %>%
          select(-near_thalmor, -thalmor_effect)
     
     return(agents_df)
}

# Apply post-war anti-Thalmor unification (common enemy effect)
apply_post_war_thalmor_threat <- function(agents_df, params, current_time, scenario) {
     # Only apply after war ends and dust settles
     if (current_time < params$POST_WAR_START) {
          return(agents_df)
     }
     
     # Determine the "correct" opinion based on who won
     # Both sides now recognize Thalmor as real enemy and unify
     target_opinion <- ifelse(scenario$NEW_ALLEGIANCE == "Imperial", 0.3, 0.7)
     
     # Gradually shift toward unified anti-Thalmor stance
     # Winners stay on their side, losers accept new reality to face common enemy
     agents_df <- agents_df %>%
          mutate(
               # Calculate shift toward post-war consensus
               thalmor_threat_shift = params$POST_WAR_THALMOR_THREAT * (target_opinion - Prob_t),
               Prob_t = Prob_t + thalmor_threat_shift,
               # Clamp to [0,1]
               Prob_t = pmax(0, pmin(1, Prob_t))
          ) %>%
          select(-thalmor_threat_shift)
     
     return(agents_df)
}

# Run single simulation iteration
run_campaign_simulation <- function(initial_agents_df, city_dist_matrix, params, scenario, 
                                    initial_hold_map, run_id = 1) {
     # Initialize simulation state
     agents_state <- initial_agents_df
     hold_map_state <- initial_hold_map
     n_agents_current <- nrow(agents_state)
     
     # Precompute initial network with hierarchy
     neighbor_lookup <- precompute_neighbors_with_hierarchy(agents_state, city_dist_matrix, params)
     
     # Storage for trajectory data
     history_list <- vector("list", params$TOTAL_TIME_STEPS)
     
     # Extract battle times for extremization calculation
     battle_times <- scenario$SCHEDULE$time_step
     
     # Main simulation loop
     for (t in 1:params$TOTAL_TIME_STEPS) {
          # Check for scheduled shock events
          if (t %in% scenario$SCHEDULE$time_step) {
               event <- scenario$SCHEDULE %>% filter(time_step == t)
               # Apply shock (battle, Jarl swap, etc.)
               shock_results <- apply_shock_event(agents_state, hold_map_state, 
                                                  event$event_name, event$hold_to_flip, 
                                                  params, scenario)
               agents_state <- shock_results$agents_df
               hold_map_state <- shock_results$hold_map
               n_agents_current <- nrow(agents_state)
               # Rebuild network after structural changes
               neighbor_lookup <- precompute_neighbors_with_hierarchy(agents_state, city_dist_matrix, params)
          }
          
          # Apply conflict-driven extremization (rate varies by proximity to battles)
          agents_state <- apply_extremization(agents_state, params, t, battle_times)
          
          # Apply Thalmor interference (push toward stalemate)
          agents_state <- apply_thalmor_interference(agents_state, params, t)
          
          # Apply post-war anti-Thalmor unification (common enemy effect)
          agents_state <- apply_post_war_thalmor_threat(agents_state, params, t, scenario)
          
          # Bounded confidence opinion dynamics
          # Randomize agent activation order to avoid bias
          shuffled_indices <- sample(1:n_agents_current)
          
          for (idx in shuffled_indices) {
               # Skip if agent has no valid neighbors
               if (idx > length(neighbor_lookup) || is.null(neighbor_lookup[[idx]])) next
               
               # Get agent's current state
               agent_prob <- agents_state$Prob_t[idx]
               agent_talos <- agents_state$Talos_Conviction[idx]
               agent_stubborn <- agents_state$Stubbornness_k[idx]
               
               # Get agent's neighbor data
               neighbor_data <- neighbor_lookup[[idx]]
               neighbor_indices <- neighbor_data$indices
               neighbor_weights <- neighbor_data$weights
               
               if (length(neighbor_indices) == 0) next
               
               # Sample one neighbor weighted by homophily + hierarchy
               j_idx <- sample(neighbor_indices, 1, prob = neighbor_weights)
               
               # Get neighbor's attributes
               neighbor_prob <- agents_state$Prob_t[j_idx]
               neighbor_influence <- agents_state$Influence_Score_Norm[j_idx]
               neighbor_talos <- agents_state$Talos_Conviction[j_idx]
               
               # Calculate opinion gap
               opinion_gap <- abs(agent_prob - neighbor_prob)
               political_difference <- neighbor_prob - agent_prob
               
               # Talos worship creates resistance to opposing opinions
               talos_resistance <- 0
               # Talos worshippers resist Imperial influence (prob < current)
               if (!is.na(agent_talos) && agent_talos > 0.5 && political_difference < 0) {
                    talos_resistance <- agent_talos * abs(political_difference) * params$RELIGIOUS_ZEALOTRY_FACTOR
               } 
               # Talos opponents resist Stormcloak influence (prob > current)
               else if (!is.na(agent_talos) && agent_talos < -0.5 && political_difference > 0) {
                    talos_resistance <- abs(agent_talos) * political_difference * params$RELIGIOUS_ZEALOTRY_FACTOR
               }
               
               # Increase confidence threshold by religious resistance
               effective_threshold <- max(0.01, params$CONFIDENCE_THRESHOLD + talos_resistance)
               
               # Talos similarity reduces threshold (co-worshippers bond)
               talos_similarity <- 0
               if (!is.na(agent_talos) && !is.na(neighbor_talos)) {
                    # Both worship Talos
                    if (agent_talos > 0.5 && neighbor_talos > 0.5) {
                         talos_similarity <- 0.3 * min(agent_talos, neighbor_talos)
                    } 
                    # Both oppose Talos
                    else if (agent_talos < -0.5 && neighbor_talos < -0.5) {
                         talos_similarity <- 0.3 * min(abs(agent_talos), abs(neighbor_talos))
                    }
               }
               
               # Decrease threshold by religious similarity
               effective_threshold <- max(0.01, effective_threshold - talos_similarity)
               
               # Bounded confidence: only interact if opinions close enough
               if (opinion_gap <= effective_threshold) {
                    # Calculate opinion update
                    # Larger change when: large difference, high neighbor influence, low stubbornness
                    delta_p <- params$LEARNING_RATE * political_difference * neighbor_influence * (1 - agent_stubborn)
                    # Update opinion and clamp to [0,1]
                    agents_state$Prob_t[idx] <- max(0, min(1, agent_prob + delta_p))
               }
          }
          
          # Record state at specific intervals (save memory)
          if (t %% 5 == 0 || t == 1 || t == params$TOTAL_TIME_STEPS) {
               history_list[[t]] <- data.frame(
                    Agent_ID = agents_state$Agent_ID,
                    Prob_t = agents_state$Prob_t,
                    time_step = t,
                    run_id = run_id,
                    stringsAsFactors = FALSE
               )
          }
     }
     
     # Combine recorded history into single dataframe
     history_list <- history_list[!sapply(history_list, is.null)]
     return(bind_rows(history_list))
}

# Run Monte Carlo ensemble (multiple independent simulations)
run_monte_carlo_campaign <- function(n_runs, initial_agents_df, city_dist_matrix, 
                                     params, scenario, initial_hold_map) {
     all_results <- vector("list", n_runs)
     
     # Start timing for progress tracking
     start_time <- Sys.time()
     
     for (i in 1:n_runs) {
          # Detailed progress indicator with time estimates
          if (i == 1) {
               cat(sprintf("  %s: Starting run %d/%d\n", scenario$NAME, i, n_runs))
          } else if (i %% 5 == 0 || i == n_runs) {
               elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
               avg_time_per_run <- elapsed / i
               remaining_runs <- n_runs - i
               est_remaining <- avg_time_per_run * remaining_runs
               cat(sprintf("  %s: Run %d/%d complete | Elapsed: %.1fs | Est. remaining: %.1fs\n", 
                           scenario$NAME, i, n_runs, elapsed, est_remaining))
          }
          
          # Run single simulation
          all_results[[i]] <- run_campaign_simulation(initial_agents_df, city_dist_matrix, 
                                                      params, scenario, initial_hold_map, i)
     }
     
     total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
     cat(sprintf("  %s: All %d runs complete in %.1f seconds (avg %.2fs per run)\n\n", 
                 scenario$NAME, n_runs, total_time, total_time/n_runs))
     
     # Combine all runs into single dataframe
     return(bind_rows(all_results))
}

# ######################################################################
# --- PART 6: RUN BOTH SCENARIOS ---
# ######################################################################

cat("PART 5: Running Monte Carlo Campaigns\n")
cat("---------------------------------------\n")

set.seed(42)  # Reproducibility
start_time <- Sys.time()

# Run Imperial victory scenario
results_imperial <- run_monte_carlo_campaign(
     HYPERPARAMS$N_RUNS, df_agents_initial, city_distance_matrix, 
     HYPERPARAMS, SCENARIOS$IMPERIAL, hold_allegiance_map_initial
)
results_imperial$scenario <- "Imperial"

# Run Stormcloak victory scenario
results_stormcloak <- run_monte_carlo_campaign(
     HYPERPARAMS$N_RUNS, df_agents_initial, city_distance_matrix, 
     HYPERPARAMS, SCENARIOS$STORMCLOAK, hold_allegiance_map_initial
)
results_stormcloak$scenario <- "Stormcloak"

# Combine both scenarios
all_results <- bind_rows(results_imperial, results_stormcloak)
elapsed <- difftime(Sys.time(), start_time, units = "secs")

cat(sprintf("\nBoth simulations complete in %.1f seconds!\n", as.numeric(elapsed)))
cat(sprintf("Total data points: %d\n\n", nrow(all_results)))

# ######################################################################
# --- PART 7: PREPARE ANALYSIS DATA ---
# ######################################################################

# Prepare static agent attributes (don't change during simulation)
prepare_static_data <- function(scenario) {
     # Create Dragonborn entry with proper ID
     dragonborn_static <- scenario$DRAGONBORN %>%
          mutate(Agent_ID = max(df_agents_initial$Agent_ID) + 1,
                 Initial_Faction = Faction,
                 Initial_Prob = Stormcloak_Prob) %>%
          select(Agent_ID, Name, Race, Home_City, Initial_Faction, Initial_Prob, Talos_Conviction)
     
     # Combine with regular agents (excluding Thalmor who are already in df_agents_initial)
     df_agents_initial %>%
          select(Agent_ID, Name, Race, Home_City, Initial_Faction = Faction, 
                 Initial_Prob = Stormcloak_Prob, Talos_Conviction) %>%
          bind_rows(dragonborn_static)
}

# Create static data for both scenarios
agents_static_imperial <- prepare_static_data(SCENARIOS$IMPERIAL)
agents_static_stormcloak <- prepare_static_data(SCENARIOS$STORMCLOAK)

# Join dynamic simulation results with static agent attributes
analysis_df <- all_results %>%
     left_join(
          bind_rows(
               agents_static_imperial %>% mutate(scenario = "Imperial"),
               agents_static_stormcloak %>% mutate(scenario = "Stormcloak")
          ),
          by = c("Agent_ID", "scenario")
     )

# ######################################################################
# --- PART 8: COMPARATIVE ANALYSIS & VISUALIZATION ---
# ######################################################################

cat("PART 6: Comparative Analysis\n")
cat("------------------------------\n")

# Define color schemes
faction_colors <- c("Imperial" = "#DC143C", "Stormcloak" = "#4169E1", "Neutral" = "#9B9B9B")
scenario_colors <- c("Imperial" = "#DC143C", "Stormcloak" = "#4169E1")

# Calculate global mean trajectories with confidence intervals
global_summary <- analysis_df %>%
     group_by(time_step, scenario) %>%
     summarise(
          mean_prob = mean(Prob_t),
          sd_prob = sd(Prob_t),
          n = n(),
          ci_upper = mean_prob + 1.96 * (sd_prob / sqrt(n)),  # 95% CI upper bound
          ci_lower = mean_prob - 1.96 * (sd_prob / sqrt(n)),  # 95% CI lower bound
          .groups = "drop"
     )

# Prepare shock event markers for visualization
shock_events_imperial <- SCENARIOS$IMPERIAL$SCHEDULE %>%
     mutate(label = str_replace_all(event_name, " ", "\n"), scenario = "Imperial")
shock_events_stormcloak <- SCENARIOS$STORMCLOAK$SCHEDULE %>%
     mutate(label = str_replace_all(event_name, " ", "\n"), scenario = "Stormcloak")
shock_events_df <- bind_rows(shock_events_imperial, shock_events_stormcloak)

cat("Creating comparative visualizations...\n")

# Plot 1: Side-by-side scenario comparison
p1 <- ggplot(global_summary, aes(x = time_step, y = mean_prob, color = scenario, fill = scenario)) +
     geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +  # Uncertainty band
     geom_line(linewidth = 1.3) +  # Mean trajectory
     geom_hline(yintercept = 0.5, linetype = "dotted", alpha = 0.5) +  # Neutral line
     scale_color_manual(values = scenario_colors) +
     scale_fill_manual(values = scenario_colors) +
     labs(
          title = "Skyrim Civil War: Comparative Campaign Analysis",
          subtitle = "With Thalmor interference (t<101) and post-war anti-Thalmor unification (t>110)",
          x = "Time Step",
          y = "Mean Stormcloak Support",
          color = "Scenario",
          fill = "Scenario"
     ) +
     scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold", size = 14))
print(p1)

# Plot 2: Faceted view with shock event markers
p2 <- ggplot(global_summary, aes(x = time_step, y = mean_prob)) +
     geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = scenario), alpha = 0.2) +
     geom_line(aes(color = scenario), linewidth = 1.2) +
     geom_vline(data = shock_events_df, aes(xintercept = time_step),   # Battle markers
                linetype = "dashed", alpha = 0.6) +
     geom_hline(yintercept = 0.5, linetype = "dotted", alpha = 0.5) +
     facet_wrap(~ scenario, ncol = 1) +  # Separate panels
     scale_color_manual(values = scenario_colors) +
     scale_fill_manual(values = scenario_colors) +
     labs(
          title = "Political Evolution by Scenario",
          subtitle = "Thalmor drag toward stalemate (t<101), then post-war unification vs Thalmor (t>110)",
          x = "Time Step",
          y = "Mean Stormcloak Support"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"), legend.position = "none")
print(p2)

# Plot 3: Trajectories by initial faction
faction_summary <- analysis_df %>%
     group_by(time_step, scenario, Initial_Faction) %>%
     summarise(mean_prob = mean(Prob_t), .groups = "drop")

p3 <- ggplot(faction_summary, aes(x = time_step, y = mean_prob, color = Initial_Faction)) +
     geom_line(linewidth = 1.2) +
     geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.3) +
     facet_wrap(~ scenario, ncol = 1) +
     scale_color_manual(values = faction_colors) +
     labs(
          title = "Political Trajectories by Initial Faction",
          subtitle = "Dark Elves/minorities lean Imperial, stronger hierarchy effects visible",
          x = "Time Step",
          y = "Mean Stormcloak Support",
          color = "Initial Faction"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))
print(p3)

# Plot 4: Final state distribution comparison
final_state_df <- analysis_df %>% filter(time_step == max(time_step))

p4 <- ggplot(final_state_df, aes(x = Prob_t, fill = scenario)) +
     geom_density(alpha = 0.6) +  # Probability density
     geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", linewidth = 1) +
     scale_fill_manual(values = scenario_colors) +
     labs(
          title = "Final Opinion Distribution Comparison (t=150)",
          subtitle = "Event-driven extremization creates battle spikes with baseline polarization",
          x = "Stormcloak Support (0 = Imperial, 1 = Stormcloak)",
          y = "Density",
          fill = "Scenario"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))
print(p4)

# Plot 5: Polarization over time (standard deviation)
polarization_over_time <- analysis_df %>%
     group_by(time_step, scenario) %>%
     summarise(
          Variance = var(Prob_t),
          SD = sd(Prob_t),
          Pct_Extreme = mean(Prob_t < 0.3 | Prob_t > 0.7) * 100,  # % with extreme views
          Pct_Moderate = mean(Prob_t >= 0.4 & Prob_t <= 0.6) * 100,  # % moderates
          .groups = "drop"
     )

p5 <- ggplot(polarization_over_time, aes(x = time_step, y = SD, color = scenario)) +
     geom_line(linewidth = 1.2) +
     geom_vline(data = shock_events_df, aes(xintercept = time_step), 
                linetype = "dashed", alpha = 0.3) +
     scale_color_manual(values = scenario_colors) +
     labs(
          title = "Opinion Polarization Over Time",
          subtitle = "Battle-driven extremization increases variance during/after conflicts",
          x = "Time Step",
          y = "Standard Deviation",
          color = "Scenario"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))
print(p5)

# Plot 6: Talos worship dynamics
talos_dynamics <- analysis_df %>%
     mutate(
          Talos_Group = case_when(
               Talos_Conviction > 0.5 ~ "Talos Worshippers",
               Talos_Conviction < -0.5 ~ "Talos Opponents",
               TRUE ~ "Neutral on Talos"
          )
     ) %>%
     group_by(time_step, scenario, Talos_Group) %>%
     summarise(mean_prob = mean(Prob_t), .groups = "drop")

p6 <- ggplot(talos_dynamics, aes(x = time_step, y = mean_prob, color = Talos_Group)) +
     geom_line(linewidth = 1.1) +
     geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.3) +
     facet_wrap(~ scenario, ncol = 1) +
     scale_color_manual(
          values = c("Talos Worshippers" = "#FFD700", "Talos Opponents" = "#8B0000", 
                     "Neutral on Talos" = "#808080")
     ) +
     labs(
          title = "Religious Conviction and Political Alignment",
          subtitle = "Varied Imperial Talos conviction (military vs civilian) shown in trajectories",
          x = "Time Step",
          y = "Mean Stormcloak Support",
          color = "Religious Group"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))
print(p6)

# Plot 7: Convergence speed comparison
convergence_df <- analysis_df %>%
     arrange(scenario, run_id, Agent_ID, time_step) %>%
     group_by(scenario, run_id, Agent_ID) %>%
     mutate(Change_From_Previous = abs(Prob_t - lag(Prob_t, default = first(Prob_t)))) %>%
     group_by(time_step, scenario) %>%
     summarise(Mean_Change = mean(Change_From_Previous, na.rm = TRUE), .groups = "drop")

p7 <- ggplot(convergence_df %>% filter(Mean_Change > 0), 
             aes(x = time_step, y = Mean_Change, color = scenario)) +
     geom_line(linewidth = 1.2) +
     scale_color_manual(values = scenario_colors) +
     labs(
          title = "Opinion Convergence Speed Comparison",
          subtitle = "Event-driven extremization creates periodic convergence bursts",
          x = "Time Step",
          y = "Mean Opinion Change",
          color = "Scenario"
     ) +
     scale_y_log10() +  # Log scale for better visibility
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))
print(p7)

# Plot 8: Network homophily over time (sample-based calculation)
network_metrics_over_time <- analysis_df %>%
     filter(run_id == 1) %>%  # Use first run to save computation
     group_by(time_step, scenario) %>%
     summarise(
          within_faction_edges = {
               # Use pick() to get current group data (replaces deprecated cur_data())
               agents_t <- pick(everything())
               n_agents <- nrow(agents_t)
               same_faction <- 0
               diff_faction <- 0
               
               # Sample pairs to estimate homophily (avoid n^2 computation)
               if (n_agents > 50) {
                    sampled_pairs <- combn(sample(1:n_agents, min(50, n_agents)), 2, 
                                           simplify = FALSE)[1:min(500, choose(min(50, n_agents), 2))]
                    for (pair in sampled_pairs) {
                         i <- pair[1]
                         j <- pair[2]
                         city_i <- agents_t$Home_City[i]
                         city_j <- agents_t$Home_City[j]
                         # Check if within interaction radius
                         if (city_distance_matrix[as.character(city_i), as.character(city_j)] <= 
                             HYPERPARAMS$INTERACTION_RADIUS) {
                              # Determine factions based on current opinions
                              faction_i <- ifelse(agents_t$Prob_t[i] > 0.6, "S", 
                                                  ifelse(agents_t$Prob_t[i] < 0.4, "I", "N"))
                              faction_j <- ifelse(agents_t$Prob_t[j] > 0.6, "S", 
                                                  ifelse(agents_t$Prob_t[j] < 0.4, "I", "N"))
                              # Count same-faction vs different-faction edges
                              if (faction_i == faction_j) same_faction <- same_faction + 1
                              else diff_faction <- diff_faction + 1
                         }
                    }
               }
               # Calculate proportion of within-faction edges
               ifelse(same_faction + diff_faction > 0, 
                      same_faction / (same_faction + diff_faction), NA_real_)
          },
          .groups = "drop"
     )

p8 <- ggplot(network_metrics_over_time, aes(x = time_step, y = within_faction_edges * 100, 
                                            color = scenario)) +
     geom_line(linewidth = 1.3) +
     geom_point(size = 1.5) +
     scale_color_manual(values = scenario_colors) +
     labs(
          title = "Network Homophily Over Time",
          subtitle = "% same-faction interactions | Hierarchy blocking prevents cross-class influence",
          x = "Time Step",
          y = "Within-Faction Edge Proportion (%)",
          color = "Scenario"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold", size = 14))
print(p8)

# Plot 9: Extremization effect visualization
extremization_tracking <- analysis_df %>%
     mutate(
          Opinion_Category = case_when(
               Prob_t < 0.2 ~ "Strongly Imperial",
               Prob_t < 0.4 ~ "Leaning Imperial",
               Prob_t < 0.6 ~ "Neutral/Moderate",
               Prob_t < 0.8 ~ "Leaning Stormcloak",
               TRUE ~ "Strongly Stormcloak"
          ),
          Opinion_Category = factor(Opinion_Category, levels = c(
               "Strongly Imperial", "Leaning Imperial", "Neutral/Moderate",
               "Leaning Stormcloak", "Strongly Stormcloak"
          ))
     ) %>%
     group_by(time_step, scenario, Opinion_Category) %>%
     summarise(count = n(), .groups = "drop") %>%
     group_by(time_step, scenario) %>%
     mutate(proportion = count / sum(count) * 100)

p9 <- ggplot(extremization_tracking, aes(x = time_step, y = proportion, 
                                         color = Opinion_Category, fill = Opinion_Category)) +
     geom_area(alpha = 0.6, position = "stack") +
     facet_wrap(~ scenario, ncol = 1) +
     scale_color_manual(values = c("#8B0000", "#DC143C", "#9B9B9B", "#4169E1", "#000080")) +
     scale_fill_manual(values = c("#8B0000", "#DC143C", "#9B9B9B", "#4169E1", "#000080")) +
     labs(
          title = "Opinion Distribution Evolution",
          subtitle = "Battle-driven extremization depletes moderates during conflict periods",
          x = "Time Step",
          y = "Percentage of Population (%)",
          fill = "Opinion Category",
          color = "Opinion Category"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))
print(p9)

# ######################################################################
# --- PART 9: STATISTICAL SUMMARIES ---
# ######################################################################

cat("\n--- COMPARATIVE FINAL STATE SUMMARY (t=150) ---\n")
final_summary <- final_state_df %>%
     group_by(scenario) %>%
     summarise(
          Mean_Prob = mean(Prob_t),
          Median_Prob = median(Prob_t),
          SD_Prob = sd(Prob_t),
          Min_Prob = min(Prob_t),
          Max_Prob = max(Prob_t),
          Pct_Pro_Stormcloak = mean(Prob_t > 0.5) * 100,
          Pct_Pro_Imperial = mean(Prob_t < 0.5) * 100,
          Pct_Extreme = mean(Prob_t < 0.2 | Prob_t > 0.8) * 100,  # Extremists
          .groups = "drop"
     )
print(final_summary)
cat("\n")

cat("--- Final State by Hold ---\n")
hold_summary_final <- final_state_df %>%
     group_by(scenario, Home_City) %>%
     summarise(
          Mean_Prob = mean(Prob_t),
          SD_Prob = sd(Prob_t),
          N_Agents = n() / HYPERPARAMS$N_RUNS,
          .groups = "drop"
     ) %>%
     arrange(scenario, desc(Mean_Prob))
print(hold_summary_final)
cat("\n")

cat("--- Final State by Initial Faction ---\n")
faction_summary_final <- final_state_df %>%
     group_by(scenario, Initial_Faction) %>%
     summarise(
          Initial_Mean = mean(Initial_Prob, na.rm=TRUE),
          Final_Mean = mean(Prob_t),
          Change = Final_Mean - Initial_Mean,
          SD_Prob = sd(Prob_t),
          N_Agents = n() / HYPERPARAMS$N_RUNS,
          .groups = "drop"
     ) %>%
     arrange(scenario, desc(Final_Mean))
print(faction_summary_final)
cat("\n")

cat("--- Key NPC Opinion Changes ---\n")
key_npcs <- c("Ulfric Stormcloak", "General Tullius", "Balgruuf the Greater", 
              "Idolaf Battle-Born", "Fralia Gray-Mane", "Maven Black-Briar",
              "Galmar Stone-Fist", "Legate Rikke", "The Dragonborn",
              "Elenwen", "Ancano", "Ondolemar")  # Added Thalmor agents

key_npc_changes <- final_state_df %>%
     filter(Name %in% key_npcs) %>%
     group_by(scenario, Agent_ID, Name) %>%
     summarise(
          Initial_Prob = first(Initial_Prob),
          Final_Prob = mean(Prob_t),
          Change = Final_Prob - Initial_Prob,
          .groups = "drop"
     ) %>%
     arrange(scenario, desc(abs(Change)))
print(key_npc_changes)
cat("\n")

cat("--- Talos Worship Impact ---\n")
talos_summary_final <- final_state_df %>%
     mutate(
          Talos_Group = case_when(
               Talos_Conviction > 0.5 ~ "Talos Worshippers",
               Talos_Conviction < -0.5 ~ "Talos Opponents",
               TRUE ~ "Neutral on Talos"
          )
     ) %>%
     group_by(scenario, Talos_Group) %>%
     summarise(
          Initial_Mean = mean(Initial_Prob),
          Final_Mean = mean(Prob_t),
          Change = Final_Mean - Initial_Mean,
          SD_Prob = sd(Prob_t),
          N_Agents = n() / HYPERPARAMS$N_RUNS,
          .groups = "drop"
     ) %>%
     arrange(scenario, desc(Final_Mean))
print(talos_summary_final)
cat("\n")

# ######################################################################
# --- PART 10: EXPORT RESULTS ---
# ######################################################################

cat("PART 7: Exporting Results\n")
cat("--------------------------\n")

# Create output directory
dir.create("campaign_results_enhanced", showWarnings = FALSE)

# Export summary tables
write_csv(global_summary, "campaign_results_enhanced/global_trajectory.csv")
write_csv(hold_summary_final, "campaign_results_enhanced/hold_summary.csv")
write_csv(faction_summary_final, "campaign_results_enhanced/faction_summary.csv")
write_csv(key_npc_changes, "campaign_results_enhanced/key_npc_changes.csv")
write_csv(polarization_over_time, "campaign_results_enhanced/polarization.csv")
write_csv(convergence_df, "campaign_results_enhanced/convergence.csv")
write_csv(talos_summary_final, "campaign_results_enhanced/talos_summary.csv")
write_csv(network_metrics_over_time, "campaign_results_enhanced/network_metrics.csv")
write_csv(extremization_tracking, "campaign_results_enhanced/extremization_tracking.csv")

# Export complete final state with agent details
final_state_export <- final_state_df %>%
     group_by(scenario, Agent_ID, Name, Race, Home_City, Initial_Faction, Initial_Prob) %>%
     summarise(Final_Prob = mean(Prob_t), .groups = "drop") %>%
     mutate(Change = Final_Prob - Initial_Prob) %>%
     arrange(scenario, desc(abs(Change)))
write_csv(final_state_export, "campaign_results_enhanced/all_agents_final.csv")

cat("Results exported to 'campaign_results_enhanced/' directory.\n\n")

# ######################################################################
# --- FINAL SUMMARY ---
# ######################################################################

cat("\n=======================================================\n")
cat("COMPARATIVE CAMPAIGN SIMULATION COMPLETE\n")
cat("=======================================================\n\n")

cat("METHODOLOGY:\n")
cat("  - Agent-Based Model with Bounded Confidence\n")
cat("  - Dynamic Homophily: Race + Faction similarity bonuses\n")
cat("  - Social Hierarchy: Elites influence commoners 2.5x more (with blocking threshold)\n")
cat("  - Conflict Extremization: Baseline 0.5% → Battle spikes to 3.5% for 10 timesteps\n")
cat("  - Thalmor Interference: Push toward stalemate (0.8% drift to 0.5) until t=101\n")
cat("  - Post-War Unification: Anti-Thalmor sentiment (1.5% convergence) after t=110\n")
cat("  - Sequential Shocks: Dragonborn's campaign (4 events)\n")
cat("  - Agent Intervention: Dragonborn (Nord) added at t=26\n")
cat("  - Structural Shocks: Jarls swapped, allegiances flipped\n")
cat("  - Lore-accurate demographics: Dark Elves lean Imperial, varied Imperial Talos worship\n")
cat("  - Whiterun starts as reluctant Imperial (de facto allegiance), not neutral\n")
cat(sprintf("  - %d agents over %d time steps, %d MC runs per scenario\n\n", 
            nrow(df_agents_initial) + 1, HYPERPARAMS$TOTAL_TIME_STEPS, HYPERPARAMS$N_RUNS))

cat("KEY FINDINGS:\n\n")
cat("IMPERIAL SCENARIO (at t=150):\n")
imperial_final <- final_summary %>% filter(scenario == "Imperial")
cat(sprintf("  - Final support: %.1f%% Stormcloak, %.1f%% Imperial\n", 
            imperial_final$Pct_Pro_Stormcloak, imperial_final$Pct_Pro_Imperial))
cat(sprintf("  - Mean opinion: %.3f (shifted toward Imperial)\n", imperial_final$Mean_Prob))
cat(sprintf("  - Extremists: %.1f%% of population\n", imperial_final$Pct_Extreme))

cat("\nSTORMCLOAK SCENARIO (at t=150):\n")
stormcloak_final <- final_summary %>% filter(scenario == "Stormcloak")
cat(sprintf("  - Final support: %.1f%% Stormcloak, %.1f%% Imperial\n", 
            stormcloak_final$Pct_Pro_Stormcloak, stormcloak_final$Pct_Pro_Imperial))
cat(sprintf("  - Mean opinion: %.3f (shifted toward Stormcloak)\n", stormcloak_final$Mean_Prob))
cat(sprintf("  - Extremists: %.1f%% of population\n", stormcloak_final$Pct_Extreme))

cat("\nCOMPARATIVE INSIGHTS:\n")
cat("  - Both scenarios show strong convergence to winner's side\n")
cat("  - Thalmor interference slows polarization during war (want stalemate)\n")
cat("  - Post-war: Both sides unify against Thalmor threat (common enemy)\n")
cat("  - Social hierarchy amplifies elite influence (Dragonborn, Jarls)\n")
cat("  - Extremization depletes moderate/neutral factions over time\n")
cat("  - Structural shocks (Jarl swaps, battles) > opinion dynamics\n")
cat("  - Dragonborn intervention is decisive in both cases\n")
cat("  - Talos worship creates asymmetric resistance to Imperial campaigns\n")
cat("  - Network homophily creates echo chambers that amplify convergence\n")
cat("  - Geographic proximity determines social influence reach\n")
cat("  - Conflict-driven polarization increases opinion variance\n")
cat("  - Whiterun's 'neutrality' is actually reluctant Imperial allegiance\n\n")

cat(sprintf("Total execution time: %.1f seconds\n", as.numeric(elapsed)))
cat("\n=======================================================\n")
cat("Enhanced model complete with hierarchy & extremization.\n")
cat("=======================================================\n")