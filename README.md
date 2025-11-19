Skyrim Political Unification Model

An Agent-Based Model (ABM) built in R to simulate the civil war in The Elder Scrolls V: Skyrim. This project uses a "digital laboratory" approach to test how a polarized society unifies.



📊 Project Scale

Agents: 1,013 unique NPCs (1,009 Named NPCs from base game + 3 Named Thalmor + 1 Dragonborn).



Simulation: 50 Monte Carlo runs per scenario over 150 timesteps.



Total Dataset: 3,140,300 unique observations tracking political opinion dynamics.



🚀 Key Findings

The simulation ran an A/B test of two unification strategies: an Imperial "pragmatic" victory vs. a Stormcloak "ideological" victory.



The War is a Stalemate: Neither the Imperial nor Stormcloak campaigns could successfully unify the population on their own. The "sacred value" conflicts kept the society polarized.



The "Common Enemy" Effect: The only variable that achieved >99% consensus was the introduction of the "Thalmor Threat" (a common enemy) at timestep t=110. This external threat proved to be a stronger unifier than any internal political ideology.



💻 Tech Stack

Language: R



Modeling Approach: Stochastic Agent-Based Model (ABM)



Key Libraries: dplyr, ggplot2 (for visualization), tidyr

