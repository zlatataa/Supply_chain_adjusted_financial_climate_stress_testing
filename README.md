# financial_climate_stress_testing


This repository contains scripts that implement the methodology presented in manuscript by Tabachova et.al. "Combined climate stress testing of supply-chain networks and the financial system" (2025). These scripts demonstrate key techniques and concepts from the paper, providing a practical example for further exploration and replication. Datasets used in the paper are confidential, hence we demonstrate the methodology on dummy datasets.

CST_toy_model.R: contains data preparation, methods and plotting of the results 

functions_toy_model_CST.R: contains functions used in CST_toy_model.R to calculate emissions, defaults and financial losses

functions_supply_chain_contagion.R: installs libraries used for supply chain contagion and prepares data 

climate_sector_maps.csv: is a concordance table of industry sectors and climate policy relevant sectors by S.Battiston et.al. (from https://www.df.uzh.ch/en/people/professor/battiston/projects/CPRS.html)
