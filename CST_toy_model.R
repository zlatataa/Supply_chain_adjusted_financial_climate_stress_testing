# Load required libraries
library(Matrix)
library(data.table)
library(igraph)
library(colorspace)
library(parallel)

# Set working directory

# put your path here
path <- "https://github.com/zlatataa/financial_climate_stress_testing/raw/refs/heads/main"  # change to final path of public github
path <- "C:/Users/CD/Documents/GitHub/financial_climate_stress_testing"

# Load custom functions for calculations
source(paste0(path, "/functions_toy_model_CST.R"))  # Functions for defaults and financial losses

##############################
###### DATA PREPARATION ######
##############################


# Firm network: edge list with supply chain links
# Firm list: data table with firm characteristics
# - id
# - out strength
# - nace4 (industry code)
# - financial variables: net profit, operational profit, retained earnings, equity, liquidity
# - reported emissions of ETS 1 participants

# Banks equity: vector of bank equities
# Firm-bank loans: matrix of firms x banks with loan amounts

# Define oil and gas NACE4 lists
oil_nace4_list <- c(4671, 4730, 610, 1920)
gas_nace4_list <- c(3523, 3521, 3522)


# Firms
n <- 15  # Number of firms

# Create firm list
firm_list <- data.table(1:n)
colnames(firm_list) <- c("id")
firm_list$nace4 <- c(1920, 3523, 2410, 710, 4221, 710, 4221, 5221, 5221, 4531, 4221, 4334, 4612, 710, 4612)
firm_list$nace2 <- trunc(firm_list$nace4 / 100)

# Financial variables (values in 1000 HUF)
firm_list$net_profit <- c(1000, 400, 1000, 200, 150, 40, 900, 710, 2500, 300, 500, 150, 100, 50, 60)
firm_list$operational_profit <- c(1200, 450, 1120, 300, 660, 80, 1100, 900, 2700, 190, 700, 80, 20, 5, 10)
firm_list$retained_earnings <- c(200, 100, 50, 40, 10, 10, 50, 10, 10, 10, 20, 10, 10, 10, 10)
firm_list$equity <- c(500, 200, 300, 200, 100, 100, 50, 100, 50, -100, 200, 80, 60, 50, 50)
firm_list$liquidity <- c(300, 100, 300, 100, 80, 150, 80, 50, 60, 200, 150, 100, 50, 60, 0)

# Add a filtering column based on financial reporting variables of firms that is 1 if all variables are reported positively needed for Step 3 in the methods section
firm_list[, filtrating := 0]
firm_list[net_profit >= 0 & operational_profit >= 0 & equity >= 0 & liquidity >= 0, filtrating := 1]

# Reported emissions (ETS 1) in tonnes of CO2 equivalent
firm_list$emissions_reported <- c(93, NA, NA, NA, NA, NA, NA, 47, NA, NA, NA, NA, NA, NA, NA)


# Edge list: (supplier id, buyer id, link weight)
el <- rbind(
  c(1, 8, 300),
  c(1, 4, 300),
  c(1, 6, 300),
  c(1, 9, 500),
  c(1, 3, 500), 
  c(2, 1, 1000),
  c(2, 10, 500),
  c(2, 7, 400),
  c(2, 3, 250),
  c(4, 2, 400),
  c(4, 3, 1000),
  c(4, 6, 600),
  c(5, 3, 1000),
  c(5, 7, 500),
  c(7, 9, 300),
  c(7, 11, 200),
  c(9, 10, 600),
  c(12, 8, 100),
  c(12, 11, 100),
  c(13, 2, 500),
  c(14, 11, 200),
  c(14, 12, 400),
  c(14, 13, 650),
  c(15, 1, 100)
)

firm_network <- data.table(el)
colnames(firm_network) <- c("supplier", "buyer", "weight")

# Assign NACE4 codes from firm_list to buyers and suppliers
firm_network <- merge(firm_network, firm_list[, .(id, buyer_nace4 = nace4)], by.x = "buyer", by.y = "id")
firm_network <- merge(firm_network, firm_list[, .(id, supplier_nace4 = nace4)], by.x = "supplier", by.y = "id")

# Adjacency matrix of the supply chain network (SCN)
W <- Matrix::sparseMatrix(i = el[, 1], j = el[, 2], x = el[, 3], dims = c(n, n))

# Calculate out-strength of firms
firm_list$out_strength <- rowSums(W)

# Banks
# Number of banks
t <- 4
equity_banks <- c(700, 100, 100, 700)  # Equity in 1000 HUF

# Firm-bank links with outstanding principal
bl <- rbind(
  c(1, 2, 100),
  c(1, 3, 50),
  c(1, 5, 50), 
  c(1, 6, 100), 
  c(2, 3, 60),
  c(2, 7, 40),
  c(2, 9, 10),
  c(3, 10, 20),
  c(3, 8, 30), 
  c(3, 15, 30), 
  c(4, 10, 10),
  c(4, 8, 30),
  c(4, 9, 200),
  c(4, 11, 10),
  c(4, 12, 30)
)

# Firm x bank matrix with loans
bank_firm_matrix_out <- Matrix::sparseMatrix(i = bl[, 2], j = bl[, 1], x = bl[, 3], dims = c(n, t))

# Firm x bank matrix weighted by bank equities
loan_fb_matrix_bank_equities <- t(t(bank_firm_matrix_out) / equity_banks)





#####################################
### Plot Combined Firm-Bank Network ####
#####################################

# Define the number of firms and banks
n_firms <- nrow(W)  # Number of firms
n_banks <- length(equity_banks)  # Number of banks

# Combine firm and bank matrices to create an adjacency matrix for the full network
WL <- rbind(
  cbind(W, bank_firm_matrix_out),  # Firms to banks
  matrix(0, n_banks, n_firms + n_banks)  # Padding for banks
)

# Create a network graph for the combined adjacency matrix
WL_net <- igraph::graph_from_adjacency_matrix(as.matrix(WL), weighted = TRUE)

# Set node size: firms and banks have different sizes
V(WL_net)$size <- c(
  2 * sqrt(sqrt(rowSums(W) + colSums(W) + 1) )+ 1,  # Firm sizes
  sqrt(equity_banks)  # Bank sizes
)

# Map sectors to colors (reuse from the firm network)
sector_numbers <- seq_along(unique(firm_list$nace2))  # Unique sector codes
names(sector_numbers) <- as.character(unique(firm_list$nace2))

V(WL_net)$color <- c(
  colorspace::qualitative_hcl(
    length(sector_numbers), palette = "Dark 3"
  )[sector_numbers[as.character(firm_list$nace2)]],
  rep("gray", n_banks)  # Banks are gray
)

# Define the shape for each node: firms are circles, banks are squares
V(WL_net)$shape <- c(
  rep("circle", vcount(WL_net) - n_banks),  # Firms
  rep("square", n_banks)  # Banks
)

# Assign labels to nodes: firms as F1, F2,... and banks as B1, B2,...
V(WL_net)$label <- c(
  paste0("F", seq_len(n_firms)),  # Firm labels
  paste0("B", seq_len(n_banks))  # Bank labels
)

# Customize edge appearance
# Edge line type: dashed for links to banks
E(WL_net)$lty <- ifelse(
  head_of(WL_net, E(WL_net)) > n_firms, "dashed", "solid"
)

# Edge width for better visibility
E(WL_net)$width <- 2

# Edge color: black for firm-to-firm links, gray for links to banks
E(WL_net)$color <- ifelse(
  head_of(WL_net, E(WL_net)) <= n_firms & tail_of(WL_net, E(WL_net)) <= n_firms, 
  "black",  # Firm-to-firm links
  "gray"  # Links to banks
)

# Customize node borders
V(WL_net)$frame.color <- "black"  # Node border color
V(WL_net)$frame.width <- 2  # Slightly thicker border width

# Update node labels: bold and black for nodes with emissions
V(WL_net)$label.color <- c(
  ifelse(firm_list$nace4 %in% union(oil_nace4_list, gas_nace4_list), "white", "black"),  # white for firms with emissions
  rep("black", n_banks)  # Keep bank labels black
)

# Set bold labels for nodes with emissions
V(WL_net)$label.font <- c(
  ifelse(firm_list$nace4 %in% union(oil_nace4_list, gas_nace4_list), 2, 1),  # Bold (2) for emissions, normal (1) otherwise
  rep(1, n_banks)  # Banks have normal font
)

# Plot the combined firm-bank network
set.seed(100)  # For reproducibility
plot(WL_net, edge.arrow.size = 0.5)







#####################################
#### STEP 1: EMISSIONS ESTIMATION ####
#####################################

# Define oil and gas NACE4 lists
oil_nace4_list <- c(4671, 4730, 610, 1920)
gas_nace4_list <- c(3523, 3521, 3522)

# Emissions of the commercial sector in tonnes of CO2 equivalent
emissions_oil <- 300
emissions_gas <- 200

# Estimate emissions for firms
firm_list <- emissions_estimation(
  firm_network = firm_network,
  firm_list = firm_list,
  emissions_gas = emissions_gas,
  emissions_oil = emissions_oil
)






#############################################################
#### STEP 2: CARBON COSTS (WITH AND WITHOUT PASS-THROUGH) ####
#############################################################

# Calculate market shares
market_shares <- market_share(firm_list = firm_list)

# Define carbon prices (in EUR)
carbon_prices <- seq(10, 100, by = 10)
carbon_prices <- append(carbon_prices, 45, after = 4)


# Emissions vector
emissions <- firm_list$dirty_emission
# Overwriting estimated emissions of ETS I firms by 0
# (we are stress testing only ETS II emitters)
emissions[firm_list$ETS_participant==1]<-0

# Weighted adjacency matrix
rs<-rowSums(W)
out_str_inv<-1/rs
out_str_inv[is.infinite(out_str_inv)]<-0

W_w<-W*out_str_inv

# Carbon cost redistribution with pass-through
redistr_costs <- distribute_costs(
  W_w = W_w,
  emissions = emissions,
  carbon_prices = carbon_prices,
  exch_rate = 400,
  market_shares = market_shares
)

# Carbon costs without pass-through
carbon_costs_np <- matrix(0, nrow = length(firm_list$equity), ncol = length(carbon_prices))
for (i in 1:length(carbon_prices)) {
  carbon_costs_np[, i] <- emissions * 400 * carbon_prices[i] / 1000
}






################################
#### STEP 3: DIRECT DEFAULTS ####
################################

# Prepare and clean financial data
fin_data <- prepare_fin_data(firm_list = firm_list)

# Initial defaults with pass-through
init_defs_pt <- initial_defaults(
  carbon_costs = redistr_costs$costs_distr,
  fs_profit_inv = fin_data$fs_profit_inv
)

# Initial defaults without pass-through
init_defs <- initial_defaults(
  carbon_costs = carbon_costs_np,
  fs_profit_inv = fin_data$fs_profit_inv
)

# Initial shock matrix with pass-through
psi_pt <- Matrix(init_defs_pt$initial_defaults)

# Initial shock matrix without pass-through
psi <- Matrix(init_defs$initial_defaults)





###############################################
#### STEP 4: SUPPLY CHAIN NETWORK CONTAGION ####
###############################################

### install shock propagation packages and create input matrices for the GL_cascade( ) function
source(paste0(path, "/functions_supply_chain_contagion.R"))  # Functions for production contagion



# GL Production Function: With Pass-Through
production_losses_GL_pt <- GL_cascade(
  W = W,
  p = p,
  p_market = p,
  ess_mat_sec = ess_mat_sec_gl,
  h_weights = FALSE,
  psi_mat = psi_pt,
  track_h = TRUE,
  track_conv = TRUE,
  conv_type = 1,
  eps = 1e-2,
  use_rcpp = FALSE,
  ncores = 0,
  run_id = "pessimistic_cascade"
)

# Access simulation results (with pass-through):
# - Losses of individual firms
production_losses_GL_pt$hd_T_mat
production_losses_GL_pt$hu_T_mat


# - Network-wide losses
production_losses_GL_pt$ESRI

# Linear Production Function: With Pass-Through
production_losses_lin_pt <- GL_cascade(
  W = W,
  p = p,
  p_market = p,
  ess_mat_sec = ess_mat_sec_lin,
  h_weights = FALSE,
  psi_mat = psi_pt,
  track_h = TRUE,
  track_conv = TRUE,
  conv_type = 1,
  eps = 1e-2,
  use_rcpp = FALSE,
  ncores = 0,
  run_id = "pessimistic_cascade"
)

# Access simulation results (with pass-through):
# - Losses of individual firms
production_losses_lin_pt$hd_T_mat
production_losses_lin_pt$hu_T_mat


# - Network-wide losses
production_losses_lin_pt$ESRI

# Pessimistic scenario: Max production losses after SCN contagion
H_ps_pt <- pmax(production_losses_GL_pt$hd_T_mat, production_losses_GL_pt$hu_T_mat)

# Optimistic scenario: Max production losses after SCN contagion
H_os_pt <- pmax(production_losses_lin_pt$hd_T_mat, production_losses_lin_pt$hu_T_mat)

# GL Production Function: Without Pass-Through
production_losses_GL <- GL_cascade(
  W = W,
  p = p,
  p_market = p,
  ess_mat_sec = ess_mat_sec_gl,
  h_weights = FALSE,
  psi_mat = psi,
  track_h = TRUE,
  track_conv = TRUE,
  conv_type = 1,
  eps = 1e-2,
  use_rcpp = FALSE,
  ncores = 0,
  run_id = "pessimistic_cascade"
)

# Access simulation results (without pass-through):
# - Losses of individual firms
production_losses_GL$hd_T_mat

# - Network-wide losses
production_losses_GL$ESRI

# Linear Production Function: Without Pass-Through
production_losses_lin <- GL_cascade(
  W = W,
  p = p,
  p_market = p,
  ess_mat_sec = ess_mat_sec_lin,
  h_weights = FALSE,
  psi_mat = psi,
  track_h = TRUE,
  track_conv = TRUE,
  conv_type = 1,
  eps = 1e-2,
  use_rcpp = FALSE,
  ncores = 0,
  run_id = "pessimistic_cascade"
)

# Access simulation results (without pass-through):
# - Losses of individual firms
production_losses_lin$hd_T_mat

# - Network-wide losses
production_losses_lin$ESRI

# Pessimistic scenario: Max production losses after SCN contagion
H_ps <- pmax(production_losses_GL$hd_T_mat, production_losses_GL$hu_T_mat)

# Optimistic scenario: Max production losses after SCN contagion
H_os <- pmax(production_losses_lin$hd_T_mat, production_losses_lin$hu_T_mat)





###############################################################################
# Visualization: Losses per Carbon Price
###############################################################################

# Plot ESRI losses against carbon prices
plot(NA, xlim = range(carbon_prices), ylim = c(0, 1), type = "n", 
     xlab = "Carbon Prices", ylab = "ESRI",
     main = "Production Losses: LIN vs. GL")

# Add points for GL (red triangles)
points(carbon_prices, production_losses_GL$ESRI[, 1], col = "red", pch = 17)

# Add points for LIN (blue circles)
points(carbon_prices, production_losses_lin$ESRI[, 1], col = "blue", pch = 16)

# Add points for GL with pass-through (green squares)
points(carbon_prices, production_losses_GL_pt$ESRI[, 1], col = "green", pch = 15)

# Add points for LIN with pass-through (black cross)
points(carbon_prices, production_losses_lin_pt$ESRI[, 1], col = "black", pch = 3)

# Add a legend
legend("bottomright", legend = c("GL", "LIN", "GL with pass-through", "LIN with pass-through"), 
       col = c("red", "blue", "green", "black"), pch = c(17, 16, 15, 3))




# note that in the example upstream losses dominate, such that optimistic and pessimistic yield the same result

##################################################
#### STEP 5: INDIRECT DEFAULTS AND BANK LOSSES ####
##################################################

# Pessimistic scenario with pass-through
financial_losses_ps_pt <- financial_shock_carbon(
  H = H_ps_pt,
  initial_defaults = psi_pt,
  profit = fin_data$profit,    
  eq_inv = fin_data$eq_inv, 
  liq_inv = fin_data$liq_inv,
  param_eq = 1,
  param_liq = 1,
  lgd = 1,
  loan_fb_matrix_bank_equities = loan_fb_matrix_bank_equities,
  bank_equities = equity_banks,
  C = redistr_costs$costs_distr
)

# Optimistic scenario with pass-through
financial_losses_os_pt <- financial_shock_carbon(
  H = H_os_pt,
  initial_defaults = psi_pt,
  profit = fin_data$profit,    
  eq_inv = fin_data$eq_inv, 
  liq_inv = fin_data$liq_inv,
  param_eq = 1,
  param_liq = 1,
  lgd = 1,
  loan_fb_matrix_bank_equities = loan_fb_matrix_bank_equities,
  bank_equities = equity_banks,
  C = redistr_costs$costs_distr
)

# Pessimistic scenario without pass-through
financial_losses_ps <- financial_shock_carbon(
  H = H_ps,
  initial_defaults = psi,
  profit = fin_data$profit,    
  eq_inv = fin_data$eq_inv, 
  liq_inv = fin_data$liq_inv,
  param_eq = 1,
  param_liq = 1,
  lgd = 1,
  loan_fb_matrix_bank_equities = loan_fb_matrix_bank_equities,
  bank_equities = equity_banks,
  C = redistr_costs$costs_distr
)

# Optimistic scenario without pass-through
financial_losses_os <- financial_shock_carbon(
  H = H_os,
  initial_defaults = psi_pt,
  profit = fin_data$profit,    
  eq_inv = fin_data$eq_inv, 
  liq_inv = fin_data$liq_inv,
  param_eq = 1,
  param_liq = 1,
  lgd = 1,
  loan_fb_matrix_bank_equities = loan_fb_matrix_bank_equities,
  bank_equities = equity_banks,
  C = redistr_costs$costs_distr
)




####################################
#### PLOTS as in the manuscript ####
####################################

##############
### Fig 1a ####
##############

## Data Preparation

# Load the table with NACE 1-4 industry categories mapped to CPRS classification
sectors <- read.csv2(paste0(path, "/climate_sector_maps.csv"), sep = ',')


# Create a vector of NACE 1 codes for firms in the SCN network
firm_list_isic <- get_nace1(sectors = sectors, firm_list = firm_list)

# Aggregate ETS 1 reported emissions across NACE 1 sectors
emissions_reported <- firm_list$emissions_reported
emissions_reported[is.na(emissions_reported)] <- 0
emis <- as.data.table(emissions_reported)
emis <- cbind(emis, firm_list_isic)
emis <- emis[, lapply(.SD, sum, na.rm = TRUE), by = firm_list_isic]
emis <- emis[order(firm_list_isic)]

# Aggregate ETS 1 reported emissions and estimated emissions across NACE 1 sectors
emissions_shock_all <- firm_list$dirty_emission
emissions_shock_all[emissions_reported > 0] <- emissions_reported[emissions_reported > 0]
emis_all <- as.data.table(emissions_shock_all)
emis_all <- cbind(emis_all, firm_list_isic)
emis_all <- emis_all[, lapply(.SD, sum, na.rm = TRUE), by = firm_list_isic]
emis_all <- emis_all[order(firm_list_isic)]

### Plot
barplot(emis_all$emissions_shock,
        names.arg = unique(emis_all$firm_list_isic),
        col = ohra, xlab = 'Industry Sector (NACE 1)', ylab = 'CO2 Emissions in Sector [t]',
        cex.lab = 1.8, cex.axis = 1.8, ylim = c(0, max(emis_all$emissions_shock)))

barplot(emis$emissions_reported,
        names.arg = unique(emis_all$firm_list_isic), add = TRUE,
        col = bluenorm, xlab = 'Industry Sector (NACE 1)', ylab = 'CO2 Emissions in Sector [t]',
        cex.lab = 1.8, cex.axis = 1.8, ylim = c(0, max(emis_all$emissions_shock)))

legend('topright', 
       legend = c(paste0('ETS II, newly estimated ', sum(emis_all$emissions_shock) - sum(emis$emissions_reported), 't'),
                  paste0('ETS I, reported ', sum(emis$emissions_reported), 't')
       ),
       cex = 1.2, fill = c(ohra, bluenorm),  
       box.lwd = 0, box.col = "white", bg = "white")



##############
### Fig 1b ####
##############

## Data Preparation
out_str_w <- firm_list$out_str / sum(firm_list$out_str)

# Aggregate production of ETS 1 firms
ind_emt_old <- !is.na(firm_list$emissions_reported)
em_out <- as.data.table(as.matrix(ind_emt_old * out_str_w))
em_out <- cbind(em_out, firm_list_isic)
em_out <- em_out[, lapply(.SD, sum, na.rm = TRUE), by = firm_list_isic]
em_out <- em_out[order(firm_list_isic)]

emissions_shock_all <- firm_list$dirty_emission
emissions_shock_all[emissions_reported > 0] <- emissions_reported[emissions_reported > 0]
ind_emt_no <- emissions_shock_all == 0

# Aggregate production of non-emitters across NACE 1
em_out_no <- as.data.table(as.matrix(ind_emt_no * out_str_w))
em_out_no <- cbind(em_out_no, firm_list_isic)
em_out_no <- em_out_no[, lapply(.SD, sum, na.rm = TRUE), by = firm_list_isic]
em_out_no <- em_out_no[order(firm_list_isic)]

# Aggregate production of all firms across NACE 1
out_nace1_dt <- as.data.table(out_str_w)
out_nace1_dt <- cbind(out_nace1_dt, firm_list_isic)
out_nace1_dt <- out_nace1_dt[, lapply(.SD, sum, na.rm = TRUE), by = firm_list_isic]
out_nace1_dt <- out_nace1_dt[order(firm_list_isic)]

### Plot
barplot(out_nace1_dt$out_str_w,
        names.arg = unique(out_nace1_dt$firm_list_isic),
        col = ohra,
        ylim = c(0, 0.4), xlab = 'Industry Sector (NACE 1)', ylab = 'SCN Sales in Sector [%]',
        cex.lab = 1.8, cex.axis = 1.8)

barplot(em_out$V1 + em_out_no$V1,
        col = bluenorm,
        add = TRUE, ylim = c(0, 0.4), xlab = 'Industry Sector (NACE 1)', ylab = 'SCN Sales in Sector [%]',
        cex.lab = 1.8, cex.axis = 1.8)

barplot(em_out_no$V1,
        col = "white",
        add = TRUE, ylim = c(0, 0.4), xlab = 'Industry Sector (NACE 1)', ylab = 'SCN Sales in Sector [%]',
        cex.lab = 1.8, cex.axis = 1.8)

legend('topright', 
       legend = c('ETS II Emitters',
                  'ETS I Emitters',
                  'Non-Emitters'),
       cex = 1.2, fill = c(ohra, bluenorm, 'white'), 
       box.lwd = 0, box.col = "white", bg = "white")




##############
### Fig 2a ####
##############

## Data Preparation
out_str_w <- firm_list$out_strength / sum(firm_list$out_strength)

# Direct production losses from all defaulted firms with pass-through
prod_dir_pt <- colSums(psi_pt * out_str_w)

# Vector of firms with loans
with_loan <- rowSums(bank_firm_matrix_out) > 0

# Direct production losses from defaulted firms with loans with pass-through
prod_dir_wl_pt <- colSums(psi_pt * out_str_w * with_loan)

# Direct production losses from all defaulted firms without pass-through
prod_dir <- colSums(psi * out_str_w)

# Direct production losses from defaulted firms with loans without pass-through
prod_dir_wl <- colSums(psi * out_str_w * with_loan)

### Plot
cex_main <- 1.8
plot(carbon_prices, prod_dir_pt, ylim = c(0, 1),
     col = rednorm, type = 'l', lwd = 4,
     xlab = 'Carbon Price (EUR/t)', ylab = "Production Loss", 
     cex.lab = cex_main, cex.axis = cex_main, lty = 'dashed')
lines(x = c(45, 45), y = c(0, 1), lty = 'dashed', lwd = 2)
text(45, 0.5, '45 EUR Price Cap', cex = 1.3)
points(carbon_prices, prod_dir_wl_pt, col = rednorm, type = 'l', lwd = 4, lty = 'solid')
points(carbon_prices, prod_dir, col = violet, type = 'l', lwd = 4, lty = 'dashed')
points(carbon_prices, prod_dir_wl, col = violet, type = 'l', lwd = 4, lty = 'solid')

legend("topleft", 
       legend = c('With Costs Pass-Through',
                  "Without Costs Pass-Through"),
       box.lwd = 0, cex = 1.3, box.col = "white", bg = "white",
       fill = c(rednorm, violet))

legend("topright", 
       legend = c('All Defaulted Firms',
                  "Defaulted Firms with Loans"),
       box.lwd = 0, cex = 1.3, box.col = "white", bg = "white",
       lwd = 4, lty = c('dashed', 'solid'))




##############
### Fig 2b ####
##############

## Data Preparation
banks_norm <- equity_banks / sum(equity_banks)

# Direct financial system losses normalized
dir_fsri <- colSums(t(financial_losses_ps_pt$loss_dir) * banks_norm)

### Plot
cex_main <- 1.8

plot(carbon_prices, financial_losses_ps_pt$loss_dir[, 1],
     ylim = c(0, 1), col = black, type = 'l', lwd = 2,
     xlab = 'Carbon Price (EUR/t)', ylab = 'Loan Write-Off / Bank Equity', 
     cex.lab = cex_main, cex.axis = cex_main)

# Add individual bank losses
for (i in 2:ncol(financial_losses_ps_pt$loss_dir)) {
  points(carbon_prices, financial_losses_ps_pt$loss_dir[, i], col = black, type = 'l', lwd = 2)
}

# Add mean and system-wide losses
points(carbon_prices, rowMeans(financial_losses_ps_pt$loss_dir), col = green, lwd = 4, type = 'l')
points(carbon_prices, dir_fsri, type = 'l', lwd = 4, col = bluenorm, lty = "dashed")

legend('topleft', 
       legend = c('Single Bank Losses',
                  'System-Wide Losses',
                  'Average Over Single Banks'),
       box.lwd = 0, cex = 1.3, box.col = "white", bg = "white",
       col = c(black, bluenorm, green), lwd = 4, lty = c("solid", "dashed", "solid"))



###############################
### Fig 3 with pass-through ####
###############################

## Data Preparation
out_str_w <- firm_list$out_strength / sum(firm_list$out_strength)

# Direct production losses with and without SCN contagion
prod_dir_pt <- colSums(psi_pt * out_str_w)
prod_pt_ps <- colSums(H_ps_pt * firm_list$out_strength) / sum(firm_list$out_strength)  # Pessimistic (Leontief) same as production_losses_GL_pt$ESRI[,1]
prod_pt_os <- colSums(H_os_pt * firm_list$out_strength) / sum(firm_list$out_strength)  # Optimistic (Linear)

banks_norm <- equity_banks / sum(equity_banks)

# Direct financial losses
dir_fsri_pt <- colSums(t(financial_losses_ps_pt$loss_dir) * banks_norm)

### Plot
plot(carbon_prices, prod_pt_ps, type = 'l', lwd = 3, ylim = c(0, 1), col = rednorm,
     xlab = 'Carbon Price (EUR/t)', ylab = 'Losses [%]', cex.lab = 1.8, cex.axis = 1.8)

# Add cap annotation
text(40, 0.6, '45 EUR Price Cap', cex = 1.3)
lines(x = c(45, 45), y = c(0, 1), lty = 'dashed', lwd = 2)

# Add data lines
points(carbon_prices, prod_dir_pt, type = 'l', lwd = 4, col = rednorm, lty = 'dotted')
points(carbon_prices, prod_pt_os, type = 'l', lwd = 4, col = rednorm, lty = 'twodash')

# financial losses
points(carbon_prices, dir_fsri_pt, type = 'l', lwd = 4, col = bluenorm, lty = "dotted")
points(carbon_prices, financial_losses_ps_pt$fsri_either, type = 'l', lwd = 4, col = bluenorm, lty = 'solid')
points(carbon_prices, financial_losses_os_pt$fsri_either, type = 'l', lwd = 4, col = bluenorm, lty = 'twodash')


# Legends
legend('topleft', 
       legend = c('Contagion Loss - Pessimistic',
                  'Contagion Loss - Optimistic',
                  'Direct Loss'),
       lty = c('solid', 'twodash', 'dotted'), lwd = 3,
       box.lwd = 0, cex = 1.3, box.col = "white", bg = "white")

legend('topright', 
       legend = c('SCN Production',
                  'Banks Equity'),
       fill = c(rednorm, bluenorm),
       box.lwd = 0, cex = 1.3, box.col = "white", bg = "white")




###############################
### Fig 3 without pass-through ####
###############################

## Data Preparation
out_str_w <- firm_list$out_strength / sum(firm_list$out_strength)

# Direct production losses with and without SCN contagion
prod_dir <- colSums(psi * out_str_w)
prod_ps <- colSums(H_ps * firm_list$out_strength) / sum(firm_list$out_strength)  # Pessimistic (Leontief)
prod_os <- colSums(H_os * firm_list$out_strength) / sum(firm_list$out_strength)  # Optimistic (Linear)

banks_norm <- equity_banks / sum(equity_banks)

# Direct financial losses
dir_fsri <- colSums(t(financial_losses_ps$loss_dir) * banks_norm)

### Plot
plot(carbon_prices, prod_ps, type = 'l', lwd = 3, ylim = c(0, 1), col = rednorm,
     xlab = 'Carbon Price (EUR/t)', ylab = 'Losses [%]', cex.lab = 1.8, cex.axis = 1.8)

# Add cap annotation
text(40, 0.6, '45 EUR Price Cap', cex = 1.3)
lines(x = c(45, 45), y = c(0, 1), lty = 'dashed', lwd = 2)

# Add data lines
points(carbon_prices, prod_dir, type = 'l', lwd = 4, col = rednorm, lty = 'dotted')
points(carbon_prices, prod_os, type = 'l', lwd = 4, col = rednorm, lty = 'twodash')

# financial losses
points(carbon_prices, dir_fsri, type = 'l', lwd = 4, col = bluenorm, lty = "dotted")
points(carbon_prices, financial_losses_ps$fsri_either, type = 'l', lwd = 4, col = bluenorm, lty = 'solid')
points(carbon_prices, financial_losses_os$fsri_either, type = 'l', lwd = 4, col = bluenorm, lty = 'twodash')

# Legends
legend('topleft', 
       legend = c('Contagion Loss - Pessimistic',
                  'Contagion Loss - Optimistic',
                  'Direct Loss'),
       lty = c('solid', 'twodash', 'dotted'), lwd = 3,
       box.lwd = 0, cex = 1.3, box.col = "white", bg = "white")

legend('topright', 
       legend = c('SCN Production',
                  'Banks Equity'),
       fill = c(rednorm, bluenorm),
       box.lwd = 0, cex = 1.3, box.col = "white", bg = "white")




########################
#### FINAL COMMENTS ####
########################
# Ensure that additional functions (e.g., emissions_estimation, GL_cascade, financial_shock_carbon) 
# are implemented and tested before running this full workflow.

# Customize plots and adjust analysis as needed for specific research questions or datasets.
