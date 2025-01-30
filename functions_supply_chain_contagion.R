#==============================================================================#
#########################  Supply Chain Contagion ##############################
#==============================================================================#
# This section calculates production losses from supply chain contagion using 
# the GLcascade and fastcascade R packages. It also includes data preparation 
# and visualization of results for different scenarios.
# for details on shock propagation see: 
# Diem, C., Borsos, A., Reisch, T., Kertész, J., & Thurner, S. (2022). Quantifying firm-level economic systemic risk from nation-wide supply networks. Scientific reports, 12(1), 7719.
# Diem, C., Borsos, A., Reisch, T., Kertész, J., & Thurner, S. (2024). Estimating the loss of economic predictability from aggregating firm-level production networks. PNAS nexus, 3(3), pgae064.
#==============================================================================#

#-------------------------------------------------------------------------------#
# Download and Install Required Packages
#-------------------------------------------------------------------------------#
# Set paths to the .zip or .tar.gz files (hosted on GitHub)
path_sc <- "https://github.com/ch-diem/misestimation_from_aggregation/raw/refs/heads/main"

# Dynamically detect the operating system
os_type <- ifelse(.Platform$OS.type == "windows", "win", "unix")

# Install the required packages based on the operating system
if (os_type == "win") {
  install.packages(paste0(path_sc, "/fastcascade_0.9.3.1.zip"), 
                   repos = NULL, type = "win.binary")
  install.packages(paste0(path_sc, "/GLcascade_0.9.3.1.zip"), 
                   repos = NULL, type = "win.binary")
}

if (os_type == "unix") {
  install.packages(paste0(path_sc, "/fastcascade_0.9.3.1.tar.gz"), 
                   repos = NULL, type = "source")
  install.packages(paste0(path_sc, "/GLcascade_0.9.3.1.tar.gz"), 
                   repos = NULL, type = "source")
}

#-------------------------------------------------------------------------------#
# Load Installed Packages
#-------------------------------------------------------------------------------#
library(GLcascade)
library(fastcascade)

# Documentation lookup for the GLcascade package
??GLcascade


#-------------------------------------------------------------------------------#
# Data Preparation for Scenarios
#-------------------------------------------------------------------------------#

# Ensure that nace4 has leading zeros where possible, 
# as the sectors need to match with row and column names of ess_mat_sec_n4_ihs
p <- sprintf("%04d", as.numeric(firm_list$nace4))


# Extract unique NACE 4-digit sectors for data preparation
sectors <- sort(unique(p))
m <- length(unique(p))


#-------------------------------------------------------------------------------#
# Essentiality Matrices
#-------------------------------------------------------------------------------#

# Essentiality Matrix for GL Production Function:
# - Element (k, l) in the matrix can take the following values:
#   0: Input from sector k is negligible for production in sector l.
#   1: Input from sector k is non-essential for production in sector l.
#   2: Input from sector k is essential for production in sector l.
# 
# The matrix is m x m, where m is the number of sectors. It is provided by
# the GLcascade package and is subsetted to include only the relevant sectors.
ess_mat_sec_gl <- GLcascade::ess_mat_n4_ihs[sectors, sectors] 

# Essentiality Matrix for Linear Production Function:
# - All sector inputs are set as non-essential (matrix filled with 1s).

ess_mat_sec_lin <- ess_mat_sec_gl
ess_mat_sec_lin[ess_mat_sec_lin == 2] <-1 
