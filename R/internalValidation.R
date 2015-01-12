# Functions for internal validation
# Created: 12.01.2015 

# (1) Weights Distance

#' computes the weight distance 
#'
#' @param design_weights design weights of the survey
#' @param simulated_weights simulated weights
#' @return distance between weights
#' @ examples
#' getD(a, b)
#' getD(c, d)
getD <- function(design_weights, simulated_weights){
    distance_weights <- abs(simulated_weights - design_weights)}

# (2) Total Chi-squared distance
getChi <- function(design_weights, simulated_weights){
    total_distance_chi2 <- sum(1/2 * (design_weights * simulated_weights)^2 / design_weights)}

# (3) Mean Chi-squared distance
getMChi <- function(design_weights, simulated_weights, population_size){
    total_distance_chi2 <- sum(1/2 * (design_weights * simulated_weights)^2 / design_weights)/population_size}

# (4) Total absolute distance (TAD)
getTAD <- function(design_weights, simulated_weights){
    TAD <- sum(abs(simulated_weights-design_weights))}

# (5) Error in Margin (EM)
getEM <- function(design_weights, simulated_weights){
    EM <- (sum(design_weights) - sum(simulated_weights)) / sum(design_weights)}

# (6) Error in Distribution (ED)
getED <- function(design_weights, simulated_weights){
    ED <- abs(sum(design_weights) - sum(simulated_weights)) / sum(design_weights)}

# (7) Total absolute error (TAE)
getTAE <- function(observed, simulated){
    obs <- as.numeric(observed)
    sim <- as.numeric(simulated)
    TAE <- sum(abs(obs-sim))}

# (8) Standardized absolute error (SAE)
getSAE <- function(observed, simulated, population_size){
    obs <- as.numeric(observed)
    sim <- as.numeric(simulated)
    SAE <- abs(observed-simulated) / population_size}

# (9) Percentage error (PSAE)
getPSAE <- function(observed, simulated, population_size){
    obs <- as.numeric(observed)
    sim <- as.numeric(simulated)
    PSAE <- abs(observed-simulated) / population_size * 100}

# (10) Z-statistic
getZ <- function(observed, simulated){
    obs <- as.numeric(observed)
    sim <- as.numeric(simulated)
    r = simulated/sum(observed)
    p = observed/sum(observed)
    Z <- (r-p)/sqrt(p*(1-p)/sum(observed))}

# (11) Correlation Coefficient (Pearson Correlation)
getPearson <- function(observed, simulated){
    pearson <- cor(cbind(observed, simulated), use="complete.obs", method="pearson")}

# (12) Independent samples t-Test
getTTest <- function(observed, simulated){
    ttest <- t.test(observed, simulated)$p.value}

# (13) Coefficient of determination
getR <- function(observed, simulated){
    lm.X <- lm(observed ~ simulated)
    r2 <- summary(lm.X)$r.squared}
