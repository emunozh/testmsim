#' Weights Distance
#'
#' Computes the weight distance 
#' \deqn{D_i = \sum_j^m |w_j - d_j|}
#'
#' @param design_weights design weights of the survey
#' @param simulated_weights simulated weights
#' @return distance between weights
#' @examples
#' getD(10, 20)
#' getD(30, 40)
getD <- function(design_weights, simulated_weights){
    distance_weights <- abs(simulated_weights - design_weights)}

#' Total Chi-squared distance
#'
getChi <- function(design_weights, simulated_weights){
    total_distance_chi2 <- sum(1/2 * (design_weights * simulated_weights)^2 / design_weights)}

#' Mean Chi-squared distance
getMChi <- function(design_weights, simulated_weights, population_size){
    total_distance_chi2 <- sum(1/2 * (design_weights * simulated_weights)^2 / design_weights)/population_size}

#' Total absolute distance (TAD)
getTAD <- function(design_weights, simulated_weights){
    TAD <- sum(abs(simulated_weights-design_weights))}

#' Error in Margin (EM)
getEM <- function(design_weights, simulated_weights){
    EM <- (sum(design_weights) - sum(simulated_weights)) / sum(design_weights)}

#' Error in Distribution (ED)
getED <- function(design_weights, simulated_weights){
    ED <- abs(sum(design_weights) - sum(simulated_weights)) / sum(design_weights)}

#' Total absolute error (TAE)
getTAE <- function(observed, simulated){
    obs <- as.numeric(observed)
    sim <- as.numeric(simulated)
    TAE <- sum(abs(obs-sim))}

#' Standardized absolute error (SAE)
getSAE <- function(observed, simulated, population_size){
    obs <- as.numeric(observed)
    sim <- as.numeric(simulated)
    SAE <- abs(observed-simulated) / population_size}

#' Percentage error (PSAE)
getPSAE <- function(observed, simulated, population_size){
    obs <- as.numeric(observed)
    sim <- as.numeric(simulated)
    PSAE <- abs(observed-simulated) / population_size * 100}

#' Z-statistic
getZ <- function(observed, simulated){
    obs <- as.numeric(observed)
    sim <- as.numeric(simulated)
    r = simulated/sum(observed)
    p = observed/sum(observed)
    Z <- (r-p)/sqrt(p*(1-p)/sum(observed))}

#' Correlation Coefficient (Pearson Correlation)
getPearson <- function(observed, simulated){
    pearson <- cor(cbind(observed, simulated), use="complete.obs", method="pearson")}

#' Independent samples t-Test
getTTest <- function(observed, simulated){
    ttest <- t.test(observed, simulated)$p.value}

#' Coefficient of determination
getR <- function(observed, simulated){
    lm.X <- lm(observed ~ simulated)
    r2 <- summary(lm.X)$r.squared}
