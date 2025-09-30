#set up workspace
rm(list=ls())

setwd("~/Colin/Keck Center/Equivelancy/Final Guide/multivariate tutorial")

source("multivariateProcessStability.R")
source("binLimits.R")
source("multivariatePowerAnalysis.R")
source("multivariateEquivalency.R")
source("monetary_cost_of_experiment.R")
library(ggpubr)
library(paletteer)
library(viridis)
library(parallel)
library(data.table)
library(foreach)
library(doParallel)
library(Matrix)
library(ggplot2)
library(viridis)
library(dplyr)

#import data from reference process 
referenceProcess = read.csv("referenceProcess.csv", header = TRUE)

#set constants
b = 8 #number of bins
alpha = .01 #significance level used in stopping rule
simulations = 10 #number values of rho tested in simulations to find optimal sampling strategy. This and the following constant should be higher, but that's inefficient
iterations = 10 #number of simulations per parameter value of rho 
varNumber = ncol(referenceProcess)
powerThreshold = .8
lambdaVec = seq(.1, .25, by = .05)#lambdaVec = seq(.1, .25, by = .025)
varVec = seq(1, 49, by = 5)#varVec = seq(1, 49, by = 3)
nVec = seq(10, 350, by = 25) #nVec = seq(10, 350, by = 5) 
delta = 0.02 #0.08
omega = 1
#is process in control
outOfControlCount = multivariateProcessStability(referenceProcess)
print(outOfControlCount)

if(outOfControlCount$num_out_of_control > 0){
  stop("The process is not stable, Do Not Continue.");
}

#get reference bin limits for each factor 
referenceLimits = binLimits(referenceProcess, b)

#get lower and upper bounds to correlation
refCorrelations = cor(referenceProcess)
lowerCor = min(refCorrelations) #lower range for pairwise correlations
upperCor = max(refCorrelations[refCorrelations<1]) #upper range for pairwise correlations

### Here we treat the function multivariatePowerAnalysis like other power analysis functions; depending on the type of inputs it gets, it will give different outputs. Traditionally a power analysis will take three of power, significance level, effect size, and sample size and then output the remaining variable. Here, we set the significance level to a constant value and focus on the remaining 3 variables. Sample size here refers to the number of printed artifacts (n), the number of measured predictor variables (p) can still vary. That is, two variables get inputted into the function, and the remainder gets outputted. Each combination of inputs has differing simulation requirements. When the power level is unknown, but the effect size and sample size are known, then the only thing is needed are simulations across values of p. When the sample size is unknown, but power and the effect size are known, then simulations need to be run across n and p, but for a set effect size. Finally, if the effect size is unknown, but power and sample size are known, then simulations need to be run across potential effect sizes, n, and p. This last case is, unsurprisingly, the one which takes the most time. 

# getting different designs with different initial assumptions. here we assume powerThreshold = NULL
n = 40
lambda = .25
alpha = .01
powerThreshold = NULL

data = multivariatePowerAnalysis(powerThreshold, lambda, n, alpha, b, lowerCor, upperCor, lambdaVec, varVec, nVec, simulations, iterations)
data$UnitCost = omega*n+n*data$variableNumber*delta*omega

data$minFlag = data$Power == min(data$Power[data$Power>.8])
ggplot(data, aes(x = UnitCost, y = Power)) + geom_point(size = 3) +
  scale_color_viridis_c(direction = -1) + theme_bw() + geom_point(data = subset(data, minFlag), shape = 21, size = 5, stroke = 1.2, fill = NA, color = "forestgreen") + theme_bw(base_size = 14) + labs(x = "Unit Cost", y = "Power")

variableNumber = data$variableNumber[min(which(data$Power>.8))]
  
print(paste("Final Design: n = ", n, ", p = ", variableNumber, sep = ""))

# getting different designs with different initial assumptions. here we assume n = NULL
n = NULL
lambda = .25
alpha = .01
powerThreshold = 0.8

data = multivariatePowerAnalysis(powerThreshold, lambda, n, alpha, b, lowerCor, upperCor, lambdaVec, varVec, nVec, simulations, iterations)
data$UnitCost = omega*data$n+data$n*data$variableNumber*delta*omega

data$minFlag = data$UnitCost == min(data$UnitCost)
ggplot(data, aes(x = n, y = variableNumber, color = UnitCost)) + geom_point(size = 3) +
  scale_color_viridis_c(direction = -1) + theme_bw() + geom_point(data = subset(data, minFlag), shape = 21, size = 5, stroke = 1.2, fill = NA, color = "forestgreen") + theme_bw(base_size = 14) + labs(x = "Sample Size (n)", y = "Number of Variables (p)")

variableNumber = data$variableNumber[which.min(data$UnitCost)]
n = round(data$n[which.min(data$UnitCost)])

print(paste("Final Design: n = ", n, ", p = ", variableNumber, sep = ""))
  
# getting different designs with different initial assumptions. here we assume lambda = NULL
n = 100
lambda = NULL
alpha = .01
powerThreshold = 0.8

data = multivariatePowerAnalysis(powerThreshold, lambda, n, alpha, b, lowerCor, upperCor, lambdaVec, varVec, nVec, simulations, iterations)
data$UnitCost = omega*data$nArtifact+data$nArtifact*data$variableNumber*delta*omega

min_points = data %>%
  group_by(lambda) %>%
  slice_min(UnitCost, with_ties = FALSE)

ggplot(data, aes(x = UnitCost, y = lambda)) + geom_point(size = 3) +
  scale_color_viridis_c(direction = -1) + theme_bw() + geom_point(data = min_points, shape = 21, size = 5, stroke = 1.2, fill = NA, color = "forestgreen") + theme_bw(base_size = 14) + labs(x = "Unit Cost", y = "Effect Size")

print(paste("Final Design(s): n = ", round(min_points$nArtifact), ", p = ", min_points$variableNumber, sep = ""))

### estimate cost of experiment given a real monetary value for omega
n = round(min_points$nArtifact)
p = min_points$variableNumber[1]
omega = 1000
monetary_cost_of_experiment(p, n, delta, omega)

### determine equivalency between reference and candidate processes 

#import data assuming we went with sample size estimate from above
candidateProcessEquivalent = read.csv("candidateProcessEquivalent.csv", header = TRUE)
candidateProcessNotEquivalent = read.csv("candidateProcessNotEquivalent.csv", header = TRUE)

#This candidate distribution was drawn from the same distribution as the reference, so it should be equivalent:
equivalent = multivariateEquivalency(referenceLimits, candidateProcessEquivalent, b)
print(paste("Reference and Candidate Distributions Are: ", equivalent$equivalent[1], sep = ""))

#this candidate distribution was drawn from a different distribution, so it should not be equivalent: 
equivalent = multivariateEquivalency(referenceLimits, candidateProcessNotEquivalent, b)
print(paste("Reference and Candidate Distributions Are: ", equivalent$equivalent[1], sep = ""))