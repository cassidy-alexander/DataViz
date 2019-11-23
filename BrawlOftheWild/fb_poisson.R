# Load packages
library(dplyr)
library(skellam)
library(ggplot2)
library(purrr)
library(tidyr)
library(RCurl)

# Set working directory
setwd("~/Desktop")

# Read in dataset
data <- read.csv("fb_poisson_data.csv")

# Separate data for UM and MSU
CatGrizData <- data[5:6,]
CatGrizData

'Let the goal expectancy for the home team be E(h)
E(h) = HomeOffense x AwayDefense x HomeAvgPtsFor'

'Let the goal expectancy for the away team be E(a)
E(a) = AwayOffense x HomeOffense x AwayAvgPtsFor'

'The Cat-Griz game will be played in Bozeman for 2019,
so MSU is the home team and UM is the away team
so E(h) = E(MSU)
and E(a) = E(UM)'

# Define variables for expected points
CatsHomeOffense <- CatGrizData[2,10]
CatsHomeDefense <- CatGrizData[2,20]
CatsAvgHomePts <- CatGrizData[2,9]

GrizAwayOffense <- CatGrizData[1,17]
GrizAwayDefense <- CatGrizData[1,20]
GrizAvgAwayPts <- CatGrizData[1,16]

# Calculate expected points for Cat-Griz game
Eh <- (CatsHomeOffense*GrizAwayDefense*CatsAvgHomePts)
Ea <- (GrizAwayOffense*CatsHomeDefense*GrizAvgAwayPts)

'E(h) = 36
E(a) = 20
so predicted Cat-Griz score is 36-20, Cats win'

