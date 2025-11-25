## --------------------------------------------------------------#
## Script name: Script0-1_load_packages
##
## Purpose of script: 
##    Load general-purpose scripts used throughout project
##    Rarely used/problematic packages may be loaded in specific scripts 
##    custom telemetrytoolsFESL needs to be installed with devtools
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

###Basic packages on CRAN
#----------------------------#
library('adehabitatHR') #investigate homeranges NOTE: select function in dependency MASS conflicts with dplyr select()
library('randomForest') #Original methodology
library('ggmap') #google basemaps
library('sf') #handle shapefiles, projections, and distance calculations
library('lme4') #Mixed effects models; lmer, glmer
library('tidyverse') #Basic organizing and plotting
library('corrplot') #Plot variable correlations
library('vegan') #Calculate diversity (for substrate)
library('paletteer') #Pretty colour palettes
library('patchwork') #Add plots together
library('ggpmisc') #Add r2 to ggplots
library('ggridges') #Make ridges boxplot
library('rptR') #Look at Intraclass Correlation Coefficients
library('beepr') #Add warning chimes

###Packages off CRAN
#----------------------------#
###Lab package
#Only need to install during updates
# devtools::install_github("pbzonek/telemetrytoolsFESL")
 
#remotes::install_github("jsta/glatos")

library('telemetrytoolsFESL')
#library('glatos')
###Source functions
#----------------------------#
source("02 - Scripts/01 - Functions/Function1-1_JakesFunctions.R")
source("02 - Scripts/01 - Functions/Function1-3_HelperFunctions.R")

###Adjust Settings
#----------------------------#
theme_set(theme_classic()) #ggplot background
options(scipen=999) #Drop scientific notation
param_seed <- 1987 #Set seed for randomized analysis

plots <- list() #Home for plots
