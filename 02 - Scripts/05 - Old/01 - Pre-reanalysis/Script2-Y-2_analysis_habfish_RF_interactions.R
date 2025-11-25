## --------------------------------------------------------------#
## Script name: Script2-Y-2_analysis_habfish_RF_interactions.R 
##
## Purpose of script: 
##      Investigate interactions between predictors
##    
##
## Author: Paul Bzonek
##
## Date Created: 2024-12-23
##
## --------------------------------------------------------------#  
## Modification Notes:The packages loaded here have LOTS of dependencies   
##   and the code to remove the dependencies is a little sketchy. If 
##   issues pop up after running this script, try avoiding package 'iml'
## --------------------------------------------------------------#

library(iml, include.only = c('Predictor', 'Interaction')) 
plot_interactions <- list() #Make list to hold plots

#####Combined Life Stage##########################################----
#-------------------------------------------------------------#

#----------------------------#
###Interpret model variables with 'iml'
#----------------------------#
#Create a model object
model_RF_int_predictor <- iml::Predictor$new(model = model_RF, 
                                             data = dplyr::select(data_habfish_pseudo.test, #Slope, Light, Year, Sand, Gravel, Cobble, SubstrateDiversity
                                                                  Slope, SubstrateDiversity, WetlandDistance, Fetch, Light, Year, Sand, Gravel, Cobble, Rubble
                                                                  ), 
                                             y=data_habfish_pseudo.test$Total.Count)
#Measure the interaction strength
model_RF_int_Interaction <- iml::Interaction$new(model_RF_int_predictor) #OPTIONAL: Specify interactions: feature="FishDiversity"

#Plot H value of variables or interaction terms 
model_RF_int_Interaction$results %>%  
  dplyr::group_by(.feature) %>% 
  dplyr::summarise(H=mean(.interaction)) %>% 
  as.data.frame() %>% 
  ggplot(aes(y=H, x= reorder(.feature, H)))+
  geom_histogram(stat="identity", col="black", fill="brown", alpha=0.4)+
  xlab("Interaction")+ylab("H value")+
  theme(axis.text.x = element_text(angle = 90))


#----------------------------#
###Interpret model variable interactions with 'iml'
#----------------------------#
#Create a model object
# model_RF_int_predictor <- iml::Predictor$new(model_RF, data = dplyr::select(data_habfish_pseudo.test, -Total.Count), 
#                                     y=data_habfish_pseudo.test$Total.Count)
model_RF_int_Interaction_List <- data.frame()

loopPredictors <- c(
  "Slope", "Light", "Year", "Sand", "Gravel", "Cobble", "SubstrateDiversity"
)

for(i in unique(loopPredictors)){
  print(i)
  #Measure the interaction strength
  model_RF_int_Interaction <- iml::Interaction$new(model_RF_int_predictor, feature= i) #OPTIONAL: Specify interactions: feature="FishDiversity"
  #Plot H value of variables or interaction terms 
  df_loopA <-model_RF_int_Interaction$results %>%  
    dplyr::group_by(.feature) %>% 
    dplyr::summarise(H=mean(.interaction)) %>% 
    as.data.frame() 
  model_RF_int_Interaction_List <- rbind(model_RF_int_Interaction_List, df_loopA)
}; rm(df_loopA)



#----------------------------#
###Identify and plot important bivariate interactions
#----------------------------#
###Find and rank bivriate interactions
plot_interactions$Lolipop_model_RF <- model_RF_int_Interaction_List %>% 
  # filter(.feature %in%
  #          c("Dominant.Substrate:Macrophytes.Submergent", "Water.Temp:Dominant.Substrate", 
  #            "widthm:Max.Depth", "DO:Conductivity", "Pool.or.Riffle:Conductivity",
  #            "Water.Temp:Conductivity","DO:Macrophytes.Submergent",
  #            "BenthicAbundance:widthm")) %>% 
  
  arrange(desc(H)) %>%  # Arrange by Column A in descending order
  slice(1:10) %>%  # Select the top 10 rows

  ggplot(aes(y=H, x= reorder(.feature, H)))+
    geom_hline(yintercept = 0)+
    geom_segment(aes(x=reorder(.feature, H),
                     xend=reorder(.feature, H), y=0, yend=H))+
    geom_point(aes(fill=H), col="black", size=4.5, pch=21)+
    coord_flip()+
    ylab("H-value")+ xlab("Predictor")+
    theme_bw()+
    theme(legend.position="none",
          #axis.text.y=element_text(
          #face=c(replace(rep("plain", 8), c(4,6,8), "bold")), #Embolden key features
          #size=c(replace(rep(9, 8), c(4,6,8), 11))) #Increase size
    )+ 
    labs(title = "Feature Importance") 
plot_interactions$Lolipop_model_RF





#####Get rid of package:'iml' and its troublesum dependencies#####----
#-------------------------------------------------------------#
invisible({
  #Formally library all background loaded packages
  suppreTotal.CountackageStartupMessages(
    lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE))
  #Detach all non-base packages
  lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE)
})

#Reload project packages
source("02 - Scripts/00 - Global/ScriptX-2_LoadPackages.R") #Load packages

