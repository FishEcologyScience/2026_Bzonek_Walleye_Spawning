## --------------------------------------------------------------#
## Script name: Script2-Y_analysis_habfish_glm
##
## Purpose of script: 
##    Analyze efishing dataset with habitat data
##    
##    
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-04
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

###Look at prevelence of habitat features
data_habfish_raw %>% 
 select(Fine:Armourstone) %>% 
 colSums()

#Plot correlations
data_habfish_raw %>% 
 select(Silt:Armourstone, SubstrateDiversity, Fetch, Slope) %>% 
 cor() %>% corrplot::corrplot.mixed(order='AOE', upper='shade', tl.pos = c("lt")) 

###Look at importance of year in datasets
#Year is irrelevant to detections in pseudo dataset
performance::icc(glmer(Total.Count ~ (1|Year),
                       family = poisson,
                       data = data_habfish_pseudo.train)
                 )

#Year matters for biologicals in raw dataset
performance::icc(glmer(Fork.Length ~ (1|Year),
                       family = poisson,
                       data = data_habfish_raw)
                 )
performance::icc(glmer(Weight ~ (1|Year),
                       family = poisson,
                       data = data_habfish_raw)
                 )


##### m1 model - fish counts #####################################----
#-------------------------------------------------------------#
#Poisson counts
m1 <- glm(Total.Count ~ Slope + Light + Year +
            Sand + Gravel + Cobble + SubstrateDiversity,
            #+ Armourstone,
            family = poisson,
            data = data_habfish_pseudo.train)

# #Binomial Presence
# m1 <- glm(Presence ~ Slope + Light + Year + 
#             Sand + Gravel + Cobble + Rubble + Armourstone,
#             family = binomial,
#             data = data_habfish_pseudo.train)
anova(m1, test = "LRT")

#Investigate model with {performance}
print(m1)
performance::icc(m1) 
performance::r2(m1)
performance::check_model(m1)
performance::model_performance(m1)
 
 
     ###Plot relationships to check for linearity
     #----------------------------#
     m1_plots <- list()
     
     #Slope - Not Linear
     m1_plots$Slope <- data_habfish_pseudo %>% 
      ggplot(aes(y=Total.Count, x= Slope))+
      geom_point(aes(colour=as.factor(Year), size = Total.Count))+
      geom_smooth(colour="grey", se=FALSE)+
      ggpmisc::stat_poly_eq()+
      ggpmisc::stat_poly_line(se=FALSE, colour="black")+
      scale_colour_paletteer_d("soilpalettes::natrudoll")+
      #scale_colour_paletteer_d("fishualize::Sander_lucioperca")+
      labs(title = "Slope")
     
     
     #Light - Not linear
     m1_plots$Light <-data_habfish_pseudo %>% 
      ggplot(aes(y=Total.Count, x= Light))+
      geom_jitter(aes(colour=as.factor(Year), size = Total.Count))+
      geom_smooth(colour="grey", se=FALSE)+
      ggpmisc::stat_poly_eq()+
      ggpmisc::stat_poly_line(se=FALSE, colour="black")+
      scale_colour_paletteer_d("soilpalettes::natrudoll")+
      labs(title = "Light")
     
     #Year
     m1_plots$Year <- data_habfish_pseudo %>% 
      ggplot(aes(y=Total.Count, x= as.factor(Year)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(aes(colour=as.factor(Year), size = Total.Count))+
      ggpmisc::stat_poly_eq()+
      scale_colour_paletteer_d("soilpalettes::natrudoll")+
      labs(title = "Year")
     
     #Sand - not linear
     m1_plots$Sand <- data_habfish_pseudo %>% 
      ggplot(aes(y=Total.Count, x= Sand))+
      geom_jitter(aes(colour=as.factor(Year), size = Total.Count))+
      geom_smooth(colour="grey", se=FALSE)+
      ggpmisc::stat_poly_eq()+
      ggpmisc::stat_poly_line(se=FALSE, colour="black")+
      scale_colour_paletteer_d("soilpalettes::natrudoll")+
      labs(title = "Sand")
     
     #Gravel
     m1_plots$Gravel <- data_habfish_pseudo %>% 
      ggplot(aes(y=Total.Count, x= Gravel))+
      geom_jitter(aes(colour=as.factor(Year), size = Total.Count))+
      geom_smooth(colour="grey", se=FALSE)+
      ggpmisc::stat_poly_eq()+
      ggpmisc::stat_poly_line(se=FALSE, colour="black")+
      scale_colour_paletteer_d("soilpalettes::natrudoll")+
      labs(title = "Gravel")
     
     #Cobble
     m1_plots$Cobble <- data_habfish_pseudo %>% 
      ggplot(aes(y=Total.Count, x= Cobble))+
      geom_jitter(aes(colour=as.factor(Year), size = Total.Count))+
      geom_smooth(colour="grey", se=FALSE)+
      ggpmisc::stat_poly_eq()+
      ggpmisc::stat_poly_line(se=FALSE, colour="black")+
      scale_colour_paletteer_d("soilpalettes::natrudoll")+
      labs(title = "Cobble")
     
     #SubstrateDiversity
     m1_plots$SubstrateDiversity <- data_habfish_pseudo %>% 
      ggplot(aes(y=Total.Count, x= SubstrateDiversity))+
      geom_jitter(aes(colour=as.factor(Year), size = Total.Count))+
      geom_smooth(colour="grey", se=FALSE)+
      ggpmisc::stat_poly_eq()+
      ggpmisc::stat_poly_line(se=FALSE, colour="black")+
      scale_colour_paletteer_d("soilpalettes::natrudoll")+
      labs(title = "SubstrateDiversity")
     
     
     with(m1_plots,
           (Year + plot_spacer())/ 
           (Slope + Light)/
           (Gravel + Cobble)/
           (Sand + SubstrateDiversity)
          )+
          plot_annotation('Checking for linear relationships in Walleye count GLM')+
          plot_layout(guides = "collect")





##### m2 model - length ##########################################----
#-------------------------------------------------------------#
m2 <- lmer(Fork.Length ~ Slope + Light +
           Sand + Gravel + Cobble + Rubble + Armourstone + (1|Year),
           data = data_habfish_raw)

#Investigate model with {performance}
print(m2)
performance::icc(m2) 
performance::r2(m2)
performance::check_model(m2)
performance::model_performance(m2)
 


##### m3 model - mass ############################################----
#-------------------------------------------------------------#
m3 <- lmer(Weight ~ Slope + Light +
           Sand + Gravel + Cobble + Rubble + Armourstone + (1|Year),
           data = data_habfish_raw)

#Investigate model with {performance}
print(m3)
performance::icc(m3) 
performance::r2(m3)
performance::check_model(m3)
performance::model_performance(m3)





# ###Predict model
# #----------------------------#
# ### Make a pseudo dataset to predict spawning probability by animal id
# #Make dataset
# m2_newdata <- expand.grid(scaled_Silt = median(m_data.train$scaled_Silt, na.rm=TRUE),
#                           scaled_Sand = median(m_data.train$scaled_Sand, na.rm=TRUE),
#                           #scaled_Cobble = median(m_data.train$scaled_Cobble, na.rm=TRUE),
#                           scaled_depth_median = median(m_data.train$scaled_depth_median, na.rm=TRUE),
#                           year="2020",
#                           animal_id = unique(droplevels(m_data.train$animal_id)),
#                           scaled_Cobble = seq(min(m_data.train$scaled_Cobble, na.rm=TRUE),
#                                               max(m_data.train$scaled_Cobble, na.rm=TRUE),
#                                               length.out=100)
#                           ) %>% 
#              filter(!animal_id %in% c("A69-9006-14168", "A69-9006-16063"))
# #Predict onto dataset
# m2_newdata$prediction <- predict(m2, m2_newdata) 
# 
#  
# 
# #Plot the predicted data
# ggplot(data = m_data,
#        aes(x = scaled_Cobble, y = spawn, 
#            group = as.factor(animal_id)))+
#   geom_jitter(size = .5,
#               alpha = .5, 
#               width = 0.05, height = 0.05)+
#   geom_line(data = m2_newdata, inherit.aes = F,
#             aes(x = scaled_Cobble, y = prediction,
#                 col = as.factor(animal_id)),
#                 size = .5,
#                 alpha = .5)+
#   viridis::scale_color_viridis(discrete = TRUE)+
#   theme_minimal()+
#   theme(legend.position = "none")

