## --------------------------------------------------------------#
## Script name: Script2-X-2_analysis_t_glmer
##
## Purpose of script: 
##    Build a glm to model telem data with fish and year as
##    random predictors
##    Uses the m_data datasets
##
## Author: Paul Bzonek 
##
## Date Created: 2024-12-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#


##### m1 Model - random effects only #############################----
#-------------------------------------------------------------#

###PRODUCES A SINGULARITY WHEN DATA ARE SPLIT 50:50 SPAWN-NO-SPAWN
m1 <- glmer(spawn ~ (1| animal_id) + (1|year),
           data = m_data.train,
           family = binomial)
print(m1)
performance::icc(m1) #Intraclass correlation

 

##### m2 Model - manually selected full model ####################----
#-------------------------------------------------------------#

###Build model
#----------------------------#
m2 <- glmer(formula = spawn ~ scaled_Silt + scaled_Sand + scaled_Cobble + scaled_depth_median + 
                              (1| animal_id) + (1|year), #Random intercept for year and animal ID
                              #(1 + animal_id | year), #Random intercept for year; random slope for animal ID
                              # Additional variables ranked by rf mean decrease error
                              #scaled_Gravel + scaled_Rubble + scaled_light_max + 
                              #scaled_Substrate_Siltation + scaled_Armourstone + scaled_Boulder +
                              #scaled_Fine + scaled_Concrete + scaled_Terrestrial
                              # Ininformative data: Fine, Concrete, terrestrial, 
            data = m_data.train,
            family = binomial, na.action=na.omit)

print(m2); plot(m2)
performance::icc(m2) #Intraclass correlation
summary(m2)


###evaluate model performance with 'performance' package
#----------------------------#
library(performance)
r2(m2)
check_model(m2)
model_performance(m2)

###Predict model
#----------------------------#
### Make a pseudo dataset to predict spawning probability by animal id
#Make dataset
m2_newdata <- expand.grid(scaled_Silt = median(m_data.train$scaled_Silt, na.rm=TRUE),
                          scaled_Sand = median(m_data.train$scaled_Sand, na.rm=TRUE),
                          #scaled_Cobble = median(m_data.train$scaled_Cobble, na.rm=TRUE),
                          scaled_depth_median = median(m_data.train$scaled_depth_median, na.rm=TRUE),
                          year="2020",
                          animal_id = unique(droplevels(m_data.train$animal_id)),
                          scaled_Cobble = seq(min(m_data.train$scaled_Cobble, na.rm=TRUE),
                                              max(m_data.train$scaled_Cobble, na.rm=TRUE),
                                              length.out=100)
                          ) %>% 
             filter(!animal_id %in% c("A69-9006-14168", "A69-9006-16063"))
#Predict onto dataset
m2_newdata$prediction <- predict(m2, m2_newdata) 

 

#Plot the predicted data
ggplot(data = m_data,
       aes(x = scaled_Cobble, y = spawn, 
           group =  as.factor(animal_id)))+
  geom_jitter(size     = .5,
              alpha    = .5, 
              width = 0.05, height = 0.05)+
  geom_line(data = m2_newdata, inherit.aes = F,
            aes(x = scaled_Cobble, y = prediction,
                col = as.factor(animal_id)),
            size     = .5,
            alpha    = .5)+
  viridis::scale_color_viridis(discrete = TRUE)+
  theme_minimal()+
 theme(legend.position = "none")

