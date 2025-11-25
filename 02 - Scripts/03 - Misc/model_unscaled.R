###Build model
#----------------------------#
m3 <- glmer(formula = spawn ~ Silt + Sand + Cobble + depth_median + 
                              (1| animal_id) + (1|year), 
            data = m_data.train,
            family = binomial, na.action=na.omit)

print(m3); plot(m3)
performance::icc(m3) #Intraclass correlation


###Predict model
#----------------------------#
### Make a pseudo dataset to predict spawning probability by animal id
#Make dataset
m3_newdata <- expand.grid(Silt = median(m_data.train$Silt, na.rm=TRUE),
                          Sand = median(m_data.train$Sand, na.rm=TRUE),
                          #Cobble = median(m_data.train$Cobble, na.rm=TRUE),
                          depth_median = median(m_data.train$depth_median, na.rm=TRUE),
                          year="2020",
                          animal_id = unique(droplevels(m_data.train$animal_id)),
                          Cobble = seq(min(m_data.train$Cobble, na.rm=TRUE),
                                              max(m_data.train$Cobble, na.rm=TRUE),
                                              length.out=100)
                          ) %>% 
             filter(!animal_id %in% c("A69-9006-14168", "A69-9006-16063"))
#Predict onto dataset
m3_newdata$prediction <- predict(m3, m3_newdata) 

 

#Plot the predicted data
ggplot(data = m_data,
       aes(x = Cobble, y = spawn, 
           group =  as.factor(animal_id)))+
  geom_jitter(size     = .5,
              alpha    = .5, 
              width = 0.05, height = 0.05)+
  geom_line(data = m3_newdata, inherit.aes = F,
            aes(x = Cobble, y = prediction,
                col = as.factor(animal_id)),
            size     = .5,
            alpha    = .5)+
  viridis::scale_color_viridis(discrete = TRUE)+
  theme_minimal()+
 theme(legend.position = "none")

