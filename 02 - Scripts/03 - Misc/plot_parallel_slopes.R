#plot_paralelle slopes
library(moderndive) #Plots with parallel slopes

###Plot model
#----------------------------#

###Raw data
param_x <- "silt" #Cobble, Silt, Sand, depth_median

#Plot the data
ggplot(data = m_data,
       aes(x = m_data[[param_x]], y = spawn, 
           col = as.factor(animal_id)))+
  geom_jitter(size     = .5,
              alpha    = .5, 
              width = 0.05, height = 0.05)+
  geom_parallel_slopes(#formula = y~ poly(x,2), #A poor approximation of a logistic equation
                       se = FALSE)+
  #Global trendline
  geom_smooth(data = m_data, inherit.aes = F,
              aes(x = m_data[[param_x]], y = spawn),
              method=glm, method.args=list(family="binomial"), se = FALSE,
              colour="black", size=2)+
  viridis::scale_color_viridis(discrete = TRUE)+
  theme_minimal()+
  labs(title    = paste("Probability of spawning vs. %", param_x, "abundance"  ),
       col      = "Years of\nTeacher\nExperience"
       ) +
 theme(legend.position = "none")
