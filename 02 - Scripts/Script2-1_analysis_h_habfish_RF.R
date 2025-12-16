## --------------------------------------------------------------#
## Script name: Script2-1_analysis_h_habfish_RF
##
## Purpose of script: 
##    Build a random forest model with habitat and electrofishing data (habitat workflow)
##    Generate outputs to investigate relationships
##   
## Dependencies: 
##    - Script1-1_format_data_h.R (data_habfish_raw, data_habfish_pseudo)
##
## Author: Paul Bzonek 
##
## Date Created: 2024-07-11
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   2025-01-11: Renamed from Script2-Y_analysis_habfish_RF.R
## --------------------------------------------------------------#



##### Make the Count RF model ##########################################----
#-------------------------------------------------------------#

### Make the  RF model
#----------------------------#
#random seed for reproducibility
set.seed(param_seed)

model_RF <- randomForest(#formula=Total.Count ~ Slope + Light + Year + Sand + Gravel + Cobble + SubstrateDiversity, 
                         formula= Total.Count ~ Slope + SubstrateDiversity + Fetch +
                                  Light + Year + Sand + Gravel + Cobble + Rubble, #SAV_Cover WetlandDistance
                         
                         data = data_habfish_pseudo.train, 
                         replace=TRUE, na.action=na.omit, #na.action=na.response, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                         importance=TRUE, do.trace=1000, ntree=1000)

###Evaluate RandomForest model
#----------------------------#
  ###Evaluate RandomForest model
  data_habfish_pseudo.test$predicted_count <- predict(model_RF, data_habfish_pseudo.test)

  #Make residuals table
  temp_performance <- tibble(
     Total.Count = data_habfish_pseudo.test$Total.Count, 
     predicted = data_habfish_pseudo.test$predicted_count) %>% 
   mutate(residuals = predicted - Total.Count)
  
  #Get summary stats
  temp_performance_summary <- temp_performance %>% 
   summarize(ME = mean(residuals),
             MSE = mean(residuals^2),
             RMSE = sqrt(MSE),
             ss_total = sum((Total.Count - mean(Total.Count))^2),
             ss_residual = sum(residuals^2),
             r_squared_calc = 1 - (ss_residual / ss_total),
             r_squared_log = model_RF$rsq[length(model_RF$rsq)]
             )
  temp_performance_summary 

#Rank variable importance
model_RF_VarImp <- data.frame(importance(model_RF)) %>% 
  mutate(predictor=rownames(.)) %>% 
  arrange(-X.IncMSE)

#psuedo-R2 
model_RF_VarImp <- model_RF_VarImp  %>% 
  dplyr::mutate(#Find sum of positive values
    MDAtotal = sum(X.IncMSE[which(X.IncMSE > 0)]),
    #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
    pR2 = pmax(X.IncMSE/MDAtotal, 0),
    #Calibrate pseudo R2 by multiplying into r2
    pR2 = pR2*temp_performance_summary$r_squared_log
  )

###Plot performance metrics for predictor variables
#% Increase Mean Square Error
plots$model_RF_plot <- list()

plots$model_RF_plot$RF_R2 <- ggplot(model_RF_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
  geom_segment(aes(x=reorder(predictor, pR2),
                   xend=reorder(predictor, pR2), y=0, yend=pR2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip()+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)+
  ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))
plots$model_RF_plot$RF_R2

 
##### Make the Occurrence RF model ##########################################----
#-------------------------------------------------------------#
  
  ### Make the  RF model
  #----------------------------#
  #random seed for reproducibility
  set.seed(param_seed)
  
  model_RF2 <- randomForest(#formula=Occurrence ~ Slope + Light + Year + Sand + Gravel + Cobble + SubstrateDiversity, 
                            formula= Occurrence ~ Slope + SubstrateDiversity + Fetch +
                                    Light + Year + Sand + Gravel + Cobble + Rubble, #SAV_Cover WetlandDistance
                           
                           data = data_habfish_pseudo.train, 
                           replace=TRUE, na.action=na.omit, #na.action=na.response, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                           importance=TRUE, do.trace=1000, ntree=1000)
  
  
  ##Make confusion Matrix on test dataset with 'caret'
  caret::confusionMatrix(
    data=model_RF2$y, 
    reference=model_RF2$predicted,  
    positive="1")
  
  
  ###Investigate variable importance 
  #----------------------------#
  ###Evaluate RandomForest model
  data_habfish_pseudo.test$predicted_pres <- predict(model_RF2, data_habfish_pseudo.test)  
  model_RF2_CM <- caret::confusionMatrix(data_habfish_pseudo.test$predicted_pres, data_habfish_pseudo.test$Occurrence, positive="1")

  
  model_RF2_VarImp <- data.frame(importance(model_RF2)) %>% 
    mutate(predictor=rownames(.)) %>% 
    arrange(MeanDecreaseAccuracy)
  
  #psuedo-R2 
  model_RF2_VarImp <- model_RF2_VarImp  %>% 
    dplyr::mutate(#Find sum of positive values
      MDAtotal = sum(MeanDecreaseAccuracy[which(MeanDecreaseAccuracy > 0)]),
      #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
      pR2 = pmax(MeanDecreaseAccuracy/MDAtotal, 0),
      #Calibrate pseudo R2 by multiplying into model accuracy
      pR2 = pR2*model_RF2_CM$overall[1]
    )
  
  
  ###Plot performance metrics for predictor variables
  #% Increase Mean Square Error
  plots$model_RF_plot$RF2_R2 <- ggplot(model_RF2_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
    geom_segment(aes(x=reorder(predictor, pR2),
                     xend=reorder(predictor, pR2), y=0, yend=pR2))+
    geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
    coord_flip()+
    theme_bw()+ylab(bquote("pseudo-R"^2))+
    xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)+
    ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))
  plots$model_RF_plot$RF2_R2
  
  
  ###Plot performance metrics for predictor variables from both models
  #% Increase Mean Square Error
  
  plots$model_RF_plot$RF_RF2_R2 <- rbind(cbind(select(model_RF2_VarImp, predictor, pR2), Model="Walleye Occurrence"))  %>% 
  rbind(cbind(select(model_RF_VarImp, predictor, pR2), Model="Walleye Abundance")) %>% 
   
     ggplot(., aes(x=fct_reorder(.f=predictor, .x=pR2, .fun=max), y=pR2))+
      geom_segment(aes(x=predictor,
                       xend=predictor, y=0, yend=pR2))+
      geom_point(aes(fill=Model), color="black", size=3.5, pch=21)+
      coord_flip()+
      scale_fill_manual(values=c("#b7001480","#01c04c80"))+
      theme_bw()+ylab(bquote("pseudo-R"^2))+
      xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)+
      #ggtitle(bquote("Variable Importance for Walleye occurrence and abundance"))+
      theme(legend.position="bottom")
  
  plots$model_RF_plot$RF_RF2_R2




##### Plot the data ##############################################----
#-------------------------------------------------------------#

###Univariate Plots
#----------------------------#
plots$model_RF_plot$univiariate_RF2 <- list() #Make list to hold plots
#ylab(bquote("Marginal effect" ~(hat(y))))
#Year
plots$model_RF_plot$univiariate_RF2$Year <- model_RF2 %>% pdp::partial(pred.var = "Year", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$Year.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$Year, aes(x=as.factor(Year), y=yhat))+
  geom_boxplot(colour="#8e3253", lwd=1.25)+geom_line(aes(group=1),colour="#8e3253")+theme_bw()+xlab("Year")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$Year.plot

#Slope
plots$model_RF_plot$univiariate_RF2$Slope <- model_RF2 %>% pdp::partial(pred.var = "Slope", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$Slope.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$Slope, aes(x=Slope, y=yhat))+
  geom_smooth(col="#018be7",fill="NA", linewidth=2)+theme_bw()+xlab("Slope")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$Slope.plot

#Light
plots$model_RF_plot$univiariate_RF2$Light <- model_RF2 %>% pdp::partial(pred.var = "Light", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$Light.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$Light, aes(x=Light, y=yhat))+
  geom_smooth(col="#018be7",fill="NA", linewidth=2)+theme_bw()+xlab("Light")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$Light.plot

#Sand
plots$model_RF_plot$univiariate_RF2$Sand <- model_RF2 %>% pdp::partial(pred.var = "Sand", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$Sand.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$Sand, aes(x=Sand, y=yhat))+
  geom_smooth(col="darkgreen",fill="NA", linewidth=2)+theme_bw()+xlab("Sand")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$Sand.plot

#Gravel
plots$model_RF_plot$univiariate_RF2$Gravel <- model_RF2 %>% pdp::partial(pred.var = "Gravel", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$Gravel.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$Gravel, aes(x=Gravel, y=yhat))+
  geom_smooth(col="darkgreen",fill="NA", linewidth=2)+theme_bw()+xlab("Gravel")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$Gravel.plot

#Cobble
plots$model_RF_plot$univiariate_RF2$Cobble <- model_RF2 %>% pdp::partial(pred.var = "Cobble", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$Cobble.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$Cobble, aes(x=Cobble, y=yhat))+
  geom_smooth(col="darkgreen",fill="NA", linewidth=2)+theme_bw()+xlab("Cobble")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$Cobble.plot

#Rubble
plots$model_RF_plot$univiariate_RF2$Rubble <- model_RF2 %>% pdp::partial(pred.var = "Rubble", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$Rubble.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$Rubble, aes(x=Rubble, y=yhat))+
  geom_smooth(col="darkgreen",fill="NA", linewidth=2)+theme_bw()+xlab("Rubble")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$Rubble.plot

#SubstrateDiversity
plots$model_RF_plot$univiariate_RF2$SubstrateDiversity <- model_RF2 %>% pdp::partial(pred.var = "SubstrateDiversity", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$SubstrateDiversity.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$SubstrateDiversity, aes(x=SubstrateDiversity, y=yhat))+
  geom_smooth(col="darkgreen",fill="NA", linewidth=2)+theme_bw()+xlab("SubstrateDiversity")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$SubstrateDiversity.plot

#Fetch
plots$model_RF_plot$univiariate_RF2$Fetch <- model_RF2 %>% pdp::partial(pred.var = "Fetch", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
plots$model_RF_plot$univiariate_RF2$Fetch.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$Fetch, aes(x=Fetch, y=yhat))+
  geom_smooth(col="#018be7",fill="NA", linewidth=2)+theme_bw()+xlab("Fetch")+
  coord_cartesian(ylim=c(0.3, 0.7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plots$model_RF_plot$univiariate_RF2$Fetch.plot

# #WetlandDistance
# plots$model_RF_plot$univiariate_RF2$WetlandDistance <- model_RF2 %>% pdp::partial(pred.var = "WetlandDistance", prob = TRUE, which.class=2, train=data_habfish_pseudo) 
# plots$model_RF_plot$univiariate_RF2$WetlandDistance.plot <- ggplot(plots$model_RF_plot$univiariate_RF2$WetlandDistance, aes(x=WetlandDistance, y=yhat))+
#   geom_smooth(col="#018be7",fill="NA", linewidth=2)+theme_bw()+xlab("WetlandDistance")+
#   coord_cartesian(ylim=c(0.3, 0.7))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# plots$model_RF_plot$univiariate_RF2$WetlandDistance.plot


#Combine plots
plots$model_RF_plot$univiariate_RF2$combined <- 
 with(plots$model_RF_plot$univiariate_RF2,
     (Year.plot + Slope.plot)/
     (Light.plot + Fetch.plot)/
     (Cobble.plot + Gravel.plot)/
     (Sand.plot + Rubble.plot)+
     (SubstrateDiversity.plot + plot_spacer())
      #plot_annotation(title='Predictors of Walleye Efishing survey occurrence')
     )
plots$model_RF_plot$univiariate_RF2$combined


###Bivariate Plots
#----------------------------#
plots$model_RF_plot$biviariate_RF2 <- list()

#Cobble vs Slope
plots$model_RF_plot$biviariate_RF2$SlopeCobble <- model_RF2 %>% 
  pdp::partial(pred.var = c("Slope", "Cobble"), prob=TRUE, which.class=2, train=data_habfish_pseudo) %>% 
  ggplot(aes(x=Slope, y=Cobble,  fill=yhat))+
  geom_tile()+
  scale_fill_viridis_c()+
  xlab("Slope")+ylab("Cobble")+
  labs(title = "Slope vs Cobble")
plots$model_RF_plot$biviariate_RF2$SlopeCobble

#Sand vs Slope
plots$model_RF_plot$biviariate_RF2$SlopeSand <- model_RF2 %>% 
  pdp::partial(pred.var = c("Slope", "Sand"), prob=TRUE, which.class=2, train=data_habfish_pseudo) %>% 
  ggplot(aes(x=Slope, y=Sand,  fill=yhat))+
  geom_tile()+
  scale_fill_viridis_c()+
  xlab("Slope")+ylab("Sand")+
  labs(title = "Slope vs Sand")
plots$model_RF_plot$biviariate_RF2$SlopeSand

#Gravel vs Slope
plots$model_RF_plot$biviariate_RF2$SlopeGravel <- model_RF2 %>% 
  pdp::partial(pred.var = c("Slope", "Gravel"), prob=TRUE, which.class=2, train=data_habfish_pseudo) %>% 
  ggplot(aes(x=Slope, y=Gravel,  fill=yhat))+
  geom_tile()+
  scale_fill_viridis_c()+
  xlab("Slope")+ylab("Gravel")+
  labs(title = "Slope vs Gravel")
plots$model_RF_plot$biviariate_RF2$SlopeGravel

#Substrate vs Slope
plots$model_RF_plot$biviariate_RF2$SlopeSubstrateDiversity <- model_RF2 %>% 
  pdp::partial(pred.var = c("Slope", "SubstrateDiversity"), prob=TRUE, which.class=2, train=data_habfish_pseudo) %>% 
  ggplot(aes(x=Slope, y=SubstrateDiversity,  fill=yhat))+
  geom_tile()+
  scale_fill_viridis_c()+
  xlab("Slope")+ylab("SubstrateDiversity")+
  labs(title = "Slope vs SubstrateDiversity")
plots$model_RF_plot$biviariate_RF2$SlopeSubstrateDiversity

plots$model_RF_plot$biviariate_RF2$combined <-
 with(plots$model_RF_plot$biviariate_RF2,
     (SlopeGravel + SlopeCobble)/
     (SlopeSubstrateDiversity + SlopeSand) +
     plot_layout(guides = "collect")
     )
plots$model_RF_plot$biviariate_RF2$combined

