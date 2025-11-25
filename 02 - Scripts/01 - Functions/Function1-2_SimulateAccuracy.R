#-------------------------------------------------------------#
### Function to simulate random forest model accuracy across a range of parameters
#-------------------------------------------------------------#

function_SimulateAccuracy <- function(predictor="SSp",
                                      data_Train,
                                      data_Test,
                                      loopweight = c(1,2,5,10,20,50,100),
                                      na.response = na.roughfix,
                                      model.formula= SSp ~.
){
  df_Loop_SimOutput <- data.frame()
  
  #Loop to build multiple Random Forest models
  #----------------------------#
  for(i in unique(loopweight)){
    
    print(paste("Simulating weight:", i))
    ###Simulate random forest perfomance for model tuning
    #----------------------------#
    model_RF_Loop <- randomForest(formula=model.formula, data = data_Train, replace=TRUE, 
                                  na.action=na.response, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                                  classwt=c(1, i), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                                  importance=TRUE, do.trace=1000, ntree=1000)
    
    ###Investigate RandomForest model
    #----------------------------#
    #Make confusion Matrix on test dataset with 'caret'
    data_Test$prediction1 <- predict(model_RF_Loop, data_Test)
    #df_loopA <- caret::confusionMatrix(data=data_Test$prediction1, reference=data_Test[[predictor]],  positive="1")
    df_loopA <- caret::confusionMatrix(data=data_Test$prediction1, reference=data_Test[[predictor]])
    df_loopA <- as.vector(df_loopA$table)
    df_loopB <- data.frame(SimulatedPresenceWeight = i,
                           AccurateAbsence = df_loopA[1],
                           FalsePredictedPresence = df_loopA[2],
                           FalsePredictedAbsence = df_loopA[3],
                           AccuratePresence = df_loopA[4])
    df_Loop_SimOutput <-rbind(df_Loop_SimOutput, df_loopB)
    
    rm(df_loopA); rm(df_loopB)
  }
  
  #Produce output
  #----------------------------#
  df_Loop_SimOutput <- df_Loop_SimOutput  %>% 
    #Describe Accuracy from confusion Matrices
    mutate(PredictionAccuracy = (AccurateAbsence+AccuratePresence)/
             (AccurateAbsence+AccuratePresence+FalsePredictedPresence+FalsePredictedAbsence),
           AbsenceAccuracy = AccurateAbsence/(AccurateAbsence+FalsePredictedPresence),
           PresenceAccuracy = AccuratePresence/(AccuratePresence+FalsePredictedAbsence)
    )
  
  #Plot output
  #----------------------------#
  a <- df_Loop_SimOutput  %>% 
    #Format data for plotting
    pivot_longer(cols = c(PredictionAccuracy, AbsenceAccuracy, PresenceAccuracy),
                 names_to = "AccuracyMetric", values_to = "Accuracy") %>% 
    #Plot the data
    ggplot(aes(x=as.factor(SimulatedPresenceWeight), y=Accuracy, colour=AccuracyMetric))+
    geom_line(aes(group=AccuracyMetric), size=1.5)+
    geom_point(size=3)+
    scale_colour_manual(values=c("#B21038", "black", "#0FA3B1"))+
    xlab("Simulated 'classwt' weight")
  suppressMessages(print(a))
  
  return(df_AccuracySimulationOutput=df_Loop_SimOutput)
}





# ### Example code to run function on a RF model
# #-------------------------------------------------------------#
# temp.formula <- formula(pres~season+SAV+depth+exposure+region)
# 
# temp.data <- filter(TH_data_daily_sp, species=="Common Carp") %>% sample_n(10000)
# temp.data$nrow <- 1:length(temp.data$day)
# 
# temp.train <- temp.data %>% sample_frac(0.7)
# temp.test <- temp.data[!(temp.data$nrow %in% temp.train$nrow),]# 70/30 split.
# 
# 
# #Optimize classwt
# RF1_simulations <- function_SimulateAccuracy(predictor = "pres", model.formula=temp.formula,
#                                              data_Train = temp.train, data_Test = temp.test,
#                                              loopweight = c(2,1.5,1,0.95,0.8,0.1))
# 
# 
