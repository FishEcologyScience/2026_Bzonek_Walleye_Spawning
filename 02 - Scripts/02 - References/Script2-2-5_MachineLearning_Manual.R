
#####Mature Silver Shiner Random Forest###########################----
#-------------------------------------------------------------#
RFC_J_data <- data_DFO_site %>% select(
  SSp, Year, Water.Temperature, AvgOfStream.depth..m., 
  AvgOfWater.velocity..msec., riparianveg_None) %>% na.roughfix() #Set NAs as median value

#Split Train and Test data
set.seed(1758)
RFC_J_data_train <- sample_frac(RFC_J_data, 0.7) #Subset 70% for train
RFC_J_data_test <-anti_join(RFC_J_data, RFC_J_data_train) #Remaining 30% for test

 

###Make RandomForest model
#----------------------------#
set.seed(1758)
RFC_J_model <- randomForest(formula=SSp ~., data = RFC_J_data_train, replace=TRUE, 
                          #na.action=na.roughfix, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                          classwt=c(1,10), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                          importance=TRUE, do.trace=1000, ntree=1000)


###Evaluate RandomForest model
#----------------------------#
##Make confusion Matrix on test dataset with 'caret'
RFC_J_data_test$prediction1 <- predict(RFC_J_model, RFC_J_data_test)

caret::confusionMatrix(
  data=RFC_J_data_test$prediction1, 
  reference=RFC_J_data_test$SSp,  
  positive="1")

RFC_J_diagnostic <- list() #Start a Summary List
#Save and print confusion Matrix
RFC_J_diagnostic$ConfusionMatrix <- 
  caret::confusionMatrix(
    data=RFC_J_data_test$prediction1, 
    reference=RFC_J_data_test$SSp,  
    positive="1",
  )$table %>% print() %>% as.vector()


#Build and Print Summary Table
RFC_J_diagnostic$Summary <-
  data.frame(Accuracy = (RFC_J_diagnostic$ConfusionMatrix[1]+RFC_J_diagnostic$ConfusionMatrix[4])/sum(RFC_J_diagnostic$ConfusionMatrix),
             AbsenceAccuracy = RFC_J_diagnostic$ConfusionMatrix[1]/(RFC_J_diagnostic$ConfusionMatrix[1]+RFC_J_diagnostic$ConfusionMatrix[3]),
             PresenceAccuracy = RFC_J_diagnostic$ConfusionMatrix[4]/(RFC_J_diagnostic$ConfusionMatrix[4]+RFC_J_diagnostic$ConfusionMatrix[2])) %>% 
  print()


###Investigate variable importance 
#----------------------------#
###Evaluate RandomForest model
RFC_J_data$pred.RFC_J <- predict(RFC_J_model, RFC_J_data)
RFC_J_CM <- caret::confusionMatrix(RFC_J_data$pred.RFC_J, RFC_J_data$SSp, positive="1")
RFC_J_CM


RFC_J_VarImp <- data.frame(importance(RFC_J_model)) %>% 
  mutate(predictor=rownames(.)) %>% 
  arrange(MeanDecreaseAccuracy)

#psuedo-R2 
RFC_J_VarImp <- RFC_J_VarImp  %>% 
  dplyr::mutate(#Find sum of positive values
    MDAtotal = sum(MeanDecreaseAccuracy[which(MeanDecreaseAccuracy > 0)]),
    #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
    pR2 = pmax(MeanDecreaseAccuracy/MDAtotal, 0),
    #Calibrate pseudo R2 by multiplying into model accuracy
    pR2 = pR2*RFC_J_CM$overall[1]
  )

##Plot performance metrics for predictor variables
#Mean Decrease Accuracy
RFC_J_plot <- list()

RFC_J_plot$R2 <- ggplot(RFC_J_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
  geom_segment(aes(x=reorder(predictor, pR2),
                   xend=reorder(predictor, pR2), y=0, yend=pR2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip()+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)#+ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))
RFC_J_plot$R2




#####Juvenile Silver Shiner Random Forest###########################----
#-------------------------------------------------------------#
RFI_J_data <- RFI_data %>% select(
  immaturep, Month, Year, Water.Temperature) %>% na.roughfix() #Set NAs as median value

#Split Train and Test data
set.seed(1758)
RFI_J_data_train <- sample_frac(RFI_J_data, 0.7) #Subset 70% for train
RFI_J_data_test <-anti_join(RFI_J_data, RFI_J_data_train) #Remaining 30% for test



###Make RandomForest model
#----------------------------#
set.seed(1758)
RFI_J_model <- randomForest(formula=immaturep ~., data = RFI_J_data_train, replace=TRUE, 
                            #na.action=na.roughfix, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                            classwt=c(1,10), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                            importance=TRUE, do.trace=1000, ntree=1000)


###Evaluate RandomForest model
#----------------------------#
##Make confusion Matrix on test dataset with 'caret'
RFI_J_data_test$prediction1 <- predict(RFI_J_model, RFI_J_data_test)

caret::confusionMatrix(
  data=RFI_J_data_test$prediction1, 
  reference=RFI_J_data_test$immaturep,  
  positive="1")

RFI_J_diagnostic <- list() #Start a Summary List
#Save and print confusion Matrix
RFI_J_diagnostic$ConfusionMatrix <- 
  caret::confusionMatrix(
    data=RFI_J_data_test$prediction1, 
    reference=RFI_J_data_test$immaturep,  
    positive="1",
  )$table %>% print() %>% as.vector()


#Build and Print Summary Table
RFI_J_diagnostic$Summary <-
  data.frame(Accuracy = (RFI_J_diagnostic$ConfusionMatrix[1]+RFI_J_diagnostic$ConfusionMatrix[4])/sum(RFI_J_diagnostic$ConfusionMatrix),
             AbsenceAccuracy = RFI_J_diagnostic$ConfusionMatrix[1]/(RFI_J_diagnostic$ConfusionMatrix[1]+RFI_J_diagnostic$ConfusionMatrix[3]),
             PresenceAccuracy = RFI_J_diagnostic$ConfusionMatrix[4]/(RFI_J_diagnostic$ConfusionMatrix[4]+RFI_J_diagnostic$ConfusionMatrix[2])) %>% 
  print()


###Investigate variable importance 
#----------------------------#
###Evaluate RandomForest model
RFI_J_data$pred.RFI_J <- predict(RFI_J_model, RFI_J_data)
RFI_J_CM <- caret::confusionMatrix(RFI_J_data$pred.RFI_J, RFI_J_data$immaturep, positive="1")


RFI_J_VarImp <- data.frame(importance(RFI_J_model)) %>% 
  mutate(predictor=rownames(.)) %>% 
  arrange(MeanDecreaseAccuracy)

#psuedo-R2 
RFI_J_VarImp <- RFI_J_VarImp  %>% 
  dplyr::mutate(#Find sum of positive values
    MDAtotal = sum(MeanDecreaseAccuracy[which(MeanDecreaseAccuracy > 0)]),
    #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
    pR2 = pmax(MeanDecreaseAccuracy/MDAtotal, 0),
    #Calibrate pseudo R2 by multiplying into model accuracy
    pR2 = pR2*RFI_J_CM$overall[1]
  )

##Plot performance metrics for predictor variables
#Mean Decrease Accuracy
RFI_J_plot <- list()

RFI_J_plot$R2 <- ggplot(RFI_J_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
  geom_segment(aes(x=reorder(predictor, pR2),
                   xend=reorder(predictor, pR2), y=0, yend=pR2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip()+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)#+ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))
RFI_J_plot$R2




#####Mature Silver Shiner Random Forest###########################----
#-------------------------------------------------------------#
RFM_J_data <- RFM_data %>% select(
  maturep, Year, AvgOfStream.depth..m., 
  AvgOfWater.velocity..msec., substrate_Silt) %>% na.roughfix() #Set NAs as median value

#Split Train and Test data
set.seed(1758)
RFM_J_data_train <- sample_frac(RFM_J_data, 0.7) #Subset 70% for train
RFM_J_data_test <-anti_join(RFM_J_data, RFM_J_data_train) #Remaining 30% for test



###Make RandomForest model
#----------------------------#
set.seed(1758)
RFM_J_model <- randomForest(formula=maturep ~., data = RFM_J_data_train, replace=TRUE, 
                            #na.action=na.roughfix, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                            classwt=c(1,10), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                            importance=TRUE, do.trace=1000, ntree=1000)


###Evaluate RandomForest model
#----------------------------#
##Make confusion Matrix on test dataset with 'caret'
RFM_J_data_test$prediction1 <- predict(RFM_J_model, RFM_J_data_test)

caret::confusionMatrix(
  data=RFM_J_data_test$prediction1, 
  reference=RFM_J_data_test$maturep,  
  positive="1")

RFM_J_diagnostic <- list() #Start a Summary List
#Save and print confusion Matrix
RFM_J_diagnostic$ConfusionMatrix <- 
  caret::confusionMatrix(
    data=RFM_J_data_test$prediction1, 
    reference=RFM_J_data_test$maturep,  
    positive="1",
  )$table %>% print() %>% as.vector()


#Build and Print Summary Table
RFM_J_diagnostic$Summary <-
  data.frame(Accuracy = (RFM_J_diagnostic$ConfusionMatrix[1]+RFM_J_diagnostic$ConfusionMatrix[4])/sum(RFM_J_diagnostic$ConfusionMatrix),
             AbsenceAccuracy = RFM_J_diagnostic$ConfusionMatrix[1]/(RFM_J_diagnostic$ConfusionMatrix[1]+RFM_J_diagnostic$ConfusionMatrix[3]),
             PresenceAccuracy = RFM_J_diagnostic$ConfusionMatrix[4]/(RFM_J_diagnostic$ConfusionMatrix[4]+RFM_J_diagnostic$ConfusionMatrix[2])) %>% 
  print()


###Investigate variable importance 
#----------------------------#
###Evaluate RandomForest model
RFM_J_data$pred.RFM_J <- predict(RFM_J_model, RFM_J_data)
RFM_J_CM <- caret::confusionMatrix(RFM_J_data$pred.RFM_J, RFM_J_data$maturep, positive="1")


RFM_J_VarImp <- data.frame(importance(RFM_J_model)) %>% 
  mutate(predictor=rownames(.)) %>% 
  arrange(MeanDecreaseAccuracy)

#psuedo-R2 
RFM_J_VarImp <- RFM_J_VarImp  %>% 
  dplyr::mutate(#Find sum of positive values
    MDAtotal = sum(MeanDecreaseAccuracy[which(MeanDecreaseAccuracy > 0)]),
    #Calculate uncalibrated pseudo R2 as proportion of total MDA, with min set @ 0
    pR2 = pmax(MeanDecreaseAccuracy/MDAtotal, 0),
    #Calibrate pseudo R2 by multiplying into model accuracy
    pR2 = pR2*RFM_J_CM$overall[1]
  )

##Plot performance metrics for predictor variables
#Mean Decrease Accuracy
RFM_J_plot <- list()

RFM_J_plot$R2 <- ggplot(RFM_J_VarImp, aes(x=reorder(predictor, pR2), y=pR2))+
  geom_segment(aes(x=reorder(predictor, pR2),
                   xend=reorder(predictor, pR2), y=0, yend=pR2))+
  geom_point(fill="#195190FF", col="black", size=3.5, pch=21)+
  coord_flip()+
  theme_bw()+ylab(bquote("pseudo-R"^2))+
  xlab("Predictor")+geom_hline(yintercept = 0, linetype=2)#+ggtitle(bquote("Variable Importance (pseudo-R"^2~")"))

RFM_J_plot$R2
