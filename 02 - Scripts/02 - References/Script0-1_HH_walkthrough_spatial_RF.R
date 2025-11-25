###HH_walkthrough_spatial_RF
#Library packages
library('tidyverse') #Basic organizing and plotting
library('randomForest') #Original methodology
library('caret') #CART model support
library('iml') #interpret machine learning
library('pdp') #partial dependence plots
library('patchwork') #Stack plots together
library('spatialRF') #Spatial Random Forests



### Format data
#----------------------------#
#####Read and format data from S. Larocque #######################----
#-------------------------------------------------------------#
HH_data_daily_sp <- readRDS("01 - Data/03 - Hamilton Harbour/All_spp_fish_rcvr_dataset.rds")
HH_data_daily_sp <- HH_data_daily_sp %>% 
                     rename(season=thermo_season, hard=hard2, species=common_name_e) %>% 
                     mutate(season = case_when(season=="Spring"~"spring",
                                               season=="Summer"~"summer",
                                               season=="Fall"~"fall",
                                               season=="Winter"~"winter"),
                            season=as.factor(season))


str(HH_data_daily_sp)

#Pick a species
data<- filter(HH_data_daily_sp, species =="Largemouth Bass")
data<- data %>% 
 mutate(presence2 = as.numeric(presence)-1,
        year2 = as.numeric(year),
        season2 = as.numeric(season),
        )

#Trim dataset for expedience
data <- sample_n(data, 1000) 

###Split up data
data$nrow <- 1:length(data$date)
head(data)

data.train <- data %>% sample_frac(0.7) # 70/30 split. 
data.test <- data[!(data$nrow %in% data.train$nrow),]
data.tune <- data.test %>% sample_frac(0.5) 
data.test <- data.test[!(data.test$nrow %in% data.tune$nrow),]


#####SPatial RF example######################----
#-------------------------------------------------------------#
  ### Make the RF models
  #----------------------------#
  #random seed for reproducibility
  random.seed <- 1987
  #coordinates of the cases
  xy <- data.train[, c("deploy_lat", "deploy_long")]
  #distance matrix
  dist.matrix <- dist(xy)
  #distance thresholds (same units as distance_matrix)
  dist.thresholds <- c(0, 0.005, 0.01, 0.025, 0.05, 0.075)

  ### Non-spatial model
  RF_non.spatial <- spatialRF::rf(
    data = data.train,
    dependent.variable.name = "presence2",
    predictor.variable.names = c("season2", "year2", "SAV", "WL", "fetch", "hard", "secchi"),
    distance.matrix = as.matrix(dist.matrix),
    distance.thresholds = dist.thresholds,
    xy = xy, #not needed by rf, but other functions read it from the model
    seed = random.seed,
    verbose = TRUE
  )

  ### Spatial model
  RF_spatial <- spatialRF::rf_spatial(
    model = RF_non.spatial,
    method = "mem.moran.sequential", #default method
    verbose = FALSE,
    seed = random.seed
    )

  ### Inspect the models
  #----------------------------#
  #Plot Morans I
  spatialRF::plot_moran(
    RF_spatial, 
    verbose = FALSE)
  
  #Compare model importance
  p1 <- spatialRF::plot_importance(
    RF_non.spatial, 
    verbose = FALSE) + 
    ggplot2::ggtitle("Non-spatial model") 
  
  p2 <- spatialRF::plot_importance(
    RF_spatial,
    verbose = FALSE) + 
    ggplot2::ggtitle("Spatial model")
  
  p1 | p2 

  
   #####Build pdps
   #----------------------------#
   ###Season
   #----------------------------#
   partial.spatial.season <- RF_spatial %>% pdp::partial(pred.var = "season", prob = TRUE, which.class='1', train=data.train)
   partial.spatial.season$season <- factor(partial.spatial.season$season, levels=c("winter", "spring", "summer", "fall"))
   
   # variable partial dependencies
   partial.spatial.SAV <- RF_spatial %>% pdp::partial(pred.var = "SAV", prob = TRUE, which.class='1', train=data.train)
   partial.spatial.exposure <- RF_spatial %>% pdp::partial(pred.var = "exposure", prob = TRUE, which.class='1', train=data.train)
   partial.spatial.depth <- RF_spatial %>% pdp::partial(pred.var = "depth", prob = TRUE, which.class='1', train=data.train)
   partial.spatial.season <- RF_spatial %>% pdp::partial(pred.var = "season", prob = TRUE, which.class='1', train=data.train)
   partial.spatial.region <- RF_spatial %>% pdp::partial(pred.var = "region", prob = TRUE, which.class='1', train=data.train)
   partial.spatial.SAVseason <- RF_spatial %>% pdp::partial(pred.var = c("SAV","season"), prob = TRUE, which.class='1', train=data.train)
   partial.spatial.depthseason <- RF_spatial %>% pdp::partial(pred.var = c("depth","season"), prob = TRUE, which.class='1',train=data.train)
   partial.spatial.exposureseason <- RF_spatial %>% pdp::partial(pred.var = c("exposure","season"), prob = TRUE, which.class='1',train=data.train)
   partial.spatial.regionseason <- RF_spatial %>% pdp::partial(pred.var = c("region","season"), prob = TRUE, which.class='1',train=data.train)
   
   
   #####Plot pdps
   #----------------------------#
   ###Univariate Plots
   #----------------------------#
   plot.spatial.season <- ggplot(partial.spatial.season, aes(season, yhat))+geom_boxplot(col="#00BC59")+
     ylab(bquote(~hat(y)))+xlab("Season")+theme_bw()+
     coord_cartesian(ylim=c(1,1.5))
   
   plot.spatial.sav <- ggplot(partial.spatial.SAV, aes(SAV, yhat))+geom_smooth(col="#00BC59")+
     ylab(bquote(~hat(y)))+theme_bw()+xlab("SAV (%)")+
     coord_cartesian(ylim=c(1,1.5), xlim=c(0,100))
  
   plot.spatial.exp <- ggplot(partial.spatial.exposure, aes(exposure, yhat))+geom_smooth(col="#00BC59")+
     ylab(bquote(~hat(y)))+xlab("Exposure")+theme_bw()+
     coord_cartesian(ylim=c(1,1.5))
  
   plot.spatial.depth <- ggplot(partial.spatial.depth, aes(depth, yhat))+geom_smooth(col="#00BC59")+
     ylab(bquote(~hat(y)))+xlab("Depth (m)")+theme_bw()+
     coord_cartesian(ylim=c(1,1.5))
   
   #need to use %in% here
   plot.spatial.region <- ggplot(partial.spatial.region %>% filter(region %in% data$region), aes(region, yhat))+geom_boxplot(col="#00BC59")+
     ylab(bquote(~hat(y)))+xlab("Region")+theme_bw()+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
     coord_cartesian(ylim=c(1,1.5))
   
   
   ###Bivariate Plots
   #----------------------------#
   partial.spatial.SAVseason$season <- factor(partial.spatial.SAVseason$season, levels=c("winter", "spring", "summer", "fall"))
   plot.spatial.seasonSAV <- ggplot(partial.spatial.SAVseason, aes(season, SAV, fill=yhat))+geom_tile()+scale_fill_viridis_c()+
     theme_bw()+labs(fill=bquote(~hat(y)))+ylab("SAV (%)")+xlab("Season")
  
   partial.spatial.depthseason$season <- factor(partial.spatial.depthseason$season, levels=c("winter", "spring", "summer", "fall"))
   plot.spatial.seasondepth <- ggplot(partial.spatial.depthseason, aes(season, depth, fill=yhat))+geom_tile()+scale_fill_viridis_c()+
     theme_bw()+labs(fill=bquote(~hat(y)))+ylab("Depth (m)")+xlab("Season")
  
   partial.spatial.exposureseason$season <- factor(partial.spatial.exposureseason$season, levels=c("winter", "spring", "summer", "fall"))
   plot.spatial.seasonexposure <- ggplot(partial.spatial.exposureseason, aes(season, exposure, fill=yhat))+geom_tile()+scale_fill_viridis_c()+
     theme_bw()+labs(fill=bquote(~hat(y)))+ylab("Exposure")+xlab("Season")
  
   partial.spatial.regionseason$season <- factor(partial.spatial.regionseason$season, levels=c("winter", "spring", "summer", "fall"))
   plot.spatial.seasonregion <- ggplot(partial.spatial.regionseason, aes(season, region, fill=yhat))+geom_tile()+scale_fill_viridis_c()+
     theme_bw()+labs(fill=bquote(~hat(y)))+ylab("Region")+xlab("Season")
  
  
   ###Combined pubs
   #----------------------------#
   plot.spatial.combined <-
     plot.spatial.season+ plot_spacer()+
     plot.spatial.sav+plot.spatial.seasonSAV+
     plot.spatial.depth+plot.spatial.seasondepth+
     plot.spatial.exp+plot.spatial.seasonexposure+
     plot.spatial.region+plot.spatial.seasonregion+
     plot_layout(ncol=2)
   plot.spatial.combined
 
    
   
#####Look at residuals############################################----
#-------------------------------------------------------------#
spatial.predictors <- spatialRF::get_spatial_predictors(RF_spatial)
pr <- data.frame(spatial.predictors, xy)   

###Plot receivers
#----------------------------#
#Set map data
# Retrieve Google API key from keyring (secure storage)
# To set key initially, run once: keyring::key_set("google_api", username = "Jake")
param_google_api_key <- keyring::key_get("google_api", username = "Jake")
ggmap::register_google(key = param_google_api_key)
HHmap <- get_googlemap(center = c(lon=mean(xy$deploy_long), lat=mean(xy$deploy_lat)),
                       zoom = 12, size = c(560, 560), scale = 4,
                       maptype = c("satellite"))

###map of rec
ggmap(HHmap, extent='normal')+
  geom_point(data = pr, 
             aes(x = deploy_long, y = deploy_lat, color = spatial.predictors), 
             size = 2.5) +
  scale_color_viridis_c(option = "F") +
  theme_bw() +
  labs(color = "Eigenvalue") +
  ggtitle("Spatial autocorrelation for Species") + 
  theme(legend.position = "bottom")+ 
  xlab("Longitude") + 
  ylab("Latitude")


   
   
   
   
   
   
   
   
   
   







   
 
