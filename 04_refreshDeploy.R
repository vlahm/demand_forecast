#demand forecasting project
#created: 4/1/17
#author: Mike Vlah (vlahm13@gmail.com)

#clear environment
rm(list=ls()); cat('\014')

#set working directory (CHANGE THIS to the location of the 'app' directory on your system)
privdir = Sys.getenv('privdir')
setwd(paste0('~/git/', privdir, '/demand_forecasting_public/app/'))

#run preprocessing and model scripts
source('../02_preprocessing.R')
source('../03_fitModel.R')

#redeploy the app
if(!require(rsconnect)) install.packages('rsconnect')
rsconnect::deployApp('.', appName='demand_forecast')
