#'Function to read input and output file locations
library(XML)
library(xml2)
library(pracma)
library(kulife)
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)


source('R/Aqua_library.R')
source('R/ReadFileLocations.R')
source('R/ReadWeatherInputs.R')
source('R/ReadClockParameters.R')
source('R/ReadFieldManagement.R')
source('R/ReadModelParameters.R')
source('R/ReadIrrigationManagement.R')
source('R/ReadGroundwaterTable.R')
source('R/ComputeVariables.R')
source('R/ComputeCropCalendar.R')
source('R/CalculateHILinear.R')
source('R/CalculateHIGC.R')
source('R/ReadModelInitialConditions.R')
source('R/PerformSimulation.R')
source('R/ExtractWeatherData.R')
source('R/Solution.R')
source('R/CheckModelTermination.R')
source('R/GrowingDegreeDay.R')
source('R/CheckGroundwaterTable.R')
source('R/PreIrrigation.R')
source('R/Drainage.R')
source('R/RainfallPartition.R')
source('R/Irrigation.R')
source('R/RootZoneWater.R')
source('R/Infiltration.R')
source('R/CapillaryRise.R')
source('R/Germination.R')
source('R/GrowthStage.R')
source('R/RootDevelopment.R')
source('R/CanopyCover.R')
source('R/WaterStress.R')
source('R/SoilEvaporation.R')
source('R/EvapLayerWaterContent.R')
source('R/Transpiration.R')
source('R/AerationStress.R')
source('R/GroundwaterInflow.R')
source('R/HIrefCurrentDay.R')
source('R/BiomassAccumulation.R')
source('R/TemperatureStress.R')
source('R/HarvestIndex.R')
source('R/CCDevelopment.R')
source('R/AdjustCCx.R')
source('R/CCRequiredTime.R')
source('R/HIadjPreAnthesis.R')
source('R/HIadjPostAnthesis.R')
source('R/UpdateTime.R')
source('R/ResetInitialConditions.R')
source('R/HIadjPollination.R')
source('R/Initialise.R')
source('R/SoilHydraulicProperties.R')
source('R/UpdateCCxCDC.R')


plot_scatter <- function(u, t, folder_name){
  
  o =  strsplit(folder_name, '_')[[1]][4]
  res <- caret::postResample(as.numeric(u$Yield[1:20]),  t[[o]])
  print(res)
  plot(t[[o]], type = 'points', ylim = c(0,11.5), cex.axis = 0.8, 
       xlab = 'Observations 1982-2002',
       ylab = 'Yield (ton/ha)', pch = 19)
  points(as.numeric(u$Yield[1:20]), col='red', cex.axis = 0.8, 
         xlab = 'Observations 1982-2002',
         ylab = 'Yield (ton/ha)', pch = 19)
  legend("bottomleft", c("GUI", "AquaCropR"), col = 1:2, pch = 19,
         y.intersp=1, bty='n', title = paste('R2: ', 
                                             round(res[[2]],2), 
          'RMSE: ',  round(res[[1]],2)), cex = 0.8, xjust=0)
  #print(u$Yield)
  
  #plot(t[[o]], as.numeric(u$Yield[1:20]))
  
}



#library('AquaCropR')
#
#
folder_names <- dir(pattern='input_*')
folder_name <-  folder_names[2]
t <- read.csv('results_AquaCropGUI.csv')

tiff('test.tiff', width  = 800, height = 800, res=150)
par(mfrow = c(2,2), mar=c(4,4,2,2), oma=c(0,0.5,0,2))
for(folder_name in folder_names[c(2:5)]){
    FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', 
                                           sep=''))
    #break
    InitialiseStruct <- Initialise(FileLocation)
    
    Outputs <- PerformSimulation(InitialiseStruct)
    Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
    Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
    Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
    Outputs <- setDT(mutate(Outputs, DOY = convertDOY(Outputs$PlantingDate)))
    Outputs_month <- split(Outputs, by = 'PlantingDate')
    i = lapply(Outputs_month, function(x) x[as.numeric(which(x$Yield == 
                                                               max(x$Yield)))][1])
    u = data.frame(t(data.frame(rbind(sapply(i, function(x) x)))))
    
    plot_scatter(u, t, folder_name)
    
   
}
dev.off() 
