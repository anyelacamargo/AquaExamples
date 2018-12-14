#'Function to read input and output file locations
library(XML)
library(xml2)
library(pracma)
library(kulife)
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)


source('../AquaCropR/R/Aqua_library.R')
source('../AquaCropR/R/ReadFileLocations.R')
source('../AquaCropR/R/ReadWeatherInputs.R')
source('../AquaCropR/R/ReadClockParameters.R')
source('../AquaCropR/R/ReadFieldManagement.R')
source('../AquaCropR/R/ReadModelParameters.R')
source('../AquaCropR/R/ReadIrrigationManagement.R')
source('../AquaCropR/R/ReadGroundwaterTable.R')
source('../AquaCropR/R/ComputeVariables.R')
source('../AquaCropR/R/ComputeCropCalendar.R')
source('../AquaCropR/R/CalculateHILinear.R')
source('../AquaCropR/R/CalculateHIGC.R')
source('../AquaCropR/R/ReadModelInitialConditions.R')
source('../AquaCropR/R/PerformSimulation.R')
source('../AquaCropR/R/ExtractWeatherData.R')
source('../AquaCropR/R/Solution.R')
source('../AquaCropR/R/CheckModelTermination.R')
source('../AquaCropR/R/GrowingDegreeDay.R')
source('../AquaCropR/R/CheckGroundwaterTable.R')
source('../AquaCropR/R/PreIrrigation.R')
source('../AquaCropR/R/Drainage.R')
source('../AquaCropR/R/RainfallPartition.R')
source('../AquaCropR/R/Irrigation.R')
source('../AquaCropR/R/RootZoneWater.R')
source('../AquaCropR/R/Infiltration.R')
source('../AquaCropR/R/CapillaryRise.R')
source('../AquaCropR/R/Germination.R')
source('../AquaCropR/R/GrowthStage.R')
source('../AquaCropR/R/RootDevelopment.R')
source('../AquaCropR/R/CanopyCover.R')
source('../AquaCropR/R/WaterStress.R')
source('../AquaCropR/R/SoilEvaporation.R')
source('../AquaCropR/R/EvapLayerWaterContent.R')
source('../AquaCropR/R/Transpiration.R')
source('../AquaCropR/R/AerationStress.R')
source('../AquaCropR/R/GroundwaterInflow.R')
source('../AquaCropR/R/HIrefCurrentDay.R')
source('../AquaCropR/R/BiomassAccumulation.R')
source('../AquaCropR/R/TemperatureStress.R')
source('../AquaCropR/R/HarvestIndex.R')
source('../AquaCropR/R/CCDevelopment.R')
source('../AquaCropR/R/AdjustCCx.R')
source('../AquaCropR/R/CCRequiredTime.R')
source('../AquaCropR/R/HIadjPreAnthesis.R')
source('../AquaCropR/R/HIadjPostAnthesis.R')
source('../AquaCropR/R/UpdateTime.R')
source('../AquaCropR/R/ResetInitialConditions.R')
source('../AquaCropR/R/HIadjPollination.R')
source('../AquaCropR/R/Initialise.R')
source('../AquaCropR/R/SoilHydraulicProperties.R')
source('../AquaCropR/R/UpdateCCxCDC.R')

plot_scatter <- function(u, t, folder_name){
  
  o =  strsplit(folder_name, '_')[[1]][4]
  res <- caret::postResample(as.numeric(u$Yield[1:20]),  t[[o]])
  print(res)
  plot(t[[o]], type = 'points', ylim = c(0,11), xlab = 'Observations',
       ylab = 'Yield', pch = 19)
  points(as.numeric(u$Yield[1:20]), col='red', xlab = 'Observations',
         ylab = 'Yield', pch = 19)
  legend("bottomleft", c("GUI", "AquaCropR"), col = 1:2, pch = 19,
         y.intersp=1, bty='n', title = paste('Test:', o, 'R2: ', round(res[[2]],2), 
          'RMSE: ',  round(res[[1]],2)), cex = 0.8, xjust=0)
  #print(u$Yield)
  
  #plot(t[[o]], as.numeric(u$Yield[1:20]))
  
}



#library('AquaCropR')
#
#
    folder_name <- dir(pattern='input_maize*')
 
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
    #break
    d <- list()
    d[['RefBio']] <- 'Biomass (g m-2)'
    d[['Yield']] <- 'Yield t/ha'
    d[['CC']] <- 'Canopy cover (%)'
    d[['Infl']] <- 'Infiltration (mm)'
    d[['Irr']] <- 'Irrigation (mm)'
    d[['Et0']] <- 'Et0'
    
   
 
    for(cname in names(d)){
      tiff(paste(FileLocation$Output, 'Figure_', cname, '.tiff', sep=''),  
            width = 800,height = 600, res = 145)
      p <- ggplot(Outputs, aes(x = TotGDD, y = Outputs[[cname]], 
                               col = PlantingDate)) +
        geom_line(aes(linetype=PlantingDate, color=PlantingDate), size = 0.7) + 
        theme_bw() +  labs(y = d[[cname]], x = 'cum Degree Day (cd)') +
        theme(axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 11.5, angle = 90),
              axis.text.y = element_text(size = 11.5, angle = 90),
              # legend.text = element_text(size = 12),
              #legend.position="bottom")
              legend.position = "none")
      print(p)
      dev.off() 
   
    }


