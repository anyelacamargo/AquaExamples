library(XML)
library(xml2)
library(pracma)
library(kulife)
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)
library(elliptic)
library(doParallel)

#library(AquaCropR)

lapply(paste('../aquacropR/R/', list.files('../aquacropr/R', pattern='*.R'), 
             sep=''), function(x) source(x))

plot_scatter <- function(u, observed_data, folder_name, label_name, o){
  
  o =  strsplit(folder_name, '_')[[1]][4]
  res <- caret::postResample(as.numeric(u$Yield[1:20]),  observed_data[[o]])
  print(res)
  plot(observed_data[[o]], type = 'points', ylim = c(0,17), cex.axis = 0.8, 
       xlab = 'Obs years 1982-2002',
       ylab = 'Grain Yield (t/ha)', pch = 17, main = o)
  points(as.numeric(u$Yield[1:20]), col='red', cex.axis = 0.8, 
         xlab = 'Observations years 1982-2002',
         ylab = 'Grain Yield (t/ha)', pch = 19)
  legend("topleft", c("AquaCrop GUI", "AquaCropR"), col = 1:2, pch = c(17:19),
         y.intersp=1, bty='n', title = paste('R2: ', 
                                             round(res[[2]],2), 
          'RMSE: ',  round(res[[1]],2)), cex = 0.8, xjust=0)
  legend("bottomleft", label_name, bty='n')
  
}

#break
folder_name <- dir(pattern='input_wheat_cordoba')
print(folder_name)

FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', 
                                        sep=''))
InitialiseStruct <- Initialise(FileLocation)
Outputs <- PerformSimulation(InitialiseStruct)
Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
Outputs <- setDT(mutate(Outputs, DOY = convertDOY(Outputs$PlantingDate)))
Outputs_month <- split(Outputs, by = 'PlantingDate')
plot(Outputs$Bio)
