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

library(AquaCropR)



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


cl <- makeCluster(detectCores() -1)
registerDoParallel(cl)

folder_names <- dir(pattern='input_wheat_tunis_s1')

# foreach(folder_name =  folder_names) %do%{
#     FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', sep=''))
#     InitialiseStruct <- Initialise(FileLocation)
#     
#     Outputs <- PerformSimulation(InitialiseStruct)
#     Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
#     Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
#     Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
#     Outputs <- setDT(mutate(Outputs, DOY = convertDOY(Outputs$PlantingDate)))
#     Outputs_month <- split(Outputs, by = 'PlantingDate')
#     i = lapply(Outputs_month, function(x) x[as.numeric(which(x$Yield == 
#                                                                max(x$Yield)))][1])
#     u = data.frame(t(data.frame(rbind(sapply(i, function(x) x)))))
#     
#     d <- list()
#     d[['RefBio']] <- 'Biomass (g m-2)'
#     d[['Yield']] <- 'Grain Yield t/ha'
#     d[['CC']] <- 'Canopy cover (%)'
#     d[['Infl']] <- 'Infiltration (mm)'
#     d[['Irr']] <- 'Irrigation (mm)'
#     d[['Et0']] <- 'Et0'
#     
#      foreach(cname = names(d)) %do% {
#        tiff(paste(FileLocation$Output, 'Figure_', cname, '.tiff', sep=''),  
#             width = 800,height = 600, res = 145)
#        p <- ggplot(Outputs, aes(x = TotGDD, y = Outputs[[cname]], 
#                                 col = PlantingDate)) +
#          geom_line(aes(linetype=PlantingDate, color=PlantingDate), size = 0.7) + 
#          theme_bw() +  labs(y = d[[cname]], x = 'cum Degree Day (cd)') +
#          theme(axis.title.x = element_text(size = 16),
#                axis.title.y = element_text(size = 16),
#                axis.text.x = element_text(size = 11.5, angle = 90),
#                axis.text.y = element_text(size = 11.5, angle = 90),
#                # legend.text = element_text(size = 12),
#                #legend.position="bottom")
#                legend.position = "none")
#        print(p)
#        dev.off() 
#        
#     }
#    
# }


observed_data <- read.csv('results_AquaCropGUI.csv')
l <- c('A', 'B', 'C', 'D')
ii = 1
tiff('Fig2.tiff', width  = 800, height = 800, res=150)
  par(mfrow = c(2,2), mar=c(4,4,2,0), oma=c(0,1,0,0.5))
  foreach(folder_name = folder_names) %do% {
    FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', 
                                        sep=''))
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
    plot_scatter(u, observed_data, folder_name, l[ii])
    ii = ii + 1
  }
dev.off() 

uu <- u[, c(1:3, 40)]
