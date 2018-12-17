#'Function to read input and output file locations
library(XML)
library(xml2)
library(pracma)
library(kulife)
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)
library(AquaCropR)


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
    d[['Yield']] <- 'Grain Yield t/ha'
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
              axis.text.x = element_text(size = 11.5),
              axis.text.y = element_text(size = 11.5),
              # legend.text = element_text(size = 12),
              #legend.position="bottom")
              legend.position = "none")
      print(p)
      dev.off() 
   
    }


