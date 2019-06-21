#'Function to read input and output file locations
library(XML)
library(xml2)
library(pracma)
library(kulife)
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)
library(DEoptim)
library(AquaCropR)


    # Read observed data
    emp_data <- read.csv('input_calibrate/test_output.csv', header = TRUE)
    
    # Set folder where files are located
    folder_name <- dir(pattern='input_cali*')
 
    # Read file locations
    FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', 
                                           sep=''))
    # Initialise structure
    InitialiseStruct <- Initialise(FileLocation)
    
    
    # Set optimisation parameters
    control = DEoptim.control(itermax = 5)
    
    # Set parameters boundaries (lower and upper limits)
    CGC <- c(0.012494, 0.012494)
    CCx <- c(0.94, 0.94)
    
    param_array <- rbind(CGC, CCx)
     
    # Call optimiser
    opt_par <- SetOptimiser(param_array, InitialiseStruct, emp_data, control)
    print(opt_par)
  