rm(list = ls())

## LOAD LIBRARIES
library(tidyverse)
library(sf)
library(dplyr)
library(webshot) #needed to save maps. on new systems, may have to do: webshot::install_phantomjs()
library(mapview) #needed to make maps
library(tmap)
library(googledrive)
library(lubridate)

#make_figs = 'no' #'yes' 
make_figs = 'yes'

#find current directory, setwd to current directory
curr_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curr_dir)

fn = '2000-2024 NEFSC Data.csv'

## inputs
#years
begYEAR = 2015
endYEAR = 2020

#months
begMONTH = 1
endMONTH = 12

#season
ssn_beg=rbind(c(1,1), c(2,1), c(3,1), c(4,1), c(5,1), c(6,1), c(7,1), c(8,1), c(9,1), c(10,1), c(11,1), c(12,1))
ssn_end=rbind(c(1,31), c(2,29), c(3,31), c(4,30), c(5,31), c(6,30), c(7,31), c(8,30), c(9,30), c(10,31), c(11,30), c(12,31) )

# if necessary, download the data
file_loc = paste0(curr_dir, "/data/", fn)
# if (!file.exists(file_loc)){ #if file already exists, this statement will not be 
#   
#   is_googledrive_available <- require("googledrive") #logial variable indicating if the package is installed
#   
#   # install googledrive if necessary
#   if (is_googledrive_available == FALSE){
#     install.packages("googledrive")
#   }
#   
#   #download the file
#   library("googledrive")
#   setwd(paste0(curr_dir, "/data/"))
#   drive_download("Copy of Dan & Kelsey All FUNDY data 05-19-2023.CSV")
#   setwd(curr_dir)  
# }
