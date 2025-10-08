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

ssn_type = "within" # within year seasons
ssn_type = "across" # across year seasons, e.g. for climatologies

#find current directory, setwd to current directory
curr_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curr_dir)

fn = '2000-2024 NEFSC Data.csv'

## inputs
#years
begYEAR = 2000
endYEAR = 2000
#months
begMONTH = 1
endMONTH = 12

#season (monthly binning)
ssn_beg=rbind(c(1,1), c(2,1), c(3,1), c(4,1), c(5,1), c(6,1), c(7,1), c(8,1), c(9,1), c(10,1), c(11,1), c(12,1))
ssn_end=rbind(c(1,31), c(2,29), c(3,31), c(4,30), c(5,31), c(6,30), c(7,31), c(8,30), c(9,30), c(10,31), c(11,30), c(12,31) )

#load data
file_loc = paste0(curr_dir, "/data/", fn)
#dat <- read_csv(file = file_loc)

setwd(paste0(curr_dir, "/data/"))
## import data
dat <- read_csv(file = "2000-2024 NEFSC Data.csv",
                col_types = cols(SOURCE = col_character(),
                                 PLANE = col_character(),
                                 EVENT_NUMBER = col_double(),
                                 LATITUDE = col_double(),
                                 LONGITUDE = col_double(),
                                 FLIGHT_TYPE = col_double(),
                                 LEGTYPE = col_double(),
                                 LEGSTAGE = col_double(),
                                 PSB_LEGSTAGE = col_double(),
                                 ALTITUDE = col_double(),
                                 HEADING = col_double(),
                                 SPEED = col_double(),
                                 SST_C = col_double(),
                                 VISIBILTY_CODE = col_double(),
                                 VISIBILTY_NM = col_double(),
                                 BEAUFORT = col_double(),
                                 CLOUD_CODE = col_double(),
                                 CLOUD_PERCENT = col_double(),
                                 GLARE_L = col_double(),
                                 GLARE_R = col_double(),
                                 QUALITY_L = col_character(),
                                 QUALITY_R = col_character(),
                                 SIGHTING_NUMBER = col_double(),
                                 SPCODE = col_character(),
                                 ID_RELIABILITY = col_double(),
                                 GROUP_SIZE = col_double(),
                                 CALVES = col_double(),
                                 BEARING = col_double(),
                                 DISTANCE = col_double(),
                                 RELATIVE_HEADING = col_double(),
                                 ACTUAL_HEADING = col_double(),
                                 OBSERVER = col_character(),
                                 OBS_POSITION = col_character(),
                                 ANGLE = col_double(),
                                 CUE = col_character(),
                                 B1_FINAL_CODE = col_character(),
                                 B2_FINAL_CODE = col_character(),
                                 B3_FINAL_CODE = col_character(),
                                 B4_FINAL_CODE = col_character(),
                                 B5_FINAL_CODE = col_character(),
                                 PHOTOS = col_integer(),
                                 EFFORT_COMMENTS = col_character(),
                                 SIGHTING_COMMENTS = col_character(),
                                 EDIT1 = col_character(),
                                 EDIT2 = col_character(),
                                 EDIT3 = col_character(),
                                 MISC_EDIT1 = col_character(),
                                 MISC_EDIT2 = col_character(),
                                 MISC_EDIT3 = col_character(),
                                 MISC_EDIT4 = col_character(),
                                 RID = col_double(),
                                 EVENT_T = col_double(),
                                 DATETIME_ET = col_character()
                )
)
setwd(paste0(curr_dir))

# LEGTYPE LEGSTAGE COMBOS
a = distinct(select(dat,LEGTYPE,LEGSTAGE))
print(a)
