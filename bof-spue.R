rm(list = ls())

## LOAD LIBRARIES
library(tidyverse)
library(sf)
library(dplyr)
library(webshot) #needed to save maps. on new systems, may have to do: webshot::install_phantomjs()
library(mapview) #needed to make maps
library(tmap)
library(googledrive)

#find current directory, setwd to current directory
curr_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curr_dir)

## inputs
#years
begYEAR = 2010
endYEAR = 2020

#months
begMONTH = 8
endMONTH = 9

#season
ssn_beg=rbind(c(8,1))
ssn_end=rbind(c(9,30))

# if necessary, download the data
#file_loc = "~/Documents/WorkDocuments/Projects/Fundy/Dan & Kelsey All FUNDY data 05-19-2023.CSV"
file_loc = paste0(curr_dir, "/data/", "Copy of Dan & Kelsey All FUNDY data 05-19-2023.CSV")
if (!file.exists(file_loc)){ #if file already exists, this statement will not be 
  
  is_googledrive_available <- require("googledrive") #logial variable indicating if the package is installed
  
  # install googledrive if necessary
  if (is_googledrive_available == FALSE){
    install.packages("googledrive")
  }
  
  #download the file
  library("googledrive")
  setwd(paste0(curr_dir, "/data/"))
  drive_download("Copy of Dan & Kelsey All FUNDY data 05-19-2023.CSV")
  setwd(curr_dir)  
}

## import data
dat <- read_csv(file = file_loc, 
                col_types = cols(FILEID = col_character(),
                                 EVENTNO = col_double(),
                                 MONTH = col_double(),
                                 DAY = col_double(),
                                 YEAR = col_double(),
                                 GMT = col_double(),
                                 LATITUDE = col_double(),
                                 LONGITUDE = col_double(),
                                 LEGTYPE = col_double(),
                                 LEGSTAGE = col_double(),
                                 ALT = col_double(),
                                 HEADING = col_double(),
                                 WX = col_character(),
                                 CLOUD = col_double(),
                                 VISIBLTY = col_double(),
                                 BEAUFORT = col_double(),
                                 SPECCODE = col_character(),
                                 IDREL = col_double(),
                                 NUMBER = col_double(),
                                 CONFIDNC = col_double())
)

#restrict to R/V Nereid
dat <- dat %>%
  filter(PLATFORM == 99)

# discard opportunistic surveys
dat <- dat %>%
  mutate(fileid = str_sub(FILEID, start = 1, end = 1)) %>% 
  filter(fileid == "P" | fileid == "p") %>%
  dplyr::select(-fileid)

dat$date_ymd_gmt <- as.Date(with(dat,paste(YEAR,MONTH,DAY,sep="-")),"%Y-%m-%d")
source('padstr0.R')
GMT_strings = padstr0(dat$GMT,6) #pad GMT times so they have 6 digits
#correct instances where "200000" was stored as "02e+05"
GMT_strings[which(GMT_strings == "02e+05")] = "200000"
GMT_strings = paste(dat$date_ymd_gmt, GMT_strings) #append ymd to hms
dat$datetime_GMT = ymd_hms(GMT_strings, tz = 'GMT') #convert
dat$datetime_ET = with_tz(dat$datetime_GMT, "US/Eastern")
dat$date_jday_ET = format(dat$datetime_ET,"%j") #calculate jday based on US/Eastern time
rm(GMT_strings)

#create Year Month Day columns based on US/Eastern tz
dat$YEAR_ET <- as.numeric(format(dat$datetime_ET,"%Y"))
dat$MONTH_ET <- as.numeric(format(dat$datetime_ET,"%m"))
dat$DAY_ET <- as.numeric(format(dat$datetime_ET,"%d"))

# keep only desired years and months (based on US/Eastern tz)
dat <- dat %>%
  filter(YEAR_ET >= begYEAR & YEAR_ET <= endYEAR)
dat <- dat %>%
  filter(MONTH_ET == begMONTH | MONTH_ET == endMONTH)

# create seasons matrix
source('makeSeasons.R')
season <- makeSeasons(begYEAR,endYEAR,ssn_beg,ssn_end)
ssn_beg_date <- as.Date(paste(season[,1],season[,2],season[,3],sep="-"),"%Y-%m-%d")
ssn_end_date <- as.Date(paste(season[,1],season[,4],season[,5],sep="-"),"%Y-%m-%d")
ssn_no = season$SSN_NO
num_ssn = max(ssn_no)
ssn_no_grpd = season$SSN_GRPD_NO
# insert columns with season and season_grpd
dat$season = NA
dat$season_grpd = NA
for (i in 1:length(ssn_beg_date)){
  I = which(dat$datetime_ET >= ssn_beg_date[i] & dat$datetime_ET <= ssn_end_date[i])
  dat$season[I] = ssn_no[i]
  dat$season_grpd[I] = ssn_no_grpd[i]
}

dat <- dat %>%
  mutate(on.off.eff = if_else((BEAUFORT <= 6 & # normally require sea state 0-3, but sea state will be covariate on detection in this model
                                 (
                                   (LEGTYPE == 5 & (LEGSTAGE == 1 | LEGSTAGE == 2 | LEGSTAGE == 5)) | #start, continue, end watch while ship not underway
                                     (LEGTYPE == 6 & (LEGSTAGE == 1 | LEGSTAGE == 2 | LEGSTAGE == 5)) #legtype = 6 indicates ship not underway (listening station)
                                 ) & 
                                 (VISIBLTY >=2 | VISIBLTY == -1) & #pre-2020 changes to NARWC Sightings Database, VISIBLTY >=2 or -1 indicates visibility of at least 2 nautical miles. Negative numbers are no longer used, however this dataset was obtained in 2019 before the change.
                                 (IDREL == 3 | is.na(IDREL)) # if there is a sighting, IDREL must = 3. If no sightings, then IDREL should be NA
  ), 
  1, 0)) %>%
  #now replace all NA with 0 because those are off-effort
  mutate(on.off.eff = ifelse(is.na(on.off.eff), 0, on.off.eff)
  )

# create unique identifier for continuous segments of on-effort
# use cumsum(abs(diff())) to create continuous numbers based upon on.off.eff
dat <- dat %>%
  mutate(on.effort.id = c(1, cumsum(abs(diff(dat$on.off.eff))) + 1))
# filter out on.off.eff == 0
dat <- dat %>%
  filter(dat$on.off.eff !=0)

## REDUCE SIZE OF THE DATASET
keep.cols <- c("FILEID", 
               "EVENTNO", 
               "YEAR", "MONTH", "DAY", 
               "BEAUFORT", 
               "LEGTYPE", "LEGSTAGE", 
               "LATITUDE", "LONGITUDE", 
               "SPECCODE", "IDREL", "NUMBER", 
               "datetime_ET", "date_jday_ET", 
               "on.off.eff", 
               "season", "season_grpd",
               "on.effort.id")
tmpdat <- dat %>%
  dplyr::select(all_of(keep.cols)) #%>%
rm(keep.cols)

#write.csv(dat, file = "dat_with_dist.csv")
#write.csv(tmpdat, file = "tmpdat.csv")

make_figs = 'no' #'yes' 

## ADD GEOMETRY TO DATASET AND MAKE INTO SF OBJECT
#matrix of lat and long
locs = cbind(tmpdat$LONGITUDE, tmpdat$LATITUDE) #raw long/lat points
#convert locations to sfg object of points or linestrings
locs_pts = sfheaders::sf_point(obj = locs) #sfg object (actually it says it's a 'sf' object)

# #convert to sfc object
locs_sfc = st_as_sfc(locs_pts, crs = "EPSG:4326") #sfc object, but CRS doesn't stick. should work though.
st_crs(locs_sfc) = "EPSG:4326" #this sets CRS and it sticks

#convert data to sf object, appending original dataset
tmpdat_sf = st_sf(tmpdat, geometry = locs_sfc)    # sf object
rm(locs, locs_pts, locs_sfc) # clean up environment

# define study area as a polygon
# bof polygon from a file that i had on my computer
# polygon_matrix = cbind(
#   lon = c(-66.45, -66.28, -66.28, -66.37, -66.50, -66.62, -66.62, -66.45),
#   lat = c(44.82, 44.78, 44.67, 44.55, 44.48, 44.48, 44.70, 44.82)
# )

# large box drawn by dan
polygon_matrix = cbind(
  lon = c(-67.2, -67.2, -65.7, -65.7, -67.2),
  lat = c( 44.1,  45.1,  45.1,  44.1,  44.1)
)

polygon_sfc = st_sfc(st_polygon(list(polygon_matrix))) #create sfc object
st_crs(polygon_sfc) = "EPSG:4326" #insert crs
polygon_sf = st_sf(polygon_sfc)
mapview(polygon_sf)

# create vessel tracks for each FILEID
tracks <- tmpdat_sf %>% 
  group_by(FILEID, on.effort.id) %>% 
  arrange(FILEID, EVENTNO) %>% # put it in order
  summarise(do_union = FALSE) %>%  #if you don't do this, it returns one row for each row of tmpdat_sf (your original thing)
  #st_geometry() #%>% 
  st_cast("MULTILINESTRING")

# determine which FILEID tracks intersect the polygon
#   create logical array identifying FILEIDs that do/don't have effort within the unionized polygon
#   specify 'sparse = FALSE' to return a logical array
tracks$intersection <- st_intersects(polygon_sfc, tracks, sparse = FALSE)[1,]

# list of FILEIDs that intersect the unionized polygon
IN_fileids = tracks$FILEID[tracks$intersection]

# discard FILEIDs that do not intersect the polygon
tmpdat_sf <- tmpdat_sf %>%
  filter(FILEID %in% IN_fileids)

#rm(tracks, IN_fileids)

#create the survey map
survey_map = mapview(tracks, color = "red", lwd = 4, alpha = 1, popup = NULL) +
  #mapview(tmpdat_sf, color = "blue", cex = 2, alpha = .2, popup = NULL) +
  mapview(polygon_sf)
survey_map  #plot the survey map

# add grid ID to polygon_sf (there is only one grid cell, but there could be another later)
polygon_sf = polygon_sf %>% # add grid ID
  mutate(grid_id = 1:length(lengths(polygon_sfc)))

#number of cells in the grid
num_cells = dim(polygon_sf)[1]
print(num_cells)

# now that you have the grid set up, you should be able to create a new column in
# tmpdat_sf for grid_id and label all the rows for each grid cell this may help 
# in error checking later
tmpdat_sf$grid_id = NA
grid_id_list = st_intersects(polygon_sf, tmpdat_sf) #[i THINK] these are all row numbers from the dataset that are within the grid cells
for (ii in 1:num_cells){ #for each grid cell, insert the number of the grid cell in the correct row of tmpdat_ssf_season_survey
  tmpdat_sf$grid_id[grid_id_list[[ii]]] = ii
}
rm(ii)

# count number of surveys in each season
# the maximum number of surveys within each season is required for looping
num_survs = tibble(
  ssn = ssn_no,
  num = NA)
for (i in 1:num_ssn){
  tmpdat_sf_ssn = tmpdat_sf |> filter(season == ssn_no[i])
  num_survs[i,2] = length(unique(tmpdat_sf_ssn$FILEID))
}
rm(tmpdat_sf_ssn, i)
max_survs = max(num_survs[,2])
print(num_survs)
print(max_survs)

## CREATE DETECTION COVARIATE LISTS 
# need to produce one effort grid for each season. here we construct lists to store these values. 
# each season will be one element of effort_list, jday_list, and so on
effort_drop_NA_list = vector("list", num_ssn) #list to hold positions of NA (needed for NA-ing out values in other matrices). No required if there is only once cell/polygon
effort_list = vector("list", num_ssn) #holds effort for each survey and cell, computed using linestrings
jday_list = vector("list", num_ssn) #holds jday for each survey and cell
bft_list = vector("list", num_ssn) #holds beafort sea state for each survey and cell

## CREATE ARRAYS TO HOLD SPECIES DETECTION HISTORIES & DETECTION COVARIATE ARRAYS FOR JAGS MODELLING
# enumerate species and count them
spp = unique(dat$SPECCODE[!is.na(dat$SPECCODE)]) #need array for each species in each year (primary period)
spp = "RIWH" #reduce to RIWH for simplicity
num_spp = length(spp)
print(num_spp)

# this 'spp3d' will be a template to be copied and values stored in it for each species.
# values of these matrices will be populated later
spp3d = array(dim = c(num_cells, max_survs+1, num_ssn)) #create this and copy/rename for species in this loop, and copy to detection covariates below this loop

# loop about species to create matrices for use in jags code
for (j in 1:num_spp){
  print(spp[j])
  # generate 3d array to hold detections / non-detections. initialize 3d array
  cmd = paste(spp[j], "3d = spp3d", sep = "")
  # print(cmd)
  eval(parse(text = cmd))
}

# 3d matrices for effort and jday, and any other detection covariates
effort3d = spp3d #effort from linestrings
jday3d = spp3d 
bft3d = spp3d 

## FILL DETECTION COVARAIATE LIST OBJECTS AND 3D ARRAYS
#loop about season [i], then loop about survey [j], the loop about grid cell
for (i in 1:num_ssn){
  
  # isolate season
  tmpdat_sf_season = tmpdat_sf |> filter(season == i)
  
  # find unique FILEIDs within the season[i]. each unique FILEID is a survey
  season_ufids = unique(tmpdat_sf_season$FILEID)
  num_season_ufids = length(season_ufids)
  
  # initialize temporary spatial arrays needed for intersecting within the loop about surveys/ufids
  effort = polygon_sf
  jday = polygon_sf
  bft = polygon_sf
  
  # fill out columns to the maximum number of surveys
  # add two columns at the beginning to accommodate geometry and grid_id columns
  effort[,3:(max_survs+2)] = NA
  jday[,3:(max_survs+2)] = NA  
  bft[,3:(max_survs+2)] = NA
  
  #loop about individual surveys (FILEID) to obtain effort, jday, bft in each cell/polygon
  for (j in 1:num_season_ufids){
    
    # filter tmpdat_sf_season for each fileid/survey. store as tmpdat_sf_season_survey
    # after this, 'tmpdat_sf_season_survey' has all records from season[i] and season_ufids[j] (season_ufids = survey) 
    cmd = paste("tmpdat_sf_season_survey = tmpdat_sf_season |> filter(FILEID == '", season_ufids[j], "')", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    
    # intersect tmpdat_sf_season_survey with the grid, return tmpdat_sf_season_survey_grid  
    # after this, 'tmpdat_sf_season_survey_grid' holds indices for each grid cell, within season[i] and survey[j]
    # the indices returned are used to compute mean/mode/etc of beaufort sea state within each cell
    tmpdat_sf_season_survey_grid = st_intersects(polygon_sf, tmpdat_sf_season_survey)

    # calculate effort using linestrings
    # this chunk of code converts rows of tmpdat_sf_season_survey into a linestring, 
    # but within each fileid. the result is an sfc object
    # nereid_tracks <- tmpdat_sf_season_survey %>% 
    #   #group_by(FILEID) %>% # this is not necessary since you are working with a tmpdat file that only has one FILEID
    #   arrange(FILEID, EVENTNO) %>% # put it in order
    #   summarise(do_union = FALSE) %>%  #if you don't do this, it returns one row for each row of tmpdat_sf (your original thing)
    #   #st_geometry() %>% #this seems unnecessary
    #   st_cast("LINESTRING")
    
    nereid_tracks <- tmpdat_sf_season_survey %>% 
      group_by(on.effort.id) %>% # on/off effort
      arrange(FILEID, EVENTNO) %>% # put it in order
      summarise(do_union = FALSE) %>%  #if you don't do this, it returns one row for each row of tmpdat_sf (your original thing)
      #st_geometry() %>% #this seems unnecessary
      st_cast("MULTILINESTRING")
    
    # create figures, if 'make_figs' flag is set to 'yes'
    if (make_figs == 'yes'){
    
      # if /figs does not exist, create it
      if (!file.exists(paste0(curr_dir, "/figs"))){
        cmd = paste0('mkdir ', curr_dir, "/figs")
        system(cmd)
      }
      
      #create the survey map
      survey_map = mapview(nereid_tracks, color = "red", lwd = 4, alpha = 1, popup = NULL) +
        mapview(tmpdat_sf_season_survey, color = "blue", cex = 2, alpha = .2, popup = NULL) +
        mapview(polygon_sf)
      survey_map  #plot the survey map
      #write and view map as html file
      html_fl = paste0(curr_dir, "/figs/", unique(tmpdat_sf_season$YEAR), "_ssn", i, "_surv", j, "_", season_ufids[j], ".html")
      mapshot(survey_map, url = html_fl) #save the map
      #browseURL(html_fl) #open the map in a web browser
    }
    
    st_agr(polygon_sf) = "constant"
    st_agr(nereid_tracks) = "constant"
    
    # intersect grid with survey trackline (linestring), 
    # calculate and store trackline length in each grid cell
    intersection <- st_intersection(polygon_sf, nereid_tracks) %>%
      mutate(total_length = st_length(.)) %>%
      mutate(total_length_km = as.numeric(total_length)*0.001) %>% #changes length from [m] to <dbl> and converts from meters to kilometers
      group_by(grid_id)
    # plot(intersection$polygon_sfc)
    
    # join the 'intersection' with grid_id. this creates a matrix with the same order as all the others (e.g. 'effort').
    effort_joined <- polygon_sf %>% 
      left_join(st_drop_geometry(intersection), by = "grid_id")
    
    # below, we add lengths from effort_joined into effort
    # store effort length from each grid cell into the column for survey j
    effort[, j+2] = sum(effort_joined$total_length_km)
    rm(effort_joined, intersection, nereid_tracks)
    
    # fill jday array. no need to loop about grid cells because jday is the same for every grid cell within each survey:
    #   jday should be the same for all grid cells within a survey, so fill all rows with jday value and NA-out grid cells not surveyed below
    if (length(unique(tmpdat_sf_season_survey$date_jday_ET)) == 1){
      jday[,j+2] = as.numeric(unique(tmpdat_sf_season_survey$date_jday_ET))
    } else {
      #jday[,j+2] = -99 #there should only be one value of DAY
      print('>1 jday. STOP!')
      stop()
    }
    
    # fill bft array. NA-out grid cells not surveyed (below)
    for (k in 1:num_cells){
      # compute mean of Beaufort values
      bft[k,j+2] = mean(tmpdat_sf_season_survey$BEAUFORT[tmpdat_sf_season_survey_grid[[k]]], na.rm = T)
    }
    
    rm(tmpdat_sf_season_survey_grid)
  }
  
  #name columns in 2D detection covariates
  names(effort)[3:(num_season_ufids+2)] = season_ufids
  names(jday)[3:(num_season_ufids+2)] = season_ufids
  names(bft)[3:(num_season_ufids+2)] = season_ufids
  
  # fill 3d effort matrix
  cmd = paste("effort3d[,,", i, "] = as.matrix(st_drop_geometry(effort))", sep = "")
  print(cmd)
  eval(parse(text = cmd))
  
  # fill 3d jday matrix
  cmd = paste("jday3d[,,", i, "] = as.matrix(st_drop_geometry(jday))", sep = "")
  print(cmd)
  eval(parse(text = cmd))
  
  # fill 3d bft matrix
  cmd = paste("bft3d[,,", i, "] = as.matrix(st_drop_geometry(bft))", sep = "")
  print(cmd)
  eval(parse(text = cmd))
  
  ### # optional: produce lists for each detection variable
  # # spatialize effort_list and jday_list, for uyear[i]
  effort_list[[i]] = polygon_sf
  jday_list[[i]] = polygon_sf
  bft_list[[i]] = polygon_sf

  effort_list[[i]] = effort
  jday_list[[i]] = jday
  bft_list[[i]] = bft
  # ###
  
  #no longer need 2D versions of detection covariates, as they are stored in the '_list' & '3d' versions 
  rm(effort, jday, bft)
  
  # loop about species
  for (j in 1:num_spp){
    print(spp[j])
    
    # generate one dataset holding only data for each species (no effort, etc) 
    # the result will hold records for the species across all FILEIDs
    # example: HAPO = tmpdat_sf |> filter(SPECCODE == "HAPO")
    cmd = paste(spp[j], "_season = tmpdat_sf_season |> filter(SPECCODE == '", spp[j], "')", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    
    ### ** 
    # IF YOU SAVE RIWH_season (immediately above) you will have the points you need to plot for sighting locations.
    # The points below (in RIWH_ssn1_grid_sf) are polygons and not points, so that wouldn't work.
    ### ** 
    
    # initialize sf object with grid cells for each species, by copying area_grid_sf
    # example: HAPO_grid_sf_ssn1 = area_grid_sf, note that it is season specific
    cmd = paste(spp[j], "_ssn", i, "_grid_sf = polygon_sf", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    
    # add columns to species grids so they have max_survs columns (plus columns for geom and grid_id)
    # example: HAPO_ssn1_grid_sf[,3:max_survs] = NA
    cmd = paste(spp[j], "_ssn", i, "_grid_sf[,3:(max_survs+2)] = NA", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    
    # loop about surveys and summarize species counts within cells/polygon
    for (k in 1:num_season_ufids){
      
      print(season_ufids[k]) #display the fileid/survey
      
      # spp[j] for season[i] and survey[k]
      # example: HAPO_tmp = HAPO |> filter(FILEID == fids[j])
      cmd = paste(spp[j], "_season_survey = ", spp[j], "_season |> filter(FILEID == '", season_ufids[k], "')", sep = "")
      print(cmd)
      eval(parse(text = cmd))
      
      # within season[i] and survey[k], for spp[j], count number of SIGHTINGS (not number of ANIMALS) in each grid cell
      # example: HAPO_grid_sf$p116214 = lengths(st_intersects(area_grid_sf, HAPO_tmp))
      # cmd = paste(spp[j], "_ssn", i, "_grid_sf[,k+2]", " = lengths(st_intersects(polygon_sf,", spp[j], "_season_survey))", sep = "")
      # print(cmd)
      # eval(parse(text = cmd))

      # within season[i] and survey[k], for spp[j], count number of ANIMALS (not number of SIGHTINGS) in each grid cell      
      # use st_intersects to identify rows that have spp[j] within the polygon
      # 'idx' holds row numbers that have sightings within the cell/polygon
      # 'idx' is a sparse list with only one element, i.e. idx[[1]] = [2, 4, 1]
      cmd = paste("idx = st_intersects(polygon_sf,", spp[j], "_season_survey)", sep = "")
      print(cmd)
      eval(parse(text = cmd))
      
      #use 'idx' from above to sum NUMBER of spp[j] within the polygon
      cmd = paste(spp[j], "_ssn", i, "_grid_sf[,k+2]", " = sum(", spp[j], "_season_survey$NUMBER[idx[[1]]])", sep = "")
      print(cmd)
      eval(parse(text = cmd))
      
      # remove _season_survey created at the top of this loop
      # this is not necessary as the new one writes over the old one
      # there could be a case where it is needed
      cmd = paste("rm(", spp[j], "_season_survey)", sep = "")
      print(cmd)
      eval(parse(text = cmd))
    }
    
    # NA-out grid cells that were not visited. If there is only one cell/polygon, this isn't necessary
    cmd = paste(spp[j], "_ssn", i, "_grid_sf[effort_drop_NA_list[[", i, "]]] = NA", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    spp_ssn_name = paste(spp[j], "_ssn", i, "_grid_sf", sep = "") #generate variable name for easy deleting later
    
    # add column names
    cmd = paste("names(", spp[j], "_ssn", i, "_grid_sf)[3:(num_season_ufids+2)] = season_ufids", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    
    # fill 3d species matrix from the species list
    cmd = paste(spp[j], "3d[,,", i, "] = as.matrix(st_drop_geometry(", spp[j], "_ssn", i, "_grid_sf))", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    
    # remove unnecessary matrices
    cmd = paste("rm(", 
                #spp[j], ", ",
                #spp[j], "_season_survey",
                spp[j], "_season",
                ")", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    
  }
  
  rm(tmpdat_sf_season, num_season_ufids)
}

colSums(RIWH3d[1,-1,], na.rm = T)
colSums(effort3d[1,-1,], na.rm = T)

SPUE = colSums(RIWH3d[1,-1,], na.rm = T)/(colSums(effort3d[1,-1,], na.rm = T)/1000)

ts = data.frame(
  year = c(begYEAR:endYEAR),
  spue = SPUE
)
plot(ts)
lines(ts)