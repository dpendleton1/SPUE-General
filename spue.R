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

## define study area as a polygon
# bof polygon from a file that i had on my computer
# polygon_matrix = cbind(
#   lon = c(-66.45, -66.28, -66.28, -66.37, -66.50, -66.62, -66.62, -66.45),
#   lat = c(44.82, 44.78, 44.67, 44.55, 44.48, 44.48, 44.70, 44.82)
# )

# GSC centered region - kinda large box
# polygon_matrix = cbind(
#   lon = c(-73, -73, -66, -66, -73),
#   lat = c( 39,  43,  43,  39,  39)
# )

# GOM and GSL - big box
# polygon_matrix = cbind(
#   lon = c(-73, -73, -55, -55, -73),
#   lat = c( 39,  52,  52,  39,  39)
# )

# GSC critical habitat
polygon_matrix = cbind(
  lon = c(-69.75, -68.5167, -68.2167, -69.0833, -69.75),
  lat = c( 41.667,  42.167,  41.633,  41.000,  41.667)
)

## I DON'T NEED THIS HERE FOR THIS EXERCISE
# ## CREATE A GRID WITH SPATIAL INFORMATION AND GRID_IDS
# cell_size = 0.5
# area_grid = st_make_grid(tmpdat_sf, c(cell_size, cell_size), what = "polygons", square = FALSE)
# #mapview(area_grid)

# create polygon sf object and display it
polygon_sfc = st_sfc(st_polygon(list(polygon_matrix))) #create sfc object
st_crs(polygon_sfc) = "EPSG:4326" #insert crs
polygon_sf = st_sf(polygon_sfc)
mapview(polygon_sf)

#number of cells in the 'grid', for a single polygon this number will be = 1
num_cells = dim(polygon_sf)[1]
print(num_cells)

# add grid ID to polygon_sf (there is only one grid cell, but there could be another later)
polygon_sf = polygon_sf %>% # add grid ID
  mutate(grid_id = 1:length(lengths(polygon_sfc)))

# create vessel tracks for each fileid
tracks <- tmpdat_sf %>% 
  #group_by(fileid, on.effort.id) %>% 
  #arrange(fileid, EVENT_NUMBER) %>% # put it in order
  group_by(fileid) %>% 
  arrange(EVENT_NUMBER) %>% # put it in order
  summarise(do_union = FALSE) %>%  #if you don't do this, it returns one row for each row of tmpdat_sf (your original thing)
  #st_geometry() #%>% 
  st_cast("MULTILINESTRING")

#create the map showing all surveys
survey_map = mapview(tracks, color = "red", lwd = 0.5, alpha = 1, popup = NULL) +
  #mapview(tmpdat_sf, color = "blue", cex = 2, alpha = .2, popup = NULL) +
  mapview(polygon_sf)
survey_map  #plot the survey map

################################################################################

# determine which fileid tracks intersect the polygon
#   create logical array identifying fileids that do/don't have effort within the unionized polygon
#   specify 'sparse = FALSE' to return a logical array
tracks$intersection <- st_intersects(polygon_sfc, tracks, sparse = FALSE)[1,]

# store a character vector specifying fileids that intersect the polygon
IN_fileids = tracks$fileid[tracks$intersection]

# now that you have the grid set up, you should be able to create a new column in
# tmpdat_sf for grid_id and label all the rows for each grid cell this may help 
# in error checking later
tmpdat_sf$grid_id = NA
grid_id_list = st_intersects(polygon_sf, tmpdat_sf) #[i THINK] these are all row numbers from the dataset that are within the grid cells
for (ii in 1:num_cells){ #for each grid cell, insert the number of the grid cell in the correct row of tmpdat_ssf_season_survey
  tmpdat_sf$grid_id[grid_id_list[[ii]]] = ii
}
# unique values in $grid_id should be only NA and 1

# DOUBLE CHECK THAT ALL FILEIDS INTERSECT THE POLYGON
# test for fileids identified in IN_fileids have some effort in the polygon
# that is, that grid_id == 1 for at least one record.
# if there are any fileids that have NA for every entry (and 2015-05-22 was one), 
# then those need to be discarded
bad_fids = NA # store fileids that do not intersect the polygon in this vector
ctr = 0 # counter for bad fileids
for (ii in 1:length(IN_fileids)){
  
  # create tmp array for testing
  a <- tmpdat_sf |> filter(fileid == IN_fileids[ii])
  
  # if length is == 2, then we know we have NA and 1, since those are the only possible values 
  if (length(unique(a$grid_id)) == 2){
    print('all good')
    next
    
  # else if length == 1 and all of them are NA, then we have a problem and must remove this fileid from IN_fileids
  } else if ((length(unique(a$grid_id)) == 1) & (is.na(unique(a$grid_id)))){
    print(paste0(IN_fileids[ii], " has no records inside the polygon")) # print the non-intersecting fileid
    ctr = ctr + 1 # advance the counter
    bad_fids[ctr] = ii # add position ii to bad_fids vector
  }
}

# remove non-intersecting fileids
IN_fileids <- IN_fileids[-bad_fids]

# finally, exclude non-interesecting fileids from tmpdat_sf
tmpdat_sf <- tmpdat_sf %>%
  filter(fileid %in% IN_fileids)

################################################################################

# find maximum number of surveys within each season
num_survs = tibble(
  ssn, #ssn is a vector specifying season numbers
  num = NA)
for (i in 1:num_ssn){
  if (ssn_type == "within"){
    tmpdat_sf_ssn = tmpdat_sf |> filter(season == ssn_no[i])
  } else if (ssn_type == "across"){
    tmpdat_sf_ssn = tmpdat_sf |> filter(season_grpd == ssn_no[i])
  }
  num_survs[i,2] = length(unique(tmpdat_sf_ssn$fileid))
}
rm(tmpdat_sf_ssn, i)
max_survs = max(num_survs[,2])
print(num_survs)
print(max_survs)

# # look at what year and season combinations are present in tmpdat_sf
# a <- st_drop_geometry(tmpdat_sf)
# b <- distinct(select(a,YEAR_ET, season_grpd))
# c <- distinct(select(a,YEAR_ET, season))

## CREATE DETECTION COVARIATE LISTS 
# need to produce one effort grid for each season. here we construct lists to store these values. 
# each season will be one element of effort_list, jday_list, and so on
effort_drop_NA_list = vector("list", num_ssn) #list to hold positions of NA (needed for NA-ing out values in other matrices). No required if there is only once cell/polygon
effort_list = vector("list", num_ssn) #holds effort for each survey and cell, computed using linestrings
jday_list = vector("list", num_ssn) #holds jday for each survey and cell
bft_list = vector("list", num_ssn) #holds beafort sea state for each survey and cell

## CREATE ARRAYS TO HOLD SPECIES DETECTION HISTORIES & DETECTION COVARIATE ARRAYS FOR JAGS MODELLING
# enumerate species and count them
spp = unique(dat$SPCODE[!is.na(dat$SPCODE)]) #need array for each species in each year (primary period)
spp = "RIWH" #reduce to RIWH for simplicity
num_spp = length(spp)

# this 'spp3d' will be a template to be copied and values stored in it for each species.
# values of these matrices will be populated later
spp3d = array(dim = c(num_cells, max_survs+1, num_ssn)) #create this and copy/rename for species in this loop, and copy to detection covariates below this loop
# loop about species to create matrices for use in jags code
for (j in 1:num_spp){
  print(spp[j])
  # generate 3d array to hold detections / non-detections. initialize 3d array
  cmd = paste(spp[j], "3d = spp3d", sep = "")
  print(cmd)
  eval(parse(text = cmd))
}

# 3d matrices for effort and jday, and any other detection covariates
effort3d = spp3d #effort from linestrings
jday3d = spp3d 
bft3d = spp3d 

## FILL DETECTION COVARAIATE LIST OBJECTS AND 3D ARRAYS, THEN FILL SPECIES ARRAYS
#loop about season [i], then loop about survey [j], the loop about grid cell [k]
for (i in 1:num_ssn){
  
  # isolate season
  if (ssn_type == "within"){
    tmpdat_sf_season = tmpdat_sf |> filter(season == i)
  } else if (ssn_type == "across"){
    tmpdat_sf_season = tmpdat_sf |> filter(season_grpd == i)
  }
  
  # if there is no data in tmpdat_sf_season, it means there was not a survey in this season
  # look at num_survs to see how many surveys are in each season. if there are zero surveys in a season,
  # then the condition below will be true, and that season will be skipped by using the 'next' command:
  if (dim(tmpdat_sf_season)[1] == 0){
    next
  }
  
  # find unique fileids within the season[i]. each unique fileid is a survey
  season_ufids = unique(tmpdat_sf_season$fileid)
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
  
  #loop about individual surveys (fileid) to obtain effort, jday, bft in each cell/polygon
  for (j in 1:num_season_ufids){
    
    # filter tmpdat_sf_season for each fileid/survey. store as tmpdat_sf_season_survey
    # after this, 'tmpdat_sf_season_survey' has all records from season[i] and season_ufids[j] (season_ufids = survey) 
    cmd = paste("tmpdat_sf_season_survey = tmpdat_sf_season |> filter(fileid == '", season_ufids[j], "')", sep = "")
    print(cmd)
    eval(parse(text = cmd))
    
    # intersect tmpdat_sf_season_survey with the grid, return tmpdat_sf_season_survey_grid  
    # after this, 'tmpdat_sf_season_survey_grid' holds indices for each grid cell, within season[i] and survey[j]
    # the indices returned are used to compute mean/mode/etc of beaufort sea state within each cell
    tmpdat_sf_season_survey_grid = st_intersects(polygon_sf, tmpdat_sf_season_survey)

    survey_tracks <- tmpdat_sf_season_survey %>% 
      group_by(on.effort.id) %>% # on/off effort
      arrange(fileid, EVENT_NUMBER) %>% # put it in order
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
      survey_map = mapview(survey_tracks, color = "red", lwd = 4, alpha = 1, popup = NULL) +
        #mapview(tmpdat_sf_season_survey, color = "blue", cex = 2, alpha = .2, popup = NULL) +
        mapview(polygon_sf)
      survey_map  #plot the survey map
      #write and view map as html file
      html_fl = paste0(curr_dir, "/figs/", unique(tmpdat_sf_season$YEAR_ET), "_ssn", i, "_surv", j, "_", season_ufids[j], ".html")
      mapshot(survey_map, url = html_fl) #save the map
      #browseURL(html_fl) #open the map in a web browser
    }
    
    st_agr(polygon_sf) = "constant"
    st_agr(survey_tracks) = "constant"
    
    # intersect grid with survey trackline (linestring), 
    # calculate and store trackline length in each grid cell
    intersection <- st_intersection(polygon_sf, survey_tracks) %>%
      mutate(total_length = st_length(.)) %>%
      mutate(total_length_km = as.numeric(total_length)*0.001) %>% #changes length from [m] to <dbl> and converts from meters to kilometers
      group_by(grid_id)
    plot(intersection$polygon_sfc)
    
    # join the 'intersection' with grid_id. this creates a matrix with the same order as all the others (e.g. 'effort').
    effort_joined <- polygon_sf %>% 
      left_join(st_drop_geometry(intersection), by = "grid_id")
    
    # below, we add lengths from effort_joined into effort
    # store effort length from each grid cell into the column for survey j
    effort[, j+2] = sum(effort_joined$total_length_km)
    rm(effort_joined, intersection, survey_tracks)
    
    # fill jday array. no need to loop about grid cells because jday is the same for every grid cell within each survey:
    #   jday should be the same for all grid cells within a survey, so fill all rows with jday value and NA-out grid cells not surveyed below
    if (length(unique(tmpdat_sf_season_survey$jday)) == 1){
      jday[,j+2] = as.numeric(unique(tmpdat_sf_season_survey$jday))
    } else {
      #jday[,j+2] = -99 #there should only be one value of DAY
      print('>1 jday. STOP!')
      stop()
    }
    
    # fill bft array. NA-out grid cells not surveyed (below)
    for (k in 1:num_cells){
      # compute mean of Beaufort values
      bft[k,j+2] = mean(tmpdat_sf_season_survey$beaufort[tmpdat_sf_season_survey_grid[[k]]], na.rm = T)
    }
    
    rm(tmpdat_sf_season_survey_grid)
  }
  
  #name columns in 2D detection covariates, use first 8-characters (date)
  names(effort)[3:(num_season_ufids+2)] = substr(season_ufids, start = 1, stop = 8)
  names(jday)[3:(num_season_ufids+2)] = substr(season_ufids, start = 1, stop = 8)
  names(bft)[3:(num_season_ufids+2)] = substr(season_ufids, start = 1, stop = 8)
  
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
    # the result will hold records for the species across all fileids
    # example: HAPO = tmpdat_sf |> filter(SPCODE == "HAPO")
    cmd = paste(spp[j], "_season = tmpdat_sf_season |> filter(SPCODE == '", spp[j], "')", sep = "")
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
      # example: HAPO_tmp = HAPO |> filter(fileid == fids[j])
      cmd = paste(spp[j], "_season_survey = ", spp[j], "_season |> filter(fileid == '", season_ufids[k], "')", sep = "")
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
      
      #use 'idx' from above to sum GROUP_SIZE of spp[j] within the polygon
      cmd = paste(spp[j], "_ssn", i, "_grid_sf[,k+2]", " = sum(", spp[j], "_season_survey$GROUP_SIZE[idx[[1]]])", sep = "")
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
    cmd = paste("names(", spp[j], "_ssn", i, "_grid_sf)[3:(num_season_ufids+2)] = substr(season_ufids, start = 1, stop = 8)", sep = "")
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

colSums(RIWH3d[1, -1, ], na.rm = T)
colSums(effort3d[1, -1, ], na.rm = T)

SPUE = colSums(RIWH3d[1,-1,], na.rm = T)/(colSums(effort3d[1,-1,], na.rm = T)/1000)

ts = data.frame(
  #year = c(begYEAR:endYEAR),
  ssn,
  spue = SPUE
)
plot(ts)
lines(ts)
