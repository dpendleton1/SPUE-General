
# ## import data
# dat <- read_csv(file = file_loc, 
#                 col_types = cols(FILEID = col_character(),
#                                  EVENTNO = col_double(),
#                                  MONTH = col_double(),
#                                  DAY = col_double(),
#                                  YEAR = col_double(),
#                                  GMT = col_double(),
#                                  LATITUDE = col_double(),
#                                  LONGITUDE = col_double(),
#                                  LEGTYPE = col_double(),
#                                  LEGSTAGE = col_double(),
#                                  ALT = col_double(),
#                                  HEADING = col_double(),
#                                  WX = col_character(),
#                                  CLOUD = col_double(),
#                                  VISIBLTY = col_double(),
#                                  BEAUFORT = col_double(),
#                                  SPECCODE = col_character(),
#                                  IDREL = col_double(),
#                                  NUMBER = col_double(),
#                                  CONFIDNC = col_double())
# )


#change datetime column, then add Year Month Day columns based on US/Eastern tz
dat$datetime_et <- dmy_hms(dat$DATETIME_ET, tz = 'EST5EDT')
dat$YEAR_ET <- as.numeric(format(dat$datetime_et,"%Y"))
dat$MONTH_ET <- as.numeric(format(dat$datetime_et,"%m"))
dat$DAY_ET <- as.numeric(format(dat$datetime_et,"%d"))
dat$YMD <- format(dat$datetime_et, "%Y%m%d")

# must create survey identifier
dat <- dat %>%
  mutate(fileid = paste0(YMD, "_", PLANE))

# unique dates
length(unique(dat$fileid))
# 1575 dates

# how many dates with >1 plane?
multi_plane <-  dat %>%
  group_by(YMD) %>%
  summarize(n_planes = n_distinct(PLANE)) %>%
  filter(n_planes > 1)
dim(multi_plane)[1]
# 60 dates with > 1 plane

# keep only desired years and months (based on US/Eastern tz)
dat <- dat %>%
  filter(YEAR_ET >= begYEAR & YEAR_ET <= endYEAR)
#dat <- dat %>%
#  filter(MONTH_ET >= begMONTH | MONTH_ET <= endMONTH)

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
  I = which(dat$datetime_et >= ssn_beg_date[i] & dat$datetime_et <= ssn_end_date[i])
  dat$season[I] = ssn_no[i]
  dat$season_grpd[I] = ssn_no_grpd[i]
}

# dat <- dat %>%
#   mutate(on.off.eff = if_else((BEAUFORT <= 6 & # normally require sea state 0-3, but sea state will be covariate on detection in this model
#                                  (
#                                    (LEGTYPE == 5 & (LEGSTAGE == 1 | LEGSTAGE == 2 | LEGSTAGE == 5)) | #start, continue, end watch while ship not underway
#                                      (LEGTYPE == 6 & (LEGSTAGE == 1 | LEGSTAGE == 2 | LEGSTAGE == 5)) #legtype = 6 indicates ship not underway (listening station)
#                                  ) & 
#                                  (VISIBLTY >=2 | VISIBLTY == -1) & #pre-2020 changes to NARWC Sightings Database, VISIBLTY >=2 or -1 indicates visibility of at least 2 nautical miles. Negative numbers are no longer used, however this dataset was obtained in 2019 before the change.
#                                  (IDREL == 3 | is.na(IDREL)) # if there is a sighting, IDREL must = 3. If no sightings, then IDREL should be NA
#   ), 
#   1, 0)) %>%
#   #now replace all NA with 0 because those are off-effort
#   mutate(on.off.eff = ifelse(is.na(on.off.eff), 0, on.off.eff)
#   )
# 
# # create unique identifier for continuous segments of on-effort
# # use cumsum(abs(diff())) to create continuous numbers based upon on.off.eff
# dat <- dat %>%
#   mutate(on.effort.id = c(1, cumsum(abs(diff(dat$on.off.eff))) + 1))
# # filter out on.off.eff == 0
# dat <- dat %>%
#   filter(dat$on.off.eff !=0)

keep.cols <- c( "fileid", "EVENT_NUMBER", "datetime_et", "YEAR_ET", "MONTH_ET", "DAY_ET", "YMD",
                "LATITUDE", "LONGITUDE", "LEGTYPE", "LEGSTAGE", "PSB_LEGSTAGE",
                "BEAUFORT",  "CLOUD_CODE", "CLOUD_PERCENT",  
                "SPCODE", "GROUP_SIZE", "CALVES", "PHOTOS", "ID_RELIABILITY", "SIGHTING_NUMBER",
                "PLANE", "VISIBILTY_CODE", "VISIBILTY_NM",
                "season", "season_grpd", "SOURCE")          
tmpdat <- dat %>%
  dplyr::select(all_of(keep.cols)) #%>%
rm(keep.cols)


#write.csv(dat, file = "dat_with_dist.csv")
#write.csv(tmpdat, file = "tmpdat.csv")
