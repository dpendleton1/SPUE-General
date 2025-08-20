

#change datetime column, then add Year Month Day columns based on US/Eastern tz
dat$datetime_et <- dmy_hms(dat$DATETIME_ET, tz = 'EST5EDT')
dat$YEAR_ET <- as.numeric(format(dat$datetime_et,"%Y"))
dat$MONTH_ET <- as.numeric(format(dat$datetime_et,"%m"))
dat$DAY_ET <- as.numeric(format(dat$datetime_et,"%d"))
dat$YMD <- format(dat$datetime_et, "%Y%m%d")
dat$jday <- yday(dat$datetime_et)

# keep only desired years and months (based on US/Eastern tz)
dat <- dat %>%
  filter(YEAR_ET >= begYEAR & YEAR_ET <= endYEAR)
#dat <- dat %>%
#  filter(MONTH_ET >= begMONTH | MONTH_ET <= endMONTH)

# create survey identifier/fileid
dat <- dat %>%
  mutate(fileid = paste0(YMD, "_", PLANE))
# unique dates
length(unique(dat$fileid))

# how many dates with >1 plane?
multi_plane <-  dat %>%
  group_by(YMD) %>%
  summarize(n_planes = n_distinct(PLANE)) %>%
  filter(n_planes > 1)
dim(multi_plane)[1]

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

# truncate BEAUFORT because it is not in whole numbers
dat$beaufort <- floor(dat$BEAUFORT)

dat <- dat %>%
  mutate(on.off.eff = if_else((beaufort <= 3 & 
                                 (
                                 (LEGTYPE == 3  & (LEGSTAGE == 1 | LEGSTAGE == 3 | LEGSTAGE == 4 | LEGSTAGE == 5)) |
                                 (LEGTYPE == 4  & (LEGSTAGE == 1 | LEGSTAGE == 3 | LEGSTAGE == 4 | LEGSTAGE == 5)) |
                                 (LEGTYPE == 6  & (LEGSTAGE == 1 | LEGSTAGE == 3 | LEGSTAGE == 4 | LEGSTAGE == 5)) |
                                 (LEGTYPE == 8  & (LEGSTAGE == 1 | LEGSTAGE == 3 | LEGSTAGE == 4 | LEGSTAGE == 5)) |
                                 (LEGTYPE == 9  & (LEGSTAGE == 1 | LEGSTAGE == 3 | LEGSTAGE == 4 | LEGSTAGE == 5)) |
                                 (LEGTYPE == 10 & (LEGSTAGE == 1 | LEGSTAGE == 3 | LEGSTAGE == 4 | LEGSTAGE == 5)) |
                                 (LEGTYPE == 11 & (LEGSTAGE == 1 | LEGSTAGE == 3 | LEGSTAGE == 4 | LEGSTAGE == 5)) |
                                 (LEGTYPE == 12 & (LEGSTAGE == 1 | LEGSTAGE == 3 | LEGSTAGE == 4 | LEGSTAGE == 5))
                                 )
                               & 
                                 #(VISIBILTY_NM >= 4) & # sometimes there was no viz recorded
                                 (ID_RELIABILITY == 3 | is.na(ID_RELIABILITY))
  ),
  1, 0)) %>%
  #now replace all NA with 0 because those are off-effort
  mutate(on.off.eff = ifelse(is.na(on.off.eff), 0, on.off.eff)
  )

# create unique identifier for continuous segments of on-effort
# use cumsum(abs(diff())) to create continuous numbers based upon on.off.eff
dat <- dat %>%
  mutate(on.effort.id = c(1, cumsum(abs(diff(dat$on.off.eff))) + 1))
# filter out on.off.eff == 0 DON'T THINK YOU WANT TO DO THIS
dat <- dat %>%
  filter(dat$on.off.eff !=0)

fn_time = as.character(as.numeric(Sys.time()))
fn_time = paste0("file = 'data/dat_", fn_time, ".csv'")
cmd = paste0("write_csv(dat, ", fn_time, ")")
eval(parse(text = cmd))

keep.cols <- c( "fileid", "EVENT_NUMBER", "datetime_et", "YEAR_ET", "MONTH_ET", "DAY_ET", "YMD", "jday",
                "LATITUDE", "LONGITUDE", "LEGTYPE", "LEGSTAGE", "PSB_LEGSTAGE",
                "BEAUFORT", "beaufort", "CLOUD_CODE", "CLOUD_PERCENT",  
                "SPCODE", "GROUP_SIZE", "CALVES", "PHOTOS", "ID_RELIABILITY", "SIGHTING_NUMBER",
                "PLANE", "VISIBILTY_CODE", "VISIBILTY_NM",
                "season", "season_grpd", "SOURCE",
                "on.off.eff", "on.effort.id")          
tmpdat <- dat %>%
  dplyr::select(all_of(keep.cols)) #%>%
rm(keep.cols)


# LEGTYPE LEGSTAGE COMBOS
a = distinct(select(tmpdat,LEGTYPE,LEGSTAGE))
print(a)

#write.csv(dat, file = "dat_with_dist.csv")
#write.csv(tmpdat, file = "tmpdat.csv")
