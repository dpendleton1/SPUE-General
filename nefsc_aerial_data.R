# nefsc-aerial data

library(tidyverse)

dat_raw <- read.csv(file.choose())

nrow(dat_raw)
#  1780580 rows

dat_raw$DATETIME_ET_1 <- as.POSIXct(dat_raw$DATETIME_ET, format = "%d-%b-%y %I.%M.%S.%OS %p", tz = "UTC")

dat_update <-  dat_raw %>% 
  mutate(DATE_yymmdd = format(dat_raw$DATETIME_ET, "%y%m%d"),
         DATE_yymmdd = sprintf("%06s", DATE_yymmdd),
         fileid = paste0(DATE_yymmdd, "_", PLANE))

length(unique(dat_update$DATE_yymmdd))
# 1575 dates

length(unique(dat_update$fileid))
# 1635 fileids

multi_plane <-  dat_update %>%
  group_by(DATE_yymmdd) %>%
  summarize(n_planes = n_distinct(PLANE)) %>%
  filter(n_planes > 1)
# 60 dates with 2 planes ---- 1575 - 60 = 1515; 60 x 2 = 120; 1515 + 120 = 1635

write.csv(dat_update, "2000-2024_nefsc_data_new_column.csv", row.names = FALSE)


# for loop start
survey <- unique(dat_update$fileid)

for(fid in survey){
  
  # subset for each fileid
  each_survey <- dat_update[dat_update$fileid == fid]
  
  # add further code here
  
}

# to 'fix' DATETIME_yymmdd to 6 digits
# dat_update$DATE_yymmdd = sprintf("%06s", dat_update$DATE_yymmdd)
