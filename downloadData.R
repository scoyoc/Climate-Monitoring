#' ---
#' title: "Downloading Climate Data collected at SEUG Visitor Centers"
#' author: "Matthew W. Van Scoyoc"
#' date: "`r Sys.time()`"
#' ---
#'
#' **Developed:** 21 December 2017  
#' **Latest updated:** 26 October 2020  
#' **Data files:** downloaded from NOAA GHCND ftp site using rnoaa  
#' **Associated directories:**   
#' - "C:/R/Climate"  
#' - "R:/NR_Climate/SEUG/Analysis (1B-Perm)/Climate Monitoring"  
#' **Notes:** This script downloads data from NOAA's Global Historical Climatology 
#' Network (GHCN) then runs quality assurance-quality control procedures using the 
#' defined "flags" from NOAA. The data are then saved to an .RData files that 
#' is loaded in subsequent scripts.  
#' **Update notes:**  
#' Included month data frame and formatting of wx.dat data 
#' frame that was happening at the beginning of subsequent scripts to increase 
#' efficiency of work flow.  
#' Chunk labels were added to make it possible to track errors when knitting to 
#' PDF using RMarkdown Notebooks.  

#+ Setup ----
#-- Packages
# install.packages(c("rnoaa", "tidyverse", "lubridate", "knitr"))
library("rnoaa")
library("tidyverse")
library("lubridate")
library("knitr")

#+ Data ----
#' Clear cache to ensure new data are downloaded.
#' This step increases the time to run the script but ensures most recent data 
#' are downloaded.
#-- ClearCache
meteo_clear_cache()

#' Save the date that data were downloaded and create date stamp.
#-- DownloadDate
downloadDate <- today()
date.stamp <- paste(str_split(downloadDate, "-")[[1]], collapse = "")

#' Create a data frame of months.
#-- Months
month.df <- data.frame(month = 1:12, 
                       waterMonth = c(4:12, 1:3),
                       mthName = factor(month.abb, 
                                        levels = c(month.abb[10:12], 
                                                   month.abb[1:9])), 
                       season = c("Wi", "Wi", "Sp", "Sp", "Sp", "Su", "Su", "Su", 
                                  "Fa", "Fa", "Fa", "Wi"))

#' Download station IDs and metadata from GHCN.
#' This step takes some time to run, as the metadata file from GHCN is huge.
#-- Metadata
#' This section is hashed out to safe time fetching data. The data frame below
#' is saved as a .csv.
# stations <- ghcnd_stations() %>% # Download station metadata from GHCN
#   # Filter stations of interest
#   filter(name %in% c("ARCHES NP HQS", "CANYONLANDS-THE NECK", 
#                      "CANYONLANDS-THE NEEDLES", "HANS FLAT RS", "HOVENWEEP NM", 
#                      "NATURAL BRIDGES NM", "MOAB")) %>%
#   # Select variables of interest
#   select(id, name, state, latitude, longitude, elevation) %>%
#   distinct() # Consolidate dataframe
# write.csv(stations, "stations.csv", row.names = FALSE, col.names = TRUE)

stations <- as_tibble(read.csv("stations.csv", header = TRUE)) %>% # Consolidate dataframe
  mutate(parkUnit = factor(c("ARCH", "CANY", "CANY", "CANY", "HOVE", "Moab", 
                             "NABR"), 
                           levels = c("ARCH", "CANY", "HOVE", "NABR", "Moab")), 
         district = factor(c("ARCH", "ISKY", "NEED", "MAZE", "HOVE", "Moab", 
                             "NABR"), 
                           levels = c("ARCH", "ISKY", "NEED", "MAZE", "HOVE", 
                                      "NABR", "Moab")), 
         unitName = c("Arches National Park", 
                      "Island in the Sky District, Canyonlands National Park", 
                      "The Needles District, Canyonlands National Park", 
                      "Hans Flat Ranger Station", "Hovenweep National Monument", 
                      "Moab, Utah", "Natural Bridges National Monument"))

#' Download data from the GHCN ftp site.
#-- WeatherData
dat.raw <- meteo_pull_monitors(monitors = stations$id, keep_flags = T, 
                               var = c("PRCP", "TMAX", "TMIN"))
#' Convert from wide to long data.
#-- Wide2Long
wx.dat <- dat.raw %>%
  select(id, date, prcp, tmax, tmin) %>%
  gather(element, value, 3:5, na.rm = F) %>%
  mutate(key = paste0(id, as.numeric(date), element)) %>%
  left_join(select(dat.raw, id, date, contains("mflag")) %>%
              gather(element, MFLAG, 3:5, na.rm = F) %>%
              separate(element, c("flag", "element"), "_") %>%  
              mutate(key = paste0(id, as.numeric(date), element)) %>%
              select(key, MFLAG)) %>%
  left_join(select(dat.raw, id, date, contains("qflag")) %>%
              gather(element, QFLAG, 3:5, na.rm = F) %>%
              separate(element, c("flag", "element"), "_") %>%  
              mutate(key = paste0(id, as.numeric(date), element)) %>%
              select(key, QFLAG)) %>%
  left_join(select(dat.raw, id, date, contains("sflag")) %>%
              gather(element, SFLAG, 3:5, na.rm = F) %>%
              separate(element, c("flag", "element"), "_") %>%  
              mutate(key = paste0(id, as.numeric(date), element)) %>%
              select(key, SFLAG)) %>%
  left_join(select(stations, id, parkUnit, district)) %>%
  select(key, id, parkUnit, district, date, element, value, MFLAG, QFLAG, SFLAG)
  
#' Append dates within the *wx.data* data frame to the *stations* data frame.
#-- Append
stations <- stations %>%
  left_join(wx.dat %>% 
              group_by(id) %>%
              summarise(startDate = min(date, na.rm = T),
                        endDate = max(date, na.rm = T), 
                        timespan = as.duration(startDate %--% endDate)/dyears(1)))

#+ QAQC ----
#' Load GHCND flag definitions from csv file.
#-- Flags
flags <- read.csv("flags.csv", stringsAsFactors = F)

#' Read measurement flags (MFLAGs) in data.
#-- MFLAG
mflags <- filter(flags, Type == "MFLAG") %>%
  rename(MFLAG = Flag)
wx.dat %>%
  filter(MFLAG %in% mflags$MFLAG) %>%
  left_join(mflags) %>%
  select(Definition) %>%
  distinct()

#' Read quality flags (QFLAGs) in data.
#-- QFLAG
qflags <- filter(flags, Type == "QFLAG") %>%
  rename(QFLAG = Flag)
wx.dat %>%
  filter(QFLAG %in% qflags$QFLAG) %>%
  left_join(qflags) %>%
  select(Definition) %>%
  distinct()

#' Read source flags (SFLAGs) in data.
#-- SFLAG
sflags <- filter(flags, Type == "SFLAG") %>%
  rename(SFLAG = Flag)
wx.dat %>%
  filter(SFLAG %in% sflags$SFLAG) %>%
  left_join(sflags) %>%
  select(Definition) %>%
  distinct()

#' QAQC and format data.
#-- Clean
wx.dat <- wx.dat %>%
  mutate(value = ifelse(QFLAG %in% qflags$QFLAG, NA, value), 
         value = as.numeric(value), 
         month = month(date), 
         year = year(date), 
         waterYr = ifelse(month >= 10, year + 1, year)) %>%
  left_join(month.df)

#+ End ----
#' Session information.
#-- Session
info <- sessionInfo()
r.ver <- paste(info$R.version$major, info$R.version$minor, sep=".")

#' Save data for subsequent scripts.
#-- Save
save(month.df, dat.raw, flags, stations, wx.dat, downloadDate, date.stamp, info, 
     r.ver, file = "climateData.RData")

#' Session information.
print(info)
