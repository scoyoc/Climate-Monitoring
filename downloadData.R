#' ---
#' title: "Doanload climate data"
#' author: "Matthew W. Van Scoyoc"
#' ---
#'
#' Developed: 21 December 2017
#' Updated: 21 August 2018
#' Data files: downlaoded from NOAA ftp site using rnoaa
#' Associated directories: 
#' - "C:/R/Climate"
#' - "R:/NR_Climate/SEUG/Analysis (1B-Perm)/Climate Monitoring"

#+ Setup ----
#-- Packages
# install.packages(c("rnoaa", "tidyverse", "lubridate", "knitr"))
library("rnoaa")
library("tidyverse")
library("lubridate")

#+ Data ----
#-- Download date
downloadDate = today()

#-- Station Metadata
#' Get station IDs and metadata
#' This step takes some time to run, as the metadata file from GHCN is huge
stations <- ghcnd_stations() %>% # Download station metadata from GHCN
  # Filter stations of interest
  filter(name %in% c("ARCHES NP HQS", "CANYONLANDS-THE NECK", 
                     "CANYONLANDS-THE NEEDLES", "HANS FLAT RS", "HOVENWEEP NM", 
                     "NATURAL BRIDGES NM", "MOAB")) %>%
  # Select variables of interest
  select(id, name, state, latitude, longitude, elevation) %>%
  distinct() %>% # Consolidate dataframe
  mutate(parkUnit = factor(c("ARCH", "CANY", "CANY", "CANY", "HOVE", "MOAB", 
                             "NABR"), 
                           levels = c("ARCH", "CANY", "HOVE", "NABR", "MOAB")), 
         district = factor(c("ARCH", "ISKY", "NEED", "MAZE", "HOVE", "MOAB", 
                             "NABR"), 
                           levels = c("ARCH", "ISKY", "NEED", "MAZE", "HOVE", 
                                      "NABR", "MOAB")))
knitr::kable(stations)

#-- Weather Data
#' Clear GHCND cache files to ensure data are downloaded to current date
ghcnd_clear_cache()

#' Download data from the Global Histoic Climate Network
dat.raw <- meteo_pull_monitors(as.vector(stations$id))

#' Convert from wide to long data
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
  left_join(select(stations, id, parkunit, district)) %>%
  select(key, id, parkunit, district)
  
#-- Append metadata
stations <- stations %>%
  left_join(wx.dat %>% 
              group_by(id) %>%
              summarise(startDate = min(date, na.rm = T),
                        endDate = max(date, na.rm = T)))


#+ QAQC ----
#-- Flag definitions
flags <- read.csv("Flags.csv", stringsAsFactors = F)

#-- Measurement flags (MFLAGs)
mflags <- filter(flags, Type == "MFLAG") %>%
  rename(MFLAG = Flag)
#' Get flags in data
wx.dat %>%
  filter(MFLAG %in% mflags$MFLAG) %>%
  left_join(mflags) %>%
  select(Definition) %>%
  distinct()

#-- Quality flags (QFLAGs)
qflags <- filter(flags, Type == "QFLAG") %>%
  rename(QFLAG = Flag)
#' Get flags in data
wx.dat %>%
  filter(QFLAG %in% qflags$QFLAG) %>%
  left_join(qflags) %>%
  select(Definition) %>%
  distinct()

#-- Source flags (SFLAGs)
sflags <- filter(flags, Type == "SFLAG") %>%
  rename(SFLAG = Flag)
#' Get flags in data
wx.dat %>%
  filter(SFLAG %in% sflags$SFLAG) %>%
  left_join(sflags) %>%
  select(Definition) %>%
  distinct()

#+ Clean Data ----
wx.dat <- wx.dat %>%
  mutate(value = ifelse(QFLAG %in% qflags$QFLAG, NA, value)) %>%
  select(key, id, parkUnit, district, date, element, value, MFLAG, QFLAG, SFLAG)
  arrange(id, date, element)

#+ End Script ----
save(wx.dat, download.date, stations, flags, file = "Climate.RData")
print(paste0("This script was ran on ", now(), "."))
sessionInfo()