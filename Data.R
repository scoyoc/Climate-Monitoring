#' ---
#' title: "Doanload climate data"
#' author: "Matthew W. Van Scoyoc"
#' ---
#'
#' Developed: 21 December, 2017
#' Updated: 9 January, 2017
#' Data files: downlaoded from NOAA ftp site using rnoaa
#' Associated files:
#' Associated directories: 
#' - "C:/R/Climate"
#' - "P:/1_ResourceStewardshipScience/Natural Resource Program/Climate and weather/1 Trends"
#' References: 

#'# Setup workspace
#+ Setup, messages=F, warnings=F ----
#-- Packages
#install.packages(c("rnoaa", "data.table", "tidyverse", "lubridate"))
library("rnoaa")
library("tidyverse")
library("lubridate")

#'# Download data
#+ Data, messages=F, warnings=F ----
#' Download date
download.date = today()

#-- Station Metadata
#' Get station IDs and metadata
# stations <- ghcnd_stations()

#' List station IDs for SEUG Visitor Centers and Moab
seug.ids <- c("USC00420336", "USC00421163", "USC00421168", "USC00423600", 
              "USC00424100", "USC00426053", "USC00425733")

#-- Climate Data
ghcnd_clear_cache()

#' Download GHCN data
raw.dat <- meteo_pull_monitors(seug.ids, keep_flags = T, 
                               date_max = download.date, 
                               var = c("prcp", "tmin", "tmax"))

#' Convert from wide to long data
wx.dat <- raw.dat %>%
  select(id, date, prcp, tmax, tmin) %>%
  gather(Element, Value, 3:5, na.rm = F) %>%
  mutate(Key = paste0(id, as.numeric(date), Element)) %>%
  left_join(select(raw.dat, id, date, contains("mflag")) %>%
              gather(Element, MFLAG, 3:5, na.rm = F) %>%
              separate(Element, c("Flag", "Element"), "_") %>%  
              mutate(Key = paste0(id, as.numeric(date), Element)) %>%
              select(Key, MFLAG)) %>%
  left_join(select(raw.dat, id, date, contains("qflag")) %>%
              gather(Element, QFLAG, 3:5, na.rm = F) %>%
              separate(Element, c("Flag", "Element"), "_") %>%  
              mutate(Key = paste0(id, as.numeric(date), Element)) %>%
              select(Key, QFLAG)) %>%
  left_join(select(raw.dat, id, date, contains("sflag")) %>%
              gather(Element, SFLAG, 3:5, na.rm = F) %>%
              separate(Element, c("Flag", "Element"), "_") %>%  
              mutate(Key = paste0(id, as.numeric(date), Element)) %>%
              select(Key, SFLAG)) %>%
  select(Key, id, date, Element, Value, MFLAG, QFLAG, SFLAG) %>%
  arrange(id, date, Element) %>%
  rename(ID = id, Date = date)

#+ MFLAGs, messages=F, warnings=F ----
#-- Measurement flags
#' Define Flags
mflags <- data.frame(MFLAG = c("A", "B", "D", "K", "L", "O", "P", "T", "W"), 
                     Definition = c("value in precipitation or snow is a multi-day total", 
                                    "precipitation total formed from two 12-hour totals", 
                                    "precipitation total formed from four six-hour totals", 
                                    "converted from knots", 
                                    "temperature appears to be lagged with respect to reported hour of observation", 
                                    "converted from oktas", 
                                    "identified as 'missing presumed zero' in DSI 3200 and 3206", 
                                    "trace of precipitation, snowfall, or snow depth", 
                                    "converted from 16-point WBAN code (for wind direction)"))
#' Get flags in data
wx.dat %>%
  filter(MFLAG %in% mflags$MFLAG) %>%
  left_join(mflags) %>%
  select(Definition) %>%
  distinct()

#+ QFLAGS, messages=F, warnings=F ----
#-- Quality flags
#' Define Flags
qflags <- data.frame(QFLAG = c("D", "G", "I", "K", "L", "M", "N", "O", "R", "S",
                               "T", "W", "X", "Z"), 
                     Definition = c("failed duplicate check", 
                                    "failed gap check", 
                                    "failed internal consistency check", 
                                    "failed streak/frequent-value check", 
                                    "failed check on length of multiday period", 
                                    "failed megaconsistency check", 
                                    "failed naught check", 
                                    "failed climatological outlier check", 
                                    "failed lagged range check", 
                                    "failed spatial consistency check", 
                                    "failed temporal consistency check", 
                                    "temperature too warm for snow", 
                                    "failed bounds check", 
                                    "flagged as a result of an official Datzilla investigation"))

#' Get flags in data
wx.dat %>%
  filter(QFLAG %in% qflags$QFLAG) %>%
  left_join(qflags) %>%
  select(Definition) %>%
  distinct()

#+ SFLAGS, messages=F, warnings=F ----
#-- Source flags
#-- Need to write code --

#+ Clean Data ----
dat <- dat %>%
  mutate(Value = ifelse(QFLAG %in% qflags, NA, Value)) %>%
  arrange(id, Date, Element)

#+ End Script ----
save(wx.dat, download.date, stations.seug, file = "Climate.RData")
#rm(list = ls(all.names = T))
now()
sessionInfo()
