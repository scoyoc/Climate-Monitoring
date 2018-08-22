#' ---
#' title: "Climate Monitoring, Monthly Summary"
#' author: "Matthew W. Van Scoyoc"
#' ---
#'
#' Developed: 08 August, 2017
#' Updated: 
#' Data files: SEUG_climate.RData
#' Associated files:
#' - GHCN Data Munge.R
#' - SEUG_MonthlyPrecipReport_1.3.R
#' - SEUG_MonthlyTempReport_1.1.R
#' Associated directories: 
#' - "C:/R/Climate"
#' - "P:/1_ResourceStewardshipScience/Natural Resource Program/Climate and weather/1 Trends"
#' References: 
#' Notes:
#' Summarise temperature and precipitaiton records from SEUG weather 
#' stations relative to historic measurements and the 1981-2010 30-year 
#' reference period.
#' 
#' A Microsoft Excel workbook is produced.
#' ----------------------------------------------------------------------

# Setup ----
#-- Packages
# install.packages(c("tidyverse", "lubridate", "xlsx"))
library("xlsx") # Import and export Microsoft Excel workbooks
library("tidyverse") # Data manipulation
library("lubridate") # Easily work with date/time data

#-- Directories
root.dir <- "C:/R/Climate"
data.dir <- "C:/R/Climate/Data"

#-- Figures
# ggplot theme settings
my.theme <- theme(strip.background = element_rect(fill="white"), 
                  strip.text = element_text(hjust = 0.1), 
                  axis.title.x = element_blank(), 
                  legend.key = element_blank(), 
                  legend.position = c(0.85, 0.15), 
                  legend.title = element_blank(), 
                  legend.direction = "vertical", 
                  plot.title = element_text(hjust = 0.5))

# Data ----
#-- List of months
month.df <- data.frame(Month = 1:12, 
                       WaterMonth = c(4:12, 1:3),
                       MonthName = factor(month.abb, 
                                          levels = c(month.abb[10:12],
                                                     month.abb[1:9])))

#-- Designate target water year & month
water.yr = 2018
target.month <- 4
target.month <- month(today()) - 1
target.month <- month.df[month.df$Month == target.month, ]

#-- Load .RData
load(paste(data.dir, "SEUG_climate.RData", sep = "/"))

# Designate factor levels so data sort as desired
stations$labels = factor(c("Arches", "CANY - Island in the Sky", 
                           "CANY - The Needles", "CANY - Hans Flat", 
                           "Hovenweep", "Moab", "Natural Bridges"), 
                         levels = c("Arches", "CANY - Hans Flat", 
                                    "CANY - Island in the Sky",  
                                    "CANY - The Needles", "Hovenweep", 
                                    "Natural Bridges", "Moab"))

# Designate factor levels so data sort as desired
clim.dat <- clim.dat %>%
  mutate(Station = factor(Station, 
                          levels = c("ARCH", "MAZE", "ISKY", "NEED", "HOVE", 
                                     "NABR", "MOAB")), 
         WaterYr = ifelse(Month >= 10, Year + 1, Year)) %>%
  left_join(month.df)

#-- Subset data
temp.dat <- clim.dat %>%
  filter(Element != "PRCP") %>%
  mutate(Value = (Value * 1.8) + 32)  # Convert to farenheit

prcp.dat <- clim.dat %>%
  filter(Element == "PRCP") %>%
  mutate(Value = Value / 25.4)  # Convert to inches

# Temps ----
#-- 30-yr Stats (1981-2010 water years)
temps.30yr <- temp.dat %>%
  filter(WaterYr >= 1981 & WaterYr <= 2010) %>%
  group_by(Element, Station, WaterMonth) %>%
  summarise(Mean.30yr = round(mean(Value, na.rm = T), 3),
            P90.30yr = quantile(Value, 0.90, na.rm = T)) %>%
  left_join(month.df) %>%
  select(Element, Station, MonthName, WaterMonth, Mean.30yr, P90.30yr)

#-- Rank warmest/coolest days for each weather station
temps.day.rank = temp.dat %>%
  mutate(Date = paste(Day, Month, WaterYr, sep = "/")) %>%
  select(Element, Station, WaterMonth, Date, Value) %>%
  group_by(Element, Station, WaterMonth) %>%
  mutate(rank = min_rank(Value)) %>%
  arrange(Element, Station, WaterMonth, desc(rank))

#-- Rank warmest/coolest months for each weather station
temps.mth.rank <- temp.dat %>%
  select(Element, Station, WaterYr, WaterMonth, Value) %>%
  group_by(Element, Station, WaterYr, WaterMonth) %>%
  summarise(MonthlyAvg = mean(Value, na.rm = T)) %>%
  group_by(Element, Station, WaterMonth) %>%
  mutate(Warmest = min_rank(-MonthlyAvg),
         Coolest = min_rank(MonthlyAvg), 
         key = paste(Element, Station, WaterYr, WaterMonth, sep = "_")) %>%
  arrange(Element, Station, WaterMonth, Warmest) 

#-- Monthly Departures
temps.depart <- temps.mth.rank %>%
  left_join(temps.30yr) %>%
  mutate(Departure = MonthlyAvg - Mean.30yr) %>%
  select(Element, Station, WaterYr, MonthName, WaterYr, WaterMonth, MonthlyAvg, 
         Departure, Warmest, Coolest, Mean.30yr)

# Temp Summaries ----
#-- Table
temp.sum <- temps.depart %>%
  filter(WaterYr == water.yr & WaterMonth <= target.month$WaterMonth) %>%
  gather(Stat, Value, MonthlyAvg:Mean.30yr) %>%
  ungroup() %>%
  select(Element, WaterYr, WaterYr, Station, MonthName, Stat, Value) %>%
  spread(MonthName, Value, fill = NA) %>%
  mutate(Stat = factor(Stat, levels = c("MonthlyAvg", "Departure", "Warmest", 
                                        "Coolest", "Mean.30yr"))) %>%
  arrange(Station, Element, Stat)

#-- Figure
# Format data for figure
temp.fig.dat <- data.frame(Station = rep(stations$Station, 12), 
                      labels = rep(stations$labels, 12), 
                      Month = c(rep(1, 7), rep(2, 7), rep(3, 7), rep(4, 7), 
                                rep(5, 7), rep(6, 7), rep(7, 7), rep(8, 7), 
                                rep(9, 7), rep(10, 7), rep(11, 7), 
                                rep(12, 7))) %>%
  left_join(month.df) %>%
  left_join(temps.depart %>%
              filter(WaterYr == water.yr & 
                     WaterMonth <= target.month$WaterMonth)) %>%
  mutate(Element = ifelse(is.na(Element), "TMAX", Element))

# Plot figure
my.title <- "Departures from 30-yr Averages (1981-2010)\nat SEUG Weather Stations"
t1 <- ggplot(temp.fig.dat, aes(x = MonthName, y = Departure, fill = Element)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_hline(aes(yintercept = 0), linetype="solid", size = 1.0, 
             color = "black") +
  facet_wrap(~ labels) + 
  scale_fill_manual(values = c("orange2", "deepskyblue3")) +
  scale_color_manual(values = c("red4", "darkgreen")) +
  labs(x = "Year", y = "Departure (°F)", title = my.title) +
  theme_bw() +
  my.theme +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))
t1

# Save
my.dir = ("P:/1_ResourceStewardshipScience/Natural Resource Program/Climate and weather/1 Trends/5 Analysis/R/BiweeklyUpdate/Biweekly Update")
#my.dir <- paste(getwd(), "Figures", sep = "/")
fig.title <- paste0(today(), "_SEUG_Departures.png")
ggsave(fig.title, t1, device = "png", path = my.dir, 
       width = 9.7, height = 5.2, units = "in", dpi = 500)

# PRCP ----
#-- 30-yr Stats (1981-2010 water years)
prcp.30yr = prcp.dat %>%
  filter(WaterYr >= 1981 & WaterYr <= 2010) %>%
  group_by(Station, WaterYr, WaterMonth) %>%
  summarise(Mth.Obs = sum(Value, na.rm = T)) %>%
  mutate(WY.Obs = cumsum(Mth.Obs)) %>%
  group_by(Station, WaterMonth) %>%
  summarise(Mth.30yr = mean(Mth.Obs), 
            Mth.sd = sd(Mth.Obs), 
            WY.30yr = mean(WY.Obs), 
            WY.sd = sd(WY.Obs))

#-- Rank wettest/driest months for each weather station
# Monthly totals
prcp.mth.rank <- prcp.dat %>%
  select(Station, WaterYr, WaterMonth, Day, Value) %>%
  group_by(Station, WaterYr, WaterMonth) %>%
  summarise(Mth.Obs = sum(Value, na.rm = T)) %>%
  group_by(Station, WaterMonth) %>%
  mutate(Mth.Wettest = min_rank(-Mth.Obs),
         Mth.Driest = min_rank(Mth.Obs), 
         key = paste(Station, WaterYr, WaterMonth, sep = "_"))

# Water year totals
# Identify water years with complete datasets
complete.wys <- prcp.dat %>%
  group_by(Station, WaterYr, WaterMonth) %>%
  summarise(Mth.Obs = sum(Value, na.rm = T)) %>%
  group_by(Station, WaterYr) %>%
  summarise(n = n()) %>%
  filter(n == 12) %>%
  bind_rows(data.frame(Station = stations$Station, 
                       WaterYr = rep(water.yr, length(stations$Station)), 
                       n = rep(target.month$WaterMonth, 
                               length(stations$Station)))) %>%
  mutate(key = paste(Station, WaterYr, sep = "_"))
# Calculate ranking
prcp.wy.rank <- prcp.dat %>%
  select(Station, WaterYr, WaterMonth, Day, Value) %>%
  mutate(key = paste(Station, WaterYr, sep = "_")) %>%
  filter(key %in% complete.wys$key) %>%
  group_by(Station, WaterYr, WaterMonth) %>%
  summarise(Mth.Obs = sum(Value, na.rm = T)) %>%
  mutate(WY.Obs = cumsum(Mth.Obs)) %>%
  group_by(Station, WaterMonth) %>%
  mutate(WY.Wettest = min_rank(-WY.Obs),
         WY.Driest = min_rank(WY.Obs), 
         key = paste(Station, WaterYr, WaterMonth, sep = "_"))

#-- Monthly percent totals and departures from 30-year reference period 
prcp.pct <- prcp.mth.rank %>%
  left_join(prcp.wy.rank[, 5:8]) %>%
  left_join(prcp.30yr) %>%
  mutate(Mth.PctAvg = (Mth.Obs / Mth.30yr) * 100,
         Mth.Departure = Mth.Obs - Mth.30yr,
         WY.PctAvg = (WY.Obs / WY.30yr) * 100,
         WY.Departure = WY.Obs - WY.30yr) %>%
  select(Station, WaterYr, WaterMonth, Mth.Obs, Mth.PctAvg, Mth.Departure, 
         Mth.Wettest, Mth.Driest, Mth.30yr, Mth.sd, WY.Obs, WY.PctAvg, 
         WY.Departure, WY.Wettest, WY.Driest, WY.30yr, WY.sd)

# PRCP Summary ----
#-- Table
prcp.sum <- prcp.pct %>%
  filter(WaterYr == water.yr & WaterMonth <= target.month$WaterMonth) %>%
  mutate(Element = "PRCP") %>%
  select(Element, WaterYr, Station, WaterMonth, Mth.30yr, Mth.Obs, Mth.PctAvg, 
         Mth.Driest, Mth.Wettest, WY.30yr, WY.Obs, WY.PctAvg, WY.Driest, 
         WY.Wettest) %>%
  mutate(Mth.Rank_dw = paste(Mth.Driest, Mth.Wettest, sep = "|"), 
         WY.Rank_dw = paste(WY.Driest, WY.Wettest, sep = "|")) %>%
  select(-Mth.Driest, -Mth.Wettest, -WY.Driest, -WY.Wettest) %>%
  gather(Stat, Value, Mth.30yr:WY.Rank_dw) %>%
  mutate(Stat = factor(Stat, levels = c("Mth.30yr", "Mth.Obs", "Mth.PctAvg",
                                        "Mth.Rank_dw", "WY.30yr", "WY.Obs", 
                                        "WY.PctAvg", "WY.Rank_dw"))) %>%
  full_join(month.df) %>%
  ungroup() %>%
  select(-WaterMonth, -Month) %>%
  spread(MonthName, Value, fill = NA)

#-- Figure
# Format data for figure
prcp.fig.dat <- temp.fig.dat %>%
  select(Station, labels, Month, WaterMonth, MonthName) %>%
  distinct() %>%
  left_join(prcp.pct %>%
              filter(WaterYr == water.yr & 
                     WaterMonth <= target.month$WaterMonth) %>%
              select(Station, WaterYr, WaterMonth, Mth.PctAvg, WY.PctAvg) %>%
              gather(Stat, Value, Mth.PctAvg:WY.PctAvg))  %>%
  mutate(WaterYr = ifelse(is.na(WaterYr), water.yr, WaterYr), 
         Stat = ifelse(is.na(Stat), "Mth.PctAvg", Stat))

# Plot figure
my.title <- "Percent average precipitaion at SEUG weather stations"
p1 <- ggplot(prcp.fig.dat, aes(x = MonthName, y = Value, fill = Stat)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = c("green3", "green4"), 
                    labels = c("% of Montly Avg", "% of Water Year Avg")) +
  geom_hline(aes(yintercept = 100), linetype="dashed", size = 1.0, 
             color = "grey40") +
  facet_wrap(~ labels) +
  labs(x = "Month", y = "% of Average", title = my.title) +
  theme_bw() + 
  my.theme +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))
p1

# Save
my.dir = ("P:/1_ResourceStewardshipScience/Natural Resource Program/Climate and weather/1 Trends/5 Analysis/R/BiweeklyUpdate/Biweekly Update")
#my.dir <- paste(getwd(), "Figures", sep = "/")
fig.title <- paste(today(), "SEUG_PRCP.png", sep = "_")
ggsave(fig.title, p1, device = "png", path = my.dir, 
       width = 9.7, height = 5.2, units = "in", dpi = 500)

# Save ----
# Short summary for Biweekly Update
bwu.sum <- temps.depart %>%
  filter(WaterYr == water.yr & WaterMonth == target.month$WaterMonth &
         Element == "TMAX") %>%
  mutate(Departure = round(Departure, 2), 
         TMAX.Rank_wc = paste(Warmest, Coolest, sep = "|")) %>%
  rename(TMAX.Depart = Departure) %>%
  ungroup() %>%
  select(Station, TMAX.Depart, TMAX.Rank_wc) %>%
  left_join(temps.depart %>%
              filter(WaterYr == water.yr & 
                     WaterMonth == target.month$WaterMonth &
                     Element == "TMIN") %>%
              mutate(Departure = round(Departure, 2),
                     TMIN.Rank_wc = paste(Warmest, Coolest, sep = "|")) %>%
              rename(TMIN.Depart = Departure) %>%
              ungroup() %>%
              select(Station, TMIN.Depart, TMIN.Rank_wc)) %>%
  left_join(prcp.pct %>%
              filter(WaterYr == water.yr & 
                     WaterMonth == target.month$WaterMonth) %>%
              mutate(WY.PctAvg = round(WY.PctAvg, 2), 
                     PRCP.WY.Rank_dw = paste(WY.Driest, WY.Wettest, 
                                          sep = "|")) %>%
              ungroup() %>%
              select(Station, WY.PctAvg, PRCP.WY.Rank_dw)) %>%
  left_join(stations %>% select(Station, Duration.yrs))

#-- Format datasets for workbook
tmax.sum <- temp.sum %>% filter(Element == "TMAX")
tmin.sum <- temp.sum %>% filter(Element == "TMIN")

# Designate directory
my.dir = ("P:/1_ResourceStewardshipScience/Natural Resource Program/Climate and weather/1 Trends/5 Analysis/R/BiweeklyUpdate/Biweekly Update")
#my.dir <- paste(getwd(), "Biweekly Update", sep = "/")

#-- Save as *.xlsx
# Create workbook and write Biweekly Update summary
my.file = paste(today(), "SEUG_ClimateSummary.xlsx", sep = "_")
write.xlsx(data.frame(bwu.sum), file = paste(my.dir, my.file, sep = "/"), 
           sheetName = paste("Summary", target.month$MonthName, sep = "_"), 
           row.names = F)

# Load workbook
my.wb = loadWorkbook(paste(my.dir, my.file, sep = "/"))

# Add TMAX data
my.sheet = createSheet(my.wb, sheetName = "TMAX")
addDataFrame(data.frame(tmax.sum), my.sheet, row.names = F)

# Add TMIN data
my.sheet = createSheet(my.wb, sheetName = "TMIN")
addDataFrame(data.frame(tmin.sum), my.sheet, row.names = F)

# Add PRCP data
my.sheet = createSheet(my.wb, sheetName = "PRCP")
addDataFrame(data.frame(prcp.sum), my.sheet, row.names = F)

# Save workbook
saveWorkbook(my.wb, file = paste(my.dir, my.file, sep = "/"))

# End Script ----
rm(list = ls(all.names = T))
now()
sessionInfo()