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
#' districts relative to historic measurements and the 1981-2010 30-year 
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
library("cowplot") # Streamlining for ggplot2

#-- Figures
# ggplot theme settings
my.theme <- theme(strip.background = element_rect(fill="white"), 
                  strip.text = element_text(hjust = 0.1), 
                  axis.title.x = element_blank(), 
                  legend.key = element_blank(), 
                  legend.position = c(0.85, 0.15), 
                  legend.title = element_blank(), 
                  legend.direction = "vertical", 
                  legend.text = element_text(size = 12),
                  plot.title = element_text(hjust = 0.5))

# Data ----
#-- List of months
month.df <- data.frame(month = 1:12, 
                       waterMonth = c(4:12, 1:3),
                       mthName = factor(month.abb, 
                                        levels = c(month.abb[10:12], 
                                                   month.abb[1:9])))

#-- Designate target water year & month
water.yr = 2018
# target.month <- 4
target.month <- filter(month.df, month == (month(today()) -1 ))

#-- Load .RData
load("Climate.RData")

# Add years and months to wx.dat
wx.dat <- wx.dat %>%
  mutate(month = month(date), 
         year = year(date), 
         waterYr = ifelse(month >= 10, year + 1, year)) %>%
  left_join(month.df)

# Temps ----
#-- Subset data
temp.dat <- wx.dat %>%
  filter(element != "prcp") %>%
  mutate(degC = value / 10, 
         degF = (degC * 1.8) + 32) %>%  # Convert to farenheit
  select(-value)

#-- 30-yr Stats (1981-2010 water years)
temps.30yr <- temp.dat %>%
  filter(waterYr >= 1981 & waterYr <= 2010) %>%
  group_by(element, district, waterMonth) %>%
  summarise(mean.30yr = round(mean(degF, na.rm = T), 3),
            p90.30yr = quantile(degF, 0.90, na.rm = T)) %>%
  left_join(month.df) %>%
  select(element, district, mthName, waterMonth, mean.30yr, p90.30yr)

#-- Rank warmest/coolest days for each weather district
temps.dayRank = temp.dat %>%
  select(element, district, waterMonth, date, degF) %>%
  group_by(element, district, waterMonth) %>%
  mutate(rank = min_rank(degF)) %>%
  arrange(element, district, waterMonth, desc(rank))

#-- Rank warmest/coolest months for each weather district
temps.mthRank <- temp.dat %>%
  select(element, district, waterYr, waterMonth, degF) %>%
  group_by(element, district, waterYr, waterMonth) %>%
  summarise(mthAvg = mean(degF, na.rm = T)) %>%
  group_by(element, district, waterMonth) %>%
  mutate(warmest = min_rank(-mthAvg),
         coolest = min_rank(mthAvg)) %>%
  arrange(element, district, waterMonth, warmest) 

#-- Monthly Departures
temps.depart <- temps.mthRank %>%
  left_join(temps.30yr) %>%
  mutate(depart = mthAvg - mean.30yr) %>%
  select(element, district, waterYr, mthName, waterYr, waterMonth, mthAvg, 
         depart, warmest, coolest, mean.30yr)

# Temp Summaries ----
#-- Table
temp.sum <- temps.depart %>%
  filter(waterYr == water.yr & waterMonth <= target.month$waterMonth) %>%
  mutate(mthAvg = round(mthAvg, 2), 
         depart = round(depart, 2), 
         rank_HotCold = paste(warmest, coolest, sep = "|"), 
         mean.30yr = round(mean.30yr, 2)) %>%
  ungroup() %>%
  select(element, district, waterYr, mthName, mthAvg, depart, rank_HotCold, 
         mean.30yr) %>%
  gather(var, value, mthAvg:mean.30yr) %>%  
  full_join(month.df) %>%
  select(-month, -waterMonth) %>%
  spread(mthName, value, fill = NA) %>%
  mutate(var = factor(var, levels = c("mthAvg", "depart", "rank_HotCold", 
                                      "mean.30yr"))) %>%
  arrange(district, element, var)

#-- Figure
# Format data for figure
temp.fig.dat <- temps.depart %>%
  select(waterYr, element, district, mthName, depart) %>%
  spread(mthName, depart, fill = NA) %>%
  gather(mthName, depart, Oct:Sep) %>%
  mutate(mthName = factor(mthName, levels = c(month.abb[10:12], 
                                              month.abb[1:9]))) %>%
  filter(waterYr == water.yr & waterMonth <= target.month$waterMonth)

# Plot figure
# Custom title and text
my.title <- "Montly average air temperatures at SEUG visitor centers for water year 2018 to date\nDepartures from 30-yr averages (1981-2010)"
my.text <- paste0("Data were downloaded on ", downloadDate, ".")

t1 <- ggplot(temp.fig.dat, aes(x = mthName, y = depart, fill = element)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_hline(aes(yintercept = 0), linetype="solid", size = 1.0, 
             color = "black") +
  facet_wrap(~ district) + 
  scale_fill_manual(values = c("orange2", "deepskyblue3"), 
                    labels = c("Daily Max Temp (TMAX)", 
                               "Daily Min Temp (TMIN)")) +
  scale_color_manual(values = c("red4", "darkgreen")) +
  labs(x = "Year", y = "Departure (?F)", title = my.title) +
  theme_bw() +
  my.theme +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5), 
        legend.position = c(0.745, 0.15))
t1 <- ggdraw(t1) + draw_label(my.text, x = 0.80, y = 0.12, size = 12)
t1

# Save
my.dir <- paste(getwd(), "Results", sep = "/")
fig.title <- paste(today(), "TemperatureDepartures.png", sep = "_")
ggsave(fig.title, device = "png", path = my.dir, width = 9.7, height = 5.2, 
       units = "in", dpi = 500)

# PRCP ----
#-- Subset data
prcp.dat <- wx.dat %>%
  filter(element == "prcp") %>%
  mutate(PRCPmm = value / 10, 
         PRCPin = PRCPmm * (1/25.4)) %>% # Convert to inches
  select(-value)

#-- 30-yr Stats (1981-2010 water years)
prcp.30yr = prcp.dat %>%
  filter(waterYr >= 1981 & waterYr <= 2010) %>%
  group_by(district, waterYr, mthName) %>%
  summarise(mthObs = sum(PRCPmm, na.rm = T)) %>%
  mutate(wyObs = cumsum(mthObs)) %>%
  group_by(district, mthName) %>%
  summarise(mth.30yr = mean(mthObs), 
            mth.sd = sd(mthObs), 
            wy.30yr = mean(wyObs), 
            wy.sd = sd(wyObs))

#-- Rank wettest/driest months for each weather district
#- Monthly totals
prcp.mth.rank <- prcp.dat %>%
  select(district, waterYr, mthName, date, PRCPmm) %>%
  group_by(district, waterYr, mthName) %>%
  summarise(mthObs = sum(PRCPmm, na.rm = T)) %>%
  group_by(district, mthName) %>%
  mutate(mthWettest = min_rank(-mthObs),
         mthDriest = min_rank(mthObs), 
         key = paste(district, waterYr, mthName, sep = "_")) %>%
  arrange(district, mthWettest)

#- Water year totals
# Identify water years with complete datasets
complete.wys <- prcp.dat %>%
  group_by(district, waterYr, mthName) %>%
  summarise(mthObs = sum(PRCPmm, na.rm = T)) %>%
  group_by(district, waterYr) %>%
  summarise(n = n()) %>%
  # filter(n == 12) %>%
  # bind_rows(data.frame(district = stations$district, 
  #                      waterYr = rep(water.yr, length(stations$district)), 
  #                      n = rep(target.month$mthName, 
  #                              length(stations$district)))) %>%
  mutate(key = paste(district, waterYr, sep = "_"))

# Calculate ranking
prcp.wy.rank <- prcp.dat %>%
  select(district, waterYr, mthName, date, PRCPmm) %>%
  mutate(key = paste(district, waterYr, sep = "_")) %>%
  filter(key %in% complete.wys$key) %>%
  group_by(district, waterYr, mthName) %>%
  summarise(mthObs = sum(PRCPmm, na.rm = T)) %>%
  mutate(wyObs = cumsum(mthObs)) %>%
  group_by(district, mthName) %>%
  mutate(wyWettest = min_rank(-wyObs),
         wyDriest = min_rank(wyObs), 
         key = paste(district, waterYr, mthName, sep = "_")) %>%
  arrange(district, wyDriest)

#-- Monthly percent totals and departures from 30-year reference period 
prcp.pct <- prcp.mth.rank %>%
  left_join(prcp.wy.rank[, 5:8]) %>%
  left_join(prcp.30yr) %>%
  mutate(mthPctAvg = (mthObs / mth.30yr) * 100,
         mthDeparture = mthObs - mth.30yr,
         wyPctAvg = (wyObs / wy.30yr) * 100,
         wyDeparture = wyObs - wy.30yr) %>%
  select(district, waterYr, mthName, mthObs, mthPctAvg, mthDeparture, 
         mthWettest, mthDriest, mth.30yr, mth.sd, wyObs, wyPctAvg, 
         wyDeparture, wyWettest, wyDriest, wy.30yr, wy.sd)

# PRCP Summary ----
#-- Table
prcp.sum <- prcp.pct %>%
  left_join(month.df) %>%
  filter(waterYr == water.yr & waterMonth <= target.month$waterMonth) %>%
  mutate(element = "PRCP") %>%
  ungroup() %>%
  select(element, waterYr, district, mthName, mth.30yr, mthObs, mthPctAvg, 
         mthDriest, mthWettest, wy.30yr, wyObs, wyPctAvg, wyDriest, wyWettest) %>%
  mutate(mth.30yr = round(mth.30yr, 2),
         mthPctAvg = round(mthPctAvg, 2),
         wy.30yr = round(wy.30yr, 2), 
         wyPctAvg = round(wyPctAvg, 2),
         mthRank_DryWet = paste(mthDriest, mthWettest, sep = "|"), 
         wyRank_DryWet = paste(wyDriest, wyWettest, sep = "|")) %>%
  select(-mthDriest, -mthWettest, -wyDriest, -wyWettest) %>%
  gather(var, value, mth.30yr:wyRank_DryWet) %>%
  mutate(var = factor(var, levels = c("mth.30yr", "mthObs", "mthPctAvg",
                                      "mthRank_DryWet", "wy.30yr", "wyObs", 
                                      "wyPctAvg", "wyRank_DryWet"))) %>%
  full_join(month.df) %>%
  select(-month, -waterMonth) %>%
  spread(mthName, value, fill = NA)

#-- Figure
prcp.fig.dat <- prcp.pct %>%
  select(waterYr, district, mthName, mthPctAvg) %>%
  spread(mthName, mthPctAvg, fill = NA) %>%
  mutate(Var = "mthPctAvg") %>%
  bind_rows(select(prcp.pct, waterYr, district, mthName, wyPctAvg) %>%
              spread(mthName, wyPctAvg, fill = NA) %>%
              mutate(Var = "wyPctAvg")) %>%
  gather(mthName, Value, Oct:Sep) %>%
  mutate(mthName = factor(mthName, levels = c(month.abb[10:12], 
                                              month.abb[1:9]))) %>%
  left_join(month.df) %>%
  filter(waterYr == water.yr)
  
# Plot figure
# Custom title and text
my.title <- "Percent average precipitaion at SEUG visitor centers\nfor water year 2018  to date"
my.text <- paste0("Data were downloaded on ", downloadDate, ".")

p1 <- ggplot(prcp.fig.dat, aes(x = mthName, y = Value, fill = Var)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = c("green3", "green4"), 
                    labels = c("% of Montly Avg", "% of Water Year Avg")) +
  geom_hline(aes(yintercept = 100), linetype="dashed", size = 1.0, 
             color = "grey40") +
  facet_wrap(~ district) +
  labs(x = "Month", y = "% of Average", title = my.title) +
  theme_bw() + 
  my.theme +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5), 
        legend.position = c(0.725, 0.15))
p1 <- ggdraw(p1) + draw_label(my.text, x = 0.80, y = 0.12, size = 12)
p1

# Save
my.dir <- paste(getwd(), "Results", sep = "/")
fig.title <- paste(today(), "Precipitation.png", sep = "_")
ggsave(fig.title, p1, device = "png", path = my.dir, width = 9.7, height = 5.2, 
       units = "in", dpi = 500)

# Excel ----
#-- Short summary
bwu.sum <- temps.depart %>%
  filter(waterYr == water.yr & waterMonth == target.month$waterMonth &
         element == "tmax") %>%
  mutate(tmaxDeparture = round(depart, 2), 
         tmaxRank_HotCold = paste(warmest, coolest, sep = "|")) %>%
  ungroup() %>%
  select(district, tmaxDeparture, tmaxRank_HotCold) %>%
  left_join(temps.depart %>%
              filter(waterYr == water.yr & 
                     waterMonth == target.month$waterMonth &
                     element == "tmin") %>%
              mutate(tminDeparture = round(depart, 2),
                     tminRank_HotCold = paste(warmest, coolest, sep = "|")) %>%
              ungroup() %>%
              select(district, tminDeparture, tminRank_HotCold)) %>%
  left_join(prcp.pct %>%
              filter(waterYr == water.yr & 
                       mthName == target.month$mthName) %>%
              mutate(wyPctAvg = round(wyPctAvg, 2), 
                     wyRank_DryWet = paste(wyDriest, wyWettest, 
                                          sep = "|")) %>%
              ungroup() %>%
              select(district, wyPctAvg, wyRank_DryWet)) %>%
  left_join(select(stations, district, startDate, endDate) %>%
              mutate(duration.yrs = round(as.numeric(endDate - startDate)/365, 2)))


#-- Save as *.xlsx
# Designate directory
my.dir <- paste(getwd(), "Results", sep = "/")

# Create file name
my.file = paste(today(), "ClimateSummary.xlsx", sep = "_")

# Create and save Excel workbook
write.xlsx(data.frame(bwu.sum), file = paste(my.dir, my.file, sep = "/"), 
           sheetName = paste(target.month$mthName, "Summary", sep = "_"), 
           row.names = F)

# Load workbook
my.wb = loadWorkbook(paste(my.dir, my.file, sep = "/"))

# Add TMAX data
my.sheet = createSheet(my.wb, sheetName = "TMAX")
addDataFrame(data.frame(filter(temp.sum, element == "tmax")), 
             my.sheet, row.names = F)

# Add TMIN data
my.sheet = createSheet(my.wb, sheetName = "TMIN")
addDataFrame(data.frame(filter(temp.sum, element == "tmin")), 
             my.sheet, row.names = F)

# Add PRCP data
my.sheet = createSheet(my.wb, sheetName = "PRCP")
addDataFrame(data.frame(prcp.sum), my.sheet, row.names = F)

# Save workbook
saveWorkbook(my.wb, file = paste(my.dir, my.file, sep = "/"))

# End Script ----
print(paste0("This script was ran on ", now(), "."))
sessionInfo()
