#' ---
#' title: "Climate Monitoring, Monthly Summary"
#' author: "Matthew W. Van Scoyoc"
#' date: "`r Sys.Date()`"
#' ---
#'
#' Updated: 6 October 2020
#' Data files: climateData.RData
#' Associated files:
#' - downlaodData.R
#' Associated directories: 
#' - "C:/R/Climate-Monitoring"
#' References: 
#' Notes:
#' Summarise temperature and precipitaiton records from SEUG Co-op weather stations 
#' relative to historic measurements and the 1981-2010 30-year reference period.
#' ----------------------------------------------------------------------

# Setup ----
#-- Packages
# install.packages(c("tidyverse", "lubridate", "cowplot"))
library("tidyverse") # Data manipulation
library("lubridate") # Easily work with date/time data
library("cowplot") # Streamlining for ggplot2

#-- Custom functions
# Multiplot function from Cookbook for R
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#-- Figures
# ggplot theme settings
my.theme <- theme(strip.background = element_rect(fill = "white"), 
                  strip.text = element_text(hjust = 0.1), 
                  axis.title.x = element_blank(), 
                  legend.key = element_blank(), 
                  legend.position = c(0.85, 0.15), 
                  legend.title = element_blank(), 
                  legend.direction = "vertical", 
                  legend.text = element_text(size = 12),
                  plot.title = element_text(hjust = 0.5))

# Data ----
#-- Designate target water year & month
water.yr = 2020

#-- Load .RData
load("climateData.RData")

# Temps ----
#-- Subset temp data
temp.dat <- wx.dat %>%
  filter(element != "prcp") %>%
  mutate(degC = value / 10) %>%  # Convert to Celsius
  select(-value)

#-- 30-yr Temp Stats (1981-2010 water years)
temps.30yr <- temp.dat %>%
  filter(waterYr >= 1981 & waterYr <= 2010) %>%
  group_by(district, element, waterMonth) %>%
  summarise(p10.30yr = quantile(degC, 0.10, na.rm = T), 
            mean.30yr = mean(degC, na.rm = T), 
            med.30.yr = median(degC, na.rm = T),
            p90.30yr = quantile(degC, 0.90, na.rm = T)) %>%
  left_join(month.df) %>%
  select(element, district, mthName, p10.30yr, mean.30yr, med.30.yr, p90.30yr)

#-- Monthly Temp Departures
temps.depart <- temp.dat %>%
  select(element, district, waterYr, date, month, degC) %>%
  left_join(month.df) %>%
  group_by(district, element, waterYr, mthName) %>%
  summarise(mean.degC = mean(degC, na.rm = T)) %>%
  left_join(select(temps.30yr, element, district, mthName, mean.30yr)) %>%
  mutate(departure = mean.degC - mean.30yr)

temps.rank <- temps.depart %>%
  group_by(element, district, mthName) %>%
  mutate(warmest = min_rank(-departure),
         coolest = min_rank(departure)) %>%
  arrange(element, district, mthName, warmest)

#-- Bar plot
# Custom title, text, and data
my.title <- paste("Montly average air temperatures at SEUG visitor centers for water year",  
                  water.yr, "to date\nDepartures from 30-yr averages (1981-2010)", 
                  sep = " ")
my.text <- paste0("Data were downloaded on ", downloadDate, ".")
plot.temp <- filter(temps.depart, waterYr == water.yr) %>%
  full_join(month.df) %>%
  select(element, district, waterYr, mthName, departure) %>%
  spread(mthName, departure) %>%
  gather("mthName", "departure", Oct:Sep) %>%
  filter(!is.na(element)) %>%
  mutate(mthName = factor(mthName, levels = c(month.abb[10:12], 
                                                month.abb[1:9])))

t1 <- ggplot(plot.temp, aes(x = mthName, y = departure, fill = element)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_hline(aes(yintercept = 0), linetype="solid", size = 1.0, color = "black") +
  facet_wrap(~ district) + 
  scale_fill_manual(values = c("orange2", "deepskyblue3"), 
                    labels = c("Daily Max Temp (TMAX)", 
                               "Daily Min Temp (TMIN)")) +
  scale_color_manual(values = c("red4", "darkgreen")) +
  labs(y = "Departure (°F)", title = my.title) +
  theme_bw() +
  my.theme +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5), 
        legend.position = c(0.745, 0.15))
t1 <- ggdraw(t1) + draw_label(my.text, x = 0.827, y = 0.11, size = 12)
t1
# Save
my.dir <- paste(getwd(), "Results", sep = "/")
ggsave(paste(date.stamp, "Temperature Departures.png", sep = "_"), 
       device = "png", path = my.dir, width = 9.7, height = 5.2, units = "in", 
       dpi = 300)

#-- Line plots
yr.labs <- stations %>%
  select(district, startDate, endDate) %>%
  mutate(label = paste(year(startDate), year(endDate), sep = " - "), 
         waterYr = water.yr)
# TMAX
t2 <- ggplot(filter(temps.depart, element == "tmax"), 
             aes(x = mthName, y = departure, group = waterYr)) +
  geom_line(stat = "identity", color = "gray") +
  geom_text(data = yr.labs, mapping = aes(x = Inf, y = -Inf, label = label), 
            color = "gray40", hjust = 1.2, vjust = -1) +
  geom_line(data = filter(temps.depart, element == "tmax" & waterYr == water.yr), 
            stat = "identity", color = "red3", size = 1.5) +
  geom_text(data = yr.labs, mapping = aes(x = Inf, y = Inf, label = waterYr), 
            color = "red3", hjust = 2, vjust = 1.5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.75, 
             color = "black") +
  facet_wrap(~ district, scales = "free_y") +
  labs(title = "Departures from the 30-year Average (1981-2010)\nA. Maximum Daily Tempertuare (TMAX)", 
       y = "Departure (°C)") +
  theme_bw()+
  my.theme +
  theme(plot.title = element_text(hjust = 0), 
        axis.text.x  = element_text(angle=90, vjust=0.5))

# TMIN
t3 <- ggplot(filter(temps.depart, element == "tmin"), 
             aes(x = mthName, y = departure, group = waterYr)) +
  geom_line(stat = "identity", color = "gray") +
  geom_text(data = yr.labs, mapping = aes(x = Inf, y = -Inf, label = label), 
            color = "gray40", hjust = 1.2, vjust = -1) +
  geom_line(data = filter(temps.depart, element == "tmin" & waterYr == water.yr), 
            stat = "identity", color = "deepskyblue3", size = 1.5) +
  geom_text(data = yr.labs, mapping = aes(x = Inf, y = Inf, label = waterYr), 
            color = "deepskyblue3", hjust = 2, vjust = 1.5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.75, 
             color = "black") +
  facet_wrap(~ district, scales = "free_y") +
  labs(title = "B. Minimum Daily Tempertuare (TMIN)", 
       y = "Departure (°C)") +
  theme_bw()+
  my.theme +
  theme(plot.title = element_text(hjust = 0), 
        axis.text.x  = element_text(angle = 90, vjust = 0.5))
multiplot(t2, t3, cols = 1)

# Save
ggsave(filename = paste(date.stamp, water.yr, "Temp Trends.png", sep = "_"), 
       plot = multiplot(t2, t3, cols = 1), 
       device = "png", path = my.dir, 
       height = 11, width = 8.5, units = "in", dpi = 300)

# PRCP ----
#-- Subset PRCP data
prcp.dat <- wx.dat %>%
  filter(element == "prcp") %>%
  mutate(PRCPmm = value / 10, 
         PRCPin = PRCPmm * (1/25.4)) %>% # Convert to inches
  select(-value)

#-- 30-yr PRCP Stats (1981-2010 water years)
prcp.30yr = prcp.dat %>%
  filter(waterYr >= 1981 & waterYr <= 2010) %>%
  group_by(district, waterYr, mthName) %>%
  summarise(element = "mth", 
            Obs = sum(PRCPmm, na.rm = T))
wy.30yr <- prcp.30yr %>%
  mutate(element = "wy", 
         Obs = cumsum(Obs))
prcp.30yr <- bind_rows(prcp.30yr, wy.30yr) %>%
  group_by(district, mthName, element) %>%
  summarise(mean.30yr = mean(Obs), 
            sd.30yr = sd(Obs))

#- Water Year Totals
# This dataframe can be used to filter out incomplete water years. 
complete.wys <- prcp.dat %>%
  group_by(district, waterYr, mthName) %>%
  summarise(mthObs = sum(PRCPmm, na.rm = T)) %>%
  group_by(district, waterYr) %>%
  summarise(n = n()) %>%
  filter(n == 12) %>%
  mutate(key = paste(district, waterYr, sep = "_"))

#- Calculate percent average and departure
# Calculate monthly precip totals
mth.prcp <- prcp.dat %>%
  select(district, year, waterYr, mthName, date, PRCPmm) %>%
  # mutate(key = paste(district, waterYr, sep = "_")) %>% # Use these two lines to filter
  # filter(key %in% complete.wys$key) %>%                 # incomplete water years
  group_by(district, year, waterYr, mthName) %>%
  summarise(element = "mth", 
            Obs = sum(PRCPmm, na.rm = T))
# Calculate water year precip totals
wy.prcp <- mth.prcp %>%
  mutate(element = "wy", 
         Obs = cumsum(Obs))
# Combine dataframes and calculate pct avg and departure
pct.prcp <- bind_rows(mth.prcp, wy.prcp) %>%
  left_join(prcp.30yr) %>%
  mutate(pct.avg = (Obs / mean.30yr) * 100,
         departure = Obs - mean.30yr)

# Rank monthly accumulations by water year.
wy.rank <- wy.prcp %>%
  group_by(district, mthName) %>%
  mutate(Wettest = min_rank(-Obs),
         Driest = min_rank(Obs))

#-- Bar Plot
# Custom title and text
my.title <- paste("Total precipitation at SEUG visitor centers for water year", 
                  water.yr, "to date\nPresented as a percentage of the 30-yr averages (1981-2010)", 
                  sep = " ")
my.text <- paste0("Data were downloaded on ", downloadDate, ".")
plot.prcp <- filter(pct.prcp, waterYr == water.yr) %>%
  full_join(month.df) %>%
  select(element, district, waterYr, mthName, pct.avg) %>%
  spread(mthName, pct.avg) %>%
  gather("mthName", "pct.avg", Oct:Sep) %>%
  filter(!is.na(element)) %>%
  mutate(mthName = factor(mthName, levels = c(month.abb[10:12], 
                                              month.abb[1:9])))

p1 <- ggplot(filter(plot.prcp, waterYr == water.yr), 
             aes(x = mthName, y = pct.avg, fill = element)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = c("forestgreen", "chocolate3"), 
                    labels = c("Monthly Total", "Water Year Accumulation")) +
  geom_hline(aes(yintercept = 100), linetype="dashed", size = 1.0, 
             color = "grey40") +
  facet_wrap(~ district) +
  labs(x = "Month", y = "% of 30-yr Average", title = my.title) +
  theme_bw() + 
  my.theme +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5), 
        legend.position = c(0.705, 0.15))
p1 <- ggdraw(p1) + draw_label(my.text, x = 0.787, y = 0.11, size = 12)
p1
# Save
ggsave(paste(date.stamp, "Precipitation.png", sep = "_"), p1, device = "png", 
       path = my.dir, width = 9.7, height = 5.2, units = "in", dpi = 300)

#-- Line Plot
# TMAX
p2 <- ggplot(filter(pct.prcp, element == "mth"), 
             aes(x = mthName, y = pct.avg, group = waterYr)) +
  geom_line(stat = "identity", color = "gray") +
  geom_text(data = yr.labs, mapping = aes(x = -Inf, y = Inf, label = label), 
            color = "gray40", hjust = -0.1, vjust = 1.5) +
  geom_line(data = filter(pct.prcp, element == "mth" & waterYr == water.yr), 
            stat = "identity", color = "forestgreen", size = 1.5) +
  geom_text(data = yr.labs, mapping = aes(x = Inf, y = Inf, label = waterYr), 
            color = "forestgreen", hjust = 1.25, vjust = 2.5) +
  geom_hline(aes(yintercept = 100), linetype="dashed", size = 0.75, 
             color = "black") +
  facet_wrap(~ district, scales = "free_y") +
  labs(title = "Precipitation as a Percent of 30-year Averages (1981-2010)\nC. Monthly Totals", 
       y = "% of 30-yr Average") +
  theme_bw()+
  my.theme +
  theme(plot.title = element_text(hjust = 0), 
        axis.text.x  = element_text(angle=90, vjust=0.5))

# TMIN
p3 <- ggplot(filter(pct.prcp, element == "wy"), 
             aes(x = mthName, y = pct.avg, group = waterYr)) +
  geom_line(stat = "identity", color = "gray") +
  geom_text(data = yr.labs, mapping = aes(x = -Inf, y = Inf, label = label), 
            color = "gray40", hjust = -0.2, vjust = 1.5) +
  geom_line(data = filter(pct.prcp, element == "wy" & waterYr == water.yr), 
            stat = "identity", color = "chocolate3", size = 1.5) +
  geom_text(data = yr.labs, mapping = aes(x = Inf, y = Inf, label = waterYr), 
            color = "chocolate3", hjust = 1.25, vjust = 2.5) +
  geom_hline(aes(yintercept = 100), linetype="dashed", size = 0.75, 
             color = "black") +
  facet_wrap(~ district, scales = "free_y") +
  labs(title = "D. Water Year Accumulation", 
       y = "% of 30-yr Average") +
  theme_bw()+
  my.theme +
  theme(plot.title = element_text(hjust = 0), 
        axis.text.x  = element_text(angle=90, vjust=0.5))
multiplot(p2, p3, cols = 1)

# Save
ggsave(filename = paste(date.stamp, water.yr, "PRCP Trends.png", sep = "_"), 
       plot = multiplot(p2, p3, cols = 1), 
       device = "png", path = my.dir, 
       height = 11, width = 8.5, units = "in", dpi = 300)

#-- Aggregate figures
# Bar plots
ggsave(filename = paste(date.stamp, water.yr, "Climate Trends - bar plot.png", sep = "_"), 
       plot = multiplot(t1, p1, cols = 1), 
       device = "png", path = my.dir, 
       height = 8.5, width = 11, units = "in", dpi = 300)

# Line plots
ggsave(filename = paste(date.stamp, water.yr, "Climate Trends.png", sep = "_"), 
       plot = multiplot(t2, t3, p2, p3, cols = 2), 
       device = "png", path = my.dir, 
       height = 8.5, width = 11, units = "in", dpi = 300)

# Save *.csv
csv.dat <- wy.rank %>%
  filter(waterYr == water.yr) %>%
  mutate(rank.dw = paste(Driest, Wettest, sep = "|"), 
         element = "prcp.dw") %>%
  select(district, element, mthName, rank.dw) %>%
  spread(mthName, value = rank.dw) %>%
  bind_rows(temps.rank %>%
              filter(waterYr == water.yr) %>%
              mutate(rank.wc = paste(warmest, coolest, sep = "|"), 
                     element = paste(element, "wc", sep = ".")) %>%
              select(district, element, mthName, rank.wc) %>%
              spread(mthName, value = rank.wc) %>%
              arrange(element, district))
my.file <- paste(date.stamp, water.yr, "wyRank.csv", sep = "_")
write.csv(csv.dat, file = paste(my.dir, my.file, sep = "/"), 
          row.names = FALSE)
