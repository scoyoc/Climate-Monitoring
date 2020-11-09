#' ---
#' title: "Render Climate Summaries"
#' author: "Matthew W. Van Scoyoc"
#' date: "`r Sys.Date()`"
#' ---
#'
#' Developed: 30 October 2020
#' Associated files:
#' - downlaodData.R
#' - wySummary_Park.Rmd
#' Associated directories: 
#' - "C:/R/Climate-Monitoring"
#' References: 
#' Notes:
#' Renders climate summaries from an RMarkdown file (.Rmd) and saves them in the
#' results directory with the appropriate name for the park unit summarized.
#' ----------------------------------------------------------------------

#+ Load Packages ----
# install.packages(c("tidyverse", "lubridate"))
library("tidyverse") # Data manipulation
library("lubridate") # Easily work with date/time data

#+ Render Report function ----
#' **Description**
#' Knit parameterized report and save it with an appropriate name in the Results
#' sub-directory of your RStudio project.
#' **Arguments**
#' park.name  The full name of the park unit of interest.
#' water.yr   The four-digit water year (YYYY) you want to summarize.
#' my.park    The four-letter park code for the park of interest.
#' my.rmd     The RMarkdown document you want to use to render the report. The 
#'            default is "wySummary_Park.Rmd".

renderSummary = function(park.name, water.yr, my.park, my.rmd="wySummary.Rmd") {
  rmarkdown::render(
    my.rmd, params = list(
      park.name = park.name,
      water.yr = water.yr, 
      my.park = my.park
    ),
    output_file = paste(paste(getwd(), "Results", sep = "/"), 
                        paste0(paste(str_split(today(), "-")[[1]], collapse = ""),
                               "_", my.park, "_ClimateSummary_WY", water.yr, ".pdf"), 
                        sep = "/")
  )
}

#+ Render Reports----
renderSummary("Arches National Park", 2020, "ARCH")
renderSummary("Hovenweep National Monument", 2020, "HOVE")
renderSummary("Natural Bridges National Monument", 2020, "NABR")
renderSummary("The City of Moab, Utah", 2020, "Moab")
