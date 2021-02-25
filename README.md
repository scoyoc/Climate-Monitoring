# Climate Monitoring in Southeast Utah National Parks 
This project downloads and summarizes Co-op weather station data from Arches 
(ARCH) and Canyonlands (CANY) national parks, Hovenweep (HOVE) and Natural 
Bridges (NABR) national monuments, and the City of Moab, UT  from the [Global 
Historical Climatology Network - Daily (GHCND)](https://www.ncdc.noaa.gov/ghcn-daily-description)
using R and the [climateAnalyzeR](https://github.com/scoyoc/climateAnalyzeR) 
package (Van Scoyoc, 2021).  

## Description
This repository contains an RMarkdown script ([wySummary.Rmd](wySummary.Rmd))that download and summarize data 
from the GHCND and a R script to render the summary([renderSummaries.R](renderSummaries.R)).

* [wySummary.Rmd](wySummary.Rmd) produces a brief automated report summarizing temperature and  precipitation for ARCH, HOVE, NABR, and Moab. See [20210225_ARCH_ClimateSummary_WY2020.pdf](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results/20210225_ARCH_ClimateSummary_WY2020.pdf).

* The [renderSummaries.R](renderSummaries.R) script runs parameterized reports (e.g., my.park = "ARCH" and water.yr = 2020) that call wySummary.Rmd or wySummary_CANY.Rmd to render reports. The PDF's are then named with a date stamp and 4-letter park code and saved in the [Results sub-directory](https://github.com/scoyoc/Climate-Monitoring/blob/master/Results).

## Authors
* **Matthew Van Scoyoc** - *Initial work* - [scoyoc](https://github.com/scoyoc)

# License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
