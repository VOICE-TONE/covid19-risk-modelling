require(lubridate)
require(dplyr)
require(ggplot2)
require(zoo)
require(censusapi)
require(RCurl)
require(stringr)
require(ggpubr)
require(plotly)
require(devtools)
require(rmarkdown)
require(shiny)
require(shinydashboard)
require(flexdashboard)
require(leaflet)
require(widgetframe)
#require(choroplethrZip)
require(covidcast)
require(tigris)
require(usmap)
require(mapproj)
require(mapview)
require(satellite)
require(tabulizer)
require(tabulizerjars)
require(tidygeocoder)
require(webshot)
require(gpclib)
require(foreign)

### Link for the package ChoroplethrZip
### https://rdrr.io/github/arilamstein/choroplethrZip/man/zip_choropleth.html

suppressPackageStartupMessages(require(sp))
suppressPackageStartupMessages(require(sf))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(ggiraph))

### Saving data.census.gov API key in the environment
Sys.setenv(CENSUS_KEY="423e5eab1ff90e9683995aaab8ff20c826296484")


#devtools::install_github("cmu-delphi/covidcast", ref = "main",subdir = "R-packages/covidcast")
require(covidcast)


# install.packages("remotes")
# remotes::install_github("e3bo/epidatr")
