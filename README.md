# COVID
## SARS-COV2-outbreak visualization and forecast

### Aim
* Look for correlations of number of cases of the Novel Coronavirus and known indicators
* Forecast number of cases and deaths

### Data sources:
* [2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE](https://github.com/CSSEGISandData/COVID-19)
* [United Nations Human Development reports](http://hdr.undp.org/en/data)
* [World Bank indicators](https://data.worldbank.org/indicator/sp.pop.totl?end=2018&start=2018)

### Updates:
Daily

### Deliverables:
* Dashboard, which can also be obtained by executing the following on R console:

library(shinydashboard)
library(shiny)
library(leaflet)
library(dplyr)
library(xts)
library(dygraphs)
library(ggplot2)
library(scales)
library(plotly)

runGitHub("fernandojru/COVID","fernandojru",subdir="Dashboard")

* Notebook with data cleaning, processing, EDA and feature engineering

