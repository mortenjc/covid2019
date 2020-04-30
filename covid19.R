# Copyright (c) 2020 Morten Jagd Christensen
# Attribution 4.0 International (CC BY 4.0)

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rJava, xlsx, httr, dplyr, ggplot2, cowplot, scales, REAT)

source("ecdc_funcs.R") # functions to handle the ECDC dataset

####### The fun starts below #######
# Get data to work with
ecdcdata <- getexcelfromurl()
regions <- read.xlsx("region_names.xlsx", 1)

# get list of countries from data frame, write to file
countries <- unique(ecdcdata %>% select(countriesAndTerritories))
write.xlsx(countries, "country_list.xlsx", sheetName="countries")

# gapminder compatible data frame
MinCases <- 100
gapm <- gapminder(ecdcdata, countries, regions, MinCases)

# write data to exel, for gapminder
write.xlsx(gapm, "gapminder.xlsx", sheetName="countries", row.names=FALSE)

source("plot_funcs.R") # plotting functions (ggplot2 + cowplot)
singleplot(gapm, ReferenceCountries, MinCases, 1100000, "jCAPS Corp.")

multiplot(gapm, "", MinCases, 1100000)
#multiplot(gapm, "Total Deaths", 1, 100000)
#multiplot(gapm, "Cases", 1, 1000)

#cleanup()
