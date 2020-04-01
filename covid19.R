# Copyright (c) 2020 Morten Jagd Christensen
# Attribution 4.0 International (CC BY 4.0)

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rJava, xlsx, httr, dplyr, ggplot2, cowplot, scales, REAT)

source("ecdc_funcs.R") # functions to handle the ECDC dataset
source("plot_funcs.R") # plotting functions (ggplot2 + cowplot)

####### The fun starts below #######

#
res <- getexcelfromurl()

# read the region database xlsx
regres <- read.xlsx("region_names.xlsx", 1)

# get list of countries from data frame
countries <- unique(res %>% select(countriesAndTerritories))

# write new country list
write.xlsx(countries, "country_list.xlsx", sheetName="countries")

# gapminder compatible data frame
final <- gapminder(100)

# write data to exel, for gapminder
write.xlsx(final, "gapminder.xlsx", sheetName="countries", row.names=FALSE)
multiplot(final)

#cleanup()
