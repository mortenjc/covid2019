
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rJava, xlsx, httr, dplyr, readxl)


# Read the covid excel sheet from file
getexcelfromfile <- function() {
  file = file.choose(new = FALSE)
  res <- read.xlsx(file, 1)
  # add new columns
  res$date <- 0
  res$aggcases <- 0
  res$aggdeaths <- 0
  res$region <- "na"
  return(res)
}

# From ECDC should get excel from URL (currently doesn't work)
getexcelfromurl <- function() {
  #create the URL where the dataset is stored with automatic updates every day
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
 
  #download the dataset from the website to a local temporary file
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
  #res <- read_excel(tf)  #read the Dataset sheet into “R”
  res <- read.xlsx(tf, 1)

  # add new columns
  res$date <- 0
  res$aggcases <- 0
  res$aggdeaths <- 0
  res$region <- "na"
  return(res)
}


# add the aggregated cases and deaths for a single country
# by summation on date sorted data 
sumcountry <- function(name) {
  country <- as.character(name)
  print(paste("name: ", name))
  cdata <- subset(res, res$Countries.and.territories == country)
  scdata <- cdata[order(cdata$DateRep),]

  sumcases <- 0
  sumdeaths <- 0
  region <- subset(regres, regres$Countries.and.territories == country)
  regiontext = as.character(region$region)
  for (row in 1:nrow(scdata)) {
    date <- scdata[row, "DateRep"]
    cases <- scdata[row, "Cases"]
    deaths <- scdata[row, "Deaths"]
    #print(paste("date: ", date, " cases: ", cases, " deaths: ", deaths))
    sumcases <- sumcases + cases
    sumdeaths <- sumdeaths + deaths
    scdata[row, "date"] <- as.character(date, format="%Y%m%d")
    scdata[row, "region"] <- regiontext
    scdata[row, "aggcases"] <- sumcases
    scdata[row, "aggdeaths"] <- sumdeaths
  } # end loop dates
  #print(paste("added ", nrow(scdata), " rows"))
  return (scdata)
}


# Select columns relevant for Gapminder
trimdata <- function(df) {
  return(subset(df, select=c('Countries.and.territories', 'date', 'aggcases', 'aggdeaths', 
                             'Cases', 'Deaths', 'region')))
}


# CLEAN UP #################################################
cleanup <- function() {
  # Clear environment
  rm(list = ls())
  
  # Clear console
  cat("\014")  # ctrl+L
}

#
# #
#

res <- getexcelfromurl()
#res <- getexcelfromfile()

#str read the region database xlsx
regres <- read.xlsx("region_names.xlsx", 1)

#Get list of countries from data frame
countries <- unique(res %>% select(Countries.and.territories))

# uncomment if you want to generate new country list
#country_file = file.choose(new = TRUE)
#write.xlsx(countries, country_file, sheetName="countries")

total <- data.frame(DateRep=integer(), Day=integer(), Month=integer(), Year=integer(),
                 Cases=integer(), Deaths=integer(), Countries.and.territories=factor(),
                 GeoId=factor(), date=integer(), aggcases=integer(),
                 aggdeaths=integer(), region=character(), stringsAsFactors=FALSE)

# Loop through countries
for (row in 1:nrow(countries)) {
  total = rbind(total, sumcountry(countries[row, "Countries.and.territories"]))
}

final <- trimdata(total)

# write data to exel, for gapminder
write.xlsx(final, "gapminder.xlsx", sheetName="countries", row.names=FALSE)

cleanup()
