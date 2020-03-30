
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rJava, xlsx, httr, dplyr, ggplot2, REAT)


# Read the covid excel sheet from file
getexcelfromfile <- function() {
  file = file.choose(new = FALSE)
  res <- read.xlsx(file, 1)
  # add new columns
  res$date <- 0
  res$dayssince100 <- 0
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
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tempfile <- tempfile(fileext = ".xlsx")))
  if (file.info(tempfile)$size < 50000) {
    stop("EXCEL file not available, try again later.")
  }

  #read the Dataset sheet into R
  res <- read.xlsx(tempfile, 1)
  # add new columns
  res$date <- 0
  res$dayssince100 <- 0
  res$aggcases <- 0
  res$aggdeaths <- 0
  res$region <- "na"
  return(res)
}


# add the aggregated cases and deaths for a single country
# by summation on date sorted data 
sumcountry <- function(name) {
  init <- FALSE
  country <- as.character(name)
  #print(paste("name: ", name))
  cdata <- subset(res, res$countriesAndTerritories == country)
  scdata <- cdata[order(cdata$dateRep),]

  date0 <- scdata$dateRep[1]
  sumcases <- 0
  sumdeaths <- 0
  days100 <- 0
  region <- subset(regres, regres$countriesAndTerritories == country)
  if (nrow(region) == 0) {
    region <- subset(regres, regres$countriesAndTerritories == "UNNAMED")
    print(paste("country:", country, " is not in region DB"))
  }
  regiontext = as.character(region$region)
  for (row in 1:nrow(scdata)) {
    date <- scdata[row, "dateRep"]
    totdays <- as.integer(date - date0)
    #print(paste("date: ", date, " days: ", totdays))
    cases <- scdata[row, "cases"]
    deaths <- scdata[row, "deaths"]
    #print(paste("date: ", date, " cases: ", cases, " deaths: ", deaths))
    sumcases <- sumcases + cases
    if (isFALSE(init) && as.numeric(sumcases) >= 100) {
      init <- TRUE
      d100 <- date
      #print(paste("100 reached on ", date))
    }
    if (isTRUE(init)) {
      scdata[row, "dayssince100"] <- as.integer(date - d100)
      sumdeaths <- sumdeaths + deaths
      scdata[row, "date"] <- as.character(date, format="%Y%m%d")
      scdata[row, "days"] <- totdays
      scdata[row, "region"] <- regiontext
      scdata[row, "aggcases"] <- sumcases
      scdata[row, "aggdeaths"] <- sumdeaths
    }
  } # end loop dates
  if (isTRUE(init)) {
    return (scdata)
  } else {
    return (FALSE)
  }
}


# Select columns relevant for Gapminder
trimdata <- function(df) {
  return(subset(df, select=c('countriesAndTerritories', 'date', 'dayssince100', 'aggcases', 'aggdeaths',
                             'cases', 'deaths', 'region')))
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

#readline(prompt="Loaded functions. Press [enter] to continue")

res <- getexcelfromurl()


#str read the region database xlsx
regres <- read.xlsx("region_names.xlsx", 1)

#Get list of countries from data frame
countries <- unique(res %>% select(countriesAndTerritories))

# uncomment if you want to generate new country list
country_file = "untitled.xlsx"
write.xlsx(countries, country_file, sheetName="countries")

total <- data.frame(dateRep=integer(), day=integer(), month=integer(), year=integer(),
                 cases=integer(), deaths=integer(), countriesAndTerritories=factor(),
                 geoId=factor(), date=integer(), aggcases=integer(),
                 aggdeaths=integer(), dayssince100=integer(), region=character(), stringsAsFactors=FALSE)

# Loop through countries
for (row in 1:nrow(countries)) {
  result <- sumcountry(countries[row, "countriesAndTerritories"])
  if (isFALSE(result) == FALSE) {
    total = rbind(total, result)
  }
}

final <- trimdata(subset(total, total$aggcases >= 100))

# write data to exel, for gapminder
write.xlsx(final, "gapminder.xlsx", sheetName="countries", row.names=FALSE)

cleanup()
