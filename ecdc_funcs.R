# Copyright (c) 2020 Morten Jagd Christensen
# Attribution 4.0 International (CC BY 4.0)

# Read the covid excel sheet from file
getexcelfromfile <- function() {
  file = file.choose(new = FALSE)
  res <- read_excel(file)
  # add new columns
  res$date <- 0
  res$dayssince100 <- 0
  res$aggcases <- 0
  res$aggdeaths <- 0
  res$region <- "na"
  return(res)
}

# Get today's excel file from ECDC site via URL
getexcelfromurl <- function() {
  # create the URL where the dataset is stored with automatic updates every day
  datestr <- format(Sys.time(), "%Y-%m-%d") # or 
  datestr <- "2020-05-26"
  
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/", 
               "COVID-19-geographic-disbtribution-worldwide-",
               datestr, ".xlsx", sep = "")
  
  # download the dataset from the website to a local temporary file
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tempfile <- tempfile(fileext = ".xlsx")))
  
  if (file.info(tempfile)$size < 50000) {
    print("In CET+1 the excel file is available in the afternoon.")
    stop("EXCEL file not available, try again later. ")
  }
  
  # read the Dataset sheet into R, add new columns
  res <- read_excel(tempfile)
  res$date <- 0
  res$dayssince100 <- 0
  res$aggcases <- 0
  res$aggdeaths <- 0
  res$region <- "na"
  return(res)
}


# Add the aggregated cases and deaths for a single country
# by summation of date-sorted data (scdata is short for sorted country data)
sumcountry <- function(scdata, regions, country, mincases) {
  init <- FALSE
  
  date0 <- scdata$dateRep[1]
  sumcases <- 0
  sumdeaths <- 0
  region <- subset(regions, regions$countriesAndTerritories == country)
  if (nrow(region) == 0) {
    region <- subset(regions, regions$countriesAndTerritories == "UNNAMED")
    print(paste("country:", country, " is not in region DB"))
  }
  regiontext = as.character(region$region)
  for (row in 1:nrow(scdata)) {
    date <- scdata$dateRep[row]
    totdays <- as.integer(date - date0)
    #print(paste("country, date, date0, diff", country, date, date0, totdays))
    stopifnot( totdays >= 0)
  
    cases <- scdata[row, "cases"]
    sumcases <- sumcases + cases
    if (isFALSE(init) && as.numeric(sumcases) >= mincases) {
      init <- TRUE
      d100 <- date
    }
    if (isTRUE(init)) {
      scdata[row, "dayssince100"] <- as.integer(date - d100)
      deaths <- scdata[row, "deaths"]
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


# Select only columns relevant for Gapminder
trimdata <- function(df) {
  return(subset(df, select=c('countriesAndTerritories', 'date', 'dayssince100', 'aggcases', 'aggdeaths',
                             'cases', 'deaths', 'region')))
}


# Create a dataset compatible with Gapminder offline tools>
gapminder <- function(data, countries, regions, MinCases) {
  data <- data[order(data$dateRep),] # do sorting by dateRep once
  
  tmpdf <- data.frame(dateRep=integer(), day=integer(), month=integer(), year=integer(),
                      cases=integer(), deaths=integer(), countriesAndTerritories=factor(),
                      geoId=factor(), date=integer(), aggcases=integer(),
                      aggdeaths=integer(), dayssince100=integer(), region=character(), stringsAsFactors=FALSE)
  
  
  # Loop through countries, append to total (cname is short for country name)
  for (row in 1:nrow(countries)) {
    cname = as.character(countries[row, "countriesAndTerritories"])
    countrydata <- subset(data, countriesAndTerritories == cname)
    if (row %% 20 == 0) {
      print(paste("row ", row, " : ", cname))
    }
    result <- sumcountry(countrydata, regions, cname, MinCases)
    if (isFALSE(result) == FALSE) {
      tmpdf = rbind(tmpdf, result)
    }
  }
  
  final <- trimdata(subset(tmpdf, tmpdf$aggcases >= MinCases))
  return (final)
}


# CLEAN UP #################################################
cleanup <- function() {
  # clear plots
  if(!is.null(dev.list())) dev.off()
  
  # Clear environment
  rm(list = ls())
  
  # Clear console
  cat("\014")  # ctrl+L
}