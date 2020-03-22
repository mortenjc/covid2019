
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rJava, xlsx, dplyr) 

# Read the covid excel sheet
file = file.choose(new = FALSE)
res <- read.xlsx(file, 1)
# add new columns
res$date <- 0
res$aggcases <- 0
res$aggdeaths <- 0
res$region <- "na"

# read the region database xlsx
regres <- read.xlsx("names.xlsx", 1)

#Get list of countries from data frame
countries <- unique(res %>% select(Countries.and.territories))

# uncomment if you want to generate new country list
#country_file = file.choose(new = TRUE)
#write.xlsx(countries, country_file, sheetName="countries")



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
    scdata[row, "region"] <- regiontext
    scdata[row, "aggcases"] <- sumcases
    scdata[row, "aggdeaths"] <- sumdeaths
  } # end loop dates
  print(paste("added ", nrow(scdata), " rows"))
  return (scdata)
}


total <- sumcountry("Guernsey")
tail(total)

# Loop through countries
for (row in 1:nrow(countries)) {
  country <- countries[row, "Countries.and.territories"]
  tmp <- sumcountry(country)
  total = rbind(total, tmp)
}

# write data to exel, for gapminder
write.xlsx(total, "gap.xlsx", sheetName="countries")


# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)

