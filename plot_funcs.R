# Copyright (c) 2020 Morten Jagd Christensen
# Attribution 4.0 International (CC BY 4.0)

ReferenceCountries <- c("United_States_of_America", "Italy", "Spain", 
                        "United_Kingdom", "South_Korea", "Japan", 
                        "Denmark", "Germany", "China")
CountryColor <- "#202020"
BgColor <- "#FFF2E6"
BgLineColor <- "#BEB9AF"
GridColor <- "#EDDDD0"
LblColor <- "#59544D"
SelColor <- "#ff2020"

# Return a ggplot2 plot
subplot <- function(df, yval, ylbl, ymin, ymax, name, nameList) {
  ss <- subset(df, countriesAndTerritories %in% nameList & 
                 aggcases >= 100 & 
                 dayssince100 < 50)
  ss2 <- subset(df, countriesAndTerritories == name & 
                 aggcases >= 100 & 
                 dayssince100 < 50)

  ggp <- ggplot(ss, aes(x=dayssince100, y=get(yval), group=countriesAndTerritories, 
                        color=countriesAndTerritories)) + 
         theme(plot.background = element_rect(fill=BgColor)) +
         theme(panel.background = element_rect(fill=BgColor, color=GridColor)) +
         theme(
          panel.grid.major = element_line(color=GridColor),
          panel.grid.minor = element_line(color=GridColor)
         ) +
         geom_line(size=0.8) +
         geom_line(data=ss2, aes(x=dayssince100, y=get(yval), size = 0.2), color=LblColor, size=1.2) +
         geom_text(data = subset(ss, dayssince100 == "20"), 
                   aes(label=countriesAndTerritories), 
                   color=LblColor, size=3) +
         #theme(legend.position = c(0.2, 0.75)) +
         theme(legend.position = "none") +
         scale_y_continuous(limit = c(ymin, ymax), trans = "log10", labels = comma) +
         labs(color=LblColor, x = "Days since 100 cases", y = ylbl) +
         theme(legend.background =element_rect(fill = NA))
         
  
         
         #ggtitle("Aggregated Cases")
 
  return (ggp)
}

singleplot <- function(Data, RefCountries, YMin, YMax) {
  ss <- subset(Data, aggcases >= 3 & dayssince100 < 1000)
  RefCnt <- subset(ss, countriesAndTerritories %in% RefCountries)
  agg <- aggregate(dayssince100 ~ countriesAndTerritories, RefCnt, max)
  max <- merge(agg, RefCnt)
  
  ggplot(Data, aes(x=dayssince100, y=aggcases, group=countriesAndTerritories , color="#85807B")) +
         theme(legend.position = "none") +
         theme(plot.background = element_rect(fill=BgColor)) +
         theme(panel.background = element_rect(fill=BgColor, color=GridColor)) +
         theme(panel.grid.major = element_line(color=GridColor),
               panel.grid.minor = element_line(color=GridColor)) +
         annotate("text", x=10, y=200000, label="jCAPS Corporation", 
                  color="#CFC5B9", size=6) +
         geom_line(color=BgLineColor, size=0.5) +
         geom_line(data=RefCnt, 
                   aes(x=dayssince100, y=aggcases, color=countriesAndTerritories),
                   size=1.0) +
         geom_point(data=max, 
                    aes(x=dayssince100, y=aggcases, color=countriesAndTerritories),
                    stroke = 0.5) +
         geom_text(data=max, aes(label=countriesAndTerritories), 
                   color=LblColor, size=3, 
                   hjust = 0, nudge_x = 0.5) +
      

         scale_y_continuous(limit = c(YMin, YMax), trans = "log10", labels = comma) 
}

# Arrange four plots in a grid
multiplot <- function(total, yval, ylbl, ymin, ymax) {
  p1 <- subplot(total, yval, ylbl, ymin, ymax, "Italy", 
                c("Italy", "Spain", "Germany", "France", "China"))
  p2 <- subplot(total, yval, ylbl, ymin, ymax, "Japan",
                c("Japan", "South_Korea", "Iran", "China"))
  p3 <- subplot(total, yval, ylbl, ymin, ymax, "Denmark", 
                c("Denmark", "Sweden", "Norway", "Finland", "China"))
  p4 <- subplot(total, yval, ylbl, ymin, ymax, "United_States_of_America",
                c("United_States_of_America", "Canada", "Australia", "United_Kingdom", "China"))
  plot_grid(p1, p2, p3, p4)
}


