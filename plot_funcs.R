# Copyright (c) 2020 Morten Jagd Christensen
# Attribution 4.0 International (CC BY 4.0)

# Return a ggplot2 plot
subplot <- function(df, nameList) {
  ss <- subset(df, countriesAndTerritories %in% nameList & 
                 aggcases >= 100 & 
                 dayssince100 < 45)
  ggp <- ggplot(ss, aes(x=dayssince100, y=aggcases, color=countriesAndTerritories)) + 
         geom_line() +
         theme(legend.position = c(0.7, 0.3)) +
         scale_y_continuous(limit = c(100, 500000), trans = "log10", labels = comma) +
         labs(color="", x = "Days since 100 cases", y = "Total Cases") +
         theme(legend.background =element_rect(fill = NA)) #+
         #ggtitle("Aggregated Cases")
  return (ggp)
}

multiplot <- function(total) {
  p1 <- subplot(total, c("Italy", "Spain", "Germany", "France", "China"))
  p2 <- subplot(total, c("Japan", "South_Korea", "Iran", "China"))
  p3 <- subplot(total, c("Denmark", "Sweden", "Norway", "Finland", "China"))
  p4 <- subplot(total, c("United_States_of_America", "Canada", "Australia", "United_Kingdom", "China"))
  plot_grid(p1, p2, p3, p4)
}