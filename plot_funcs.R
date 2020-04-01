# Copyright (c) 2020 Morten Jagd Christensen
# Attribution 4.0 International (CC BY 4.0)

# Return a ggplot2 plot
subplot <- function(df, yval, ylbl, ymin, ymax, nameList) {
  ss <- subset(df, countriesAndTerritories %in% nameList & 
                 aggcases >= 100 & 
                 dayssince100 < 45)

  ggp <- ggplot(ss, aes(x=dayssince100, y=get(yval), color=countriesAndTerritories)) + 
         theme(plot.background = element_rect(fill="#fff1e5")) +
         theme(panel.background = element_rect(fill="#fff1e5", color="#c0c0c0")) +
         theme(
          panel.grid.major = element_line(color="#c0c0c0"),
          panel.grid.minor = element_line(color="#c0c0c0")
         ) +
         geom_line() +
         theme(legend.position = c(0.7, 0.3)) +
         scale_y_continuous(limit = c(ymin, ymax), trans = "log10", labels = comma) +
         labs(color="", x = "Days since 100 cases", y = ylbl) +
         theme(legend.background =element_rect(fill = NA)) #+
         #ggtitle("Aggregated Cases")
 
  return (ggp)
}

# Arrange four plots in a grid
multiplot <- function(total, yval, ylbl, ymin, ymax) {
  p1 <- subplot(total, yval, ylbl, ymin, ymax, 
                c("Italy", "Spain", "Germany", "France", "China"))
  p2 <- subplot(total, yval, ylbl, ymin, ymax, 
                c("Japan", "South_Korea", "Iran", "China"))
  p3 <- subplot(total, yval, ylbl, ymin, ymax, 
                c("Denmark", "Sweden", "Norway", "Finland", "China"))
  p4 <- subplot(total, yval, ylbl, ymin, ymax, 
                c("United_States_of_America", "Canada", "Australia", "United_Kingdom", "China"))
  plot_grid(p1, p2, p3, p4)
}
