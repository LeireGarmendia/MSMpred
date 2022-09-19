common_theme <- theme_bw() +
  theme(panel.border = element_rect(fill = "transparent", color = "white"),
      plot.title = element_text(size = 16, face="bold"),
      axis.text = element_text(size = 14, face = 'bold'),
      axis.title.x = element_text(size = 14, face = 'bold'),
      axis.title.y = element_text(size = 14, face = 'bold'),
      legend.title = element_text(size = 14, face="bold"),
      legend.text = element_text(size = 14, face = 'bold'))

#Maximo HR representable en la tabla y el forest plot
max_HR <- 10^5