#box plot for respiration data


library(tidyverse)
library(ggplot2)

library(readxl)
rep_rates_7_17_25 <- read_excel("rep_rates_7.17.25.xlsx")
View(rep_rates_7_17_25)

rep_rates_7_17_25_reordered <- rep_rates_7_17_25 [, c(2,1,4,3)]

# Select only the columns you want for the boxplot
df_long <- stack(rep_rates_7_17_25_reordered[, c('storm_control', 'storm_exposed', 'n_temp_control', 'n_temp_exposed')])

# The resulting df_long has:
#   - values: all the numbers
#   - ind:    group (column) name


  
  
 resp_boxplot <- ggplot(df_long, aes(x = ind, y = values, fill = ind)) +
    geom_boxplot(outlier.shape = 1) +
    scale_x_discrete(labels = c("Control", "Exposed", "Control", "Exposed")) +
    scale_fill_manual(values = c('orange', 'chocolate3', 'orange', 'chocolate3')) +
    labs(
      y = expression(O[2]*" Consumption (pmol/min per individual)"),
      x = NULL   # This removes the "ind" label
    ) +
    theme_classic() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      #panel.border = element_rect(color = "black", fill = NA, size = 1)
    ) +
    theme(
      legend.position = "none",    # Hides the legend on the right
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 18),
      plot.title = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold")# Ensures the x-axis title stays blank
    ) +
    annotate(
      "text", x = 1.5, y = - 0.1, label = "E. brachionus Storm", size = 5, fontface = "bold.italic"
    ) +
    annotate(
      "text", x = 3.5, y = - 0.1, label = "E. chihuahuaensis N Temp", size = 5, fontface = "bold.italic"
    )
 print(resp_boxplot)
  #for saving at 300 DPI
 # ggsave("respiration_box_plot.png", plot = resp_boxplot, dpi = 300)