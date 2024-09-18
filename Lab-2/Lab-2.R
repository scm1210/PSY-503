## ----setup, include=FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, cache = FALSE, fig.path = "lab_2_plots/")
options(scipen=999)
set.seed(123)


## ----load-packages---------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") #run this if you don't have pacman 
library(pacman)
pacman::p_load(tidyverse, tinytex,ggdist,gghalves,knitr,install = T) 


## --------------------------------------------------------------------------------------------------------
compute_SS <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sum_squared_diff <- sum((x - mean_x)^2)
  return(sum_squared_diff)
}

vector_a <- c(5, 5, 5, 5, 5)
vector_b <- c(1, 3, 5, 7, 9)

print(paste("Sum of squares for vector a:", compute_SS(vector_a)))
print(paste("Sum of squares for vector b:", compute_SS(vector_b)))



## ----load datasauRs--------------------------------------------------------------------------------------
library(datasauRus)
lab2_datasaurus<- datasaurus_dozen

#define plot aesthetic
plot_aes = theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Futura Medium"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 1.5),
        plot.caption = element_text(hjust = 1, size = 10),
        legend.text = element_text(size = 7, family = "Futura Medium")  # Adjust legend text here
  )



## ----fig.height=8, fig.width=8---------------------------------------------------------------------------
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset)) +
  geom_point() +
  facet_wrap(~dataset) +
  plot_aes


## ----fig.height=8, fig.width=8---------------------------------------------------------------------------
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset)) +
  geom_point() +
  facet_wrap(~dataset) +
  plot_aes +
  scale_colour_grey() ### adding grey-scale after we use the aesthetic object to override the colors


## ----fig.height=8, fig.width=8---------------------------------------------------------------------------
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset, shape = dataset)) + 
  geom_point() +
  facet_wrap(~dataset) +
  plot_aes + 
  scale_shape_manual(values = c(0:13)) +  # Specify more shapes manually
  scale_colour_grey()


## --------------------------------------------------------------------------------------------------------
ggplot(lab2_datasaurus, aes(x = dataset, y = x, color = dataset)) +
  geom_boxplot() +
  geom_half_violin(side = "l", alpha = 0.5) +  
  geom_jitter(width = 0.2, height = 0, alpha = 0.2) +  # Adding jitter with some transparency
  plot_aes




## --------------------------------------------------------------------------------------------------------
summary_stats <- lab2_datasaurus %>%
  summarise(
    mean_x = mean(x, na.rm = TRUE),
    mean_y = mean(y, na.rm = TRUE),
    median_x = median(x, na.rm = TRUE),
    median_y = median(y, na.rm = TRUE),
    variance_x = var(x, na.rm = TRUE),
    variance_y = var(y, na.rm = TRUE),
    correlation_xy = cor(x, y, use = "complete.obs")
  )

# Print the dataframe using kable
kable(summary_stats, caption = "Summary Statistics for x and y")



## ----fig.height=8, fig.width=8---------------------------------------------------------------------------
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset)) + 
  geom_point() +
  facet_wrap(~dataset) +
  geom_point(aes(x = mean(x), y = mean(y)), size = 5, shape = 1, color = "red", alpha = 0.5) +  # Overlaying the mean of x and y
  plot_aes 



## --------------------------------------------------------------------------------------------------------
knitr::purl("Lab-2.Rmd")

