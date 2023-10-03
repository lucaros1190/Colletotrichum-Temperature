
# R code to plot raw data - Colletotrychum termal growth.
# Created by Luca Rossini on 14 August 2023
# E-mail: luca.rossini@unitus.it
# Last update: 14 August 2023

  # Load libraries

library(ggplot2)
library(ggpubr)


# Dataset acquisition - File 'PlotRawRates-Dataset.csv'


colly <- read.csv2(file.choose(), header = T, sep = ";", dec = ".", 
                   na.string="NA")
head(colly)

  # Array assignment

temperature <- as.factor(colly$Temp)
col1 <- as.numeric(colly$COL1)
col6 <- as.numeric(colly$COL6)
col2 <- as.numeric(colly$COL2)
col3 <- as.numeric(colly$COL3)
col4 <- as.numeric(colly$COL4)


# Single plots

plot_col1 <- ggplot(colly, aes(x=temperature, y=col1)) + 
  ggtitle("COL-1") + ylim(0, 0.45) +
  geom_boxplot(width=0.43) + 
  xlab("Temperature (°C)") + 
  ylab("Growth rate (mm/day)") + 
  theme(plot.title = element_text(hjust=0.5))

plot_col2 <- ggplot(colly, aes(x=temperature, y=col2)) + 
  ggtitle("COL-2") + ylim(0, 0.45) +
  geom_boxplot(width=0.43) + 
  xlab("Temperature (°C)") + 
  ylab("Growth rate (mm/day)") + 
  theme(plot.title = element_text(hjust=0.5))

plot_col3 <- ggplot(colly, aes(x=temperature, y=col3)) + 
  ggtitle("COL-3") + ylim(0, 0.45) +
  geom_boxplot(width=0.43) + 
  xlab("Temperature (°C)") + 
  ylab("Growth rate (mm/day)") + 
  theme(plot.title = element_text(hjust=0.5))

plot_col4 <- ggplot(colly, aes(x=temperature, y=col4)) + 
  ggtitle("COL-4") + ylim(0, 0.45) +
  geom_boxplot(width=0.43) + 
  xlab("Temperature (°C)") + 
  ylab("Growth rate (mm/day)") +
  theme(plot.title = element_text(hjust=0.5))

plot_col6 <- ggplot(colly, aes(x=temperature, y=col6)) + 
  ggtitle("COL-6") + ylim(0, 0.45) +
  geom_boxplot(width=0.43) + 
  xlab("Temperature (°C)") +
  ylab("Growth rate (mm/day)") + 
  theme(plot.title = element_text(hjust=0.5))

# Makes the grid with 9 plots

grid_plot <- ggarrange(plot_col1, plot_col2, plot_col3, plot_col4, plot_col6,
                       ncol = 2, nrow= 3)
grid_plot
