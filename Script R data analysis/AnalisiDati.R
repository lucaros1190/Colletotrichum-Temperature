
# Data analysis radial growth Colletotrichum Isolates
# Created by Luca Rossini on 28 November 2023
# Last update 29 November 2023
# E-mail: luca.rossini@unitus.it


# Acquisition of the data - File 'Complete dataset + Results.xlsx'

library(readxl)

filename <- file.choose()
data_radius = read_excel(filename, sheet = 'AnalysisR_Mycelium', 
                         col_names = T)

head(data_radius)

temperature <- as.factor(data_radius$Temperature)
isolate <- as.factor(data_radius$Isolate)
dish <- as.factor(data_radius$Dish)
direction <- as.factor(data_radius$OrthDirecton)
weeklyRadius <- as.numeric(data_radius$WeeklyRadius)

  # Check the levels

levels(temperature)
levels(isolate)
levels(dish)
levels(direction)

  # Create sub-datasets for each temperature for a deeper analysis of the 
  # isolates

dataset_T5 <- data_radius[data_radius$Temperature == "5", ][-1]
dataset_T10 <- data_radius[data_radius$Temperature == "10", ][-1]
dataset_T15 <- data_radius[data_radius$Temperature == "15", ][-1]
dataset_T20 <- data_radius[data_radius$Temperature == "20", ][-1]
dataset_T25 <- data_radius[data_radius$Temperature == "25", ][-1]
dataset_T30 <- data_radius[data_radius$Temperature == "30", ][-1]
dataset_T35 <- data_radius[data_radius$Temperature == "35", ][-1]


# LM - Whole dataset: 'weeklyRadius' variable

    # Check if the dataset needs transformation

library(lattice)
qqmath(weeklyRadius)

    # The answer is yes, so data should be transformed!

library(bestNormalize)
weeklyRadius_Trans <- bestNormalize(weeklyRadius) 
weeklyRadius_Trans <- weeklyRadius_Trans$x.t

library(lme4)

GenLin_weeklyRadius <- lmer(weeklyRadius_Trans ~ temperature + isolate + 
                              (1 | dish) + (1 | direction) , data=data_radius)

summary(GenLin_weeklyRadius)

    # Check the dispersion and the model reliability

library(DHARMa)

testDispersion(GenLin_weeklyRadius)
Output_weeklyRadius <- simulateResiduals(fittedModel = GenLin_weeklyRadius, 
                                         plot = T)

library(lattice)
qqmath(GenLin_weeklyRadius)

    # Pairwise comparison - Temperature + isolate:

library(multcompView)
library(emmeans)

marginal_weeklyRadius = emmeans(GenLin_weeklyRadius, ~ temperature + isolate)
pairs(marginal_weeklyRadius, adjust="bonferroni")

    # Letters of significance:

library(multcomp)

lettere_weeklyRadius <- cld(marginal_weeklyRadius, alpha=0.05, 
                            Letters=letters, adjust="bonferroni")
lettere_weeklyRadius

    # Pairwise comparison - temperature:

marginal_weeklyRadius_temperature = emmeans(GenLin_weeklyRadius, ~ temperature)
pairs(marginal_weeklyRadius_temperature, adjust="bonferroni")

    # Letters of significance:

lettere_weeklyRadius_temperature <- cld(marginal_weeklyRadius_temperature, alpha=0.05, 
                                    Letters=letters, adjust="bonferroni")
lettere_weeklyRadius_temperature

    # Pairwise comparison - isolate:

marginal_weeklyRadius_isolate = emmeans(GenLin_weeklyRadius, ~ isolate)
pairs(marginal_weeklyRadius_isolate, adjust="bonferroni")

    # Letters of significance:

lettere_weeklyRadius_isolate <- cld(marginal_weeklyRadius_isolate, alpha=0.05, 
                                      Letters=letters, adjust="bonferroni")
lettere_weeklyRadius_isolate




# LM - Only T5 to check differences among the isolates

    # Check if the dataset needs transformation

qqmath(dataset_T5$WeeklyRadius)

    # The answer is yes, so data should be transformed!

weeklyRadius_Trans_T5 <- bestNormalize(dataset_T5$WeeklyRadius) 
weeklyRadius_Trans_T5 <- weeklyRadius_Trans_T5$x.t

GenLin_T5 <- lmer(weeklyRadius_Trans_T5 ~ dataset_T5$Isolate + 
                    (1 | dataset_T5$Dish) + (1 | dataset_T5$OrthDirecton), 
                    data=dataset_T5)

summary(GenLin_T5)

    # Check the dispersion and the model reliability

testDispersion(GenLin_T5)
Output_weeklyRadius_T5 <- simulateResiduals(fittedModel = GenLin_T5, plot = T)

qqmath(GenLin_T5)

    # Pairwise comparison - Isolate:

marginal_T5 = emmeans(GenLin_T5, ~ "Isolate")
pairs(marginal_T5, adjust="bonferroni")

    # Letters of significance:

lettere_T5 <- cld(marginal_T5, alpha=0.05, 
                            Letters=letters, adjust="bonferroni")
lettere_T5



  # LM - Only T10 to check differences among the isolates

    # Check if the dataset needs transformation

qqmath(dataset_T10$WeeklyRadius)

    # The answer is yes, so data should be transformed!

weeklyRadius_Trans_T10 <- bestNormalize(dataset_T10$WeeklyRadius) 
weeklyRadius_Trans_T10 <- weeklyRadius_Trans_T10$x.t

GenLin_T10 <- lmer(weeklyRadius_Trans_T10 ~ dataset_T10$Isolate + 
                    (1 | dataset_T10$Dish), 
                  data=dataset_T10)

summary(GenLin_T10)

    # Check the dispersion and the model reliability

testDispersion(GenLin_T10)
Output_weeklyRadius_T10 <- simulateResiduals(fittedModel = GenLin_T10, plot = T)

qqmath(GenLin_T10)

    # Pairwise comparison - Isolate:

marginal_T10 = emmeans(GenLin_T10, ~ "Isolate")
pairs(marginal_T10, adjust="bonferroni")

    # Letters of significance:

lettere_T10 <- cld(marginal_T10, alpha=0.05, 
                  Letters=letters, adjust="bonferroni")
lettere_T10




  # LM - Only T15 to check differences among the isolates

    # Check if the dataset needs transformation

qqmath(dataset_T15$WeeklyRadius)

    # The answer is yes, so data should be transformed!

weeklyRadius_Trans_T15 <- bestNormalize(dataset_T15$WeeklyRadius) 
weeklyRadius_Trans_T15 <- weeklyRadius_Trans_T15$x.t

GenLin_T15 <- lmer(weeklyRadius_Trans_T15 ~ dataset_T15$Isolate + 
                     (1 | dataset_T15$Dish), 
                     data=dataset_T15)

summary(GenLin_T15)

    # Check the dispersion and the model reliability

testDispersion(GenLin_T15)
Output_weeklyRadius_T15 <- simulateResiduals(fittedModel = GenLin_T15, plot = T)

qqmath(GenLin_T15)

    # Pairwise comparison - Isolate:

marginal_T15 = emmeans(GenLin_T15, ~ "Isolate")
pairs(marginal_T15, adjust="bonferroni")

    # Letters of significance:

lettere_T15 <- cld(marginal_T15, alpha=0.05, 
                   Letters=letters, adjust="bonferroni")
lettere_T15




  # LM - Only T20 to check differences among the isolates

    # Check if the dataset needs transformation

qqmath(dataset_T20$WeeklyRadius)

    # The answer is yes, so data should be transformed!

weeklyRadius_Trans_T20 <- bestNormalize(dataset_T20$WeeklyRadius) 
weeklyRadius_Trans_T20 <- weeklyRadius_Trans_T20$x.t

GenLin_T20 <- lmer(weeklyRadius_Trans_T20 ~ dataset_T20$Isolate + 
                     (1 | dataset_T20$Dish) + (1 | dataset_T20$OrthDirecton), 
                     data=dataset_T20)

summary(GenLin_T20)

    # Check the dispersion and the model reliability

testDispersion(GenLin_T20)
Output_weeklyRadius_T20 <- simulateResiduals(fittedModel = GenLin_T20, plot = T)

qqmath(GenLin_T20)

    # Pairwise comparison - Isolate:

marginal_T20 = emmeans(GenLin_T20, ~ "Isolate")
pairs(marginal_T20, adjust="bonferroni")

    # Letters of significance:

lettere_T20 <- cld(marginal_T20, alpha=0.05, 
                   Letters=letters, adjust="bonferroni")
lettere_T20




# LM - Only T25 to check differences among the isolates

    # Check if the dataset needs transformation

qqmath(dataset_T25$WeeklyRadius)

    # The answer is yes, so data should be transformed!

weeklyRadius_Trans_T25 <- bestNormalize(dataset_T25$WeeklyRadius) 
weeklyRadius_Trans_T25 <- weeklyRadius_Trans_T25$x.t

GenLin_T25 <- lmer(weeklyRadius_Trans_T25 ~ dataset_T25$Isolate + 
                     (1 | dataset_T25$Dish), data=dataset_T25)

summary(GenLin_T25)

    # Check the dispersion and the model reliability

testDispersion(GenLin_T25)
Output_weeklyRadius_T25 <- simulateResiduals(fittedModel = GenLin_T25, plot = T)

qqmath(GenLin_T25)

    # Pairwise comparison - Isolate:

marginal_T25 = emmeans(GenLin_T25, ~ "Isolate")
pairs(marginal_T25, adjust="bonferroni")

    # Letters of significance:

lettere_T25 <- cld(marginal_T25, alpha=0.05, 
                   Letters=letters, adjust="bonferroni")
lettere_T25




  # LM - Only T30 to check differences among the isolates

    # Check if the dataset needs transformation

qqmath(dataset_T30$WeeklyRadius)

    # The answer is yes, so data should be transformed!

weeklyRadius_Trans_T30 <- bestNormalize(dataset_T30$WeeklyRadius) 
weeklyRadius_Trans_T30 <- weeklyRadius_Trans_T30$x.t

GenLin_T30 <- lmer(weeklyRadius_Trans_T30 ~ dataset_T30$Isolate + 
                     (1 | dataset_T30$Dish), data=dataset_T30)

summary(GenLin_T30)

    # Check the dispersion and the model reliability

testDispersion(GenLin_T30)
Output_weeklyRadius_T30 <- simulateResiduals(fittedModel = GenLin_T30, plot = T)

qqmath(GenLin_T30)

    # Pairwise comparison - Isolate:

marginal_T30 = emmeans(GenLin_T30, ~ "Isolate")
pairs(marginal_T30, adjust="bonferroni")

    # Letters of significance:

lettere_T30 <- cld(marginal_T30, alpha=0.05, 
                   Letters=letters, adjust="bonferroni")
lettere_T30




  # LM - Only T35 to check differences among the isolates

    # Check if the dataset needs transformation

qqmath(dataset_T35$WeeklyRadius)

    # The answer is yes, so data should be transformed!

weeklyRadius_Trans_T35 <- bestNormalize(dataset_T35$WeeklyRadius) 
weeklyRadius_Trans_T35 <- weeklyRadius_Trans_T35$x.t

GenLin_T35 <- lmer(weeklyRadius_Trans_T35 ~ dataset_T35$Isolate + 
                     (1 | dataset_T35$Dish), data=dataset_T35)

summary(GenLin_T35)

    # Check the dispersion and the model reliability

testDispersion(GenLin_T35)
Output_weeklyRadius_T35 <- simulateResiduals(fittedModel = GenLin_T35, plot = T)

qqmath(GenLin_T35)

    # Pairwise comparison - Isolate:

marginal_T35 = emmeans(GenLin_T35, ~ "Isolate")
pairs(marginal_T35, adjust="bonferroni")

    # Letters of significance:

lettere_T35 <- cld(marginal_T35, alpha=0.05, 
                   Letters=letters, adjust="bonferroni")
lettere_T35




# Detailed plots - Isolate growth per substrate

library(ggplot2)


    # Temperature T5

boxPlotSub_T5 <- ggplot(dataset_T5, aes(x=Isolate, 
                                        y=WeeklyRadius, 
                                        fill=Isolate)) + 
                          geom_boxplot(width=0.7) + 
                          xlab("Isolate") + 
                          ylab("Radius lenght (mm)") + 
                          ggtitle("Temperature 5 °C") +
                          theme(plot.title = element_text(hjust=0.5), 
                                      text = element_text(size=21)) + 
                          theme(legend.position = "none") +
                          scale_x_discrete(labels = c("COL-1","COL-2", "COL-3",
                                                      "COL-4", "COL-6"))

boxPlotSub_T5


    # Temperature T10

boxPlotSub_T10 <- ggplot(dataset_T10, aes(x=Isolate, 
                                          y=WeeklyRadius, 
                                          fill=Isolate)) + 
                          geom_boxplot(width=0.7) + 
                          xlab("Isolate") + 
                          ylab("Radius lenght (mm)") + 
                          ggtitle("Temperature 10 °C") +
                          theme(plot.title = element_text(hjust=0.5), 
                                      text = element_text(size=21)) + 
                          theme(legend.position = "none") +
                          scale_x_discrete(labels = c("COL-1","COL-2", "COL-3",
                                                      "COL-4", "COL-6"))

boxPlotSub_T10


    # Temperature T15

boxPlotSub_T15 <- ggplot(dataset_T15, aes(x=Isolate, 
                                          y=WeeklyRadius, 
                                          fill=Isolate)) + 
                          geom_boxplot(width=0.7) + 
                          xlab("Isolate") + 
                          ylab("Radius lenght (mm)") + 
                          ggtitle("Temperature 15 °C") +
                          theme(plot.title = element_text(hjust=0.5), 
                                      text = element_text(size=21)) + 
                          theme(legend.position = "none") +
                          scale_x_discrete(labels = c("COL-1","COL-2", "COL-3",
                                                      "COL-4", "COL-6"))

boxPlotSub_T15


    # Temperature T20

boxPlotSub_T20 <- ggplot(dataset_T20, aes(x=Isolate, 
                                          y=WeeklyRadius, 
                                          fill=Isolate)) + 
                          geom_boxplot(width=0.7) + 
                          xlab("Isolate") + 
                          ylab("Radius lenght (mm)") + 
                          ggtitle("Temperature 20 °C") +
                          theme(plot.title = element_text(hjust=0.5), 
                                      text = element_text(size=21)) + 
                          theme(legend.position = "none") +
                          scale_x_discrete(labels = c("COL-1","COL-2", "COL-3",
                                                      "COL-4", "COL-6"))

boxPlotSub_T20


    # Temperature T25

boxPlotSub_T25 <- ggplot(dataset_T25, aes(x=Isolate, 
                                          y=WeeklyRadius, 
                                          fill=Isolate)) + 
                          geom_boxplot(width=0.7) + 
                          xlab("Isolate") + 
                          ylab("Radius lenght (mm)") + 
                          ggtitle("Temperature 25 °C") +
                          theme(plot.title = element_text(hjust=0.5), 
                                      text = element_text(size=21)) + 
                          theme(legend.position = "none") +
                          scale_x_discrete(labels = c("COL-1","COL-2", "COL-3",
                                                      "COL-4", "COL-6"))

boxPlotSub_T25


    # Temperature T30

boxPlotSub_T30 <- ggplot(dataset_T30, aes(x=Isolate, 
                                          y=WeeklyRadius, 
                                          fill=Isolate)) + 
                          geom_boxplot(width=0.7) + 
                          xlab("Isolate") + 
                          ylab("Radius lenght (mm)") + 
                          ggtitle("Temperature 30 °C") +
                          theme(plot.title = element_text(hjust=0.5), 
                                      text = element_text(size=21)) + 
                          theme(legend.position = "none") +
                          scale_x_discrete(labels = c("COL-1","COL-2", "COL-3",
                                                      "COL-4", "COL-6"))

boxPlotSub_T30


    # Temperature T35

boxPlotSub_T35 <- ggplot(dataset_T35, aes(x=Isolate, 
                                          y=WeeklyRadius, 
                                          fill=Isolate)) + 
                          geom_boxplot(width=0.7) + 
                          xlab("Isolate") + 
                          ylab("Radius lenght (mm)") + 
                          ggtitle("Temperature 35 °C") +
                          theme(plot.title = element_text(hjust=0.5), 
                                      text = element_text(size=21)) + 
                          theme(legend.position = "none") +
                          scale_x_discrete(labels = c("COL-1","COL-2", "COL-3",
                                                      "COL-4", "COL-6"))

boxPlotSub_T35




    # Making a grid resuming the plots

library(ggpubr)

grid_plot <- ggarrange(boxPlotSub_T5, boxPlotSub_T10, boxPlotSub_T15, 
                       boxPlotSub_T20, boxPlotSub_T25, boxPlotSub_T30, 
                       boxPlotSub_T35, ncol = 3, nrow = 3)

grid_plot

