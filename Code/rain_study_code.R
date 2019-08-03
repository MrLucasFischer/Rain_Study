library(tidyverse)
library(caret)
library(keras)

data = read_csv("../input/weatherAUS.csv")
dim(data)

data = data %>% 
    mutate_at(vars(Location, WindGustDir, WindDir9am, WindDir3pm, RainToday, RainTomorrow), as.factor)

data %>%
    summarise_each(list(~ sum(is.na(.)) / length(.) * 100)) %>%
    t()

data = data %>% select(-Evaporation, -Sunshine, -Cloud9am, -Cloud3pm)

data %>%
    group_by(RainTomorrow) %>%
    summarise_each(list(~ sum(is.na(.)) / length(.) * 100)) %>%
    t()

data = data %>% na.exclude()
dim(data)

data %>%
    ggplot(aes(x = RainToday, fill = RainTomorrow, color = RainTomorrow)) +
    geom_bar(aes(y = ((..count..) / sum(..count..))), position = "identity", alpha = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05),
                       labels = scales::percent) +
    labs(x = "Rained Today",
         y = "Percentage",
         title = "Bar plot of the distribution of the RainToday variable, colorized depending if it rained the next day",
         color = "Rained the next day",
         fill = "Rained the next day"
    )


data %>%
    filter(Rainfall > 3 & Rainfall < 50) %>% 
    ggplot(aes(x = Rainfall, fill = RainTomorrow, color = RainTomorrow)) +
    geom_histogram(aes(y = ((..count..) / sum(..count..))), position = "identity", alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 50, by = 5)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.02),
                       labels = scales::percent) +
    labs(x = "Amount of RainFall (mm)",
         y = "Percentage",
         title = "Distribution of the amount of Rainfall (between 3 and 50 mm)")

data %>%
    filter(Rainfall > 50 & Rainfall < 100) %>% 
    ggplot(aes(x = Rainfall, fill = RainTomorrow, color = RainTomorrow)) +
    geom_histogram(aes(y = ((..count..) / sum(..count..))), position = "identity", alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 100, by = 5)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.02),
                       labels = scales::percent) +
    labs(x = "Amount of RainFall (mm)",
         y = "Percentage",
         title = "Distribution of the amount of Rainfall (between 50 and 100 mm)")

data %>%
    filter(Rainfall > 1 & Rainfall < 100) %>% 
    ggplot(aes(x = MaxTemp, y = Rainfall, color = RainTomorrow)) +
    scale_x_continuous(breaks = seq(0, 60, by = 2)) +
    scale_y_continuous(breaks = seq(0, 100, by = 5)) +
    geom_point(alpha = 0.8) +
    labs(x = "Max Temperature",
         y = "Amount of Rainfall (mm)",
         title = "Max Temperature vs Amount of Rainfall")