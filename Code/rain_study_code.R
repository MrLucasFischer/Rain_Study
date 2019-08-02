library(tidyverse)

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