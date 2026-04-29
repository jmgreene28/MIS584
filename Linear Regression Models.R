packagesDF <- c("dplyr","readr","tidyverse","tidytext","tidymodels","quanteda","SnowballC","rmarkdown","knitr","ggthemes")

install.packages(packagesDF)
library(dplyr)
library(readr)
library(tidyverse)
library(tidytext)
library(tidymodels)
library(quanteda)
library(SnowballC)
library(rmarkdown)
library(knitr)
  
LondonCyclingDF <- read.csv("C:/Users/jmgre/OneDrive/Documents/Grad School/MIS584/Group Presentation/london_merged.csv", header = TRUE)

head(LondonCyclingDF)

model_t1 <- lm(cnt ~ t1, LondonCyclingDF)
model_t2 <- lm(cnt ~ t2, LondonCyclingDF)
model_hum <- lm(cnt ~ hum, LondonCyclingDF)
model_ws <- lm(cnt ~ wind_speed, LondonCyclingDF)
model_sea <- lm(cnt ~ season, LondonCyclingDF)
model_lm <- lm(cnt ~ t1 + hum + wind_speed + is_weekend + season, data = LondonCyclingDF)
summary(model_t1)
summary(model_t2)
summary(model_hum)
summary(model_ws)
summary(model_sea)
summary(model_lm)
plot(model_t1)
plot(model_t2)
plot(model_hum)
plot(model_ws)
plot(model_lm)

ggplot(LondonCyclingDF, aes(x = t1, y = cnt)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(
    title = "Hourly Bike Rentals vs. Temperature",
    x = "Temerature (C)",
    y = "Hourly Rentals"
  )

ggplot(LondonCyclingDF, aes(x = hum, y = cnt)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(
    title = "Hourly Bike Rentals vs. Temperature",
    x = "Humidity %",
    y = "Hourly Rentals"
  )
