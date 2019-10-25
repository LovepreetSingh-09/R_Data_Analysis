library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)
library(splines)
library(gapminder)

gapminder %>% print(n=20)
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

gapminder %>%filter(year==2007) %>%
  group_by(country) %>%
  summarize(mean=mean(lifeExp)) %>% arrange(mean)%>%
  print(n=Inf)

?dplyr::arrange
gapminder %>%filter(year==2007) %>%
  group_by(country) %>%
  summarize(mean=mean(lifeExp),gdp=gdpPercap) %>% arrange(desc(gdp))%>%
  print(n=Inf)

nz <- filter(gapminder, country == "New Zealand")
nz
nz %>%
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend + ")

nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")

# Nested data
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
by_country

by_country$data[[1]]
