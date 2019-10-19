library(tidyverse)
library(lubridate)
library(nycflights13)
library(magrittr)

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()

# %T>% returns the lefthand side object of it instead of righthandsize to the next upcming object
rnorm(100) %>%
  matrix(ncol=2)%T>%
  plot() %>% 
  str()
  
mtcars %$%
  cor(disp, mpg)


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
range(2:20)
x <- c(1:10, Inf)
rescale01(x)

# Here finite=TRUE removes the values with Inf
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

# load --------------------------------------------------------------------
identical(0L, 0)

