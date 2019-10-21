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
object.size(mtcars)

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(x)
}
wt_mean(1:6, 6:1, na.rm = "foo")

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")

# na.mr=TRUE adds 1 to the total sum or mean or other functions
x <- c(6, 2)
mean(x, na.mr = TRUE)

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}
x <- show_missings(mtcars)
class(x)
dim(x)
# add some NA values
mtcars %>%
  show_missings() %>%
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>%
  show_missings()

# R finds the variable outside the function scope if it is not declared in the function
f <- function(x) {
  x + y
}
y <- 100
f(10)

y <- 1000
f(10)
