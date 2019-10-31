library(tidyverse)
library(lubridate)
library(nycflights13)
library(magrittr)
library(pryr)

typeof(1) # double
typeof(1L) # use L suffix to make integer
c(-1, 0, 1) / 0

x <- "This is a reasonably long string."
pryr::object_size(x)
# R makes pointer of 8 bytes to point to the same value if it comes multiple times
y <- rep(x, 1000)
pryr::object_size(y)

typeof(c(TRUE, 1L))
# Always the most complex type is taken by the vector
typeof(c(1L, 1.5))

1:10 + 1:3
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
x[c(1, 1, 5, 5, 5, 2)]
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

a<- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a)
str(a[[1]])
str(a[1])

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
attributes(x)
x

x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

y <- vector("double", 0)
y
seq_along(y)
1:length(y)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
df

means <- c(0, 1, 2)
output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)
runif(10)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

map_dbl(df, mean)
df %>% map_dbl(sd)
models <- mtcars %>%
  split(.$cyl) %>%
  map(function(df) lm(mpg ~ wt, data = df))
models
mtcars %>%
  split(.$cyl)

models %>%
  map(summary) %>%
  map_dbl(~.$r.squared)
models %>%
  map(summary) %>%
  map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06),
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78),
  c(0.93, 0.21, 0.65, 0.13, 0.27),
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)
threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()

# return 2 vectors results and error out of which 1 is normally NULL
safe_log <- safely(log)
str(safe_log(10))

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))
x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

mu <- list(5, 10, -3)
mu %>%
  map(rnorm, n = 5) %>%
  str()
# multiple related inputs
sigma <- list(1, 5, 10)
map2(mu, sigma, rnorm, n = 5) %>% str()

n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>%
  str()

params <- tribble(
  ~mean, ~sd, ~n,
  5, 1, 1,
  10, 5, 3,
  -3, 10, 5
)
params %>%
  pmap(rnorm)

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)
invoke_map(f, param, n = 5) %>% str()

x <- list(1, "a", 3)
x %>%walk(print)

iris %>%
  keep(is.factor) %>%
  str()

iris %>%
  discard(is.factor) %>%
  str()
