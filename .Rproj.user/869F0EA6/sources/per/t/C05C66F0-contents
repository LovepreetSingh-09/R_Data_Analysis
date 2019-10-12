library(tidyverse)
library(nycflights13)

as_tibble(iris)
tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5)

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)
lubridate::now()

# Displaying all columns
nycflights13::flights %>%
  print(n = 10, width = Inf)

class(as.data.frame(flights))

read_csv("a,b,c
         1,2,3
         4,5,6")

read_csv("The first line of metadata
The second line of metadata
x,y,z
1,2,3", skip = 2)

read_csv("1,2,3\n4,5,6", col_names = FALSE)
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
read_csv("a,b,c\n1,2,.", na = ".")

str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))

# Now we get an error in 3rd and 4th term
x <- parse_integer(c("123", "345", "abc", "123.45"))
problems(x)

# using locale to identify decimal point
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))

# parse_number ignores nonnumeric characters before and after the number.
parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45")
# using locale to identify number
parse_number("123.456.789",locale = locale(grouping_mark = "."))
parse_number("123'456'789",locale = locale(grouping_mark = "'"))

# ASCII values
charToRaw("Hadley")
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
# using different encoders
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

# guessing the encoding of a value
guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

# unable to parse values to a predifined factor
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

parse_datetime("2010-10-01T2010")
parse_datetime("20101010")
parse_date("2010-10-01")
parse_time("01:10 am")
parse_time("20:10:01")
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")
# parsing language argument
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
date_names_langs()

# guess_parser to guess the parsing
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))
str(parse_guess("2010-10-10"))

# read_csv read column wise 1000 examples and guess the vector type according to that.
# But if next values are of different type the problem arises
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)

# To avoid that we can define col_types
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_character() ))

tail(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date() ))
tail(challenge)










