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
  print(n = 15, width = Inf)

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

# reading more than maximum limit of 1000 values of a row
challenge2 <- read_csv(
  readr_example("challenge.csv"),
  guess_max = 1001
)
challenge2

# reading variables as character
challenge2 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character()) )
challenge2

df <- tribble(
  ~x, ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df
# conveting the suitable datatype of each variable
type_convert(df)

write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv")

# rds store data in R’s custom binary format called RDS
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

# The feather package implements a fast binary file format that 
# can be shared across programming languages
library(feather)
write_feather(challenge, "challenge.feather")
read_feather("challenge.feather")

table1
table2
table3
table4a
table4b

table1 %>%
  mutate(rate = cases / population * 10000)
table1 %>%
  count(year, wt = cases)
library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

# gathering shortens the data table width
table4a
gather(table4a,'1999','2000',key='year',value='cases')
table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")

tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")

left_join(tidy4a, tidy4b)

# Spreading increases the data width
table2
spread(table2, key=type, value=count)

# separate() pulls apart one column into multiple columns, by splitting
# wherever a separator character appears.
table3
separate(table3,rate,into=c('cases','population'))
# We can aslo use a separate argument
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/")

# separate make the columns as char type by default
# But we can change it to a better type using convert=TRUE
table3 %>%
  separate(
    rate,
    into = c("cases", "population"),
    convert = TRUE
  )
# We can also separate a column using the position number
table3 %>%
  separate(year, into = c("century", "year"), sep = 2)

# Unite
table5
unite(table5,new,century, year)
# unite unite the values of two columns using default _ sign
table5 %>%
  unite(new, century, year, sep = "")

stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c( 1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
b=spread(stocks,key=year,value=return)
gather(b,key=year,value=return, `2015` , `2016` ,na.rm=TRUE)

# complete will show all the explicit missing values and complete the whole tibble
complete(stocks,year,return)

treatment <- tribble(
  ~ person, ~ treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, 9,
  "Katherine Burke", 1, 4
)
# fill fills the missing values with the most recent value
treatment
fill(treatment,person)

who
who1=gather(who,new_sp_m014:newrel_f65,key='key',value='count',na.rm=TRUE)
count(who1,key)
who2=mutate(who1,key=stringr::str_replace(key,'newrel','new_rel'))
who2
who2=separate(who2,key,into=c('class','category','age'),sep='_')
who2 %>% count(class)
who2=select(who2,-class,-iso2,-iso3)
who2
who_final=separate(who2,age,into=c('gender','age'),sep=1)
who_final
