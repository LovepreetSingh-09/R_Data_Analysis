library(tidyverse)
library(nycflights13)

str_length(c("a", "R for data science", NA))

# combine strings
str_c("x","y")
str_c("x","y","z",sep=',')
x=c("abc",NA)
str_c('|-',x,'-|')
str_c("|-",str_replace_na(x),"-|")
str_c("prefix-", c("a", "b", "c"), "-suffix")
str_c(c("x", "y", "z"), collapse = ", ")

# sunbsetting strings
x <- c("Apple", "Banana", "Pear")
str_sub(x,1,4)
str_sub(x,-3,-1)
# we can also modify string by str_sub 
str_sub(x,1,1)=str_to_lower(str_sub(x,1,1))
x

# Locales 
str_to_upper(c("i", "ı"))
# using the language code to modify string
str_to_upper(c("i", "ı"),locale='tr')

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")
str_sort(x,locale='haw')
