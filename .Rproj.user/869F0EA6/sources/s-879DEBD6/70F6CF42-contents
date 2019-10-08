library(tidyverse)
library(nycflights13)

nycflights13::flights
str(flights)

july28 <- filter(flights,month==7,day==28)
july28
# Using assignment operators
# Then we can write the above equation as follows
filter(flights,month==7 & day==28)

sqrt(2) ^ 2 == 2 # False
near(sqrt(2)^2,2) # True

# Filtering flights of 11 and 12 months both
filter(flights,month==11 | month==12)
# This is same as 
filter(flights, month %in% c(11,12))
filter(flights, !(arr_delay>120 | dep_delay>120))

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df,is.na(x) | x>1)

filter(flights, dest=='IAH')
filter(flights,dep_time==2400 | dep_time<=600)
?between
# Checking NA
summary(flights)

# Arranging the data with arrange()
# arranging on the basis of dep_time descending order
arrange(flights,desc(dep_time,month,day))
# Flights with most air delay
arrange(flights,desc(arr_delay))

# Missing values are always sorted at the end irrespective of the order
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))
? arrange
# arranging NA vlaues at the start
arrange(flights,desc(is.na(air_time)))
# this is same as
arrange(flights,-(is.na(air_time)))
arrange(flights,desc(distance))

# selecting columns or variables with select()
select(flights,year,month,day)
select(flights,year:dep_time)
select(flights,-year)









