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
?select

# renaming the column name from tailnum to tail_num
rename(flights,tail_num=tailnum)

# Bringing the specified column in the front and the remaining after that
select(flights,dep_time,arr_delay,everything())
# Calling a variable multiple time in  select will do nothing
select(flights,dep_time,dep_time,arr_delay,everything())

select(flights,one_of(flights))

# selcting the variables contains TIME while ignoring the proper vocabulary
select(flights, contains("TIME"))
# We can turn it off using ignore.case=False
select(flights, contains("TIME",ignore.case=FALSE))
select(flights, contains("time",ignore.case=FALSE))

# Adding new variables using mutate()
# We can add new variable which are the functions of the existing variables
flights_sml <- select(flights, year:day, ends_with("delay"),distance, air_time)
mutate(flights_sml,gain=arr_delay-dep_delay,speed=distance/air_time*60)
# For keeping only the new variables use transmute()
transmute(flights, gain = arr_delay - dep_delay,hours = air_time / 60, gain_per_hour = gain / hours)

# %/% for getting the dividing factor and %% for remainder
transmute(flights, dep_time, hour = dep_time %/% 100,minute = dep_time %% 100)

x <- c(1:10)
# leading value of current value of x
lead(x)
# lagging value
lag(x)

# R provides functions for running sums, products, mins, and maxes: cumsum(), cumprod(), cummin(), cummax().
# dplyr also provides cummean() for cumulative means.
cummean(x)
cumsum(x)

y <- c(1, 2, 2, NA, 3, 4)
# For getting the rank of each value where NA's rank always in every command remains NA
# Duplicates have same rank but other values' rank change on the basis of no. of duplicate values
min_rank(y)
# It gives the rank to every element means duplicate values have different rank
row_number(y)
?row_number
# Duplicates have same rank and it doesn't change the rank of other elements
dense_rank(y)
#  a number between 0 and 1 computed by rescaling min_rank to [0, 1]
# Rank range of all liies b/w 0 and 1 where minimum value has 0 and maximum has 1
#y Duplicates have same rank
percent_rank(y)
?percent_rank
 #a cumulative distribution function. Proportion of all values less than or equal to the current rank.
cume_dist(y)

transmute(flights,dt_hr=dep_time%/%100,dt_min=dep_time%%100,sdt_hr=sched_dep_time%/%100,sdt_min=sched_dep_time%%100)

# Summarize() for getting values groupby
# Average delayfor every day of the year
# Count=n() for the count of total values or flights on that day
delay=summarize(group_by(flights,year,month,day),count=n(),delay=mean(dep_delay,na.rm=TRUE))
filter(delay,count>1000)

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,count = n(),dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")
delay
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# The upper full code we can write in a compact way using pipe
delay <- flights %>% group_by(dest) %>%
  summarize(count=n(),dist=mean(distance,na.rm=TRUE),delay=mean(arr_delay,na.rm=TRUE)) %>%
  filter(count>20,dest!='HNL')
delay  

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay),!is.na(arr_delay))

not_cancelled %>% group_by(year,month,day) %>% 
  summarize(mean=mean(dep_delay))

delays=not_cancelled %>% group_by(tailnum) %>% summarize(delay=mean(arr_delay))
ggplot(data=delays)+geom_freqpoly(mapping=aes(x=delay),binwidth=10)

delays=not_cancelled %>% group_by(tailnum) %>% summarize(delay=mean(arr_delay),count=n(),na.rm=TRUE)
ggplot(data=delays)+geom_point(mapping=aes(x=delay,y=count),alpha=1/10)

# Neglecting the first first 25 counts
delays %>% filter(count>25) %>% ggplot(mapping=aes(x=delay,y=count))+
  geom_point(alpha=1/10)

batting <- as_tibble(Lahman::Batting)

batters=batting %>% group_by(playerID) %>%
  summarize(ba=sum(H,na.rm=TRUE)/sum(AB,na.rm=TRUE),ab=sum(AB,na.rm=TRUE))
batters

batters %>% filter(ab>25) %>% ggplot(mapping=aes(x=ab,y=ba))+
  geom_point() + geom_smooth(se=FALSE)

not_cancelled %>% group_by(year,month,day) %>% 
  summarize(avg_delay1=mean(arr_delay),avg_delay2=mean(arr_delay[arr_delay>0]))

not_cancelled %>% group_by(dest) %>%
  summarize(sd=sd(distance)) %>% arrange(desc(sd))

not_cancelled %>% group_by(year, month, day) %>%
  summarize(first = min(dep_time),last = max(dep_time))
# We can also use first() and last() at the place of min and max
not_cancelled %>%group_by(year, month, day) %>%
  summarize(first_dep = first(dep_time),last_dep = last(dep_time))

not_cancelled %>%group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%filter(r %in% range(r))

# For calculating the unique values
not_cancelled %>% group_by(dest) %>%
  summarize(carriers=n_distinct(carrier)) %>% arrange(desc(carriers))

not_cancelled %>% count(dest)

# We can use the count of variable with the weight or on the basis of other variables
not_cancelled %>% count(tailnum, wt = distance)

not_cancelled %>% group_by(year, month, day) %>%
  summarize(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% group_by(year, month, day) %>%
  summarize(hour_perc = mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)
daily
# daily no. of flights
(per_day <- summarize(daily, flights = n()))
# Monthly no. of flights
(per_month <- summarize(per_day, flights = sum(flights)))
# Yearly no. of flights
(per_year <- summarize(per_month, flights = sum(flights)))

daily %>% ungroup() %>% 
  summarize(flights = n())

popular_dests <- flights %>% group_by(dest) %>%
  filter(n() > 365)
popular_dests

popular_dests %>% filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)


