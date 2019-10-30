library(tidyverse)
library(lubridate)
library(nycflights13)

now()
today()

# for only creating dates
ymd(20190507)
dmy('28st February,2019')

# For creating dates with time
ymd_hms('20190705 20:11:00')
ymd_hm('20190705 20:11')
?tz # tz = timezone
ymd_hm('20190705 20:11',tz='Asia/Kolkata')

fl=flights %>%
  select(year, month, day, hour, minute)
fl %>% mutate(dep_time=make_datetime(year,month,day,hour,minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}


flights
flights_dt <- flights %>% filter(!is.na(dep_time),!is.na(arr_time)) %>%
  mutate(dep_time=make_datetime_100(year,month,day,dep_time),
         arr_time = make_datetime_100(year, month, day, arr_time),
         sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
         sched_arr_time = make_datetime_100( year, month, day, sched_arr_time)) %>%
  select(origin,dest,ends_with('delay'),ends_with('time'))
flights_dt

ggplot(flights_dt,aes(dep_time)) +
  geom_freqpoly(binwidth=86400) # 86400=no. of seconds in a day

flights_dt %>% filter(dep_time>ymd(20130728) & dep_time<ymd(20130729)) %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth=600)

as_datetime(today())
as_date(now())
# Count from 1970-0-01 00:00:00 UTC
as_datetime(60*60*24*5)
as_date(365*10+2)

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)
# To show the levels of months
month(datetime,label=TRUE)
# To show the full name use abbr=FALSE
month(datetime,label=TRUE,abbr=FALSE)

flights_dt %>% mutate(wday=wday(dep_time,label=TRUE)) %>%
  ggplot(aes(x=wday))+
  geom_bar()
flights_dt %>% mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>% summarize(avg_delay = mean(arr_delay, na.rm = TRUE), n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
  geom_line()

sched_dep <- flights_dt %>% mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>% summarize(avg_delay = mean(arr_delay, na.rm = TRUE), n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()
ggplot(sched_dep, aes(minute, n)) +
  geom_line()


# Each ceiling_date()function takes a vector of dates to adjust and then the name of t
# he unit to round down (floor), round up (ceiling), or round to.
flights_dt %>%
  count(week = floor_date(dep_time, "week"))
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line()

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime) <- 2020
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
update(datetime,year=2021,month=07,mday=28,hour=00)
ymd("2015-02-01") %>%
  update(mday = 30)
ymd("2015-02-01") %>% update(hour = 400)
# Dep_time of flights on the 1st day of the year
flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

# Calculating Durations which always tells the duration in seconds with the appropriate duration of years, months, etc
h_age <- today() - ymd(19791014)
h_age
as.duration(h_age)
dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)
2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)
# Here the addition of 1 day changes the time with another tz 
one_pm <- ymd_hms(
  "2016-03-12 13:00:00",
  tz = "America/New_York")
one_pm
one_pm + ddays(1)

#To solve this use Periods it makes accurate addition and doesn't change time but changes tz
one_pm
one_pm + days(1)
seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

# Duration doesn't take leap year into account but periods does
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)
one_pm + ddays(1)
one_pm + days(1)

flights_dt %>%
  filter(arr_time < dep_time)
flights_dt <- flights_dt %>%
  mutate(overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1))

flights_dt %>%
  mutate(overnight = arr_time < dep_time,
         arr_time = arr_time + days(overnight * 1),
         sched_arr_time = sched_arr_time + days(overnight * 1))
flights_dt %>%
  filter(overnight, arr_time < dep_time)         

# intervals
years(1) / days(1)
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
(today() %--% next_year) %/% days(1)

Sys.timezone()
length(OlsonNames())
OlsonNames()
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
x1 - x2
x1 - x3
x4 <- c(x1, x2, x3)
x4
# with_tz() doesn't makes difference with the original timezones
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a-x4
# force_tz() makes difference in different timezones
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b-x4
