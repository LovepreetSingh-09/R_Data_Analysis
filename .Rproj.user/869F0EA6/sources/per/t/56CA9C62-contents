library(tidyverse)
library(nycflights13)

airlines
planes
flights
weather

flights %>% count(tailnum) %>% filter(n>1)
weather %>% count(year,month,day,hour,origin) %>% filter(n>1)
flights %>% count(year,month,day,tailnum) %>% filter(n>1)

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2
count(airlines,carrier)
count(weather,origin)
count(flights2,origin)

flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")
# This is same as
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name=airlines$name[match(carrier,airlines$carrier)])

# Duplicate Joins
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")

weather
flights2
count(weather,origin)
count(flights2,origin)
# By default joining, by = c("year", "month", "day", "hour", "origin")
left_join(flights2,weather)

planes
# Every single tailnum is just once in the planes so, it is a unique key
count(planes,tailnum)
# year is in both datasets so in the final the year.x and year.y will be formed.
flights2 %>% left_join(planes,by='tailnum')

view(airports)
# faa and name which is identity of an airport is just once and hence it is a unique key
count(airports,faa)
count(flights2,dest)
flights2
flights2 %>% left_join(airports,by=c('dest'='faa'))
flights2 %>% left_join(airports,by=c('origin'='faa'))

# Makes the map of USA
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +geom_point() +
  coord_quickmap()

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest

flights %>% filter(dest %in% top_dest$dest)
# This can be easily done by semi_join which represent the intersect or common values
semi_join(flights,top_dest)

# anti_join represent the values ehich are only in the left dataset not in the 2nd one.
# counting the plane no. which are not in the planes dataset
anti_join(flights,planes,by='tailnum') %>% 
  count(tailnum,sort=T)

# set operations :-
# intersect for common values
# union for both the values
# setdiff for values in x not in y
df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)
df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)
intersect(df1, df2)
union(df1,df2)
setdiff(df1,df2)
setdiff(df2,df1)

