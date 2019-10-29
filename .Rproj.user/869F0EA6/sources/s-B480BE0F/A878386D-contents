library(tidyverse)
library(nycflights13)
library(hexbin)

nycflights13::flights
str(flights)
diamonds
str(diamonds)
str(mpg)

# Categorical
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
diamonds %>%count(cut)

# Continuous
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
# Making bins of carat
diamonds %>% count(cut_width(carat, 0.5))

smaller <- diamonds %>%filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) +geom_histogram(binwidth = 0.1)
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +geom_freqpoly(binwidth = 0.1)
ggplot(data = smaller, mapping = aes(x = carat)) +geom_histogram(binwidth = 0.01)

faithful
ggplot(data = faithful, mapping = aes(x = eruptions)) +geom_histogram(binwidth = 0.25)

ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)
# Zoom on y-axis to see outliers using coord_cartesian
ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% filter(y < 3 | y > 20) %>% arrange(y)
unusual

diamonds2 <- diamonds %>% filter(between(y, 3, 20))
diamonds2 <- diamonds %>%  mutate(y = ifelse(y < 3 | y > 20, NA, y))
# ggplot automatically excludes the NA values and gives a warning message
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point()
# we can also use the following to avoid warning
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

nycflights13::flights %>%  mutate(cancelled = is.na(dep_time),sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,sched_dep_time = sched_hour + sched_min / 60) %>%
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly( mapping = aes(color = cancelled),binwidth = 1/4 )

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot( data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

# Box plot is a better graph to interpret
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

# ordering the classes based on median by FUNC
ggplot(data = mpg) +geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median),y = hwy))
# Flipping the axis
ggplot(data = mpg) +geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median),y = hwy))

# Count diagram for x and y corelations
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

# Using geom_tile which is a kind of heatmap
diamonds %>% count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

ggplot(diamonds) + 
  geom_point(aes(x=carat,y=price),color='green',alpha=1/10)

# for creating 2D bins which are actually shown as small box for x and y values
ggplot(diamonds) + 
  geom_bin2d(aes(x=carat,y=price))

# This is same as previous but it is shown as hexagonal instead of box
ggplot(diamonds) + 
  geom_hex(aes(x=carat,y=price))

# Boxplot on the carat bins
ggplot(smaller,mapping=aes(x=carat,y=price)) + 
  geom_boxplot(aes(group=cut_width(carat,0.1)))

# displaying same no. of ponts in each bin this will change the bin width
# Here 5 is the no. pf boxplots
ggplot(smaller,mapping=aes(x=carat,y=price)) + 
  geom_boxplot(aes(group=cut_number(carat,5)))

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, color = cut)) +
  geom_boxplot()

ggplot(diamonds, aes(colour = cut_number(carat, 5), y = price, x = cut)) +
  geom_boxplot()

library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)
diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))

ggplot(data = diamonds2) +
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) +
  geom_boxplot(mapping = aes(x = cut, y = resid))
