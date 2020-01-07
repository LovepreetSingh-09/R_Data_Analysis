library(tidyverse)
library(modelr)

options(na.action = na.warn)

library(nycflights13)
library(lubridate)
library(splines)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins = 50)

diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2 ^ lprice)
ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)
diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond, "lresid")
ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

grid=diamonds2 %>% data_grid(cut,.model=mod_diamond2) %>%
  add_predictions(mod_diamond2)
grid

ggplot(grid,aes(x=cut, y=pred)) +
  geom_point() 

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond2, "lresid2")
diamonds2

ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)

diamonds2 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamond2) %>%
  mutate(pred = round(2 ^ pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)

daily <- flights %>% 
  mutate(date=make_date(year,month,day)) %>%
  group_by(date) %>%
  summarize(n=n())
daily

ggplot(daily,aes(date,n))+
  geom_line()

daily <- daily %>% 
  mutate(wday=wday(date,label=TRUE))
daily

ggplot(daily,aes(wday,n)) +
  geom_boxplot()

mod <- lm(n ~ wday,data=daily)
mod

grid <- daily %>% data_grid(wday) %>%
  add_predictions(mod,'n')
grid

ggplot(daily,aes(wday,n)) +
  geom_boxplot() +
  geom_point(data=grid,color='red',size=5)

daily <- daily %>% add_residuals(mod)
daily

ggplot(daily,aes(date,resid)) +
  geom_line() +
  geom_ref_line(h=0)

ggplot(daily,aes(date,resid,color=wday)) +
  geom_line() +
  geom_ref_line(h=0)

daily %>% filter(resid< -100)

daily %>% ggplot(aes(date,resid)) +
  geom_ref_line(h=0) +
  geom_line(color='grey50') +
  geom_smooth(se=FALSE,span=0.1)

daily %>% filter(wday=='Sat') %>%
  ggplot(aes(date,n)) +
  geom_line() +
  geom_point()+
  scale_x_date(date_breaks='month',date_labels='%b')

term=function(date){
  cut(date, breaks = ymd(20130101,20130501,20130728,20140101),labels=c('spring','summer','fall'))
}
daily <- daily %>% mutate(term=term(date))
daily

daily %>% filter(wday=='Sat') %>% 
  ggplot(aes(date,n,color=term)) +
  geom_line() +
  scale_x_date(date_breaks='month',date_labels='%b')

daily %>%
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily$`term(date)`=NULL
daily %>% gather_residuals(without_term=mod1,with_term=mod2) %>%
  ggplot(aes(date,resid,color=model)) +
    geom_line(alpha=0.75)
  
gather_residuals(daily,without_term=mod1,with_term=mod2) 

grid <- daily %>%
  data_grid(wday,term) %>% add_predictions(mod2,'n')
grid

ggplot(daily,aes(wday,n)) +
  geom_boxplot() +
  geom_point(data=grid,color='red') +
  facet_wrap(term~.)

daily
mod3=MASS::rlm(n~wday*term,data=daily)

daily %>% add_residuals(mod3,'resid') %>%
  ggplot(aes(date,resid)) +
  geom_hline(yintercept=0,size=1,color='green')+
  geom_line()

?MASS::rlm()
?ns()
women
a=ns(women$height, df = 5)
a
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>%
  add_predictions(mod)%>%
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point()




















