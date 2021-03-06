library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)
library(splines)
library(ggrepel)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs( title = paste(
    "Fuel efficiency generally decreases with engine size" ))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs( title = paste( "Fuel efficiency generally decreases with engine size"),
        subtitle = paste( "Two seaters (sports cars) are an exception because of their light weight" ),
        caption = paste("Data from fueleconomy.gov")
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs( x = "Engine displacement (L)",
        y = "Highway fuel economy (mpg)",
        colour = "Car type"
  )

df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(x=quote(sum(x[i]^2,i==1,n)),
       y=quote(alpha + beta +frac(delta,theta)))

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)
best_in_class
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_text(aes(label=model),data=best_in_class)

# nudge_y moves the label slightly upward
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(aes(label=model),data=best_in_class,
             nudge_y=2,alpha=0.5)

# ggrepel automatically adjust the labels so that they don't overlap
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size=3,shape=1,data=best_in_class) +
  ggrepel::geom_label_repel(aes(label=model),data=best_in_class)

class_avg <- mpg %>%
  group_by(class) %>%
  summarize(
    displ = median(displ),
    hwy = median(hwy)
  )
class_avg
ggplot(mpg, aes(displ, hwy, color = class)) +
  ggrepel::geom_label_repel(aes(label = class),
               data = class_avg,  size = 5,
                label.size = 0,segment.color = NA ) +
   geom_point() +
    theme(legend.position = "none")

label <- mpg %>%
  summarize( displ = max(displ),
    hwy = max(hwy),
    label = paste(
      "Increasing engine size is \nrelated to decreasing fuel economy."
    ))
label
label2 <- tibble(
  displ = Inf,
  hwy = Inf,
  label = paste(
    "Increasing engine size is \nrelated to decreasing fuel economy."
  ))
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label2,
    vjust = "top",
    hjust = "right"
  )
"Increasing engine size related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()

# default scales :-
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15,40,by=5),labels=NULL)

# geom_segment used for indicating arrow towards a point
presidential
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend=end,yend=id))+
  scale_x_date(NULL,breaks=presidential$start,date_labels='%y')

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position='bottom')+
  guides(color=guide_legend(nrow=1,override.aes = list(size=3)))

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = 'Set1')

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican='red',Democratic='green'))

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")
x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))
ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +x_scale +
  y_scale +col_scale

ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave("my-plot.pdf")
