library(tidyverse)
ggplot2::ggplot()
mpg
summary(mpg)
str(mpg)

# same color for every point
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy),color='blue')

# Point color on the basis of drive train
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,color=class))

ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,shape=drv))

# For the color intensity
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,alpha=drv))

# Default color after declaring is red
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy,color='yellow'))

# Facets - facet_wrap()  and  facet_grid()
# Making the multiple plots for each of the facet variables value
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+facet_wrap(~class,nrow=2)

# plot for each of the value of 2 facet variables 
ggplot(data=mpg)+geom_point(aes(x=displ,y=hwy))+facet_grid(drv~cyl)

# First variable before ~ means facet variable values row-wise otherwise column-wise
ggplot(data=mpg)+geom_point(aes(x=displ,y=hwy))+facet_grid(.~drv)

# Geometric Objects :
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,color=class))
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ,y=hwy,linetype=drv))

ggplot(data=mpg)+geom_point(aes(x=displ,y=hwy,color=drv))+
  geom_smooth(aes(x=displ,y=hwy,linetype=drv,color=drv))

ggplot(data=mpg)+geom_point(aes(x=displ,y=hwy,color=drv))+
  geom_smooth(aes(x=displ,y=hwy,group=drv))

# Using glaobal assignment for mapping , then local assignment can overwrite and update the mapping
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()

# se means the standard deviation or greyish region around the smooth line
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point(mapping=aes(color=class))+
  geom_smooth(data=filter(mpg,class=='subcompact'),se=TRUE)


# Bar Charts
diamonds
str(diamonds) 
summary(diamonds)
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut))
# This is same as the previous one becoz every geom has default stat and vice-versa
ggplot(data=diamonds)+stat_count(mapping=aes(x=cut))
# count is the default value for the geom_bar where as geo_col uses stat_identity as the default
# Geom_col represent the values instead of count
?geom_bar
ggplot(data=diamonds)+geom_col(mapping=aes(x=cut,y=price))

demo <- tribble( ~a, ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40)
demo
# for presenting the raw values of x with y use stat='identity' while using geom_bar
ggplot(data=demo)+geom_bar(mapping=aes(x=a,y=b),stat='identity')

# using proportion in geom_bar  and group means to group the values with respect to whole data
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,y=..prop..,group=1))

# creating a summary using stat_summary
ggplot(data=diamonds)+stat_summary(mapping=aes(x=cut,y=depth),fun.ymin=min,fun.ymax=max,fun.y=median)
?stat_summary
?stat_smooth # same as geom_smooth

# Color is used for edge colors
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,color=cut))
# fill is used to color the bar
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=cut))

# for a stacked bar chart
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity))

# position = "identity" will place each object exactly where it falls in the context of the graph. 
# This is not very useful for bars, because it overlaps them as you can see from the foll9wing graphs.
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity,alpha=1/5),position = "identity")
ggplot(data = diamonds,mapping = aes(x = cut, color = clarity)) +
  geom_bar(fill = NA, position = "identity")
                                
# position = "fill" works like stacking, but makes each set of stacked bars the same height.
# This makes it easier to compare proportions across groups.
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity),position='fill')

# position = "dodge" places overlapping objects directly beside one another.
# This makes it easier to compare individual values:
ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity),position='dodge')

# position = "jitter" adds a small amount of random noise to each point.
# This is useful where two data points overlaps because those 2 points are not likely to get same random noise.
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy),position='jitter')
# We can use the geom_jitter for the same thing
ggplot(data=mpg)+geom_jitter(mapping=aes(x=displ,y=hwy))
