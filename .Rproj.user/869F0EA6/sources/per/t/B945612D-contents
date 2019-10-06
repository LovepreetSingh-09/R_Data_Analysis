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






                                