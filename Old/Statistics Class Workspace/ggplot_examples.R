# Load the diamonds dataset
library(ggplot2)

# Make a smaller dataset (diamonds has like 100,000 examples)
d_sml = diamonds[sample(nrow(diamonds), 1000), ]

# A ggplot is just an object connected to a plot
g = ggplot()

# What you actually do with a ggplot object is add layers to it, for example:
# g + geom_point()
# or:
# ggplot() +
#   geom_point()

# Notice that that code gives an error, this is because ggplot and its layers use
# "aesthetics" to determine what you plot in each layer.
# Aesthetics and datasets, which you pass to ggplot like this:

# ggplot(data=DATASET, aes(AESTHETICS))

# Are inherited, so if I add a geometry layer, 
# ggplot(data=DATASET, aes(AESTHETICS)) +
#   geom_point()
# That layer will "inhereit" the aesthetics from the lower layer. You can still
# override what you set lower down, or set up a new dataset.

# Let's look at an example
ggplot(data=d_sml) +
  geom_point(aes(carat,price))

# geom_point plots your data as points, so you get a scatterplot. You could also use 
# geom_line, geom_box, etc...

ggplot(data=d_sml) +
  geom_point(aes(carat,price)) +
  geom_line(aes(carat,price))

# Line doesn't really make sense here so we won't use that again. But what if we want
# to break down our dataset by the "cut" of the diamonds? We might use color to do that.
# Note that for some geometries the color is only the outline of the shape, so you 
# have to use fill= instead of color= (or both fill and color to get consistency)

ggplot(data=d_sml) +
  geom_point(aes(carat,price,color=cut))

# Maybe we also want to know what the linear model, which we tested separately, outputs

ggplot(data=d_sml,aes(carat,price,color=cut)) +
  geom_point() +
  geom_smooth(method="lm")

# Note that each additional piece we add is "layered" on top of the previous ones.
# In theory, you can add any number of layers and ggplot will continue re-sizing and
# re-shaping the plot to accomodate them.
# Here's how to add a title

ggplot(data=d_sml,aes(carat,price)) +
  geom_point(aes(color=cut)) +
  geom_smooth(method="lm",color="black") +
  ggtitle("Carat vs. Price") +
  labels(title="Carat")

# More generally you want to us e

ggplot(data=d_sml,aes(carat,price)) +
  geom_point(aes(color=cut)) +
  geom_smooth(method="lm",color="black") +
  facet_grid(.~cut) +
  ggtitle("Carat vs. Price") +
  labels(title="c")

ggplot(data=d_sml,aes(carat,price,color=cut)) +
  geom_point() +
  geom_smooth(method="lm")
  
cors = ddply(data,c("Type"),summarise, cor=round(cor(Pasthapp,Futurehapp),2), p=round(cor.test(Pasthapp,Futurehapp)$p.value,2))

library(plyr)
A = ddply(d_sml,c("cut"),summarise,mu=mean(price))


ggplot(data=d_sml,aes(carat,price,color=cut)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_jitter(position=position_jitter(width=3,height=3)) +
  geom_text(data=A,aes(label=round(A$mu,2),x=1,y=20000)) +
  facet_grid(.~cut)


