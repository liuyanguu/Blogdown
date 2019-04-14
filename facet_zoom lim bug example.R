library(ggplot2)
library(ggforce)
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point(size = 1) +
  coord_cartesian(ylim = c(1,3)) +
  facet_zoom(ylim = c(1.5,2), horizontal = F)

packageVersion("ggforce")

ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  scale_y_continuous(limits = c(0,2)) +
  facet_zoom(ylim = c(0,1), horizontal = F)


# legend position ---------------------------------------------------------

mtcars$cyl <- factor(mtcars$cyl, labels=c("four","six","eight"))
library(gridExtra)

a <- ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point(aes(colour=cyl)) + labs(title = "Legend is top left") + 
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))

b <- ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point(aes(colour=cyl)) + labs(title = "Legend is bottom right") +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0))

c <- ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point(aes(colour=cyl)) + labs(title = "Legend is bottom left") +
  theme(legend.justification = c(0, 0), legend.position = c(0, 0))

d <- ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point(aes(colour=cyl)) + labs(title = "Legend is top right") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1))

grid.arrange(a,b,c,d)
