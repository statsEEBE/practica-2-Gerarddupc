#problema 1 pratctica 2

#Codigo para problema 2

iris

mis_dades <- iris
mis_dades

y <- mis_dades$Sepal.Length
y

x <- mis_dades$Petal.Length
x

plot (x,y)

xbar <- mean(x)
xbar

ybar <- mean(y)
ybar

x-xbar

m <- sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
m

b <- ybar-m*xbar
b

m*1.5+b

mod <- lm(y~x)

mod
data.frame(x=x)

ypredicted <- predict(mod,data.frame(x=x))
ypredicted

plot(x,y, pch=16, col= 'red')
lines(x,ypredicted, pch=16, col='blue')
#R^2=0 la dispersion es total,los datos hacen ua "bola",
#R^2=1 la dispersion es exacta, los datos estan sobre la recta

rsq <- sum((ypredicted-ybar)^2)/sum((y-ybar)^2)
rsq

summary(mod)
sqrt(rsq)
cor.test(x,y)
