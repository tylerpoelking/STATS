library (ISLR)
set.seed (1)
train=sample (392 ,196)

lm.fit = lm(mpg~horsepower, data = Auto, subset = train)

attach (Auto)

#test MSE linear
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)


lm.fit2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
#test MSE quad
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)


lm.fit3 = lm(mpg~poly(horsepower,3), data = Auto, subset = train)
#test MSE cubic
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)


library(boot)

glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err =cv.glm(Auto ,glm.fit)
cv.err$delta



cv.error=rep (0,5)
for (i in 1:5){
     glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
     cv.error[i]=cv.glm (Auto ,glm.fit)$delta [1]
}
cv.error



set.seed (17)
cv.error.10= rep (0 ,10)
for (i in 1:10) {
     glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
     cv.error.10[i]=cv.glm (Auto ,glm.fit ,K=10) $delta [1]
     }
cv.error.10

