# ==============================================================================
# 08장. 직관으로 이해하는 회귀분석
# 원본: 08-intuitive.Rmd
# R Markdown의 R 코드 청크와 코드 주석을 추출한 강의용 스크립트입니다.
# 예제 간 객체·패키지 의존성이 있으므로 위에서부터 절별로 실행하세요.
# ==============================================================================

# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 산업인력개발에서의 회귀분석 > 변수간의 관계를 구명하는 연구가설들
# ------------------------------------------------------------------------------

library(tidyverse)


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석과 1차 함수 > 절편과 기울기의 의미
# ------------------------------------------------------------------------------

ID<-c(1,2,3,4,5,6,7,8,9,10) 
X<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5)
Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)
Learning <-data.frame(ID, X, Y)
Learning %>% 
  ggplot()+geom_point(aes(x=X, y=Y))+labs(x="learning(hrs)", y="job performance(point)")+xlim(0,6)+ylim(0,26) -> plot1

Learning %>%
  ggplot()+geom_point(aes(x=X, y=Y))+
  labs(x="learning(hrs)", y="job performance(point)")+
  xlim(0,6)+ylim(0,26)+
  geom_abline(intercept = 10, slope= 3, color="blue")+
  theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=4, y=10, label= "Y=10+3X", size=5)+
  annotate("text", x=4, y=8, label= "intercept=10")+
  annotate("text", x=3.8, y=6, label= "slope=3") -> plot2



# 코드 청크: figures-side

par(mar = c(4, 4, .1, .1))
plot(plot1)
plot(plot2)


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석과 1차 함수 > 기울기의 의미
# ------------------------------------------------------------------------------

X2<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5, 1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5)
Y2<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25, 15, 13, 23, 28, 29, 33, 32, 33, 35, 43)
gender<-c("M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F")
Learning2 <-data.frame(X2, Y2, gender)
Learning2$gender <- factor(gender, levels= c("M", "F") )
Learning2 %>% 
  ggplot()+geom_point(aes(x=X2, y=Y2, colour= gender))+labs(x="learning(hrs)", y="job performance(point)")+
  xlim(0,6)+
  geom_abline(intercept = 10, slope= 3, color="red")+
  geom_abline(intercept = 10, slope= 6, color="blue")+
  theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=4, y=15, label= "Y=10+3X", size=5)+
  annotate("text", x=4, y=40, label= "Y=10+6X", size=5 )


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석과 1차 함수 > , 와 잔차(residual)
# ------------------------------------------------------------------------------

ID<-c(1,2,3,4,5,6,7,8,9,10) 
X<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5)
Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)
Learning <-data.frame(ID, X, Y)
learningReg <-lm(Y ~ X, data=Learning)
summary(learningReg)
resid(learningReg)


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석과 1차 함수 > 독립변수가 두 개 이상일때의 회귀식
# ------------------------------------------------------------------------------

library("plot3D")
ID<-c(1,2,3,4,5,6,7,8,9,10)
x<-X1<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5)
y<-X2<-c(16, 20, 28, 29, 32, 30, 50, 41, 40, 60)
z<-Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)

scatter3D(x,y,z, bty="g", ticktype = "detailed", pch =20, cex = 2,
          type="h", xlab = "X1(learning.hours)", ylab = "X2(tenure)",
          zlab = "Y(performance score)") -> plotr3 

fit <- lm(z ~x+y)
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)
scatter3D(x, y, z, pch =20, cex = 2, bty="g",
          theta = 5, phi = 25, ticktype = "detailed",
          xlab = "X1(learning.hours)", ylab = "X2(tenure)", 
          zlab = "Y(performance score)",
          surf = list(x = x.pred, y = y.pred, z = z.pred, facets = NA, fit = fitpoints), main = "multi-variate regession") -> plotr4


ID<-c(1,2,3,4,5,6,7,8,9,10) 
X1<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5)
X2<-c(16, 20, 28, 29, 32, 30, 50, 41, 40, 60)
Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)
multiLearning <-data.frame(ID, X1, X2, Y)
multiReg <-lm(Y ~ X1+X2, data=multiLearning)
summary(multiReg)
resid(multiReg)


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석과 1차 함수 > added variable plot
# ------------------------------------------------------------------------------

ID<-c(1,2,3,4,5,6,7,8,9,10) 
X1<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5)
X2<-c(16, 20, 28, 29, 32, 30, 50, 41, 40, 60)
Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)
multiLearning <-data.frame(ID, X1, X2, Y)
multiReg <-lm(Y ~ X1+X2, data=multiLearning)
car::avPlots(multiReg)


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석과 2차함수 > 비선형 회귀분석
# ------------------------------------------------------------------------------

a=1
b=2
c=-24
f= function (x){
  a*x^2 +b*x+c
}
x=-10:10
plot(x, f(x), type = 'l')
abline(h=0)
abline(v=0)

##find vertex
find.vertex = function(a, b, c) {
  x_vertex = -b/(2 * a)
  y_vertex = f(x_vertex)
  c(x_vertex, y_vertex)
}
V = find.vertex(a, b, c)

# add the vertex to the plot
points(x = V[1], y = V[2],
       pch = 16, cex = 1)
text(x = V[1], y = V[2],
     labels = "Vertex", pos = 3)
# find the x-intercepts of f(x)
find.roots = function(a, b, c) {
  discriminant = b^2 - 4 * a * c
  if (discriminant > 0) {
    c((-b - sqrt(discriminant))/(2 * a), (-b + sqrt(discriminant))/(2 * a))
  }
  else if (discriminant == 0) {
    -b / (2 * a)
  }
  else {
    NaN
  }
}
solutions = find.roots(a, b, c)
# add the x-intercepts to the plot
points(x = solutions, y = rep(0, length(solutions)),
       pch = 16, cex = 1, col = 'blue')
text(x = solutions, y = rep(0, length(solutions)),
     labels = rep("x-intercept", length(solutions)),
     pos = 3, col = 'blue')
V = find.vertex(a, b, c)

# add the y-intercept to the plot
find.intercept=function (a, b, c) {
   x_intercept=0
   y_intercept=f(x_intercept)
   c(x_intercept, y_intercept)
}
V2=find.intercept(a,b,c)

points(x = V2[1], y = V2[2],
       pch = 16, cex = 1, col='red')
text(x = 3, y = -23,
     labels = "y-intercept", pos = 3)



par(mfrow=c(1,2))

a=2
b=20
c=100
f= function (x){
  a*x^2 +b*x+c
  }
x=-10:10
plot(x, f(x), type = 'l', main = expression(y==2*x^2 + 20*x + 100))
abline(h=0)
abline(v=0)



a=-2
b=20
c=100
f= function (x){
  a*x^2 +b*x+c
}
x=-5:20
plot(x, f(x), type = 'l', main = expression(y==-2*x^2 + 20*x + 100))
abline(h=0)
abline(v=0)

par(mfrow=c(1,1))



par(mfrow=c(1,2))

a=2
b=-20
c=100
f= function (x){
  a*x^2 +b*x+c
  }
x=-5:30
plot(x, f(x), type = 'l', main=expression(y==2*x^2 - 20*x + 100))
abline(h=0)
abline(v=0)

a=-2
b=-2
c=50
f= function (x){
  a*x^2 +b*x+c
}
x=-5:5
plot(x, f(x), type = 'l', main = expression(y==-2*x^2 - 2*x + 50))
abline(h=0)
abline(v=0)

par(mfrow=c(1,1))



# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석과 2차함수 > 임금방정식으로 이해하는 비선형 회귀분석
# ------------------------------------------------------------------------------

par(mfrow=c(1,2))

a=5
b=100
f= function (x){
  a*x+b
  }
x=20:85
plot(x, f(x), type = 'l', main = expression(y==5*x + 100), 
     xlab= "age(year)", 
     ylab= "earning(10,000won)")
abline(h=0)
abline(v=0)

a=-0.04
b=5
c=200
f= function (x){
  a*x^2+b*x+c
  }
x=20:85
plot(x, f(x), type = 'l', main = expression(y==-0.04*x^2 + 5*x + 200), 
     xlab= "age(year)", 
     ylab= "earning(10,000won)")
abline(h=0)
abline(v=0)

par(mfrow=c(1,1))


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석에서 더미변수 활용 > 회귀분석에 적합한 변수의 척도
# ------------------------------------------------------------------------------

ID<-c(1,2,3,4,5,6,7,8,9,10) 
X<-c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3)
Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)
bloodLearning <-data.frame(ID, X, Y)
bloodReg <-lm(Y ~ X, data=bloodLearning)
summary(bloodReg)
resid(bloodReg)


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석에서 더미변수 활용 > 범주형 변수가 독립변수로 투입될 때 > 이분형(dichotomous) 척도가 독립변수일 때
# ------------------------------------------------------------------------------

ID<-c(1,2,3,4,5,6,7,8,9,10) 
X<-c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)
genderLearning <-data.frame(ID, X, Y)
genderReg <-lm(Y ~ X, data=genderLearning)
summary(genderReg)
t.test(Y ~ X, genderLearning)


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀분석에서 더미변수 활용 > 범주형 변수가 독립변수로 투입될 때 > 3집단 이상의 범주형 척도가 독립변수일 때
# ------------------------------------------------------------------------------

ID<-c(1,2,3,4,5,6,7,8,9,10) 
X<-c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3)
D1 <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
D2 <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0)
Y<-c(12, 11, 14, 15, 19, 20.5, 18.5, 18.5, 27, 25)
bloodLearning2 <-data.frame(ID, X, D1, D2, Y)
bloodReg2 <-lm(Y ~ D1 + D2, data=bloodLearning2)
bloodLearning2
summary(bloodReg2)


par(mfrow=c(1,2))

bloodtype<-c("A", "B", "O") 
performance<-c(-13.000, -6.875, 0)
bloodbar <-data.frame(bloodtype, performance)
barplot(height=bloodbar$performance, names=bloodbar$bloodtype)
title(main = "y hat of O as zero")

bloodtype2<-c("A", "B", "O") 
performance2<-c(13.000, 19.125, 26.000)
bloodbar2 <-data.frame(bloodtype2, performance2)
barplot(height=bloodbar2$performance2, names=bloodbar2$bloodtype2)
title(main = "y hat of O type as real value")


par(mfrow=c(1,1))


# ------------------------------------------------------------------------------
# 직관으로 이해하는 회귀분석 > 회귀식으로 이해하는 조절효과 > 분석결과 해석 예제
# ------------------------------------------------------------------------------

ID<-c(1:20) 
X<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5, 1.2, 0.7, 2.2, 3.3, 3.1, 3.7, 4.1, 3.9, 4.3, 5)
M<-c(2, 1, 1.2, 1, 1, 1.1, 1, 1.5, 1, 2, 5, 7, 7, 6, 7, 7, 6, 7, 4, 5)
Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25, 12, 11, 18, 19, 28, 29, 26, 27.5, 27.5, 30)
moderLearning <-data.frame(ID, X1, M, Y)
moderLearning$XM <-moderLearning$X * moderLearning$M 
moderLearning
moderReg <-lm(Y ~ X+M+ XM, data=moderLearning)
summary(moderReg)



a=10.8737
b=2.3928
f= function(x){
  a+b*x
}

c=1.531
d=3.2546
e=3.6855
g=4.1164
f2= function(x){
  a+c*x
}
f3= function(x){
  a+d*x
}
f4= function(x){
  a+e*x
}
f5= function(x){
  a+g*x
}
x=-3:5
plot(x, f(x), type = 'l', main = expression(y==10.8737+2.3928*x), 
     xlab= "learningtime", 
     ylab= "performance")
lines(x, f2(x), type='l', col='red')
lines(x, f3(x), type='l', col='purple')
lines(x, f4(x), type='l', col='blue')
lines(x, f5(x), type='l', col='pink')


abline(h=0)
abline(v=0)

