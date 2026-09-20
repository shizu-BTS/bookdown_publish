##3dplot
install.packages("plot3D")
library("plot3D")
ID<-c(1,2,3,4,5,6,7,8,9,10)
x<-X1<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5)
y<-X2<-c(16, 20, 28, 29, 32, 30, 50, 41, 40, 60)
z<-Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)
scatter3D(x,y,z, bty="g", ticktype = "detailed", pch =20, cex = 2, type="h",
          xlab = "X1(learning.hours)", ylab = "X2(tenure)", zlab = "Y(performance score)")

fit <- lm(z ~x+y)
fit
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy),
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)
scatter3D(x, y, z, pch =20, cex = 2, bty="g",
          theta = 5, phi = 25, ticktype = "detailed",
          xlab = "X1(learning.hours)", ylab = "X2(", zlab = "Y(performance score)",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints), main = "multi-variate regession")
x<-X1<-c(1, 0.5, 2, 3, 3, 3.5, 8, 4, 4.5, 7)

install.packages("caret")
library(caret)
install.packages("kenlab")
###3D 예제
data(mtcars)
# x, y, z variables
x <- mtcars$wt
y <- mtcars$disp
z <- mtcars$mpg
# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy),
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
# scatter plot with regression plane
scatter3D(x, y, z, pch = 18, cex = 2,
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "wt", ylab = "disp", zlab = "mpg",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints), main = "mtcars")

5/0.08
a=1
b=2
c=-20
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
       pch = 17, cex = 1, col = 'blue')
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
       pch = 12, cex = 1)
text(x = 3, y = -20,
     labels = "y-intercept", pos = 3)

Y
2
26.000-6.875
2.3928+0.4309*M=0


2.3928-0.4309*2
2.3928
2.3928+(0.4309*2)
2.3928+(0.4309*3)
2.3928+(0.4309*4)
(15+30+45+55+70+100)/6
Y<-c(15, 30, 45, 55, 70, 100)
var(Y)
Y1<-c(15, 30, 45, 55, 70, 100)
Y2<-c(100, 70, 55, 45, 30, 15)
Y3<-c(50, 30, 10, 100, 40, 80)
mean(Y3)
A<-c(6, 9, 10, 14, 16, 17)
mean(A)
B<-c(0, 6, 8, 16, 28, 32)
mean(B)
C<-c(0, 60, 80, 160, 280, 320)
mean(C)

## f distirubtion
par("mar")
par(mar=c(1,1,1,1))
출처: https://gigle.tistory.com/88 [모조리 기획해주마:티스토리]


x = seq(0, 5, length = 100)
plot(x, df(x = x, df1 = 1, df2 = 1))
curve(df(x, df1=4, df2=13), from=0, to=5)
