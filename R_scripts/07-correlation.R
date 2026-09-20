# ==============================================================================
# 07장. 분산, 공분산 그리고 상관
# 원본: 07-correlation.Rmd
# R Markdown의 R 코드 청크와 코드 주석을 추출한 강의용 스크립트입니다.
# 예제 간 객체·패키지 의존성이 있으므로 위에서부터 절별로 실행하세요.
# ==============================================================================

# ------------------------------------------------------------------------------
# 분산, 공분산 그리고 상관 > 분산과 공분산 > 분산의 의미 > 시각적으로 이해하는 분산
# ------------------------------------------------------------------------------

library(tidyverse)
par(mfrow=c(1,2))

X<-c(1, 2, 3, 4, 5, 6)
Y<-c(15, 30, 45, 55, 70, 100)
certifi <-data.frame(X, Y)
certifi %>% 
  ggplot()+geom_point(aes(x=X, y=X))+labs(x="X", y="X")+
  xlim(0,7)+ylim(0,7)+
  geom_abline(intercept=3.5, slope=0)+
  geom_vline(xintercept=3.5)+
  coord_fixed()-> plot1

certifi %>% 
  ggplot()+geom_point(aes(x=Y, y=Y))+labs(x="Y", y="Y")+
  xlim(0,110)+ylim(0,110)+
  geom_abline(intercept=52.5, slope=0)+
  geom_vline(xintercept=52.5)+
  coord_fixed()-> plot2

plot1
plot2
par(mfrow=c(1,1))



# ------------------------------------------------------------------------------
# 분산, 공분산 그리고 상관 > 분산과 공분산 > 두 변수의 분산, 공분산
# ------------------------------------------------------------------------------


X<-c(1, 2, 3, 4, 5, 6)
Y1<-c(15, 30, 45, 55, 70, 100)
Y2<-c(100, 70, 55, 45, 30, 15)
Y3<-c(30, 15, 100, 70, 55, 45)
variance3 <-data.frame(X, Y1, Y2, Y3)
variance3 %>% 
  ggplot()+geom_point(aes(x=X, y=Y1))+labs(x="X", y="Y1")+
  xlim(0,7)+ylim(0,120)+
  geom_hline(yintercept=52.5)+
  geom_vline(xintercept=3.5)-> ploty1

variance3 %>% 
  ggplot()+geom_point(aes(x=X, y=Y2))+labs(x="X", y="Y2")+
  xlim(0,7)+ylim(0,120)+
  geom_hline(yintercept=52.5)+
  geom_vline(xintercept=3.5)-> ploty2

variance3 %>% 
  ggplot()+geom_point(aes(x=X, y=Y3))+labs(x="X", y="Y3")+
  xlim(0,7)+ylim(0,120)+
  geom_hline(yintercept=52.5)+
  geom_vline(xintercept=3.5)-> ploty3

variance3
ploty1
ploty2
ploty3




# ------------------------------------------------------------------------------
# 분산, 공분산 그리고 상관 > 상관계수 > 상관계수 추정 및 유의성 검정 > R을 활용한 상관계수 검정
# ------------------------------------------------------------------------------


X<-c(1, 1, 2, 3, 3, 2, 4, 5, 5, 7)
Y<-c(15, 30, 45, 40, 50, 55, 60, 70, 80, 100)
correlation <-data.frame(X, Y)
cor.test(correlation$X, correlation$Y)

