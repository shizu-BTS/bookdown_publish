# ==============================================================================
# 04장. 데이터 시각화
# 원본: 04-graph.Rmd
# R Markdown의 R 코드 청크와 코드 주석을 추출한 강의용 스크립트입니다.
# 예제 간 객체·패키지 의존성이 있으므로 위에서부터 절별로 실행하세요.
# ==============================================================================

# ------------------------------------------------------------------------------
# 데이터 시각화 > ggplot2의 설치 및 소개 > ggplot2 설치
# ------------------------------------------------------------------------------

library(tidyverse)


# ------------------------------------------------------------------------------
# 데이터 시각화 > ggplot2의 설치 및 소개 > ggplot2의 작동 원리
# ------------------------------------------------------------------------------

worker_3 <-read.delim("HCCP_Work_3rd.txt", header = T, fileEncoding="utf8")

worker_3 %>% 
  rename_all(tolower)->worker_3

worker_3 %>% 
  select(w3_id1, w3_id3, w3_id4, w3_ind1, w3_team, w3_posit, w301_01, w301_02, w303_02, w3_sex, w3_birthy, w3_marr, w3_edu, w306_01, w306_02, w306_03, w306_04, w306_05, w306_06, w306_07, w306_08, w311_01, w311_02, w311_05, w311_06, w312, w329_01, w329_02, w329_03, w329_04, w333, w335, w336_01, w336_02, w337_01, w337_02) -> worker_3_se


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 1개인 graph > 변인이 연속형(continuous)인 경우 > historam
# ------------------------------------------------------------------------------

ggplot(worker_3_se)+
  geom_histogram(aes(x=w336_02))


ggplot(worker_3_se)+
  geom_histogram(aes(x=w336_02), binwidth=1)


ggplot(worker_3_se)+
  geom_histogram(aes(x=w336_02), binwidth=1, color="black", fill="blue", alpha=0.2)


ggplot(worker_3_se)+
  geom_histogram(aes(x=w336_02), binwidth=1, color="black", fill="skyblue", alpha=0.2)+
  labs(title="근로자의 주당 초과근로시간", x="주당 초과근로시간", y="빈도(명)")


ggplot(worker_3_se)+
  geom_histogram(aes(x=w336_02), binwidth=1, color="black", fill="skyblue", alpha=0.2)+
  labs(title="근로자의 주당 초과근로시간", x="주당 초과근로시간", y="빈도(명)")+
  theme_minimal()


worker_3_se %>% 
  filter(w336_02>=0) %>% 
  ggplot()+
  geom_histogram(aes(x=w336_02, y=..density..), binwidth=1, color="black", fill="skyblue", alpha=0.2)+
  labs(title="근로자의 주당 초과근로시간", x="주당 초과근로시간", y="빈도(명)")+
  theme_minimal()


worker_3_se %>% 
  filter(w336_02>=0 & w336_01>=0) %>% 
  mutate(whour=w336_01+w336_02) %>% 
  ggplot()+geom_histogram(aes(x=whour), binwidth=1, color="black", fill="skyblue", alpha=0.2)+
  labs(title="근로자의 주당 근로시간", x="주당 근로시간", y="빈도(명)")+
  theme_minimal()


worker_3_se %>% 
  filter(w336_02>=0 & w336_01>=0) %>% 
  mutate(whour=w336_01+w336_02) %>% 
  ggplot()+geom_histogram(aes(x=whour, y=..density..), binwidth=1, color="black", fill="skyblue", alpha=0.2)+
  labs(title="근로자의 주당 근로시간", x="주당 근로시간", y="density")+
  theme_minimal()


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 1개인 graph > 변인이 연속형(continuous)인 경우 > density curve
# ------------------------------------------------------------------------------

worker_3_se %>% 
  filter(w336_02>=0 & w336_01>=0) %>% 
  mutate(whour=w336_01+w336_02) %>% 
  ggplot()+geom_density(aes(x=whour), color="black", fill="skyblue", alpha=0.2)+
  labs(title="근로자의 주당 근로시간 ", x="주당 근로시간", y="density")+
  theme_minimal()


worker_3_se %>% 
  filter(w336_02>=0 & w336_01>=0) %>% 
  mutate(whour=w336_01+w336_02) %>% 
  ggplot()+
  geom_density(aes(x=whour), color="black", fill="skyblue", alpha=0.2)+
  geom_vline(aes(xintercept=mean(whour)), color="blue", linetype="dashed", size=1)+
  labs(title="근로자의 주당 근로시간 ", x="주당 근로시간", y="density")+
  theme_minimal()


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 1개인 graph > 변인이 연속형(continuous)인 경우 > histogram + density curve
# ------------------------------------------------------------------------------

worker_3_se %>% 
  filter(w336_02>=0 & w336_01>=0) %>% 
  mutate(whour=w336_01+w336_02) %>% 
  ggplot()+
  geom_density(aes(x=whour), linetype="dashed", fill="blue", alpha=0.1)+
  geom_histogram(aes(x=whour, y=..density..), binwidth=1, color="black", fill="skyblue", alpha=0.2)+
  labs(title="근로자의 주당 근로시간 비중", x="주당 근로시간", y="density")+
  theme_minimal()


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 1개인 graph > 변인이 연속형(continuous)인 경우 > 집단별 그래프 비교
# ------------------------------------------------------------------------------

str(worker_3_se$w3_sex)
worker_3_se %>% 
  count(w3_sex)


worker_3_se %>% 
  filter(w336_02>=0 & w336_01>=0) %>% 
  mutate(whour=w336_01+w336_02) %>% 
  filter(w3_sex>0) %>%
  mutate(gender=as_factor(w3_sex)) %>% 
  mutate(gender=fct_recode(gender, male="1", female="2"))-> worker_3_fa
str(worker_3_fa$gender)


worker_3_fa %>% 
  ggplot()+geom_histogram(aes(x=whour, fill=gender), position="dodge")


worker_3_fa %>% 
  ggplot()+geom_histogram(aes(x=whour, y=..density.., fill=gender), position="dodge", binwidth=1)


worker_3_fa %>% 
  ggplot()+geom_histogram(aes(x=whour, y=..density..), binwidth=1)+
  facet_wrap(~gender)


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 2개인 graph > Continuous X, Continuous Y > 산점도 goem_point
# ------------------------------------------------------------------------------

worker_3_fa %>% 
  filter(w337_02>0) %>% 
  ggplot()+geom_point(aes(x=whour, y=w337_02))


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 2개인 graph > Continuous X, Continuous Y > 선그래프 goem_smoth
# ------------------------------------------------------------------------------

worker_3_fa %>% 
  filter(w337_02>0) %>% 
  ggplot(aes(x=whour, y=w337_02))+geom_point()+geom_smooth(method='lm')


worker_3_fa %>% 
  mutate(industry=as_factor(w3_ind1)) %>% 
  mutate(industry=fct_recode(industry, maufacture="1", finance="2", service="3")) %>% 
  mutate(position=as_factor(w3_posit)) %>% 
  mutate(position=fct_recode(position, whitemanager="1",bluemanager="2", whiteworker="3", blueworker="4"))->worker3_final

worker3_final %>% 
  filter(w337_02>0) %>% 
  ggplot()+geom_point(aes(x=whour, y=w337_02, colour = industry))+geom_smooth(aes(x=whour, y=w337_02, colour=industry), method='lm')

worker3_final %>% 
  filter(w337_02>0) %>% 
  ggplot()+geom_point(aes(x=whour, y=w337_02, colour = position))+geom_smooth(aes(x=whour, y=w337_02, colour=position), method='lm')


worker3_final %>% 
  filter(w337_02>0) %>% 
  ggplot(aes(x=whour, y=w337_02))+geom_point()+geom_smooth(method='lm')+facet_wrap(~industry)

worker3_final %>% 
  filter(w337_02>0) %>% 
  ggplot(aes(x=whour, y=w337_02))+geom_point()+geom_smooth(method='lm')+facet_wrap(~position)+theme(legend.position="bottom")


worker3_final %>% 
  filter(w337_02>0) %>% 
  ggplot(aes(x=whour, y=w337_02))+geom_point()+geom_smooth(method='lm')+facet_grid(position~industry)


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 2개인 graph > Discrete X, Continuous Y > bar chart
# ------------------------------------------------------------------------------

#x변수 only, stat="count"
worker3_final %>% 
  filter(w337_02>0) %>% 
  ggplot(aes(x=position))+geom_bar(fill="skyblue", alpha=0.8)

#x,y변수, stat="identity"
worker3_final %>% 
  filter(w337_02>0) %>% 
  ggplot(aes(x=position, y=whour))+geom_bar(stat="identity", fill="skyblue", alpha=0.8)

#stat="identity"의 y축의 값이 sum값과 같음
worker3_final %>% 
  filter(w337_02>0) %>%
  group_by(position) %>% 
  summarise(sum=sum(whour))


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 2개인 graph > Discrete X, Continuous Y > stat_과 geom_의 관계
# ------------------------------------------------------------------------------

install.packages("Hmisc", dependencies=TRUE, repos="https://cran.rstudio.com")
library("Hmisc")


worker3_final %>% 
  ggplot(aes(x=position, y=whour))+
  geom_bar(
    stat="summary", 
    fun = "mean", 
    fill="red", 
    alpha=0.3)+
  geom_errorbar( 
    stat="summary", 
    fun.data = "mean_cl_normal", 
    colour="black")


worker3_final %>% 
  ggplot(aes(x=position, y=whour))+
  stat_summary(
    geom="bar", 
    fun = "mean", 
    fill="blue", 
    alpha=0.3)+
  stat_summary(
    geom="errorbar", 
    fun.data = "mean_cl_normal", 
    colour="black")


# ------------------------------------------------------------------------------
# 데이터 시각화 > 변인이 2개인 graph > Discrete X, Continuous Y > boxplot
# ------------------------------------------------------------------------------

worker3_final %>% 
  filter(w337_02>0) %>%
  ggplot(aes(x=position, y=w337_02))+
  geom_boxplot(
    fill="skyblue", 
    alpha=0.3)

