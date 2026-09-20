# ==============================================================================
# 03장. 데이터 전처리
# 원본: 03-literature.Rmd
# R Markdown의 R 코드 청크와 코드 주석을 추출한 강의용 스크립트입니다.
# 예제 간 객체·패키지 의존성이 있으므로 위에서부터 절별로 실행하세요.
# ==============================================================================

# ------------------------------------------------------------------------------
# 데이터 전처리 > 들어가며 > 패키지 준비하기
# ------------------------------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)


# ------------------------------------------------------------------------------
# 데이터 전처리 > 들어가며 > 작업공간 설정하기
# ------------------------------------------------------------------------------

getwd()
setwd("C:\\Users\\Owner\\Documents\\new") 


# ------------------------------------------------------------------------------
# 데이터 전처리 > 들어가며 > 데이터 불러오기
# ------------------------------------------------------------------------------

data_csv <- read.table("data_csv.csv", header = T, sep=",") 
data_spss <- read.spss("data_sav.sav", use.value.labels=T, to.data.frame=T)


# ------------------------------------------------------------------------------
# 데이터 전처리 > dplyr 패키지의 이해
# ------------------------------------------------------------------------------

library(tidyverse)


# ------------------------------------------------------------------------------
# 데이터 전처리 > dplyr 패키지의 이해 > chain operator
# ------------------------------------------------------------------------------

x<-c(30, 20, 10, 0)
sqrt(mean(abs(x)))


x  %>% 
  abs() %>% 
  mean() %>% 
  sqrt()


# ------------------------------------------------------------------------------
# 데이터 전처리 > dplyr의 주요 기능 > filter
# ------------------------------------------------------------------------------

install.packages("nycflights13", repos = "http://cran.us.r-project.org")
library(nycflights13)
head(flights) # head 자료 수개를 보여줌
flight_df <-data.frame(flights)#data frame으로 변환
str(flight_df)


##month=2인 자료만 필터링
flight_df %>% 
  filter(month==2)
##month=2 or day=1 자료만 필터링
flight_df %>% 
  filter(month==2 | day==1)   #shift+\
##month=2 and day=1 자료만 필터링
flight_df %>% 
  filter(month==2, day==1) #쉼표나 & 모두 사용 가능

##month=2가 아닌 자료만 필터링
flight_df %>% 
  filter(month!=2)  #느낌표는 not의 의미
##month가 5이상인 자료만 필터링
flight_df %>% 
  filter(month >=5) 
##month가 5, 7, 10인 자료만(복수의 조건) 필터링
flight_df %>% 
  filter(month %in% c(5,7,10))
##na 값 표시 또는 제거 해서 필터링
flight_df %>% 
  filter(is.na(month)) #na인 row만 표시
flight_df %>% 
  filter(!is.na(month)) #na가 아닌 row만 표시


flight_df %>% 
  filter(month %in% c(5,7,10)) -> filter_df2

summary(filter_df2$month)


# ------------------------------------------------------------------------------
# 데이터 전처리 > dplyr의 주요 기능 > select
# ------------------------------------------------------------------------------

#month, day 변수만 선택
flight_df %>%
  select(month, day)
#year에서 day까지의 변수만 선택
flight_df %>%
  select(year:day) 
#year에서 day까지의 변수만 제외해서 선택
flight_df %>%
  select(! year:day)
#복수의 변수를 제외하고 싶은 경우 -c()를 사용
flight_df %>%
  select(-c(year, month)) 
#dep이라는 단어로 시작하는 변수들 선택
flight_df %>% 
  select(starts_with("dep"))
#time이라는 단어로 끝나는 변수들 선택
flight_df %>% 
  select(ends_with("time"))
#time 또는 delay 중 하나라도 포함되어 있는 변수들 선택
flight_df %>% 
  select(one_of(c("time", "delay")))


# ------------------------------------------------------------------------------
# 데이터 전처리 > dplyr의 주요 기능 > arrange
# ------------------------------------------------------------------------------


##month, day 순으로 오름차순
flight_df %>%
  arrange(month, day) 
##month는 오름차순, day는 내림차순
flight_df %>%
  arrange(month, -day) 
flight_df %>%
  arrange(month, desc(day))
##month, day 순으로 내림차순
flight_df %>%
  arrange(-month, -day) 
flight_df %>%
  arrange(desc(month), desc(day))


# ------------------------------------------------------------------------------
# 데이터 전처리 > dplyr의 주요 기능 > mutate
# ------------------------------------------------------------------------------

#평균 또는 ratio로 연산하여 새로운 변수를 생성
flight_df %>%
  mutate(mean_distance=distance/hour, 
         ratio_delay=arr_delay/(hour*60+minute)) -> flght_df_mutate1
#ifelse를 활용하여 category변수 생성, ifelse(조건, 조건이 true일때, 조건이 false)
flight_df %>%
  mutate(arr_delay_group=ifelse(arr_delay>0, "delay", "no delay")) ->flight_df_mutate
#사용한 변수를 삭제하고 새로운 변수만 남기는 경우
flight_df %>% 
  transmute(total_min=hour*60+minute) -> flight_df_mutate2


# ------------------------------------------------------------------------------
# 데이터 전처리 > dplyr의 주요 기능 > group_by와 summarise
# ------------------------------------------------------------------------------

flight_df_mutate %>% 
  count(arr_delay_group)


flight_df_mutate %>% 
  group_by(arr_delay_group) %>% 
  summarise(max_s=max(arr_delay),
            min_s=min(arr_delay), 
            mean_s=mean(arr_delay), 
            med_s=median(arr_delay), 
            per25_s=quantile(arr_delay, 0,25))


flight_df_mutate %>%
  filter(!is.na(arr_delay)) %>%  #na가 아닌 row만 표시
  mutate(arr_delay_group=ifelse(arr_delay>0, "delay", "no delay")) %>% 
  group_by(arr_delay_group) %>% 
  summarise(max_s=max(arr_delay),
            min_s=min(arr_delay), 
            mean_s=mean(arr_delay), 
            med_s=median(arr_delay), 
            per20_s=quantile(arr_delay, 0,25),
            n_s=n())->final
final


# ------------------------------------------------------------------------------
# 데이터 전처리 > 데이터 결합하기 > bind_cols와 bind_rows로 데이터 결합하기
# ------------------------------------------------------------------------------

#bts1 dataframe 만들기
btsname <-c("RM", "Jin", "Suga","Jhope", "Jimin", "V", "JK")
btsyear <-c(1994, 1992, 1993, 1994, 1995, 1995, 1997)
btsposition <-c("rap", "vocal", "rap", "rap", "vocal", "vocal","vocal")
bts1 <-data.frame(btsname, btsyear, btsposition, stringsAsFactors = FALSE)
#bts2 dataframe 만들기
soloSong <-c("her", "epiphany", "seesaw", "justDance", "serendipity", "singularity", "euphoria")
bts2<-data.frame(soloSong)
#bts1과 bts2를 횡으로 결합하기 (변수추가)
bind_cols(bts1, bts2)


singularity <-c(NA, "vocal", NA, NA, "vocal", "vocal", "vocal")
tear <-c("rap", NA, "rap", "rap", NA, NA, NA)
bts3<-data.frame(singularity)
bts4<-data.frame(tear)
bind_cols(bts1, bts2, bts3, bts4)->bts


army <-data.frame(btsname="army", btsyear=NA, btsposition=NA, soloSong=NA, singularity=NA, tear=NA)
bind_rows(bts, army)


army2 <-data.frame(btsname="army", btsyear=NA, btsposition=NA, soloSong=NA, singularity=NA, tear=NA, nations="worldwide")
bind_rows(bts, army2)


bind_rows(list(data1=bts, data2=army), .id="flag")


# ------------------------------------------------------------------------------
# 데이터 전처리 > 데이터 결합하기 > join으로 데이터 결합하기 > left_join : 첫번째 데이터를 기준으로 결합
# ------------------------------------------------------------------------------

#data1 만들기 
id <- c(1, 2, 3)
name <- c("RM", "jin", "suga")
solo <- c("her", "ephipany", "seesaw")
data1 <- data.frame(id, name, solo)
data1

#data2 만들기
id <- c(1,2,3,4,5,6,7)
name <- c("RM", "jin", "suga", "jhope", "jimin", "v", "jk")
solo <- c("her", "epiphany", "seesaw", "justDance", "serendipity", "singularity", "euphoria")
position <-c("rap", "vocal", "rap", "rap", "vocal", "vocal","vocal")
data2 <-data.frame(id, name, solo, position)
data2

#left_join
data1 %>% 
  left_join(data2, by= "id") ->left2
data1 %>% 
  left_join(data2, by="id", suffix=c("_data1", "data2")) -> left3



# ------------------------------------------------------------------------------
# 데이터 전처리 > 데이터 결합하기 > join으로 데이터 결합하기 > right_join :두번째 데이터를 기준으로 결합
# ------------------------------------------------------------------------------

data1 %>% 
  right_join(data2, by="id")


# ------------------------------------------------------------------------------
# 데이터 전처리 > 데이터 결합하기 > join으로 데이터 결합하기 > inner_join :첫번째 데이터와 두번째 데이터의 교집합 행만 결합
# ------------------------------------------------------------------------------

army <-data.frame(id=8, name="army", solo=NA)
bind_rows(data1, army)
data1
data1 %>% 
  inner_join(data2, by="id")


# ------------------------------------------------------------------------------
# 데이터 전처리 > 데이터 결합하기 > join으로 데이터 결합하기 > full_join : 두 데이터의 모든 행을 결합
# ------------------------------------------------------------------------------

data1 %>% 
  full_join(data2, by="id")


# ------------------------------------------------------------------------------
# 데이터 전처리 > 데이터를 타이디하게 만들기 : pivot_longer와 pivot_wider
# ------------------------------------------------------------------------------

album <- c("youngForever", "youngForever", "youngForever", "youngForever", "wings", "wings", "wings", "wings", "youNeverWalkAlone", "youNeverWalkAlone", "youNeverWalkAlone", "loveYourself_Her", "loveYourself_Her", "loveYourself_Her", "loveYourself_Tear", "loveYourself_Tear", "loveYourself_Answer", "loveYourself_Answer")
year <-c(2016,2017,2018,2019,2016,2017,2018,2019,2017,2018,2019,2017,2018,2019,2018, 2019, 2018, 2019)
sales <-c(368369, 89761, 129838, 66344, 751301, 93132, 129790, 66770, 768402, 111838, 63580, 1493443, 333445, 133534, 1849537, 122742, 2197808, 154676)
btsAlbumSales_long<-data.frame(album, year, sales)


album2 <- c("youngForever", "wings", "youNeverWalkAlone", "loveYourself_Her", "loveYourself_Tear", "loveYourself_Answer")
year2016 <-c(368369, 751301, NA, NA, NA, NA)
year2017 <-c(89761, 93132, 768492, 1493443, NA, NA)
year2018 <-c(129838, 129790, 111838, 333445, 1849537, 2197808)
year2019 <-c(66344, 66770, 63580, 133534, 122742, 154676)
btsAlbumSales_wide <-data.frame(album2, year2016, year2017, year2018, year2019)


btsAlbumSales_wide
btsAlbumSales_long


# ------------------------------------------------------------------------------
# 데이터 전처리 > 데이터를 타이디하게 만들기 : pivot_longer와 pivot_wider > 길게 만들기 : pivot_longer 활용하기
# ------------------------------------------------------------------------------

btsAlbumSales_wide %>% 
  tidyr::pivot_longer(col=year2016:year2019, names_to="year", values_to="sales") ->long
long


btsAlbumSales_wide %>% 
  pivot_longer(col=year2016:year2019, names_to="year", values_to="sales") %>% 
  mutate(year=str_replace(year, "year", ""))->long_replace
long_replace


# ------------------------------------------------------------------------------
# 데이터 전처리 > 데이터를 타이디하게 만들기 : pivot_longer와 pivot_wider > 넓게 만들기 : pivot_wider 활용하기
# ------------------------------------------------------------------------------

btsAlbumSales_long %>% 
  pivot_wider(names_from=year, values_from = sales)->wide
wide


# ------------------------------------------------------------------------------
# 데이터 전처리 > 패널데이터를 활용한 데이터 전처리 실습 > 데이터의 불러오기(import)
# ------------------------------------------------------------------------------

company_3 <-read.delim("HCCP_Head_3th.txt", header = T)
company_4 <-read.delim("HCCP_Head_4th.txt", header = T)

worker_3 <-read.delim("HCCP_Work_3rd.txt", header = T)
worker_4 <-read.delim("HCCP_Work_4th.txt", header = T)


company_4 <-read.delim("HCCP_Head_4th.txt", header = T, fileEncoding="utf16")


worker_3 %>% 
  count(W3_id1)
worker_3 %>% 
  count(W3_id2)
worker_3 %>% 
  count(W3_id3)


worker_3 %>% 
  rename_all(tolower)->worker_3
worker_4 %>% 
  rename_all(tolower)->worker_4
company_3 %>% 
  rename_all(tolower)->company_3
company_4 %>% 
  rename_all(tolower)->company_4


worker_3 %>% 
  count(w3_id1)
worker_3 %>% 
  count(w3_id2)
worker_3 %>% 
  count(w3_id3)
worker_3 %>% 
  count(w3_id4)


# ------------------------------------------------------------------------------
# 데이터 전처리 > 패널데이터를 활용한 데이터 전처리 실습 > 필요한 변수만 선택(select)
# ------------------------------------------------------------------------------

worker_3 %>% 
  select(w3_id1, w3_id3, w3_id4, w3_ind1, w3_team, w3_posit, w301_01, w301_02, w303_02, w3_sex, w3_birthy, w3_marr, w3_edu, w306_01, w306_02, w306_03, w306_04, w306_05, w306_06, w306_07, w306_08, w311_01, w311_02, w311_05, w311_06, w312, w329_01, w329_02, w329_03, w329_04, w333, w335, w336_01, w336_02, w337_01, w337_02) -> worker_3_se

worker_4 %>% 
  select(w4_id1, w4_id3, w4_id4, w4_ind1, w4_team, w4_posit, w401_01, w401_02, w403_02, w4_sex, w4_birthy, w4_marr, w4_edu, w406_01, w406_02, w406_03, w406_04, w406_05, w406_06, w406_07, w406_08, w411_01, w411_02, w411_05, w411_06, w412, w429_01, w429_02, w429_03, w429_04, w433, w435, w436_01, w436_02, w437_01, w437_02) -> worker_4_se


company_3 %>% 
  select(c3_id1, c3_ind1, c3_ksic1, c3_scale, c3a02_07, c3b02_01_01, c3b02_01_02, c3b02_01_03, c3b02_01_04, c3b02_02_01, c3b02_02_02, c3b02_02_05, c3b02_03_02, c3b02_03_03, c3b02_03_04, c3b02_05_01, c3b02_05_02, c3c01_01, c3c01_01_01, c3c01_02, c3c01_03, c3c01_06_01, c3c01_06_02, c3c02_03_01, c3c02_03_02, c3c02_03_03, c3c02_03_04, c3c02_03_05, c3c04_02_01, c3c04_02_02, c3c04_02_03)->company_3_se

company_4 %>% 
  select(c4_id1, c4_ind1, c4_ksic1, c4_scale, c4a02_07, c4b02_01_01, c4b02_01_02, c4b02_01_03, c4b02_01_04, c4b02_02_01, c4b02_02_02, c4b02_02_05, c4b02_03_02, c4b02_03_03, c4b02_03_04, c4b02_05_01, c4b02_05_02, c4c01_01, c4c01_01_01, c4c01_02, c4c01_03, c4c01_06_01, c4c01_06_02, c4c02_03_01, c4c02_03_02, c4c02_03_03, c4c02_03_04, c4c02_03_05, a_3rd)->company_4_se


# ------------------------------------------------------------------------------
# 데이터 전처리 > 패널데이터를 활용한 데이터 전처리 실습 > 기업 데이터 결합(one-to-one match)
# ------------------------------------------------------------------------------

company_3_se %>% 
  count(c3_id1) %>% 
  filter(n>1)

company_4_se %>% 
  count(c4_id1) %>% 
  filter(n>1)


#데이터 결합
company_3_se %>% 
  full_join(company_4_se, c("c3_id1"="c4_id1")) -> company_full

#key 변수 확인
company_full %>% 
  count(c3_id1)

#연도별 기업 현황 확인
company_full %>% 
  group_by(c3_ind1, c4_ind1) %>% 
  summarise(count=n())
  count(c3_ind1, c4_id1)


company_3_se %>% 
  inner_join(company_4_se, c("c3_id1"="c4_id1")) -> company_inner

company_inner %>% 
  group_by(c3_ind1, c4_ind1) %>% 
  summarise(count=n())



# ------------------------------------------------------------------------------
# 데이터 전처리 > 패널데이터를 활용한 데이터 전처리 실습 > 근로자-기업 데이터 결합(one-to-many match)
# ------------------------------------------------------------------------------


worker_3_se %>%
  left_join(company_3_se, c("w3_id1"="c3_id1")) ->worker_company_inner

worker_company_inner %>% 
  group_by(w3_id1, w3_id4) %>% 
  summarise(count=n())

worker_company_inner %>% 
  count(w3_id1)

worker_company_inner %>% 
  group_by(c3_ind1, w3_ind1) %>% 
  summarise(count=n())

