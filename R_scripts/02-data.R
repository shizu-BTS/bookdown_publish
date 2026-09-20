# ==============================================================================
# 02장. R 자료 구조의 이해
# 원본: 02-data.Rmd
# R Markdown의 R 코드 청크와 코드 주석을 추출한 강의용 스크립트입니다.
# 예제 간 객체·패키지 의존성이 있으므로 위에서부터 절별로 실행하세요.
# ==============================================================================

# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > 명령어의 구조와 자료 입력
# ------------------------------------------------------------------------------

a<-2 # a라는 객체에 2를 삽입
a #a 객체를 출력
a<-3 
a
a<-c(3,4,5)
a


# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > R에서 쓰이는 자료의 구조 > 스칼라 scala
# ------------------------------------------------------------------------------

scalar<-1
scalar
scalar<-"bts"
scalar


# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > R에서 쓰이는 자료의 구조 > 벡터 vector
# ------------------------------------------------------------------------------

vector <-c(1,2,3)
vector
vector <-c("v", "rm", "suga")
vector


# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > R에서 쓰이는 자료의 구조 > 매트릭스 matrix
# ------------------------------------------------------------------------------

matrix <-matrix(c(1,2,3,4,5,6), nrow=3)
matrix
matrix <-matrix(c(1,2,3,4,5,6), nrow=2)
matrix
matrix <-matrix(c(1:20), nrow=4, ncol=5, byrow=TRUE)
matrix


mat1 <-c(1:3)
mat2 <-c(4:6)
matrix1 <-rbind(mat1, mat2)  #rbind : row을 기준으로 종으로 붙이기
matrix1
matrix2 <-cbind(mat1, mat2)  #cbind : column을 기준으로 횡으로 붙이기
matrix2
matrix3<-c(mat1, mat2) #c()를 사용하면 벡터와 벡터를 하나의 차원으로 연결
matrix3


matrix2[1,2]
matrix2[1,] #첫번째 row의 모든 원소를 추출
matrix2[,1] #첫번째 col의 모든 원소를 추출
matrix2[c(1,2),] #1,2번째 row의 모든 원소를 추출
matrix2[1,2]=100 # 첫번째 행, 두 번째 열의 원소를 100으로 치환한다. 
matrix2


# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > R에서 쓰이는 자료의 구조 > 배열 array
# ------------------------------------------------------------------------------

matrix1<- matrix(c(1:9), nrow=3)
matrix1
matrix2<- matrix(c(10:18), nrow=3)
matrix3<- matrix(c(19:27), nrow=3)
matrix2
matrix3
array <-array(c(matrix1, matrix2, matrix3), dim=c(3,3,3))
array


# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > R에서 쓰이는 자료의 구조 > 데이터프레임 dataframe
# ------------------------------------------------------------------------------

btsname <-c("RM", "Jin", "Suga","Jhope", "Jimin", "V", "JK")
btsyear <-c(1994, 1992, 1993, 1994, 1995, 1995, 1997)
btsposition <-c("rap", "vocal", "rap", "rap", "vocal", "vocal","vocal")
bts <-data.frame(btsname, btsyear, btsposition, stringsAsFactors = FALSE)
bts  
str(bts)


# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > factor 변수
# ------------------------------------------------------------------------------

bts$btsposition <-factor(btsposition)
str(bts$btsposition)
levels(bts$btsposition)
bts$btsposition <-factor(btsposition, levels=c("vocal", "rap"))
str(bts$btsposition)
summary(bts$btsposition)


bts$btsposition <- as.numeric(bts$btsposition)
str(bts$btsposition)


# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > NA와 NULL
# ------------------------------------------------------------------------------

bts$btsposition <-c("rap", "vocal", "rap", "rap", "vocal", "vocal","vocal")
bts$btsposition <-factor(btsposition, levels=c("vocal", "rap"))


#btsyear 변수를 활용(computation)해서 age 변수를 새로 만든다
bts$age <- 2021-bts$btsyear+1
bts
bts[1,4] <-NA
bts
mean(bts$age)
mean(bts$age, na.rm=TRUE)


bts[1,4] <-28



# ------------------------------------------------------------------------------
# R 자료 구조의 이해 > R 내장함수를 활용한 기술통계량 산출
# ------------------------------------------------------------------------------

summary(bts)
str(bts)
table(bts$age)
#소숫점 두번째에서 반올림
round(prop.table(table(bts$age)),2)
#분할표를 백분율로 계산
round(prop.table(table(bts$age)),2)*100
#마진에 소계값을 계산
addmargins(table(bts$age))
# 변수 * 변수의 교차표를 산출
table(bts$age, bts$btsposition)
# 교차표의 셀별 비율 계산(열의 합을 1로)
prop.table(table(bts$age, bts$btsposition), margin=2)

