library(Lahman)
library(dplyr)

# 팀 승률, 팀 타율 모델과 팀 승률, 팀 방어율 모델
rec <- subset(Teams, yearID == 2014)
rec$wp <- rec$W / rec$G
rec$avg <- rec$H / rec$AB
avg_model <-lm(wp~avg, rec)
ERA_model <- lm(wp~ERA, rec)

summary(avg_model)
# 표준오차 0.05774
summary(ERA_model)
# 표준오차 0.04128
# 방어율 모델이 타율 모델의 표준오차보다 작아서 팀승률 예측에 더 정확한 결과 얻을 수 있음.

a <- subset(Teams, yearID == 2015)
attach(a)
a$avg <- H/AB
a$obp <-(H+BB+HBP)/(AB+BB+HBP+SF)
a$r_g <-R/162
lm(r_g~avg, a)

plot(a$avg, a$r_g)
abline(lm(r_g~avg,a))
# y = 21.254- 1.158 * x / 1푼당 팀득점 0.21점 , 162게임 35득점
# 3.5WAR *700백만 달러 = 2,450만 달러(당시 1WAR=700백만 달러)
# 이 계산법은 투수력과 수비력은 기존 연도와 변함이 없다는 가정

# 팀득점 예측을 위해 만든 추정모델 해석
# 실시간으로 업데이트되지 않아 현재 데이터가 모집단을 대표한다고 하기 어려운 경우,
# 모집단을 추정하는데 심각한 예측 오류가 없을 정도의 데이터를 빅데이터에서 임의로 추출해서 편향없이 예측하는 것도 좋은 방법.

# install.packages("pwr")
library(pwr)
pwr.f2.test(1,NULL,0.01, 0.05, 0.95) # 1은 모델에서 독립변수인 팀타율 기울기 하나의 모수 의미, u=모수의 개수, v=최소 자유도
# u+v+1=최소 표본 크기=1302
# pwr패키지는 선형회귀분서게 필요한 최소관측량 제시

a <- subset(Teams, lgID=='AL'|lgID=="NL")
b <- sample(1:nrow(a),1302)
c<-a[b,]
c$avg <- c$H/c$AB
c$r_g <- c$R/c$G
d<-lm(r_g~avg,data=c)
summary(d)
# 회귀모형은 유의하다 / 맨밑 P값값
# 회귀계수 P값 팀타율 계수는 유의하다
# 팀타율과 게임당 팀득점 간에 양의 관계가 없다:H0
# H0이 현재 갖고 있는 데이터와 일치할 가능성 약 0.1% / 일치안할 가능성 99.9%

e<- data.frame(avg=0.270)
predict(d, e, level=0.95, interval="confidence")
# 점추정의 한계를 극복하기 위해 구간추정 사용
# 2할7푼을 갖고 있는 팀으로 구성된 표본(평균 타율이 2할 7푼인 팀)에는 모수의 게임당 평균득점이 [4.78, 4.84]
# 구간에 있을 것으로 95%확신하나, 5%의 불확실성도 있음
predict(d, e, level=0.95, interval="predict") # 예측구간/ 팀 타율이 2할7푼인 개별 팀이 기록할 것으로 예측되는 팀 득점 구간 찾기
