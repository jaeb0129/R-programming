# 패키지 설치
# install.packages("devtools")
library(devtools)
# install_github('regbook/regbook')
library(regbook)

data(hweight)
hweight
class(hweight)

str(hweight)

hist(hweight$weight)
hist(hweight$height)

male <- subset(hweight, gender == 'M')
female <- subset(hweight, gender == 'F')

mean(male$height)
var(male$height)

###### 일표본 t검정(one sample t-test)
t.test(male$height, mu = 170) # 귀무가설 기각 / 170cm보다 크다

# 남자 평균 키가 170cm보다 큰가?
t.test(male$height, mu = 170, alternative = 'greater') # 귀무가설 채택 / 170cm 보다 크다

# 남자 평균 키가 170cm보다 작은가?
t.test(male$height, mu = 170, alternative = 'less') # 귀무가설 기각 / 170cm보다 작지 않다


###### 남자와 여자 키의 평균은 차이가 있는가?(독립표본 t검정)
# 두 표본의 분산이 다른지 검점(F검정)
var(male$height)
var(female$height)

var.test(male$height, female$height) # 분산이 다르다

# 두 표본의 분산이 다를 경우 / welch t검정 사용
t.test(male$height, female$height) # 남자와 여자의 키 평균은 다르다

##### 대응표본 t검정(paired t-test)
dat_M = c(117, 108, 105, 89, 101, 93, 96, 108, 108, 94, 93, 112, 92, 91, 100, 96, 120, 86, 96, 95)
dat_F = c(121, 101, 102, 114, 103, 105, 101, 131, 96, 109, 109, 113, 115, 94, 108, 96, 110, 112, 120, 100)

m = t.test(dat_M, dat_F, paired=T)
m
# 통계적으로 유의미한 차이가 있음(유의확률이 0.05보다 작다)
