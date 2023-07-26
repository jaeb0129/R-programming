dt_jk = read.csv('C:/Users/jaeb0/Desktop/학교/생명과학빅데이터분석/test_final_jk.csv')
dt_m20 = read.csv('C:/Users/jaeb0/Desktop/학교/생명과학빅데이터분석/test_final_m20.csv')

library(dplyr)

## 암, 당뇨 환자대조군 연구분석;


# 당뇨;
dt_dm = dt_m20 %>%
  filter(substr(SICK_SYM1,1,3) %in% c('E10','E11','E12','E13','E14')) %>%
  mutate(DM = 1) %>%
  mutate(STD_YYYY = substr(MDCARE_STRT_DT,1,4)) %>% 
  arrange(RN_INDI, MDCARE_STRT_DT) %>% 
  select(RN_INDI, STD_YYYY, DM)
# 암;
  dt_cancer = dt_m20 %>%
  filter(substr(SICK_SYM1,1,1) %in% c('C')) %>%
  mutate(cancer = 1) %>%
  mutate(STD_YYYY = substr(MDCARE_STRT_DT,1,4)) %>%   
  arrange(RN_INDI, MDCARE_STRT_DT) %>%   
  select(RN_INDI, STD_YYYY, cancer)


dt_cancer_2013 = distinct(dt_cancer, RN_INDI,.keep_all = T) %>% 
  filter(STD_YYYY==2013)

dt_no_cancer = anti_join(
  distinct(dt_m20 %>% 
             mutate(STD_YYYY=substr(MDCARE_STRT_DT,1,4)) %>% 
             filter(STD_YYYY==2013) %>% 
             mutate(cancer = 0) %>% 
             select(RN_INDI,STD_YYYY,cancer), RN_INDI, .keep_all=T), dt_cancer, by='RN_INDI'
)

dt_no_cancer$STD_YYYY = as.numeric(dt_no_cancer$STD_YYYY)## dt_no_cancer의 STD_YYYY의 데이터 타입과 dt_cancer_2013 의 STD_YYYY의 데이터 타입이 달라서 맞추는 작업
dt_cancer_2013$STD_YYYY = as.numeric(dt_cancer_2013$STD_YYYY)

dt_all = bind_rows(dt_cancer_2013, dt_no_cancer)
# 2013년 암전체 / 당뇨병 걸린
dt_final = left_join(dt_all, distinct(dt_dm,RN_INDI,.keep_all=T), by='RN_INDI') 
# 2013년 암전체에서 당뇨병 안걸린
dt_final[is.na(dt_final$DM),'DM'] = 0
# dt_final은 암, 당뇨병 다 체크해놓은 자료

rst = glm(cancer ~ DM, family=binomial, data=dt_final)
summary(rst)
exp(rst$coefficients)
exp(cbind(OR=coef(rst), confint(rst, level = .95)))

# p_value=0.362, 유의수준 5%에서 귀무가설 기각
# 즉 암과 당뇨병은 관련이 없다.
# 만약 관련이 있다면 당뇨병이 있는 환자는 없는 환자에 비해
# 암 발생률이 약 2.74배 높고 신뢰구간이 [0.14, 17.3]이다.
# 95% 신뢰구간 = [0.14, 17.3]

# 유의한 경우 / 당뇨병에 걸린 경우 걸리지 않은 경우에 비해 암에 걸릴 확률이 2.7배 더 높다.



#############################################################;
## 자격 정보 연계하여 성별을 추가한 모형;

dt_final_jk = left_join(
  dt_final, distinct(dt_jk,RN_INDI,.keep_all = T) %>% select(RN_INDI,SEX), by=c('RN_INDI'))

rst2 = glm(cancer ~ DM+SEX, family=binomial, data=dt_final_jk)
summary(rst2)
exp(rst2$coefficients)
