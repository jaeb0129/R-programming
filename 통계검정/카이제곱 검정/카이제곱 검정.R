#  chisq-test
# install.packages('vcd')
# install.packages('gmodels')
library(gmodels)
library(vcd)
 
str(Arthritis)
attach(Arthritis)

CrossTable(Treatment, Improved,
           expected = TRUE,
           chisq = TRUE)
# 귀무가설: 치료 여부에 따른 개선 결과가 같다. (치료 여부와 개선 결과가 연관이 없다)
# 대립가설: 치료 여부에 따른 개선 결과가 다르다. (치료 여부와 개선 결과가 연관이 있다)
# p_value = 0.001462643, 유의 수준 5%에서 유의하다. 치료 여부와 개선 결과가 연관이 있다.

