library(devtools)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_151") # 각자 맞게 추가
library(rJava)
library(KoNLP)

useNIADic()
mergeUserDic(data.frame(c("이모티콘"), c("ncn")))
# 사전에 단어 추가 / 특히 대화방, 리뷰는 많이 추가해야..
             
setwd("C:/Users/jaeb0/Desktop/워드클라우드")
getwd()
text1 <- readLines('랜더스방.txt')
text1
text2 <- extractNoun(text1)
text3 <- unlist(text2)
text4 <- gsub('[ㄱ-ㅎ]','', text3) # 전처리 / 제거하거 싶은 단어 추가 / 특히 대화방, 리뷰는 많이 추가해야
text4 <- gsub('(ㅜ|ㅠ)+','', text4) 
text4 <- gsub('\\(이모티콘\\)','', text4) 
text4 <- gsub('^','', text4) 


text5 <- text4[nchar(text4) < 5]# nchar: 각각의 벡터 요소의 글자 길이 알 수 있음 1초과 5미만 단어만 추출
text5 <- text5[nchar(text5) > 1]

text6 <- sort(table(text5), decreasing = TRUE) # table함수로 요소 빈도 체크 / sort() 오름차순, decreasing()내림차순
text7 <- head(text6, 300) # 상위 300개
text7
# install.packages("wordcloud2")
library(wordcloud2)

wordcloud2(text7, size = 0.7)
