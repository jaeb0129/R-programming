pacman::p_load(tidyverse, tidymodels)
set.seed(1234)

tennis_big3_results <- read.csv('C:/Users/jaeb0/Desktop/baseball/sports_r/tennis_big3_results.csv') %>%
  as_tibble

tennis_big3_results %>% glimpse()

pacman::p_load(janitor)

tennis_big3_results %>%
  as_tibble(., .name_repair = janitor::make_clean_names)-> tennis_big3_results
# 열 이름이 전부 소문자로 바뀜

tennis_big3_results %>%
  mutate(surface = str_to_lower(surface),
        w_l = str_to_lower(w_l)) -> tennis_big3_results # 소문자 변환 / 열

tennis_big3_results %>%
  filter(surface %in% c('clay', 'grass', 'hard')) %>%
  group_by(player, surface) %>%
  summarise(wins = sum(w_l == 'w'),
            loses = sum(w_l == 'l'),
            wp = wins / (wins + loses),
            .groups = 'drop') %>%
  arrange(player, -wp)
# 교차표 혹은 분할표 테이블 작성

### 13.3 카이제곱 검정 with infer
tennis_big3_results %>%
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) # 'w l'은 승,패 외에 기권을 뜻함

tennis_big3_results %>%
  filter(player == 'Rafael Nadal',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>%
  specify(surface ~ w_l) %>% # 함수로 문제 특정/코트별 승패 기록 알아보려함
  hypothesise(null = 'independence') %>% # 코트별 승률에는 차이가 없다. 귀무가설 / 나달은 클레이 코트에서 강하다 대립가설
  calculate(stat = 'Chisq') # 27.4

# 그냥 chisq_test()
tennis_big3_results %>%
  filter(player == 'Roger Federer',
         surface %in% c('clay', 'grass', 'hard'),
         w_l %in% c('w', 'l')) %>%
  chisq_test(surface ~ w_l) # 귀무가설 세 코트 결과가 모두 같다.
# 대립가설 적어도 어느 한 코트 결과는 다르다
# 귀무가설 기각

tennis_big3_results %>%
  filter(player == 'Roger Federer',
         surface %in% c('grass', 'hard'),
         w_l %in% c('w', 'l')) %>%
  chisq_test(surface ~ w_l)
# 귀무가설 채택 / 잔디코트에서 강하다고 보기 힘듬