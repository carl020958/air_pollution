setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('magrittr', 'dplyr', 'purrr', 'tidyr', 
               'ggplot2', 'gganimate', 'transformr',
               'sf', 
               'lubridate', 'stringr')

# ============
# 4. analysis
# ============

# -----------------------
# 4-1. NA check(176,799)
# -----------------------
na_df <- d_long %>% 
  mutate(지역_대분류 = stringr::str_sub(지역, 1, 2)) %>%
  filter(지역_대분류 == '서울') %>%
  filter(망 == '도시대기') %>% 
  filter(is.na(value)) %>% 
  mutate(측정월 = zoo::as.yearmon(lubridate::as_date(stringr::str_sub(측정일시, 1, 8))))

# table(na_df$측정소명)
# table(na_df$cate)
# table(na_df$측정월)

# ----------
# 4-2. corr
# ----------
corr_plot <- 
  d_month_long %>% 
  pivot_wider(names_from = cate, values_from = month) %>% 
  select_if(is.numeric) %>% 
  sjPlot::sjp.corr(data = ., na.deletion = "pairwise", sort.corr = T) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))

ggsave('corr_plot.jpeg', path = './gif', corr_plot)

# -------------------------------
# 4-3. Difference between region   
# -------------------------------
# 대도시 월 변화에 따른 대기오염물질 변화랑
city_plot <- 
  d_month_long %>% 
  filter(망 == '도시대기') %>% 
  filter(지역_대분류 %in% c('서울', '인천', '대전', '대구', '부산', '광주', '울산')) %>% 
  group_by(지역_대분류, cate, 측정일시_3) %>% 
  summarise(month_by_si_do = mean(month, na.rm = T)) %>% 
  ungroup() %>% 
  # filter(cate %in% c('PM10', 'PM25')) %>%
  mutate(cate = case_when(cate == 'PM10' ~ '미세먼지',
                          cate == 'PM25' ~ '초미세먼지',
                          cate == 'SO2' ~ '이황산가스',
                          cate == 'CO' ~ '일산화탄소',
                          cate == 'O3' ~ '오존',
                          cate == 'NO2' ~ '이산화질소')) %>% 
  ggplot(aes(측정일시_3, month_by_si_do, color = 지역_대분류)) + 
  geom_line() +
  facet_wrap(~ cate, ncol = 1, scales = "free_y") +
  theme_light(base_family = 'AppleGothic') +
  labs(x = "",
       y = "")
ggsave('city_plot.jpeg', path = './gif', city_plot)

# province 월 변화에 따른 대기오염물질 변화랑
province_plot <- 
  d_month_long %>% 
  filter(망 == '도시대기') %>% 
  filter(지역_대분류 %in% c('강원', '경기', '경남', '경북', '전남', '전북', '충남', '충북', '제주')) %>% 
  group_by(지역_대분류, cate, 측정일시_3) %>% 
  summarise(month_by_si_do = mean(month, na.rm = T)) %>% 
  ungroup() %>% 
  # filter(cate %in% c('PM10', 'PM25')) %>%
  mutate(cate = case_when(cate == 'PM10' ~ '미세먼지',
                          cate == 'PM25' ~ '초미세먼지',
                          cate == 'SO2' ~ '이황산가스',
                          cate == 'CO' ~ '일산화탄소',
                          cate == 'O3' ~ '오존',
                          cate == 'NO2' ~ '이산화질소')) %>% 
  ggplot(aes(측정일시_3, month_by_si_do, color = 지역_대분류)) + 
  geom_line() +
  facet_wrap(~ cate, ncol = 1, scales = "free_y") +
  theme_light(base_family = 'AppleGothic') +
  labs(x = "",
       y = "")
ggsave('province_plot.jpeg', path = './gif', province_plot)

# 지역 별 대기오염물질 차이 시각화
province_month_plot <-
  d_month_long %>%
  filter(망 == '도시대기') %>%
  filter(지역_대분류 %in% c('강원', '경기', '경남', '경북', '전남', '전북', '충남', '충북', '제주')) %>%
  group_by(지역_대분류, cate, 측정일시_3) %>%
  summarise(month_by_si_do = mean(month, na.rm = T)) %>%
  ungroup() %>%
  # filter(cate == "PM10") %>%
  ggplot(aes(지역_대분류, month_by_si_do, color = 지역_대분류)) +
  geom_boxplot() +
  facet_wrap(~ cate, ncol = 2, scales = "free_y") +
  theme_light(base_family = "AppleGothic") +
  labs(x = "",
       y = "")

ggsave('province_month_plot.jpeg', path = './gif', province_month_plot)

#도시 별 대기오염물질 차이 시각화
city_month_plot <-
  d_month_long %>%
  filter(망 == '도시대기') %>%
  filter(지역_대분류 %in% c('서울', '인천', '대전', '대구', '부산', '광주', '울산')) %>%
  group_by(지역_대분류, cate, 측정일시_3) %>%
  summarise(month_by_si_do = mean(month, na.rm = T)) %>%
  ungroup() %>%
  # filter(cate == "PM10") %>%
  ggplot(aes(지역_대분류, month_by_si_do, color = 지역_대분류)) +
  geom_boxplot() +
  facet_wrap(~ cate, ncol = 2, scales = "free_y") +
  theme_light(base_family = "AppleGothic") +
  labs(x = "",
       y = "")

ggsave('city_month_plot.jpeg', path = './gif', city_month_plot)


# -----------
# 4-4. ANOVA
# -----------
# 지역 & 대기오염 물질 별 anova 분석(nest)
aov_province_df <- 
  d_month_long %>% 
  filter(망 == '도시대기') %>% 
  filter(지역_대분류 %in% c('강원', '경기', '경남', '경북', '전남', '전북', '충남', '충북', '제주')) %>%
  group_by(지역_대분류, cate, 측정일시_3) %>% 
  summarise(month_by_si_do = mean(month, na.rm = T)) %>% 
  ungroup() %>% 
  group_nest(cate) %>% 
  # filter(cate == "PM25") %>% 
  mutate(aov = map(data, ~ broom::tidy(aov(month_by_si_do ~ 지역_대분류, data = .x)))) %>% 
  unnest(aov) %>% 
  select(-data)

# 그림 4
DT::datatable(aov_province_df, options = list(pageLength = 12))

# 도시 & 대기오염 물질 별 anova 분석(nest)
aov_city_df <- 
  d_month_long %>% 
  filter(망 == '도시대기') %>% 
  filter(지역_대분류 %in% c('서울', '인천', '대전', '대구', '부산', '광주', '울산')) %>% 
  group_by(지역_대분류, cate, 측정일시_3) %>% 
  summarise(month_by_si_do = mean(month, na.rm = T)) %>% 
  ungroup() %>% 
  group_nest(cate) %>% 
  # filter(cate == "PM25") %>% 
  mutate(aov = map(data, ~ broom::tidy(aov(month_by_si_do ~ 지역_대분류, data = .x)))) %>% 
  unnest(aov) %>% 
  select(-data)

# 그림 5
DT::datatable(aov_city_df, options = list(pageLength = 12))

# 지역 & 대기오염 물질 별 사후검정(tukey.HSD)(nest)
tukey.hsd_province_df <- 
  d_month_long %>% 
  filter(망 == '도시대기') %>% 
  filter(지역_대분류 %in% c('강원', '경기', '경남', '경북', '전남', '전북', '충남', '충북', '제주')) %>%
  group_by(지역_대분류, cate, 측정일시_3) %>% 
  summarise(month_by_si_do = mean(month, na.rm = T)) %>% 
  ungroup() %>% 
  group_nest(cate) %>% 
  mutate(aov = map(data, ~ aov(month_by_si_do ~ 지역_대분류, data = .x))) %>% 
  mutate(tukey.hsd = map(aov, ~ broom::tidy(TukeyHSD(.x)))) %>% 
  unnest(tukey.hsd) %>% 
  select(-c(data, aov))

# 지역 & 대기오염 물질 별 사후검정(tukey.HSD)(nest)
tukey.hsd_city_df <- 
  d_month_long %>% 
  filter(망 == '도시대기') %>% 
  filter(지역_대분류 %in% c('서울', '인천', '대전', '대구', '부산', '광주', '울산')) %>% 
  group_by(지역_대분류, cate, 측정일시_3) %>% 
  summarise(month_by_si_do = mean(month, na.rm = T)) %>% 
  ungroup() %>% 
  group_nest(cate) %>% 
  mutate(aov = map(data, ~ aov(month_by_si_do ~ 지역_대분류, data = .x))) %>% 
  filter(!cate %in% c('PM10', 'PM25')) %>% 
  mutate(tukey.hsd = map(aov, ~ broom::tidy(TukeyHSD(.x)))) %>% 
  unnest(tukey.hsd) %>% 
  select(-c(data, aov))


# ----------------------------------------------
# 4-5. Comparison between before & after corona
# ----------------------------------------------
#2020년과 2019년 비교(3~12월)
paried_df <- 
  d_month_long %>% 
  filter(망 == '도시대기') %>% 
  group_by(지역_대분류, cate, 측정일시_3) %>% 
  summarise(month_by_si_do = mean(month, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(year_month = lubridate::format_ISO8601(측정일시_3, precision = 'ym'),
         month = str_sub(year_month, 6, 7)) %>% 
  filter(!month %in% c('01', '02')) %>% 
  filter(측정일시_3 >= "2019-01-01") %>% 
  mutate(corona = case_when(측정일시_3 >= '2020-03-01' ~ "코로나_이후",
                                 T ~ "코로나_이전")) %>% 
  group_nest(cate) %>% 
  mutate(t.test = map(data, ~ broom::tidy(t.test(month_by_si_do ~ corona, data = .x, var.equal = T, paired = T)))) %>% 
  unnest(t.test) %>% 
  mutate(across(where(is.numeric), round, 7)) %>% 
  select(-data)

DT::datatable(paried_df)
