setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('magrittr', 'dplyr', 'purrr', 'tidyr', 
               'ggplot2', 'gganimate', 'transformr',
               'sf', 
               'lubridate', 'stringr')

# ===========
# 5. animate
# ===========

# -----------
# preprocess
# ----------
# 시도
# 인코딩 변환
si_do <- sf::st_read('./CTPRVN_202101/TL_SCCO_CTPRVN.shp') %>%
  #CP949 -> UTF-8로 인코딩 변환
  mutate(CTP_KOR_NM = iconv(CTP_KOR_NM,
                            from = 'CP949',
                            to = 'UTF-8',
                            sub = NA,
                            mark = TRUE,
                            toRaw = FALSE))


# shp 파일을 spaital 부분 추출 후, data, polygons(geometry) 부분으로 나누기
si_do_spatial <- as(si_do, 'Spatial')
si_do_data_df <- as.data.frame(si_do_spatial)
si_do_geometry_df <- fortify(si_do_spatial)

# data와 polygons을 연결하는 id 생성
si_do_data_df <- si_do_data_df %>%
  mutate(id = 1:nrow(si_do_data_df))

# 법정동코드 확인
nchar(unique(si_do_data_df$CTPRVN_CD))

# geometry의 id, integer로 변환
si_do_geometry_df <- si_do_geometry_df %>%
  mutate(id = as.integer(id))

str(si_do_data_df)
str(si_do_geometry_df)

#최종 그래프용 df
si_do_df <-  merge(si_do_data_df, si_do_geometry_df, by.x = 'id', by.y = 'id')
save(file = 'si_do.Rdata', si_do_df)

# 시군구
# 인코딩 변환
si_gun_gu <- sf::st_read('./SIG_202101/TL_SCCO_SIG.shp') %>%
  #CP949 -> UTF-8로 인코딩 변환
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM,
                            from = 'CP949',
                            to = 'UTF-8',
                            sub = NA,
                            mark = TRUE,
                            toRaw = FALSE))


# shp 파일을 spaital 부분 추출 후, data, polygons(geometry) 부분으로 나누기
si_gun_gu_spatial <- as(si_gun_gu, 'Spatial')
si_gun_gu_data_df <- as.data.frame(si_gun_gu_spatial)
si_gun_gu_geometry_df <- fortify(si_gun_gu_spatial)

# data와 polygons을 연결하는 id 생성
si_gun_gu_data_df <- si_gun_gu_data_df %>%
  mutate(id = 1:nrow(si_gun_gu_data_df))

# 법정동코드 확인
nchar(unique(si_gun_gu_data_df$SIG_CD))

# geometry의 id, integer로 변환
si_gun_gu_geometry_df <- si_gun_gu_geometry_df %>%
  mutate(id = as.integer(id))

str(si_gun_gu_data_df)
str(si_gun_gu_geometry_df)

#최종 그래프용 df
si_gun_gu_df <-  merge(si_gun_gu_data_df, si_gun_gu_geometry_df, by.x = 'id', by.y = 'id')
save(file = 'si_gun_gu.Rdata', si_gun_gu_df)


# ------------
# 전국 시각화 
# ------------
# out of memory
# # '도'와 '시' 이름을 그래프에 표시하기 위한 df
# si_do_name <- si_do_df %>% 
#   group_by(id, CTP_KOR_NM) %>%
#   summarise(long = mean(long, na.rm = T), lat = mean(lat, na.rm = T))
# 
# 
# si_do_df_v2 <- si_do_df %>% 
#   mutate(CTP_KOR_NM = case_when(CTP_KOR_NM == '강원도' ~ '강원',
#                                 CTP_KOR_NM == '경기도' ~ '경기',
#                                 CTP_KOR_NM == '경상남도' ~ '경남',
#                                 CTP_KOR_NM == '경상북도' ~ '경북',
#                                 CTP_KOR_NM == '광주광역시' ~ '광주',
#                                 CTP_KOR_NM == '대구광역시' ~ '대구',
#                                 CTP_KOR_NM == '대전광역시' ~ '대전',
#                                 CTP_KOR_NM == '부산광역시' ~ '부산',
#                                 CTP_KOR_NM == '서울특별시' ~ '서울',
#                                 CTP_KOR_NM == '세종특별자치시' ~ '세종',
#                                 CTP_KOR_NM == '울산광역시' ~ '울산',
#                                 CTP_KOR_NM == '인천광역시' ~ '인천',
#                                 CTP_KOR_NM == '전라남도' ~ '전남',
#                                 CTP_KOR_NM == '전라북도' ~ '전북',
#                                 CTP_KOR_NM == '제주특별자치도' ~ '제주',
#                                 CTP_KOR_NM == '충청남도' ~ '충남',
#                                 CTP_KOR_NM == '충청북도' ~ '충북',
#                                 T ~ '오류'))
# 
# 
# #망 == 도시대기로 이미 filter 되어있음
# pm_25_province_df <- d_month_province_long %>% 
#   filter(cate == 'PM25') %>% 
#   transmute(지역_대분류, 측정일시 = 측정일시_3, month)
# 
# 
# pm_25_province_ani <- 
#   si_do_df_v2 %>% 
#   left_join(pm_25_province_df, by = c("CTP_KOR_NM" = "지역_대분류")) %>% 
#   mutate(month = case_when(month >= 0 & month < 16 ~ '좋음',
#                            month >= 16 & month < 36 ~ '보통',
#                            month >= 36 & month < 76 ~ '나쁨',
#                            month >= 76 ~ '매우 나쁨',
#                            T ~ '값 없음'),
#          month = as.factor(month)) %>% 
#   ggplot(aes(x = long, y = lat, group = id)) +
#   geom_polygon(aes(group = group, fill = month)) +
#   # geom_path(color = 'black', size = .5) +
#   geom_text(aes(label = CTP_KOR_NM), 
#             data = si_do_name, 
#             size = 3,
#             color = 'black',
#             family = 'AppleGothic') +
#   coord_fixed(1) +
#   scale_color_manual(values = c('좋음' = '#87CEFA', 
#                                 '보통' = '#90EE90', 
#                                 '나쁨' = '#FFD700', 
#                                 '매우나쁨' = '#FF4500',
#                                 '값 없음' = '#FFFFFF'),
#                      aesthetics = c('fill')
#   ) +
#   theme_minimal(base_family = 'AppleGothic') +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()
#   ) +
#   gganimate::transition_time(측정일시) +
#   gganimate::ease_aes("linear") +
#   gganimate::enter_fade() +
#   gganimate::exit_fade() +
#   labs(title = '측정일시: {frame_time}',
#        fill = '초미세먼지')
# 
# gganimate::animate(pm_25_province_ani, fps = 10, width = 750, height = 450, duration = 60)
# gganimate::anim_save('./gif/pm_25_province_ani.gif')




# -------------------------
# 서울시 시각화 preprocess 
# -------------------------
# 서울시 '구' 이름을 그래프에 표시하기 위한 df
gu_name <- si_gun_gu_df %>%
  filter(substr(SIG_CD, 1, 2) == '11') %>% 
  group_by(id, SIG_KOR_NM) %>%
  summarise(long = mean(long, na.rm = T), lat = mean(lat, na.rm = T))

# 서울시에 속한 구 (사용 X)
# seoulsi <- c('종로구', '중구', '용산구',
#              '성동구', '광진구', '동대문구', '중랑구', '성북구', '강북구', '도봉구', '노원구',
#              '은평구', '서대문구', '마포구',
#              '양천구', '강서구', '구로구', '금천구', '영등포구', '동작구', '관악구',
#              '서초구', '강남구', '송파구', '강동구')

# ----------------------------------------
# 서울시 초미세먼지 변화(2016-2020) 그래프
# ----------------------------------------
pm_25_seoul_df <- d_month_long %>% 
  filter(지역_대분류 == '서울') %>% 
  filter(망 == '도시대기') %>%
  filter(cate == 'PM25') %>% 
  transmute(측정소명, 측정일시 = 측정일시_3, month)

pm_25_seoul_ani <- si_gun_gu_df %>% 
  filter(substr(SIG_CD, 1, 2) == '11') %>%
  left_join(pm_25_seoul_df, by = c('SIG_KOR_NM' = '측정소명')) %>% 
  mutate(month = case_when(month >= 0 & month < 16 ~ '좋음',
                           month >= 16 & month < 36 ~ '보통',
                           month >= 36 & month < 76 ~ '나쁨',
                           month >= 76 ~ '매우 나쁨',
                           T ~ '값 없음'),
         month = as.factor(month)) %>% 
  ggplot(aes(x = long, y = lat, group = id)) +
  geom_polygon(aes(group = group, fill = month)) +
  geom_path(color = 'black', size = .5) +
  geom_text(aes(label = SIG_KOR_NM), 
            data = gu_name, 
            size = 3,
            color = 'black',
            family = 'AppleGothic') +
  coord_fixed(1) +
  scale_color_manual(values = c('좋음' = '#87CEFA', 
                                '보통' = '#90EE90', 
                                '나쁨' = '#FFD700', 
                                '매우나쁨' = '#FF4500',
                                '값 없음' = '#FFFFFF'),
                     aesthetics = c('fill')
  ) +
  theme_minimal(base_family = 'AppleGothic') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  ) +
  gganimate::transition_time(측정일시) +
  gganimate::ease_aes("linear") +
  gganimate::enter_fade() +
  gganimate::exit_fade() +
  labs(title = '측정일시: {frame_time}',
       fill = '초미세먼지')

gganimate::animate(pm_25_seoul_ani, fps = 10, width = 750, height = 450, duration = 60)
gganimate::anim_save('./gif/pm_25_seoul_ani.gif')


# --------------------------------------
# 서울시 미세먼지 변화(2016-2020) 그래프
# --------------------------------------
pm_10_seoul_df <- d_month_long %>% 
  filter(지역_대분류 == '서울') %>% 
  filter(망 == '도시대기') %>%
  filter(cate == 'PM10') %>% 
  transmute(측정소명, 측정일시 = 측정일시_3, month)

pm_10_seoul_ani <- si_gun_gu_df %>% 
  filter(substr(SIG_CD, 1, 2) == '11') %>%
  left_join(pm_10_seoul_df, by = c('SIG_KOR_NM' = '측정소명')) %>% 
  mutate(month = case_when(month >= 0 & month < 30 ~ '좋음',
                           month >= 31 & month < 80 ~ '보통',
                           month >= 81 & month < 150 ~ '나쁨',
                           month >= 151 ~ '매우 나쁨',
                           T ~ '값 없음'),
         month = as.factor(month)) %>% 
  ggplot(aes(x = long, y = lat, group = id)) +
  geom_polygon(aes(group = group, fill = month)) +
  geom_path(color = 'black', size = .5) +
  geom_text(aes(label = SIG_KOR_NM), 
            data = gu_name, 
            size = 3,
            color = 'black',
            family = 'AppleGothic') +
  coord_fixed(1) +
  scale_color_manual(values = c('좋음' = '#87CEFA', 
                                '보통' = '#90EE90', 
                                '나쁨' = '#FFD700', 
                                '매우나쁨' = '#FF4500',
                                '값 없음' = '#FFFFFF'),
                     aesthetics = c('fill')
  ) +
  theme_minimal(base_family = 'AppleGothic') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  ) +
  gganimate::transition_time(측정일시) +
  gganimate::ease_aes("linear") +
  gganimate::enter_fade() +
  gganimate::exit_fade() +
  labs(title = '측정일시: {frame_time}',
       fill = '미세먼지')

gganimate::animate(pm_10_seoul_ani, fps = 10, width = 750, height = 450, duration = 60)
gganimate::anim_save('./gif/pm_10_seoul_ani.gif')