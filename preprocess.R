setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('magrittr', 'dplyr', 'purrr', 'tidyr', 
               'ggplot2', 'gganimate', 'transformr',
               'sf', 
               'lubridate', 'stringr')

# =================
# 1. Basic setting
# =================

# ---------
# function
# ---------
#read_csv
.read_csv <-  function(path){
  readr::read_csv(
    path,
    col_names = T,
    locale = readr::locale('ko', encoding = 'euc-kr'),
    na = c('.')
  )
}

# ==============
# 2. Read data 
# ==============
# 2016
dir_file_2016 <- './2016/'

file_2016_list <- dir_file_2016 %>%
  fs::dir_ls() %>%
  map(.f = .read_csv)

file_2016 <- file_2016_list %>%
  map(~mutate(., 측정소코드 = as.character(측정소코드))) %>%
  set_names(fs::dir_ls(dir_file_2016)) %>%
  bind_rows(.id = 'file_path')

# 2017
dir_file_2017 <- './2017/'

file_2017_list <- dir_file_2017 %>%
  fs::dir_ls() %>%
  map(.f = readxl::read_xlsx)

file_2017 <- file_2017_list %>%
  set_names(fs::dir_ls(dir_file_2017)) %>%
  bind_rows(.id = 'file_path')

# 2018
dir_file_2018 <- './2018/'

file_2018_list <- dir_file_2018 %>%
  fs::dir_ls() %>%
  map(.f = readxl::read_xlsx)

file_2018 <- file_2018_list %>%
  set_names(fs::dir_ls(dir_file_2018)) %>%
  bind_rows(.id = 'file_path')

# 2019
dir_file_2019 <- './2019/'

file_2019_list <- dir_file_2019 %>%
  fs::dir_ls() %>%
  map(.f = readxl::read_xlsx)

file_2019 <- file_2019_list %>%
  set_names(fs::dir_ls(dir_file_2019)) %>%
  bind_rows(.id = 'file_path')

# 2020
dir_file_2020 <- './2020/'

file_2020_list <- dir_file_2020 %>%
  fs::dir_ls() %>%
  map(.f = readxl::read_xlsx)

file_2020 <- file_2020_list %>%
  map(~mutate(., 측정소코드 = as.character(측정소코드))) %>%
  set_names(fs::dir_ls(dir_file_2020)) %>%
  bind_rows(.id = 'file_path')



# ================
# 3. Data combine 
# ================
# 망 정보 없는 df: 2016년 전부, 2018년 1,2분기

# 2017~2020 망, 측정소코드 정보 (2016년 데이터에 망 정보 없는 것 채우기 위해)
all_net_1 <- rbind(file_2017, file_2019, file_2020) %>%
  filter(!측정소명 %in% c('저구리', '석모리', '덕적도', '파도리')) %>%
  select(측정소코드, 망) %>%
  unique()

# 저구리, 석모리, 덕적도, 파도리는 망이 교외대기인데, 국가배경으로 등록되어 있는 데이터가 있음을 발견
all_net_2 <- rbind(file_2017, file_2019, file_2020) %>%
  filter(측정소명 %in% c('저구리', '석모리', '덕적도', '파도리')) %>%
  select(측정소코드, 망) %>%
  unique()

# 결합
all_net <- rbind(all_net_1, all_net_2)

d <- rbind(file_2016,
      file_2017 %>% select(-망),
      file_2018 %>% select(-망),
      file_2019 %>% select(-망),
      file_2020 %>% select(-망))

#전북 익산시 남중동은 폐쇄됨 (2019년 12월)
d <- d %>%
  left_join(all_net, by = '측정소코드') %>%
  mutate(망 = case_when(측정소코드 == '735132' ~ '도시대기',
                         T ~ 망))

# save(file = 'air_df_2015_2020.Rdata', d)
load('air_df_2015_2020.Rdata')

#wide -> long data
d_long <- d %>%
  relocate(주소, .after = 지역) %>%
  relocate(망, .after = 주소) %>%
  pivot_longer(-c(file_path, 지역, 주소, 망, 측정소코드, 측정소명, 측정일시),
               names_to = 'cate',
               values_to = 'value')

# save(file = 'air_df_2015_2020_long.Rdata', d_long)
load('air_df_2015_2020_long.Rdata')

# 한달 별 평균 값 구하기
d_month_long <- d_long %>%
  mutate(측정일시_2 = stringr::str_sub(측정일시, 1, 8),
         date = lubridate::as_date(측정일시_2),
         측정일시_3 = lubridate::format_ISO8601(date, precision = 'ym'),
         측정일시_3 = lubridate::parse_date_time(측정일시_3, 'ym')) %>%
  group_by(지역, 망, 측정소코드, 측정소명, 측정일시_3, cate) %>%
  summarise(month = mean(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(지역_대분류 = stringr::str_sub(지역, 1, 2))

# save(file = 'air_df_2015_2020_long_month.Rdata', d_month_long)
load('air_df_2015_2020_long_month.Rdata')


# 하루 별 평균 값 구하기(사용 안함)
# d_day_long <- d_long %>%
#   mutate(측정일시_2 = stringr::str_sub(측정일시, 1, 8)) %>%
#   group_by(지역, 망, 측정소코드, 측정소명, 측정일시_2, cate) %>%
#   summarise(day = mean(value, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(date = lubridate::as_date(측정일시_2)) %>%
#   mutate(지역_대분류 = stringr::str_sub(지역, 1, 2))

# save(file = 'air_df_2015_2020_long_day.Rdata', d_day_long)
load('air_df_2015_2020_long_day.Rdata')

# 지역 & 한달 별 평균 값 구하기 (망 = 도시대기) (사용안함)
# d_month_province_long <- d_long %>%
#   filter(망 == '도시대기') %>%
#   mutate(측정일시_2 = stringr::str_sub(측정일시, 1, 8),
#               date = lubridate::as_date(측정일시_2),
#               측정일시_3 = lubridate::format_ISO8601(date, precision = 'ym'),
#               측정일시_3 = lubridate::parse_date_time(측정일시_3, 'ym')) %>%
#   mutate(지역_대분류 = stringr::str_sub(지역, 1, 2)) %>%
#   group_by(지역_대분류, 측정일시_3, cate) %>%
#   summarise(month = mean(value, na.rm = T)) %>%
#   ungroup()

# save(file = 'air_df_2015_2020_long_month_province.Rdata', d_month_province_long)
# load('air_df_2015_2020_long_month_province.Rdata')
