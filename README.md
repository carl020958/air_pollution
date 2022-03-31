## Korea Air Pollution Analysis
코로나 전후의 대기오염 변화 및 지역별 대기오염 정도의 차이 분석 프로젝트
* 분석 방법: 대응표본 t검정, ANOVA 분석
---

### 데이터 출처
한국환경공단
---

### 분석에 사용된 라이브러리
* 전반적인 데이터 전처리: dplyr, tidyr, stringr
* 시간 데이터 전처리: lubridate
* 위도, 경도: sf, transformr
* 분석: purrr, broom
* 시각화: ggplot2, gganimate, sjPlot, DT
