## 성과 및 위험 평가
# 백테스트를 통해 포트폴리오 수익률 구했으면 ~~ 이제 그 수익률을 바탕으로 성과와 위험을 평가해야함. 성과가 좋아도 위험이 너무 크면 안되고, 수익률이 지속적으로 감소하는 추세일 경우 더이상 작동하지 않는 전략일 가능성도 있음. 

# 여기서 포트폴리오 에시 = 퀄리티 팩터를 종합적으로 고려한 QMJ(Quality Minus Junk) 팩터의 수익률
# QMJ 팩터 = 우량성이 높은 종목을 매수, 우량성이 낮은 종목을 공매도 하는 전략을 지수의 형태로 나타낸 것. 이 팩터의 수익률로 성과와 위험 평가해보자! 

###library###
library(dplyr)
library(readxl)
library(xts)
library(timetk)
library(PerformanceAnalytics)
library(lubridate)
library(tidyr)
library(ggplot2)
library(broom)
library(stargazer)
#############

# data download
url = 'https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Quality-Minus-Junk-Factors-Monthly.xlsx'
tf = tempfile(fileext = '.xlsx')
download.file(url, tf, mode = 'wb')

excel_sheets(tf)

# QMJ Factors로 수익률 계산,
# MKT, SMB, HML Devil, UMD, 무위험 이자율인 RF 시트의 데이터로 회귀분석할거임
# 각 시트 내 18행까지는 데이터 설명 텍스트라서 skip
# date랑 수익률에 해당하는 global만 선택
df_QMJ = read_xlsx(tf, sheet = 'QMJ Factors', skip = 18) %>%
  select(DATE, Global)
df_MKT = read_xlsx(tf, sheet = 'MKT', skip = 18) %>%
  select(DATE, Global)
df_SMB = read_xlsx(tf, sheet = 'SMB', skip = 18) %>%
  select(DATE, Global)
df_HML_Devil = read_xlsx(tf, sheet = 'HML Devil',
                         skip = 18) %>%
  select(DATE, Global)
df_UMD = read_xlsx(tf, sheet = 'UMD', skip = 18) %>%
  select(DATE, Global)
df_RF = read_xlsx(tf, sheet = 'RF', skip = 18) 

df = Reduce(function(x,y) inner_join(x,y, by='DATE'),
            list(df_QMJ, df_MKT, df_SMB,
                 df_HML_Devil, df_UMD, df_RF)) %>%  # Reduce 함수 -> 모든 데이터에 inner join 가능하게 해줌.
  setNames(c('DATE', 'QMJ', 'MKT', 'SMB','HML','UMD','RF')) %>% 
  na.omit() %>% # 각 팩터별 시작시점 달라서 NA 데이터 삭제
  mutate(DATE = as.Date(DATE, "%m/%d/%Y"),
         R_excess = QMJ - RF, # 초과수익률 구함
         Mkt_excess = MKT - RF) %>% # 시장위험 프리미엄 계산
  tk_xts(date_var = DATE)

## 결과 측정 지표 
# 수익률 지표 : 누적 수익률, 연율화 수익률, 연도별 수익률 등
# 위험 : 변동성, 낙폭 등

#library(PerformanceAnalytics)
chart.CumReturns(df$QMJ) # QMJ 팩터의 누적 수익률 그래프, 장기간동안 우상향하는 모습
prod((1+df$QMJ))-1 # 현재까지의 누적수익률
mean(df$QMJ)*12 # 연율화 수익률(산술)
(prod((1+df$QMJ)))^(12/nrow(df$QMJ))-1 # 연율화수익률(기하)


# 누적수익률 = 각 수익률에 1을 더한 값을 모두 곱하고 마지막에 1뺌
# 산술 연율화 수익률 = 수익를 평균 구하고 조정값 곱함
# 기하 연율화 수익률 = 각 수익률에 1을 더한 값의 곲을 구한 후 연율화를 위해 승수적용, 마지막에 1을 뺌. 

# 걍 함수로 구할 수 잇음
Return.cumulative(df$QMJ)
Return.annualized(df$QMJ, geometric = FALSE)
Return.annualized(df$QMJ, geometric = TRUE)

# 변동성지표 -> 연율화 변동성: sd함수로 변동성 구하고 조정값 곱함
sd(df$QMJ) * sqrt(12)
StdDev.annualized(df$QMJ)

# 또는 샤프지수(Sharpe Ratio): 수익을 위험으로 나누어 위험조정수익률을 봄.
SharpeRatio.annualized(df$QMJ, Rf = df$RF)
SharpeRatio.annualized(df$QMJ, Rf = df$RF, geometric = FALSE)

# 낙폭(Drawdown) = 수익률이 하락한 후 반등하기 전까지 얼마나 하락했는지
# 최대 낙폭 = 낙폭 중 최댓값 = 최고점에서 최저점까지 얼마나 손실을 보는지
# 최대 낙폭이 지나치게 크면 위험 증가
table.Drawdowns(df$QMJ)
maxDrawdown(df$QMJ)
chart.Drawdown(df$QMJ)

# 칼마 지수(Calmar Ratio) = 또다른 위험 조정 수익률의 지표. =  연율화 수익률 / 최대 낙폭. 안정적인 절대 수익률 추구하는 헤지펀드에서 많이 참조함! 
CalmarRatio(df$QMJ)

## QMJ 팩터의 연도별 수익률 구하기
apply.yearly(df$QMJ, Return.cumulative) %>% head()

#library(lubridate)
#library(tidyr)
#library(ggplot2)

R.yr = apply.yearly(df$QMJ, Return.cumulative) %>% 
  fortify.zoo() %>% 
  mutate(Index = year(Index)) %>% 
  gather(key, value, -Index) %>% 
  mutate(key = factor(key, levels = unique(key)))

ggplot(R.yr, aes(x=Index, y=value, fill = key))+
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Yearly Return')+
  xlab(NULL)+ylab(NULL)+
  theme_bw()+
  scale_y_continuous(expand=c(0.03,0.03))+
  scale_x_continuous(breaks = R.yr$Index,
                     expand = c(0.01,0.01))+
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, size = 8),
        panel.grid.minor.x = element_blank() ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  geom_text(aes(label = paste(round(value * 100, 2), "%"),
                vjust = ifelse(value >= 0, -0.5, 1.5)),
            position = position_dodge(width = 1),
            size = 3)

## 승률 및 롤링 윈도우 값
# 승률 = 포트폴리오가 벤치마크 대비 높은 성과를 기록한 비율 = (포트폴리오수익률>벤치마크)인 일수/전체기간

# 벤치마크가 0일경우
UpsideFrequency(df$QMJ, MAR = 0) 
# 해석: 월간 기준 수익률이 플러스를 기록했던 비율이 58.8프로다

# 위에서 구했던 각종 지표들은 투자자가 포트폴리오 시작부터 현재까지 투자를 했다는 전제하에 계산. but 시작시점은 사람마다 다름 -> 시작시점을 무작위로 골라 그 시점에 투자했을 때 향후 n개월 후의 승률 또는 연율화 수익률을 계산할 필요 잇음 --> 롤링 윈도우의 아이디어!
# 롤링 윈도우 승률 = 무작위 시점에 투자했을 시 미래 n개월 동안의 연율화 수익률을 구함. 그런 다음 그 값이 벤치마크 대비 수익이 높았던 비율 계산. 12개월 롤링 윈도우 승률이 100 % = 어떠한 시점에 투자하더라도 12개월 후에는 언제나 벤치마크를 이겼음을 의미.
# 아무리 연율화 수익률이 높은 전략이더라도 롤링 윈도우 승률이 지나치게 낮을 경우 그냥 운빨이라고 해석 가능
roll_12 = df$QMJ %>% apply.monthly(., Return.cumulative) %>% # 월간 수익률 계산
  rollapply(., 12, Return.annualized) %>% na.omit() %>% # 12개월 동안의 롤링 통계값(연율화 수익률) 계산 & 처음 n개월은 수익률 없어서 na됨. 삭제
  UpsideFrequency() # 승률 계산

roll_24 = df$QMJ %>% apply.monthly(., Return.cumulative) %>% 
  rollapply(., 24, Return.annualized) %>% na.omit() %>% 
  UpsideFrequency() # 승률 계산

roll_36 = df$QMJ %>% apply.monthly(., Return.cumulative) %>% 
  rollapply(., 36, Return.annualized) %>% na.omit() %>% 
  UpsideFrequency() # 승률 계산

roll_win = cbind(roll_12, roll_24, roll_36)
print(roll_win)
# 투자 기간이 길어질수록 승률이 높아지네

df$QMJ %>% apply.monthly(., Return.cumulative) %>% 
  rollapply(., 12, Return.annualized) %>% na.omit() %>% 
  fortify.zoo() %>%  # fortizy.zoo 는 인덱스에 있는 시간열을 실제 열로 만듦
  ggplot(aes(x = Index, y = QMJ))+
  geom_line()+
  geom_hline(aes(yintercept = 0), color='red')+
  xlab(NULL)+ylab(NULL)

## 팩터 회귀분석 및 테이블로 나타내기
# 팩터 회귀분석 --> 포트폴리오 수익률이 "어디에서" 발생한 것인지에 대한 요인을 분석하는 것. 
# 일반적으로 파마-프랜치의 3팩터 모형, 거기에 모멘텀 팩터 추가한 카하트의 4팩터 모형이 많이 쓰임

# QMJ 팩터를 카하트 모형에 넣어서 퀄리티 팩터의 수익률에 대한 요인 분석 해보자
reg = lm(R_excess ~ Mkt_excess + SMB + HML + UMD, data =df) # Mkt_excess = 시장 위험 프리미엄, SMB = 사이즈 팩터, HML = 밸류 팩터, UMD = 모멘텀 팩터
summary(reg)$coefficient

# 해석
# 1. Beta1 = -0.26 -> 시장과 퀄리티 팩터는 역의 관계에 있다.
# 2. Beta2 = -0.35 -> 사이즈와 퀄리티 팩터는 역의 관계 -> 소형주보다는 대형주 수익률과 관계가 있음(Size factor의 의미: 소형주비중을 높이는 전략. 이 값이 커지면 소형주 비중을 늘린다는 뜻)
# 3. Beta3 = -0.09 -> 밸류 팩터와 역의 관계
# 4. Beta4 = 0.08 -> 모멘텀 팩터가 좋은 시기에 퀄리티 팩터도 좋을 수 있음

# library(broom)
tidy(reg) # 계수에 해당하는 값만 요약해서 볼수있음

# library(stargazer)
stargazer(reg, type = 'text', out = 'data/reg_table.html') #결과값 저장가능
