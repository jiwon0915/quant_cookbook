## 저변동성 전략
# 변동성 = 수익률이 움직이는 정도 = 표준편차 사용
# 어떤 표준편차냐면 수 익 률 !! 의 표준편차~

# 전통적 금융 이론: 수익률의 변동성이 크다 = 위험이 크다 = 그에 대한 보상으로 기대수익률 높아야한다
# 즉 고위험 고수익
# but in reality -> 변동성이 낮은 종목들의 수익률이 더 높은 "저변동성 효과"가 발견되고 있음

# 저변동성 효과의 원인
# 가설 1. 고변동성 주식에 대한 과대 평가(by 투자자들이 자신의 능력 과신, 레버리지 투자가 되지 않는 상황에서 벤치마크 대비 높은 성과를 얻기 위한 고변동성 주식 투자 경향)
# 가설 2. Volatility Drag : 시장 상승, 하락의 반복됨에 따라 고변동성 주식이 변동성 손실로 인해 수익률 하락

# 저변동성 포트폴리오 구하기(일간 기준)
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(ggplot2)
library(dplyr)

# 최근 1년 일간 수익률 기준 변동성 최하위 30 종목 선택
KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드,6,'left',0)

ret = Return.calculate(KOR_price)
std_12m_daily = xts::last(ret,252) %>% apply(.,2,sd) %>% 
  multiply_by(sqrt(252)) # last(a,b) = a의 마지막 b개 데이터 선택, 컬럼기준(종목당) sd계산
# 연간 비율 구하기 위해 sqrt(252) 곱해줌

# std_12m_daily %>% head()

std_12m_daily %>% data.frame() %>% 
  ggplot(aes(x=`.`))+
  geom_histogram(binwidth = 0.01)+
  annotate("rect", xmin = -0.02, xmax = 0.02,
           ymin = 0,
           ymax = sum(std_12m_daily == 0, na.rm = TRUE) * 1.1,
           alpha=0.3, fill="red") +
  xlab(NULL)

# std = 0인 애들 NA처리(거래정지 때문에 가격변화없어서 변동성 0이기 때문)
std_12m_daily[std_12m_daily==0]=NA

# 하위 30개 추리자
std_12m_daily[rank(std_12m_daily)<=30] %>% 
  data.frame() %>% 
  ggplot(aes(x=1:30,y=`.`))+
  geom_col(col='black', fill='skyblue')+
  xlab(NULL)

# 변동성 하위 30개 종목명 및 티커 확인
invest_lowvol = rank(std_12m_daily)<=30
invest_low30<-KOR_ticker[invest_lowvol,] %>% 
  select(종목코드,종목명) %>% 
  mutate(변동성 = round(std_12m_daily[invest_lowvol],4))

invest_low30 %>% 
  mutate(rank = row_number((변동성))) %>% 
  ggplot(aes(x=reorder(rank,변동성),y=변동성))+
  geom_col(fill='skyblue',col='black')+
  geom_text(aes(label=종목명),size=3)

# 저변동성 포트폴리오 구하기(주간 기준)
std_12m_weekly = xts::last(ret,252) %>% 
  apply.weekly(Return.cumulative) %>% 
  apply(.,2,sd) %>% multiply_by(sqrt(52))

std_12m_weekly[std_12m_weekly==0]<-NA

invest_lowvol_weekly = rank(std_12m_weekly)<=30
invest_low30_weekly <- KOR_ticker[invest_lowvol_weekly,] %>% 
  select(종목코드,종목명) %>% 
  mutate(변동성 =  round(std_12m_weekly[invest_lowvol_weekly],4))

invest_low30_weekly %>% 
  mutate(rank = row_number(변동성)) %>% 
  ggplot(aes(x=reorder(rank,변동성),y=변동성))+
  geom_col(fill='skyblue',col='black')+
  geom_text(aes(label=종목명),size=3)

# 일간 변동성, 주간 변동성 모두 저변동인 종목 확인
intersect(invest_low30$종목명,invest_low30_weekly$종목명)
