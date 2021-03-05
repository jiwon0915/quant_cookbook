## 모멘텀 전략
# 모멘텀 = 주가/이익의 추세. 
# 모멘텀 현상 = 상승 추세의 주식은 지속적으로 상승, 하락 추세의 주식은 지속적으로 하락하는 현상
# 모멘텀 현상의 원인 = 투자자 스스로에 대한 과잉 신뢰(비합리성)

# 모멘텀 종류
# 이익 모멘텀: 기업의 이익에 대한 추세
# 가격 모멘텀: 주가의 모멘텀, 단위에 따라 단기 모멘텀, 중기 모멘텀, 장기 모멘텀으로 나뉘는데 3~12개월의 가격모멘텀을 모멘텀이라 함

# 모멘텀 포트폴리오 구하기(12개월 모멘텀)

# 최근 1년 간 수익률 상위 30개 종목 선택
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(dplyr)

KOR_price = read.csv('data/KOR_price.csv', row.names=1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names=1,
                      stringsAsFactors = FALSE)

KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드,6, "left",0)

ret = Return.calculate(KOR_price) %>% xts::last(252) # 최근 1년 수익률 계산
ret_12m = ret %>% sapply(.,function(x){
  prod(1+x)-1
}) # prod = 종목별 "누적"수익률 계산

# ret_12m %>% head()

ret_12m[rank(-ret_12m)<=30] # 마이너스 기호 붙여서 상위 삼십개 선택

invest_mom = rank(-ret_12m)<=30
KOR_ticker[invest_mom,] %>% 
  select(종목코드,종목명) %>% 
  mutate(수익률 = round(ret_12m[invest_mom],4)) %>% 
  arrange(desc(수익률))

# 모멘텀 포트폴리오 구하기(위험조정 수익률)
# 위험조정 수익률이란? 누적수익률을 변동성으로 나눠서 위험을 고려한 수익률
# 과거 수익률로만 구하면 변동성이 지나치게 큰 종목이 있을 수 있음, 위험 고려할 경우 상대적으로 안정적!

ret = Return.calculate(KOR_price) %>% xts::last(252)
ret_12m = ret %>% sapply(.,function(x){
  prod(1+x)-1
})

std_12m = ret %>% apply(.,2,sd) %>% multiply_by(sqrt(252))
sharpe_12m = ret_12m/std_12m

invest_mom_sharpe = rank(-sharpe_12m)<=30
KOR_ticker[invest_mom_sharpe,] %>% 
  select(종목코드,종목명) %>% 
  mutate(수익률 = round(ret_12m[invest_mom_sharpe],2),
            변동성 = round(std_12m[invest_mom_sharpe],2),
            위험조정수익률 = round(sharpe_12m[invest_mom_sharpe],2)) %>% 
  arrange(desc(위험조정수익률))

intersect(KOR_ticker[invest_mom,'종목명'],
          KOR_ticker[invest_mom_sharpe,'종목명'])

# 위험조정 수익률 상위 30종목의 가격 그래프
library(xts)
library(tidyr)
library(ggplot2)

KOR_price[,invest_mom_sharpe] %>% 
  fortify.zoo() %>%  # row name을 하나의 컬럼으로 만들어줌
  gather(ticker,price,-Index) %>% # long data로 변경
  ggplot(aes(x=Index,y=price))+
  geom_line()+
  facet_wrap(.~ticker, scales='free')+
  xlab(NULL)+ylab(NULL)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())
