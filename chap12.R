## 포트폴리오 백테스트
# 백테스트 = 현재 생각하고 있는 전략을 과거부터 실행했을 때 어떤 성과가 발생하는지 테스트하는 과정.
# 퀀트 투자는 과거의 데이터를 기반으로 전략 실행하기 때문에 백테스트가 핵심 단계!!
# 백테스트 결과 통해 해당 전략의 손익 & 위험을 대략적으로 판단할 수 있고, 과거의 어떤 구간에서 전략이 좋고 나빴는지도 판단 가능.

###library###
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)
#############

# Return.portfolio함수가 가장 대중적으로 사용됨!
# 각 자산 수익률, 리밸런싱 비중만 있으면 백테스트 수익률과 회전율 쉽계 계산 가능함, 또한 리밸런싱 시점과 수익률 시점 일치하지 않아도 돼서 편리!

# parameter 설명
# R: 각 자산 수익률 데이터
# weights: 리밸런싱 시기의 자산별 목표비중(생략할 경우 동일 비중 포트폴리오 가정), 리밸런싱 시점마다 적용되는 자산별 비중이 다르다면 시계열 형태로 입력해야함
# wealth.index : 포트폴리오 시작점이 1인 wealth index에 대한 생성여부, 디폴트는 false
# contribution: 포트폴리오 내에서 자산별 성과기여를 나타낼 것인지, 디폴트는 false
# geometric: 수익률 계산시 복리수익률 적용여부, default = true
# rebalance_on: 리밸런싱 주기 선택가능
# value: 초기 포트폴리오 가치, default = 1
# verbose: TRUE면 여러 detail(returns(포폴 수익률), contribution(일자별 개별 자산의 포폴 수익률 기여도), BOP.Weight(일자별 개별 자산의 포트폴리오 내 비중(시작시점 기준), 리밸런싱 없으면 직전 기간의 EOP weight과 동일), EOP.Weight(일자별 개별 자산의 포트폴리오 내 비중(종료시점 기준)), BOP.Value(일자별 개별 자산의 가치(시작시점 기준), 리밸런싱 없으면 직전 기간 EOP Value와 동일), EOP.Value(일자별 개별 자산의 가치(종료시점 기준)))

# 주식 60 채권 40 포트폴리오 백테스트 예제
#library(quantmod)
#library(PerformanceAnalytics)
#library(magrittr)
ticker = c('SPY','TLT')
getSymbols(ticker)

prices = do.call(cbind,
                 lapply(ticker, function(x) Ad(get(x)))) # 가격 데이터 저장
rets = Return.calculate(prices) %>% na.omit() # 수익률 데이터 저장

cor(rets) # 강한 음의 상관관계 있기 때문에 분산효과 기대 가능

portfolio = Return.portfolio(R = rets,
                             weights = c(0.6,0.4), # 리밸런싱 비중에 6:4 넣음
                             rebalance_on = 'years', # 연간 리밸런싱에 해당함
                             verbose = TRUE)
# 회전율 계산: |리밸런싱 전일 종료시점의 비중 - 리밸런싱 당일 시작시점의 비중|의 합계
portfolios = cbind(rets, portfolio$returns) %>%
  setNames(c('주식', '채권', '60대 40'))

charts.PerformanceSummary(portfolios,
                          main = '60대 40 포트폴리오')

turnover = xts(
  rowSums(abs(portfolio$BOP.Weight-
                timeSeries::lag(portfolio$EOP.Weight)),
          na.rm=TRUE),
  order.by = index(portfolio$BOP.Weight))

chart.TimeSeries(turnover) # 리밸런싱 시점에 해당하는 매해 첫 영업일에 회전율이 발생. 2008에서 2009 넘어갈 때 2008년 주식과 채권 등락폭이 심했기 때문에 이 때 회전율이 큼.

## 시점 선택 전략 백테스트
# = 시점별로 비중이 다른 형태의 예제 ex) 10개월 이동평균가보다 주가가 높으면 매수, 그렇지 않으면 전량 매도 후 현금보유
symbols = c('SPY','SHY') # SPY = S&P500수익률 추종하는 주식 ETF,  SHY = 미국 단기채 수익률 추종하는 현금 해당 ETF
getSymbols(symbols, src = 'yahoo')

ep = endpoints(rets, on ='month') # 매월 말일의 위치를 구함함
wts = list()
lookback = 10 # 10개월 이동평균값 게산할 거라서

i = lookback + 1
sub_price = prices[ep[i-lookback] : ep[i], 1] # 첫번째 시점의 테스트 과정 : 과거 10개월에 해당하는 가격의 이동평균이 필요하기 때문에 11개월째부터 테스트 가능, ep[i]=현재시점 주가의 위치를 의미, ep[i-lookback] = 현재시점으로부터 10개월 전 주가의 위치를 의미

head(sub_price, 3)

sma = mean(sub_price) # 10개월 주가의 평균균
wt = rep(0,2) # 비중이 들어갈 0벡터 생성
wt[1] = ifelse(last(sub_price)>sma,1,0) # wt[1] =주식의 투자비중. 현재 주가(last(sub_price)가 10개월 이동평균(sma)보다 크면 1, 그렇지 않으면 0
wt[2] = 1-wt[1] # 현금의 투자비중

wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]])) # xts통해 위에서 만들어진 투자비중 벡터를 시계열 형태로 바꾼후 wts리스트의i 번째에 저장

# 전체기간에 적용
ep = endpoints(rets, on = 'months')
wts = list()
lookback= 10

for (i in (lookback+1): length(ep)){
  sub_price = prices[ep[i-lookback] : ep[i] , 1]
  sma = mean(sub_price)
  wt = rep(0,2)
  wt[1] = ifelse(last(sub_price) > sma, 1, 0)
  wt[2] = 1 - wt[1]
  
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

wts = do.call(rbind, wts)

# 이제 Return.portfolio 함수로 포트폴리오 수익률계산
Tactical = Return.portfolio(rets, wts, verbose = TRUE) # 수익률데이터, 시점마다 다른 비중데이터 입력
portfolios = na.omit(cbind(rets[,1], Tactical$returns)) %>% 
  setNames(c('매수 후 보유', '시점 선택 전략')) # 주식 수익률 데이터와 포폴 수익률 합침. 초기 10개월에 대한 수익률은 없으므로 na.omit 처리
charts.PerformanceSummary(portfolios,
                          main='Buy & Hold Vs Tactical')

# 회전율 계산
turnover = xts(rowSums(abs(Tactical$BOP.Weight-
                             timeSeries::lag(Tactical$EOP.Weight)),
                       na.rm = TRUE),
               order.by = index(Tactical$BOP.Weight))
chart.TimeSeries(turnover)
# 매매 없는 경우도 있지만 매매 발생시 양쪽 매매로 인해 회전율 엄청 높아짐


## 동적 자산배분 백테스트
# 일반적인 자산배분 = 정적 자산배분 = 주식, 채권, 대체 자산의 투자비중을 사전에 정해놓고 약간의 비율만 수정하는것.
# 동적 자산배분 = 투자비중에 대한 제한을 두지않고 구성하는 방법.

# 1. 글로벌 10개 자산 중 과거 12개월 수익률이 높은 5개 자산을 선택
# 2. 최소분산 포트폴리오를 구성하고 이때 개별투자비율은 10~30 제약조건 설정
# 3. 매월 리밸런싱 실시

#library(quantmod)
#library(PerformanceAnalytics)
#library(RiskPortfolios)
#library(tidyr)
#library(dplyr)
#library(ggplot2)

symbols = c('SPY', # 미국 주식
            'IEV', # 유럽 주식 
            'EWJ', # 일본 주식
            'EEM', # 이머징 주식
            'TLT', # 미국 장기채
            'IEF', # 미국 중기채
            'IYR', # 미국 리츠
            'RWX', # 글로벌 리츠
            'GLD', # 금
            'DBC'  # 상품
)
getSymbols(symbols, src = 'yahoo')

prices = do.call(cbind, lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

rets = Return.calculate(prices) %>% na.omit() # 수정주가의 수익률 계산

ep = endpoints(rets, on = 'months') # 매월말일 위치 구함
wts = list()
lookback = 12 # 12개월 과거 수익률 계산할거니까
wt_zero = rep(0,10) %>% setNames(colnames(rets)) # 각 자산의 비중 들어갈 벡터 만듦

# Backtest
for (i in (lookback+1):length(ep)){
  sub_ret = rets[ep[i-lookback] : ep[i] , ]
  cum = Return.cumulative(sub_ret) # 과거 12개월동안의 자산별 누적 수익률
  
  K = rank(-cum)<=5
  covmat = cov(sub_ret[,K]) # 수익률 상위 5개 자산의 분산 - 공분산 행렬
  
  wt = wt_zero # 임시로 비중이 저장될 변수
  wt[K] = optimalPortfolio(covmat,
                           control = list(type = 'minvol',
                                          constrain = 'user',
                                          LB = rep(0.1,5),
                                          UB = rep(0.3,5)))
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
} # 투자비중계산완료!

wts = do.call(rbind, wts)

# 백테스트 수익률 계산
GDAA = Return.portfolio(rets, wts, verbose=TRUE)

# 누적 수익률 확인 -> 꾸준히 우상향한다
charts.PerformanceSummary(GDAA$returns, main = "동적자산배분")

wts %>% fortify.zoo() %>% 
  gather(key,value, -Index) %>% 
  mutate(Index = as.Date(Index)) %>% 
  mutate(key = factor(key, levels = unique(key))) %>% 
  ggplot(aes(x=Index, y=value))+
  geom_area(aes(color = key, fill=key),
            position = 'stack')+
  xlab(NULL)+ylab(NULL)+theme_bw()+
  scale_x_date(date_breaks = 'years', date_labels = '%Y',
               expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(plot.title = element_text(hjust = 0.5, size =12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, size = 8),
        panel.grid.minor.x = element_blank())+
  guides(color = guide_legend(byrow = TRUE))
# -> 자산별로 투자비중의 변화가 많은데, 그 이유는 매월마다 수익률 상위 5개에 해당하는 자산이 바뀌고, 그 비중도 계속 바뀌기 때문.

# 회전율 계산
GDAA$turnover = xts(
  rowSums(abs(GDAA$BOP.Weight -
                timeSeries::lag(GDAA$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(GDAA$BOP.Weight))

chart.TimeSeries(GDAA$turnover)

# 총 비용(세금, 수수료, 시장 충격 등)을 0.3%로 가정하고 수익률에서 회전율과 총비용의 곱을 빼서 순수익률 계산
fee = 0.0030
GDAA$net = GDAA$returns - GDAA$turnover*fee

cbind(GDAA$returns, GDAA$net) %>%
  setNames(c('No Fee', 'After Fee')) %>%
  charts.PerformanceSummary(main = 'GDAA')
