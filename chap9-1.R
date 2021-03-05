## 퀀트 전략을 이용한 종목선정
# 퀀트 투자 - 포트폴리오 운용전략/트레이딩 전략으로 나뉨
# 포트폴리오 운용전략 -> 과거 주식 시장 분석, 좋은 주식 사고 나쁜 주식 공매도
# 트레이딩 -> 단기간에 발생되는 주식 움직임 연구/예측 및 매수매도

# 포트폴리오 운용전략
# 베타: 개별 주식이 전체 주식시장의 변동에 반응하는 정도 = 회귀식 기울기
# 베타가 크다 = 주식시장보다 수익률의 움직임이 크다
# 상승장 예상시 베타가 큰 주식, 하락장 예상시 낮은 주식에 투자

# 베타 계산하기
# 주식 시장 = KOSPI 200 ETF
# 개별 주식 = 증권주 
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

symbols = c('102110.KS', '039490.KS') # 102110은 KOSPI 200 ETF인 타이거200이고 039490은 키움증권 티커
getSymbols(symbols) # 데이터 다운

prices = do.call(cbind,
                 lapply(symbols, function(x)Cl(get(x)))) # 종가만

ret = Return.calculate(prices) # 주식시장 수익률, 개별주식 수익률 계산
ret = ret['2016-01::2018-12'] # 3년치(2016-2018)

# ret %>% head()

# 베타 구하자
rm = ret[,1] # 주식시장 수익률 (x)
ri = ret[,2] # 개별주식 수익률 (y)

reg = lm(ri~rm)
summary(reg)

# y = 0.0004 + 1.764x (무위험수익률 Rf=0 가정하에)
# beta = 1.76으로 고베타주 & t값도 유의
# ps. 증권주 특성 중 하나가 고베타주라고 함!

# 베타 시각화
plot(as.numeric(rm), as.numeric(ri), pch=4, cex=0.3,
     xlab = 'KOSPI 200', ylab = 'Individual Stock', 
     xlim = c(-0.02,0.02), ylim = c(-0.02,0.02))
abline(a=0,b=1, lty = 2) # beta = 1
abline(reg,col='red') # 더 가파름

