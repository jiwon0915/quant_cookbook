## 밸류 전략(가치주 효과)
# 가치주 효과 = 내재 가치 대비 낮은 가격의 주식, 즉 저PER, 저PBR 주식이 내재 가치 대비 비싼 주식보다 수익률이 높은 현상.

# 가치 효과의 원인
# 1. 위험한 기업은 시장에서 상대적으로 낮은 가격에 거래됨. 위험감당대가로 높은 수익발생
# 2. 투자자들의 성장주에 대한 과잉 반응 -> 가치주는 시장에서 소외됨, 제자리를 찾아가는 과정에서 수익 발생

# 가치를 나타내는 지표 -> PER, PBR, PCR, PSR이 많이 사용됨

# 밸류 포트폴리오 구하기(기준 = PBR)
library(stringr)
library(ggplot2)
library(dplyr)

KOR_value = read.csv('data/KOR_value.csv', row.names=1,
                     stringsAsFactors = FALSE)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names=1,
                      stringsAsFactors = FALSE)
KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드,6,'left',0)

invest_pbr = rank(KOR_value$PBR)<=30
KOR_ticker[invest_pbr,] %>% 
  select(종목코드,종목명) %>% 
  mutate(PBR = round(KOR_value[invest_pbr,'PBR'],4))

# 각 지표 결합
library(corrplot)

rank_value = KOR_value %>% 
  mutate_all(list(~min_rank(.))) # 해당 종목이 각 컬럼에서 갖는 순위값을 계산(min_rank는 동일 순위일 경우 작은 값으로 순위부여(52241, not 53341))

cor(rank_value, use = 'complete.obs') %>% # 'complete.obs' = NA 종목 삭제하겠다는 뜻~
  round(.,2) %>% 
  corrplot(method='color',type='upper',
           addCoef.col = 'black',number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col='black',
           col=colorRampPalette(
             c('blue','white','red'))(200),
           mar=c(0,0,0.5,0)) # 각 가치지표끼리의 correlation 계산 

# PER과 PSR의 경우는 상관관계 꽤 낮음 -> 각 지표를 통합적으로 고려하면 분산 효과 기대가능한

rank_sum = rank_value %>% 
  rowSums()

invest_value = rank(rank_sum)<=30

KOR_ticker[invest_value,] %>% 
  select(종목코드,종목명) %>% 
  cbind(round(KOR_value[invest_value,],2))

# 단순 저PBR 기준 종목과 교집합
intersect(KOR_ticker[invest_pbr,'종목명'],
          KOR_ticker[invest_value,'종목명'])
