## 포트폴리오 구성
# 최적 포트폴리오의 구성 - 수식을 기반으로 최적화된 해를 찾음. 
# 최소분산포트폴리오/ 최대분산효과 포트폴리오/ 위험균형 포트폴리오/ 인덱스 포트폴리오

###library###
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)
library(tidyr)
library(dplyr)
library(corrplot)
library(nloptr)
library(RiskPortfolios)
library(quadprog)
library(ggplot2)
library(cccp)
#############

# data download(ETF data)
symbols = c('SPY', # 미국주식
            'IEV', # 유럽주식
            'EWJ', # 일본주식
            'EEM', # 이머징 주식
            'TLT', # 미국 장기채
            'IEF', # 미국 중기채
            'IYR', # 미국 리츠
            'RWX', # 글로벌 리츠
            'GLD', # 금
            'DBC') # 상품
getSymbols(symbols, src= 'yahoo')

prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x)))) %>% 
  setNames(symbols) # 수정주가만을 선택한 뒤 열의 형태로 묶어줌

rets = Return.calculate(prices) %>% na.omit() # 수익률 계산

cor(rets) %>% 
  corrplot(method='color', type='upper',
           addCoef.col = 'black', number.cex = 0.7,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black', 
           col = colorRampPalette(c('blue','white','red'))(200),
           mar = c(0,0,0.5,0))
# 같은 자산군 내에서는 강한 양의 상관관계, 주식-재권은 음의 상관관계, 주식-리츠도 꽤 높은 양의 상관관계

# 포트폴리오 최적화에는 분산-공분산 행렬을 대부분 사용
covmat = cov(rets)

## 최소분산포트폴리오 = 변동성 최소, slsqp() 함수를 이용한다
# slsqp함수는 순차적 이차 계쇡을 이용해 해를 찾음
# 목적함수에서 f(x) = 최소화하고자 하는 값 = 변동성

# slsqp(x0, fn, gr = NULL, lower = NULL, upper = NULL, 
#       hin = NULL, hinjac = NULL, heq = NULL, heqjac = NULL, nl.info = FALSE, control = list(), ...)
# x0 = 초기값
# fn = 목적함수 
# hin = 부등위 제약조건: 각 자산의 비중이 0보다 커야한다
# heq = 등위 제약조건: 투자 비중의 합이 1이어야한다
objective = function(w){
  obj = t(w) %*% covmat %*% w
  return(obj)
} # covmat = 사전에 계산된 분산-공분산 행렬, w = 각자산의 투자비중, obj = 포트폴리오의 변동성

hin.objective = function(w){
  return(w)
} # 이미 패키지 내에서 hin >=0으로 인식함. 따라서 걍 입력만 하게끔 만듦

heq.objective = function(w){
  sum_w = sum(w)
  return(sum_w-1)
} # 계산된 비중인 w의 합계를 구한 후 해당 값에서 1뺀 값을 반환. 패키지 내에서 heq==0의 형태로 인식. 따라서 sum_w-1=0 -> sum_w = 1을 의미

# library(nloptr)
result = slsqp(x0 = rep(0.1, 10),
               fn = objective,
               hin = hin.objective,
               heq = heq.objective) # 초기값인 x0에는 동일한 비중을 입력. 종목이 10개니까 10개 입력

print(result$par) # 최적화된 지점의 해. 즉 최소분산 포트폴리오를 구성하는 자산들의 투자 비중


print(result$value) # par에서 산출된 값을 fn에 입력했을 때 나오는 결과겂. 즉 포트폴리오의 분산을 의미

w_1 = result$par %>%  round(.,4) %>% 
  setNames(colnames(rets))
print(w_1)

## solve.QP() 함수를 이용한 최적화.
# solve.QP(Dmat, dvec, Amat, bvec, meq = 0, factorized = FALSE)
# Dmat = 분산-공분산 행렬
# meq = bvec의 몇 번째까지를 등위 제약조건을 설정할지에 대한 부분
Dmat = covmat
dvec = rep(0,10) # 최소분산 포트폴리오 구하는데 필요한 값 아님 -> 0벡터 입력
Amat = t(rbind(rep(1,10), diag(10), -diag(10)))
bvec = c(1, rep(0,10), -rep(1,10))
meq = 1

# library(quadprog)
result = solve.QP(Dmat, dvec, Amat, bvec, meq)

print(result$solution) # 자산들의 투자비중
print(result$value) # 포트폴리오의 분산

w_2 = result$solution %>% round(.,4) %>% 
  setNames(colnames(rets))
print(w_2)

## optimalPortfolio() 함수를 이용한 최적화
# optimalPortfolio(Sigma, mu = NULL, semiDev = NULL, control = list())
# Sigma = 분산 - 공분산 행렬
# mu = 기대수익률, simiDev = 세미편차 --> 입력하지 않아도 ㄱㅊ
# control = 포트폴리오 종류 및 제약조건에 해당하는 부분

# library(RiskPorfolios)
w_3 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constrain = 'lo')) %>%  # lo = 최소 투자 비중이 0보다 클 것
    round(.,4) %>% setNames(colnames(rets))
print(w_3)

# 결과 비교
data.frame(w_1) %>% 
  ggplot(aes(x=factor(rownames(.), levels = rownames(.)),
             y=w_1))+
  geom_col()+
  xlab(NULL)+ylab(NULL) # 이 경우는 특정자산에 대부분의 비중을 투자하고, 특정자산에는 전혀 투자하지 않음 -> 구석해(Corner solution) 문제 발생! -> 최소 및 최대 투자 비중 제약조건 추가해주자

## 최소 및 최대 투자비중 제약조건
result = slsqp(x0 = rep(0.1,10),
               fn = objective,
               hin = hin.objective,
               heq = heq.objective,
               lower = rep(0.05,10), 
               upper = rep(0.20,10)) # lower, upper 조건 추가로 입력해줌, 그럼 5%와 20% 사이에서 해를 찾게됨!

w_4 = result$par %>% round(.,4) %>% 
  setNames(colnames(rets))
print(w_4)

# solve.QP() 함수에서는 bvec항목만 수정해주면됨
Dmat = covmat
dvec = rep(0,10)
Amat = t(rbind(rep(1,10), diga(10), -diag(10)))
bvec = c(1, rep(0.05, 10), -rep(0.2, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w_5 = result$solution %>% round(.,4) %>% 
  setNames(colnames(rets))

print(w_5)

# optimalPortfolio() 함수에서는 control항목 중에서 contraint 부분을 user로 바꿔서 직접 상한하한값 수정하면 됨
w_6 = optimalPortfolio(covmat, 
                       control = list(type='minvol',
                                      contraint = 'user', 
                                      LB = rep(0.05,10),
                                      UB = rep(0.2, 10))) %>% 
  round(.,4) %>% 
  setNames(colnames(rets))

print(w_6)

# 비교
data.frame(w_4) %>% 
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w_4))+
  geom_col()+
  geom_hline(aes(yintercept = 0.05), color = 'red')+
  geom_hline(aes(yintercept = 0.2), color = 'red')+
  xlab(NULL)+ylab(NULL)

## 각 자산별 제약조건의 추가
# 투자 규모가 커질 경우 추가적인 제약조건이 필요한 경우 O -> 이건 solve.QP함수 이용이 간편함
Dmat = covmat
dvec = rep(0,10)
Amat = t(rbind(rep(1,10), diag(10), -diag(10)))
bvec = c(1, c(0.10, 0.10, 0.05, 0.05, 0.10,
              0.10, 0.05, 0.05, 0.03, 0.03),
         -c(0.25, 0.25, 0.20, 0.20, 0.20,
            0.20, 0.10, 0.10, 0.08, 0.08)) # 각 자산별로 제약조건 입력
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

result$solution %>%
  round(., 4) %>%
  setNames(colnames(rets))

## 최대분산효과 포트폴리오
# 포트폴리오의 자산 간 상관관계 존재. 그 상관관계 낮아질수록 변동성도 낮아짐.-> "분산효과"
# 분산효과의 정도를 측정하는 지표 = 분산 비율(Diversification Ratio)
# 분산 비율 = 개별 변동성의 가중합 / 포트폴리오의 변동성
# 목적함수 = max 분산비율 but 대부분의 최적화 프로그래밍에서는 목적함수 최소화하는 방식 사용 -> max DR을 min something으로 바꾸자

# 1. Transformation: 비중을 각각의 표준편차로 나눈 후 비중의 합으로 표준화(상관관계 행렬 사용)
# 2. Duality: 비중의 합으로 표준화
# 3. -DR: 표준화 ㄴㄴ

# solve.QP함수 사용
# 목적함수는 최소분산 포트폴리오와 동일한데 제약조건이 조금 다름.
Dmat = covmat
dvec = rep(0,10)
Amat = t(rbind(sqrt(diag(covmat)), diag(10)))
bvec = c(1,rep(0,10))
meq= 1
result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution %>% 
  round(.,4) %>% 
  setNames(colnames(rets))
print(w) # 비중이 1 넘음 -> 표준화 필요

w = (w/sum(w)) %>% 
  round(.,4)
print(w)

data.frame(w) %>% 
  ggplot(aes(x= factor(rownames(.), levels = rownames(.)),
             y = w))+
  geom_col()+
  xlab(NULL)+ylab(NULL)

# optimalPortfolio() 함수 사용, 목적함수 = min(-DR)
w = optimalPortfolio(covmat,
                     control = list(type='maxdiv',
                                    constraint = 'lo')) %>%  # type = maxdiv
  round(.,4)
print(w) 

# 제약조건 추가(표준화 과정 고려해서 적용!): 최소 5퍼, 최대 20퍼
Dmat = covmat
dvec = rep(0,10)
Alb = -rep(0.05,10) %*% matrix(1,1,10)+diag(10)
Aub = rep(0.20,10) %*% matrix(1,1,10)-diag(10)

Amat = t(rbind(sqrt(diag(covmat)),Alb,Aub))
bvec = c(1, rep(0,10), rep(0,10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w= result$solution
w = (w/sum(w)) %>% 
  round(.,4) %>% 
  setNames(colnames(rets))
print(w)

data.frame(w) %>% 
  ggplot(aes(x=factor(rownames(.),levels = rownames(.)),
             y = w))+
  geom_col()+
  geom_hline(aes(yintercept=0.05), color='red')+
  geom_hline(aes(yintercept=0.2), color='red')+
  xlab(NULL)+ylab(NULL)

# 각 자산 별 제약조건 추가.
Dmat = covmat
dvec = rep(0,10)
Alb = -c(0.10, 0.10, 0.05, 0.05, 0.10,
         0.10, 0.05, 0.05, 0.03, 0.03) %*%
  matrix(1, 1, 10) + diag(10)
Aub = c(0.25, 0.25, 0.20, 0.20, 0.20,
        0.20, 0.10, 0.10, 0.08, 0.08) %*%
  matrix(1, 1, 10) - diag(10)
Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, 10), rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)


## 위험균형 포트폴리오
# 한계 위험기여도 = 특정 자산의 비중을 한 단위 증가시켰을 때 전체 포트폴리오의 위험의 증가
# 위험기여도 = 특정 자산이 포트폴리오 내에서 차지하는 위험의 비중.
# 한계 위험기여도가 큰 자산이라하더라고 포트폴리오 내에서 비중이 작으면 포트폴리오 내에서 차지하는 위험의 비중이 작고, 한계 위험기여도가 작은 자산이더라도 비중이 높으면 포트폴리오 내에서 차지하는 위험의 비중은 커짐.
# --> i번째 자산의 위험기여도 = i번째 자신의 한계위험기여도 * 포트폴리오 내 비중

get_RC = function(w, covmat){
  # 한계 위험기여도 계산
  port_vol = t(w) %*% covmat %*% w 
  port_std = sqrt(port_vol)
  MRC = (covmat %*% w)/as.numeric(port_std)
  
  # 위혐도 계산
  RC = MRC*w 
  RC = c(RC/sum(RC))
  
  return(RC)
}

# ex. 주식 60% + 채권 40% 포트폴리오의 위험기여도
ret_stock_bond = rets[,c(1,5)] # 미국 주식 & 미국 장기채만 추출
cov_stock_bond = cov(ret_stock_bond)
RC_stock_bond = get_RC(c(0.6,0.4), cov_stock_bond)
RC_stock_bond = round(RC_stock_bond, 4)

print(RC_stock_bond,4)

# 투자 비중 자체는 60대 40 이지만 위험비중은 97대 3으로 주식이 포트폴리오 위험의 대부분을 차지하고 있다!! -> 모든 자산이 균일한 위험기여도를 가지도록 최적화할 필요 있음--> 그게 위험균형포트폴리오~

# rp 함수를 이용한 위험균형(동일위험기여도) 포트폴리오 최적화
# 위험균형 포트폴리오는 slsqp나 optimalPortfolio보다는 cccp패키지의 rp()함수가 좋아용
# library(cccp)
opt = rp(x0 = rep(0.1,10), # x0은 w의 초기입력값, 동일 비중인 10프로씩 입력
         P= covmat, # P = 분산-공분산 행렬
         mrc= rep(0.1,10)) # mrc = 목표로하는 자산별 위험기여도값. 동일한게 목표니까 0.1로 ㄱㄱ
w = getx(opt) %>% drop() # drop-> 벡터형태로 변환
w = (w/sum(w)) %>% # 서메이션 1로 만듦
  round(.,4) %>% 
  setNames(colnames(rets))
print(w)
get_RC(w, covmat) #위험기여도가 거의 다 0.1에 가까워짐!! 


## 위험예산 포트폴리오
# 모든 자산의 위험기여도 동일말고 자산별로 다른 위험기여도 갖게 포트폴리오를 구서앻야하는 경우도 있음. --> "위험예산포트폴리오(Risk Budget Portfolid). 이것도 rp()사용

opt = rp(x0 = rep(0.1,10),
         P = covmat,
         mrc = c(0.15,0.15,0.15,0.15,0.1,
                 0.1,0.05,0.05,0.05,0.05)) # 목표로 하는 위험 기여도
w = getx(opt) %>% drop()
w = (w/sum(w)) %>% 
  round(.,4) %>% 
  setNames(colnames(rets))
print(w)

get_RC(w,covmat)


## 인덱스 포트폴리오 구성하기
# 인덱스 포트폴리오 -> 실제 운용사에서 많이 사용된다
# 투자 전략은 크게 액티브 전략과 패시브 전략으로 나뉨. 
# 액티브 전략 = 적극적 투자. 벤치마크(하나의 지수)보다 초과수익을 거두기 위함.
# 패시브 전략 = 벤치마크 그대로 추종함. 
# 패시브 전략을 사용하는 펀드 = 패시브 펀드 or 인덱스 펀드! 
# 벤치마크가 되는 지수(=INDEX)는 주로 각 국가의 주가지수, 우리나라는 KOSPI200, 미국은 S&P500

# 주가지수는 대부분 각 주식의 시가총액비중을 이용해 구성됨. 그치만 상장된 모든 주식이 거래가 되는 것은 아니기 때문에 시가총액을 주가*상장주식수*유동비 로 계산하고, 이렇게 계산된 시가총액을 전체 시가총액의 합으로 나누어 지수내 비중을 계산함.

# KOSPI200 지수 복제해보자
# KOSPI200 산출방법 : 시가총액이 상위군에 속하고 거래량이 많은 종목 200개

# 여기서는 편의~를 위해 시가총액 상위 200개 선택하고 유동비 100퍼라고 가정하자
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names=1,
                      stringsAsFactors = FALSE)
KOSPI200 = KOR_ticker %>% filter(시장구분 =='KOSPI') %>% 
  slice(1:200) %>% # 이미 내림차순처리 돼있음
  mutate(시가총액비중 = 시가총액/sum(시가총액))

KOSPI200 %>% 
  ggplot(aes(x=reorder(종목명,-시가총액비중),y=시가총액비중))+
  geom_point()+
  xlab('종목명')+ylab('시가총액비중(%)')+
  scale_y_continuous(labels = scales::percent)
# 잘안보이고 삼성전자가 지나치게 커서 더 안보임

KOSPI200 %>% 
  ggplot(aes(x=reorder(종목명,-시가총액비중), y=시가총액비중))+
  geom_point()+
  xlab('종목명')+ylab('시가총액비중(로그 스케일링')+
  scale_y_log10()+
  scale_x_discrete(breaks = KOSPI200[seq(1,200,by=5),'종목명'])+ # 일부종목만 표시
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# 그럼 우리에게 1억이 있을 때 KOSPI200복제하여 종목별 투자금액 계산해보자
KOSPI200 = KOSPI200 %>% 
  mutate(매수금액 = 100000000*시가총액비중,
             매수주수 = floor(매수금액/종가))
KOSPI200 %>% select(매수금액, 매수주수,종목명) %>% head()

inv_money = KOSPI200 %>% mutate(실제매수금액 = 종가*매수주수) %>% 
  summarise(sum(실제매수금액))
print(inv_money)

## 팩터를 이용한 인핸스드(enhanced) 인덱스 포폴 구성
# 약간의 위험 감수해서 벤치마크 대비 미~세한 초과수익을 원할 때 -> enhanced index fund
# 층화추출법, 비중조절법, 차익거래 등의 전략을 활용

# 비중조절법: PBR을 이용해 시가총액비중을 조절
KOSPI200 = KOSPI200 %>% select(종목명, PBR, 시가총액비중) %>% 
  mutate(PBR = as.numeric(PBR))

# 1, 단순가감법: PBR 랭킹구한 후 PBR이 낮은 n개 종목에는 일정 비중씩 더하고, 나머지 종목에서 더한 비중만큼을 뺌. n결정하는 건 투자자의 재량. 여기서는 PBR 상위(=낮은) 100종목에 0.05퍼씩 더하고(총 5퍼) 나머지 100개에서 각각 0.05퍼씩 빼주겠다
KOSPI200 = KOSPI200 %>% 
  mutate(랭킹 = rank(PBR),
           조절비중 = ifelse(랭킹<=100, 시가총액비중 + 0.0005, 시가총액비중 - 0.0005),
           조절비중 = ifelse(조절비중<0,0, 조절비중),
           조절비중 = 조절비중/sum(조절비중), # sum =0 되도록 다시 만들어줌
           차이 = 조절비중 - 시가총액비중,
           pos = ifelse(차이<0,'-','+'))

head(KOSPI200)
# --> 저PBR 종목은 시가총액비중 대비 투자비중이 많고, 고PBR종목은 시가총액비중 대비 투자비중이 적음. -> 장기적으로 저PBR 종목이 고PBR 종목보다 우수한 성과보이면 이 포트폴리오가 벤치마크 대비 우수한 성과 보일 것!

KOSPI200 %>% 
  ggplot(aes(x=reorder(종목명, -시가총액비중),y = 시가총액비중))+
  geom_point()+
  geom_point(data=KOSPI200, aes(x=reorder(종목명,-시가총액비중),y=조절비중),
             color = 'red', shape = 4)+
  xlab('종목명')+ylab('비중(%)')+
  coord_cartesian(ylim=c(0,0.03))+
  scale_x_discrete(breaks = KOSPI200[seq(1,200,by=5),'종목명'])+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust= 1))

KOSPI200_mod = KOSPI200 %>% arrange(PBR) # PBR로 오름차순 정렬
KOSPI200_mod %>% 
  ggplot(aes(x=reorder(종목명,PBR), y=차이))+ # PBR로 오름차순 정렬해서 시각화
  geom_point()+ # 종목명이 x축, 차이가 y축인데 PBR로 오름차순 정렬
  geom_col(aes(x=reorder(종목명, PBR), y=PBR/10000),fill='blue',alpha=0.2)+
  xlab('종목명')+
  ylab('차이(%)')+
  scale_y_continuous(labels = scales::percent,
                     sec.axis = sec_axis(~. * 10000, name = 'PBR'))+
  scale_x_discrete(breaks = KOSPI200_mod[seq(1,200,by=10),'종목명'])+
  theme(axis.text.x = element_text(angle=60, hjust=1)) # PBR이 높을수록 차이가 -가 된다 즉 비중이감소

# 팩터에 대한 전체 종목의 틸트
# 방금 한 건 상위종목과 하위종목에 대해 "동일한" 비중을 빼주거나 더해줬음.
# 그러나 팩터가 강한 종목의 경우 더 많은 비중더하고, 약한 종목의 경우 더 많은 비중 배는 등 적극적으로 포트폴리오를 구성하는 것도 가능함

# 1. 종목의 PBR 랭킹 구함
# 2. 랭킹을 바탕으로 Z-Score구한뒤 -1곱해줌. 왜? 랭킹이 높은(즉 1위 2위처럼 상위 랭킹) 종목은 z-score이 음수로 나옴. 누적확률을 계산할 때 랭킹이 높은 애들의 누적확률을 더 크게 계산해야되는데, 음수의 누적확률을 계산하면 작게 나오니까! 부호를 반대로 바꿔서 랭킹 높은 애들의 누적확률을 크게, 랭킹 낮은(하위권) 애들의 누적확률을 작게 만들려고.
# 3. 그러면 구한 cdf는 저 PBR일 수록 그 값이 크게 나올 것
# 4. 지수 내 시가총액비중에 누적확률값을 곱해줌. 저PBR일수록 누적확률값이 1에 가까워 원래의 시가총액비중에 가깝게 나올것이고, 고PBR 종목의 경우 시가총액비중 대비 더 낮은 값을 보이게 됨
# 5. 투자비중합 = 1 되도록 표준화
# 6. 결론적으로 PBR이 낮을수록 증가되는 비중이크고, PBR이 높을수록 감소되는 비중이 커진다.

# --> PBR 대상 팩터 틸트 포트폴리오 구성하기
KOSPI200_tilt = KOSPI200 %>% 
  select(종목명, PBR, 시가총액비중, 랭킹) %>% # 랭킹 = PBR랭킹
  mutate(zscore = -scale(랭킹),
         cdf = pnorm(zscore),
         투자비중 = 시가총액비중 * cdf,
         투자비중 = 투자비중/sum(투자비중),
         차이 = 투자비중 - 시가총액비중,
         pos = ifelse(차이>0,'저PBR','고PBR'))
head(KOSPI200_tilt)
table(KOSPI200_tilt$pos)
table(KOSPI200_mod$pos)

# 각 종목의 투자비중 시각화
KOSPI200 %>% 
  ggplot(aes(x=reorder(종목명,-시가총액비중), y=시가총액비중))+
  geom_point()+ # 시가총액비중으로 내림차순해서 점으로 시각화
  geom_point(data=KOSPI200_tilt, aes(x=reorder(종목명,-시가총액비중),y=투자비중),
             color='red',shape=4)+ # 틸트 포폴의 투자비중은 빨간색으로 시각화
  xlab('종목명')+ylab('비중(%)')+
  coord_cartesian(ylim=c(0,0.03))+
  scale_x_discrete(breaks=KOSPI200[seq(1,200,by=5),'종목명'])+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x=element_text(angle=60,hjust =1))
# 아까 동일비중더하고뺀거보다 그 폭이 훨씬 큼 -> 전체 포트폴리오가 팩터에 노출된 정도가 큼을 의미

# 제약 추가: 이 폭이 지나치게 커지는 것을 방지하기 위해 그 차이가 0.5%포인트이상이 되지않도록 
KOSPI200_tilt %>% 
  ggplot(aes(x=reorder(종목명,-시가총액비중),y=차이))+
  geom_point()+
  geom_hline(aes(yintercept=0.005),color='red')+
  geom_hline(aes(yintercept=-0.005),color='red')+
  xlab('종목명')+ylab('비중 차이(%)')+
  scale_x_discrete(breaks = KOSPI200[seq(1,200,by=5),'종목명'])+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust = 1)) # 이경우엔 빨간선 넘어가는 애들은 그 비중 줄여줘야함 

KOSPI200_tilt = KOSPI200_tilt %>% 
  mutate_at(vars(투자비중),list(~ifelse(차이<-0.005, 시가총액비중 -0.005, 투자비중))) %>% 
  mutate_at(vars(투자비중),list(~ifelse(차이>0.005, 시가총액비중+0.005, 투자비중))) %>% 
  mutate(투자비중 = 투자비중/sum(투자비중),
             차이 = 투자비중-시가총액비중)
# but 재표준화하는 과정에서 차이가 50bp넘어가는 경우 발생-> 계속 반복
while(max(abs(KOSPI200_tilt$차이)) > (0.005 + 0.00001)){
  print(max(abs(KOSPI200_tilt$차이)))
  KOSPI200_tilt = KOSPI200_tilt %>% 
    mutate_at(vars(투자비중), list(~ifelse(차이 < -0.005, 시가총액비중 - 0.005, 투자비중))) %>% 
    mutate_at(vars(투자비중), list(~ifelse(차이 > 0.005, 시가총액비중 + 0.005, 투자비중))) %>% 
    mutate(투자비중 = 투자비중 / sum(투자비중),
               차이 = 투자비중 - 시가총액비중)
}


KOSPI200_tilt %>% 
  ggplot(aes(x = reorder(종목명,-시가총액비중), y = 차이))+
  geom_point()+
  geom_hline(aes(yintercept = -0.005), color='red')+
  geom_hline(aes(yintercept = 0.005), color='red')+
  xlab('종목명')+ylab('비중 차이(%)')+
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by=5),'종목명'])+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1))

KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200_tilt, aes(x = reorder(종목명, -시가총액비중), y = 투자비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

KOSPI200_tilt_mod = KOSPI200_tilt %>% arrange(PBR)

KOSPI200_tilt_mod %>% 
  ggplot(aes(x = reorder(종목명, PBR), y = 차이)) +
  geom_point() +
  geom_col(aes(x = reorder(종목명, PBR), y = PBR /2000), fill = 'blue', alpha = 0.2) +
  xlab('종목명') +
  ylab('차이(%)') +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 2000, name = "PBR")) +
  scale_x_discrete(breaks = KOSPI200_mod[seq(1, 200, by = 10), '종목명']) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
