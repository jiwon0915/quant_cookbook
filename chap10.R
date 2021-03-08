# 저변동성, 모멘텀 등의 단일 팩터를 이용한 투자도 ㄱㅊ지만
# 여러 팩터를 결합하거나 전략 정밀하게 하면 더 우수한 성과 가능한

## 섹터 중립 포트폴리오
# 팩터 전략의 단점 : 선택 종목들이 특정 섹터로 몰릴 가능성이 있다. ex) 특정 섹터의 호황기에 동일한 섹터 모든 종목이 함께 움직이는 경향

###library###
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
#############

# 12개월 모멘텀 이용 포트폴리오
KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv',row.names=1,
                      stringsAsFactors = FALSE)
KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드,6,'left',0)

ret = Return.calculate(KOR_price) %>% xts::last(252)
ret_12m = ret %>% sapply(.,function(x){
  prod(1+x)-1
})

invest_mom = rank(-ret_12m)<=30

KOR_sector = read.csv('data/KOR_sector.csv', row.names = 1,
                      stringsAsFactors = FALSE)
KOR_sector$CMP_CD = 
  str_pad(KOR_sector$CMP_CD, 6, 'left', 0)
data_market = left_join(KOR_ticker, KOR_sector,
                        by=c('종목코드'='CMP_CD',
                             '종목명'='CMP_KOR'))

# 수익률 상위 30프로들의 정보 살펴봄
data_market[invest_mom,] %>%
  select(SEC_NM_KOR) %>% 
  group_by(SEC_NM_KOR) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(SEC_NM_KOR,n),
             y=n,label=n))+
  geom_col(fill='skyblue',color='black')+
  geom_text(color='black',size=4,hjust=-0.3)+
  xlab(NULL)+
  ylab(NULL)+
  coord_flip()+
  scale_y_continuous(expand=c(0,0,0.1,0))+
  theme_classic() # 특정섹터에 12개월 모멘텀 포폴 조옴ㄱ이 몰려있음. 
# 즉 여러종목으로 포트폴리오를 구성했음에도 불구, 이를 분해해보면 특정섹터에 쏠림이 심함.

# --> 섹터 중립 포트폴리오 구성해보자
sector_neutral = data_market %>% 
  select(종목코드, SEC_NM_KOR) %>% 
  mutate(ret = ret_12m) %>% # 12개월 수익률 정보를 새로운 열에 합쳐줌
  group_by(SEC_NM_KOR) %>% 
  mutate(scale_per_sector = scale(ret),
         scale_per_sector = ifelse(is.na(SEC_NM_KOR),
                                   NA, scale_per_sector)) # 섹터정보없을 경우 NA 처리
sector_neutral %>% head()
# 섹터별로 수익률 계산한 뒤 정규화 -> 전체 종목 대비 해당 종목의 과거 수익률이 높아도 섹터끼리 정규화하기 때문에 해당 섹터 내에서의 순위가 낮으면 정규화된 값은 작아짐 -> 정규화된 값으로 포트폴리오 구성

invest_mom_neutral = 
  rank(-sector_neutral$scale_per_sector)<=30

data_market[invest_mom_neutral,] %>% 
  select(SEC_NM_KOR) %>% 
  group_by(SEC_NM_KOR) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(SEC_NM_KOR,n),
             y=n, label = n))+
  geom_col(fill='skyblue',color='black')+
  geom_text(color='black',size=4, hjust=-0.3)+
  xlab(NULL)+ylab(NULL)+
  coord_flip()+
  scale_y_continuous(expand=c(0,0,0.1,0))+
  theme_classic()
# 그래도 섹터 개수가 늘어남.

## 마법공식
# 하나의 팩터보다는 둘 이상의 팩터를 결합하여 투자하는 것이 좋음 -> 'multi factor'
# 밸류 + 퀄리티 두 개의 팩터를 조합하는 것이 전통적으로 많이 사용된 방법! -> "마법공식"

# 밸류 팩터와 퀄리티 팩터 = 좋은 기업을 싸게 사는 것!
# 가치주(밸류) = 위험이 큼 -> 시장에서 소외받아 저평가 + 위험에 대한 대가로 수익률 높음
# 우량주(퀄리티) = 기꺼이 프리미엄을 지불하려고 함 -> 수익률 높음 

# 퀄리티 지표인 매출총이익, 밸류 지표인 PBR로 두 지표 관계 확인
library(stringr)
library(dplyr)

KOR_value = read.csv('data/KOR_value.csv',row.names = 1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv',row.names=1,
                      stringsAsFactors = FALSE)
data_pbr = KOR_value['PBR']

if (lubridate::month(Sys.Date()) %in% c(1,2,3,4)){
  num_col = str_which(colnames(KOR_fs[[1]]),as.character(lubridate::year(Sys.Date())-2))
} else{
  num_col = str_which(colnames(KOR_fs[[1]]),as.character(lubridate::year(Sys.Date())-1))
}

data_gpa = 
  (KOR_fs$매출총이익/KOR_fs$자산)[num_col] %>% 
  setNames('GPA')

cbind(data_pbr, -data_gpa) %>%  # PBR은 오름차순, gpa는 내림차순이기때문에 -붙임
  cor(method = 'spearman',use = 'complete.obs') %>% 
  round(4)
# 서로 간의 반대 관계가 있음 

cbind(data_pbr, data_gpa) %>% 
  mutate(quantile_pbr = ntile(data_pbr, 5)) %>% 
  filter(!is.na(quantile_pbr)) %>% # PBR이 없는 종목은 제외
  group_by(quantile_pbr) %>% 
  summarise(mean_gpa = mean(GPA, na.rm = TRUE)) %>%  # 각 PBR 분위별로 GPA 평균계산 
  ggplot(aes(x=quantile_pbr, y =mean_gpa))+ 
  geom_col(color = 'black', fill = 'skyblue')+
  xlab('PBR')+ylab('GPA')

# PBR낮을수록 GPA 낮다 -> 가치주일수록 우량성은 떨어진다
# PBR높을수록 GPA 높다 -> 가격 높을수록 우량성 높다

# 마법공식의 첫번쨰 지표 = 이율(Earnings Yield) = 기업의 수익/가치, 밸류지표 중 하나
# 두번째 지표 = 투하자본 수익률(Return on Capital) = 기업의 수익/투자자본, 퀄리티 지표 중 하나
# 마법공식은 이 두가지 지표의 랭킹을 구한 후 랭킹의 합 상위 30개 종목을 1년간 보유한 후 매도하는 것

# 통상적으로 이율대신 PER, 투하자본수익률 대신 ROE를 사용함! but 우리 데이터에서는 구하기 가능

library(stringr)
library(dplyr)

KOR_value = read.csv('data/KOR_value.csv', row.names=1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv',row.names=1,
                      stringsAsFactors = FALSE)
KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드,6,'left',0)

if(lubridate::month(Sys.Date()) %in% c(1,2,3,4)){
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date())-2))
} else{
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date())-1))
}

# 이익수익률 = 이자 및 법인세 차감전이익/기업가치 = 이자 및 법인세 차감전이익/(시가총액 + 순차입금)
# = (당기순이익+법인세+이자비용)/(시가총액+총부채-여유자금)
# = (당기순이익+법인세+이자비용)/(시가총액+총부채-(현금-max(0,유동부차-유동자산+현금)))

# 분자
magic_ebit = (KOR_fs$지배주주순이익+KOR_fs$법인세비용+KOR_fs$이자비용)[num_col]

# 분모
magic_cap = KOR_value$PER * KOR_fs$지배주주순이익[num_col]
magic_dept = KOR_fs$부채[num_col]
magic_excess_cash_1 = KOR_fs$유동부채 - KOR_fs$유동자산 + KOR_fs$현금및현금성자산
magic_excess_cash_1[magic_excess_cash_1<0]=0
magic_excess_cash_2 = (KOR_fs$현금및현금성자산-magic_excess_cash_1)[num_col]

magic_ev = magic_cap + magic_dept - magic_excess_cash_2

# 이익수익률 
magic_ey = magic_ebit/magic_ev

# 투하자본수익률 = 이자 및 법인세 차감전이익 / 투하자본 
# = (당기순이익 + 법인세 + 이자비용) / ((유동자산 - 유동부채) + (비유동자산 - 감가상각비))
magic_ic = ((KOR_fs$유동자산-KOR_fs$유동부채)+
              (KOR_fs$비유동자산-KOR_fs$감가상각비))[num_col]

magic_roc = magic_ebit/magic_ic

# 마법공식 포트폴리오 구성
invest_magic = rank(rank(-magic_ey)+rank(-magic_roc))<=30  #내림차순으로

KOR_ticker[invest_magic,] %>% 
  select(종목코드, 종목명) %>% 
  mutate(이익수익률 = round(magic_ey[invest_magic,],4),
          투하자본수익률 = round(magic_roc[invest_magic,],4))

## 이상치 데이터 제거 및 팩터의 결합
library(magrittr)
library(ggplot2)

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)

max(KOR_value$PBR, na.rm = TRUE)

KOR_value %>% ggplot(aes(x=PBR))+
  geom_histogram(binwidth = 0.1)
# -> 오른쪽으로 꼬리가 매우 긴 분포 --> PBR에 508이라는 이상치데이터가 있기 때문!!

# 1. Trim (이상치 데이터 삭제)
library(dplyr)

value_trim = KOR_value %>% 
  select(PBR) %>% 
  mutate(PBR = ifelse(percent_rank(PBR)>0.99, NA, PBR),
         PBR = ifelse(percent_rank(PBR)<0.01,NA, PBR))
value_trim %>% 
  ggplot(aes(x=PBR))+
  geom_histogram(binwidth = 0.1) # 상하위 1프로 제거 
# 그러나 이 방법은 데이터 손실이 발생하며 제거된 종목 중 좋은 종목의 잠재력 제거해버림

# 2. Winsorizing (이상치 데이터 대체)
# 상위 99퍼를 초과하는 데이터를 99%값으로 대체, 하위 1%미만의 데이터는 1%데이터로 대체하는 방식
value_winsor = KOR_value %>% 
  select(PBR) %>% 
  mutate(PBR = ifelse(percent_rank(PBR)>0.99,
                      quantile(.,0.99,na.rm=TRUE),PBR),
         PBR = ifelse(percent_rank(PBR)<0.01,
                      quantile(.,0.01,na.rm=TRUE),PBR))
value_winsor %>% 
  ggplot(aes(x=PBR))+
  geom_histogram(binwidth = 0.1)

## 팩터의 결합방법
# 단순히 랭킹을 더하는 것의 장덤은 극단치로 인한 효과 제거, 균등한 분포를 가진다는 것
library(tidyr)

KOR_value %>% 
  mutate_all(list(~min_rank(.))) %>% 
  gather() %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(.~key)

# 그러나 x 지표를 볼 겨우 최댓값이 다 다름. 글고 서로 다른 범위의 분포를 단순히 합치는 것은 좋은 방법이 아님!
# --> 랭킹과 비중의 가중평균을 구해 포트폴리오 구성하지말고 Z-Score로 정규화하자
KOR_value %>% 
  mutate_all(list(~min_rank(.))) %>% 
  mutate_all(list(~scale(.))) %>% 
  gather() %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(.~key)


## 멀티팩터포트폴리오
# 퀄리티(자기자본이익률, 매출총이익, 영업활동현금흐롬)
# 밸류(PBR, PER, PSR, PCR)
# 모멘텀(3/6/12개월 수익률)
library(xts)
library(stringr)

KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_value = read.csv('data/KOR_value.csv', row.names=1, stringsAsFactors = FALSE)
KOR_price = read.csv('data/KOR_price.csv', row.names=1, stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names=1, stringsAsFactors = FALSE)

KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드, 6, 'left', 0)

if(lubridate::month(Sys.Date()) %in% c(1,2,3,4)){
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date())-2))
} else{
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date())-1))
}

# 퀄리티 지표
quality_roe = (KOR_fs$지배주주순이익/KOR_fs$자본)[num_col]
quality_gpa = (KOR_fs$매출총이익/KOR_fs$자산)[num_col]
quality_cfo = (KOR_fs$영업활동으로인한현금흐름/KOR_fs$자산)[num_col]

quality_profit = cbind(quality_roe, quality_gpa, quality_cfo) %>% 
  setNames(.,c('ROE','GPA','CFO'))

factor_quality = quality_profit %>% 
  mutate_all(list(~min_rank(desc(.)))) %>% 
  mutate_all(list(~scale(.))) %>% 
  rowSums()

factor_quality %>% data.frame() %>% 
  ggplot(aes(x=`.`))+
  geom_histogram()


# 밸류 지표
factor_value = KOR_value %>% 
  mutate_all(list(~min_rank(.))) %>% 
  mutate_all(list(~scale(.))) %>% 
  rowSums()

factor_value %>% 
  data.frame() %>% 
  ggplot(aes(x=`.`))+geom_histogram()


# 모멘텀 지표
library(PerformanceAnalytics)
library(dplyr)

ret_3m = Return.calculate(KOR_price) %>% xts::last(60) %>% 
  sapply(.,function(x){prod(1+x)-1})
ret_6m = Return.calculate(KOR_price) %>% xts::last(120) %>% 
  sapply(.,function(x){prod(1+x)-1})
ret_12m = Return.calculate(KOR_price) %>% xts::last(252) %>% 
  sapply(.,function(x){prod(1+x)-1})

ret_bind = cbind(ret_3m, ret_6m, ret_12m) %>% data.frame()

factor_mom = ret_bind %>% 
  mutate_all(list(~min_rank(desc(.)))) %>% 
  mutate_all(list(~scale(.))) %>% 
  rowSums()

factor_mom %>% 
  data.frame() %>% 
  ggplot(aes(x=`.`))+geom_histogram()

# 각 팩터 간 상관관계
library(corrplot)

cbind(factor_quality, factor_value, factor_mom) %>% 
  data.frame() %>% 
  setNames(c('Qaulity','Value','Momentum')) %>% 
  cor(use = 'complete.obs') %>% 
  round(.,2) %>% 
  corrplot(method='color',type='upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
           col = 
             colorRampPalette(c('blue','white','red'))(200),
           mar = c(0,0,0.5,0))

# 계산된 팩터 토대로 최종 포트폴리오 구성
factor_qvm = 
  cbind(factor_quality, factor_value, factor_mom) %>% 
  data.frame() %>% 
  mutate_all(list(~scale(.))) %>% 
  mutate(factor_quality = factor_quality * 0.33, 
         factor_value = factor_value * 0.33,
         factor_mom = factor_mom * 0.33) %>% 
  rowSums() # 각 팩터에 동일 비중 부여

invest_qvm = rank(factor_qvm)<=30

# 선택된 종목의 퀄리티 지표 분포 확인
quality_profit[invest_qvm,] %>% 
  gather() %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(.~key, scale = 'free', ncol=1)+
  xlab(NULL)

# 밸류 지표 분포 확인
KOR_value[invest_qvm,] %>% 
  gather() %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(.~key, scale = 'free', ncol = 1)+
  xlab(NULL)

# 기간별 수익률 분포 확인
ret_bind[invest_qvm,] %>% 
  gather() %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(.~key, scale='free', ncol=1)+
  xlab(NULL)

# 포트폴리오 내 종목 대상으로 전부다 확인!
KOR_ticker[invest_qvm,] %>% 
  select(종목코드, 종목명) %>% 
  cbind(round(quality_roe[invest_qvm,],2)) %>% 
  cbind(round(KOR_value$PBR[invest_qvm],2)) %>% 
    cbind(round(ret_12m[invest_qvm],2)) %>% 
  setNames(c('종목코드','종목명','ROE','PBR','12M'))

# 포트폴리오 내 종목별 지표 평균
cbind(quality_profit,KOR_value,ret_bind)[invest_qvm,] %>% 
  apply(.,2,mean) %>% round(3) %>% t()
