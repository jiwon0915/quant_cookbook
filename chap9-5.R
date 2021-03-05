## 퀄리티 전략
# 퀄리티 = 기업의 우량성!
# 우량성 관련 지표 : 수익성, 수익의 안정성, 기업구조, 수익의 성장성, 회계적 우량성, 배당, 투자
# 퀄리티 전략에는 재무제표 데이터 주로 사용함

# F-score 지표
# 저PBR을 이용한 벨류전략은 가끔 재무상태가 불량한 기업이 많다. 따라서 저 PBR 종목 중 재무적으로 우량한 기업 골라 투자하면 성과가 훨씬 좋을 수 있음!
# 이때 재무적 우량정도는 9개의 지표로 선정됨~

library(stringr)
library(ggplot2)
library(dplyr)

KOR_fs = readRDS('data/KOR_fs.Rds') # 재무제표 데이터를 리스트 형태로 불러옴
KOR_ticker = read.csv('data/KOR_ticker.csv',row.names=1,
                      stringsAsFactors = FALSE)
KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드,6,'left',0)

# 수익성 부문
ROA = KOR_fs$지배주주순이익/KOR_fs$자산
CFO = KOR_fs$영업활동으로인한현금흐름 / KOR_fs$자산
ACCURUAL = CFO-ROA

# 재무성과 부문
LEV = KOR_fs$장기차입금 / KOR_fs$자산
LIQ = KOR_fs$유동자산 / KOR_fs$유동부채
OFFER = KOR_fs$유상증자 # 발행주식수 대신 유상증자여부 사용

# 운영 효율성 부문
MARGIN = KOR_fs$매출총이익 / KOR_fs$매출액
TURN = KOR_fs$매출액 / KOR_fs$자산

# 각 지표의 충족여부에 따라 1점 or 0점 부여
if (lubridate::month(Sys.Date()) %in% c(1,2,3,4)){
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date())-2)) # 재무제표 데이터의 컬럼 중 2년 전 컬럼의 컬럼넘버를 알려줌
} else{
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date())-1)) # 재무제표 데이터의 컬럼 중 1년 전 컬럼의 컬럼 넘버를 알려줌
}

# 1월~4월에 데이터 받으면 전년도 재무제표가 일부만 들어오는 경향이 있음. 따라서 전전년도 데이터 사용해야함!! 

F_1 = as.integer(ROA[,num_col]>0) # ROA가 양수면 1점 (as.integer(TRUE)=1, as.integer(FALSE)=0)
F_2 = as.integer(CFO[, num_col] > 0) # 영업활동현금흐름이 양수면 1점
F_3 = as.integer(ROA[, num_col] - ROA[, (num_col-1)] > 0) # 전년대비 ROA 증가했으면 1점
F_4 = as.integer(ACCURUAL[, num_col] > 0) # CFO-ROA가 양수면 1점
F_5 = as.integer(LEV[, num_col] - LEV[, (num_col-1)] <= 0)  # 전년대비 레버리지 감소했으면 1점
F_6 = as.integer(LIQ[, num_col] - LIQ[, (num_col-1)] > 0) # 전년대비 유동성 증가했으면 1점
F_7 = as.integer(is.na(OFFER[,num_col]) |
                   OFFER[,num_col] <= 0) # 유상증자 항목이 없거나 0보다 작으면 1점
F_8 = as.integer(MARGIN[, num_col] -
                   MARGIN[, (num_col-1)] > 0) # 매출총이익률이 전년대비 증가했으면 1점
F_9 = as.integer(TURN[,num_col] - TURN[,(num_col-1)] > 0) # 회전율이 전년대비 증가했으면 1점

F_Table = cbind(F_1, F_2, F_3, F_4, F_5, F_6, F_7, F_8, F_9)
F_Score = F_Table %>% 
  apply(., 1, sum, na.rm = TRUE) %>% # row sum 
  setNames(KOR_ticker$종목명)
(F_dist = prop.table(table(F_Score)) %>% round(3))

F_dist %>% 
  data.frame() %>% 
  ggplot(aes(x=F_Score,y=Freq))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label = paste0(Freq,"%")),
            color='black',size=3,vjust=-0.4)+
  scale_y_continuous(expand = c(0,0,0,0.05), # 5 percent 단위
                     labels = scales::percent)+
  ylab(NULL)+
  theme_classic()

invest_F_Score = F_Score %in% c(9)
KOR_ticker[invest_F_Score,] %>% 
  select(종목코드, 종목명) %>% 
  mutate(`F-Score` = F_Score[invest_F_Score])

# 수익성 지표를 결합한 포트폴리오 만들기
# ROE(자기자본이익률), 매출총이익(Gross Profit), 영업활동현금흐롬(Cash Flow From Operationg)

library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)

KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv',row.names=1,
                      stringsAsFactors = FALSE)
KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드, 6, 'left',0)

# 이것도 1~4월이면 전전년도 데이터 사용
if(lubridate::month(Sys.Date()) %in% c(1,2,3,4)){
  col_number = str_which(colnames(KOR_fs[[1]]),as.character(lubridate::year(Sys.Date())-2))
} else{
  col_number = str_which(colnames(KOR_fs[[1]]),as.character(lubridate::year(Sys.Date())-1))
}

quality_roe = (KOR_fs$지배주주순이익 / KOR_fs$자본)[col_number]
quality_gpa = (KOR_fs$매출총이익 / KOR_fs$자산)[col_number]
quatlity_cfo = (KOR_fs$영업활동으로인한현금흐름 / KOR_fs$자산)[col_number]

quality_profit = 
  cbind(quality_roe, quality_gpa, quatlity_cfo) %>% 
  setNames(.,c('ROE','GPA','CFO'))

rank_quality = quality_profit %>% 
  mutate_all(list(~min_rank(desc(.)))) # 퀄리티 지표는 높을수록 좋음 -> 내림차순해서 랭킹계산

cor(rank_quality, use = 'complete.obs') %>% 
  round(.,2) %>% 
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt=45, tl.col = 'black',
           col = colorRampPalette(c('blue','white','red'))(200),
           mar = c(0,0,0.5,0))
# 통합적 고려시 분산효과 기대 가능!

rank_sum = rank_quality %>% rowSums()

invest_quality = rank(rank_sum)<=30

KOR_ticker[invest_quality,] %>% 
  select(종목코드,종목명) %>% 
  cbind(round(quality_profit[invest_quality,],4))
