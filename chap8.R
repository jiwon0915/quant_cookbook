# 종목정보 데이터 분석 
# 산업별 현황, 개별지표 정리파일, WICS 기준 섹터 지표를 정리한 파일 통해 데이터 분석

library(stringr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE)

#############
library(httr)
library(rvest)
library(stringr)

url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_1"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

sector_code = c('G25', 'G35', 'G50', 'G40', 'G10',
                'G20', 'G55', 'G30', 'G15', 'G45')
data_sector = list()

library(jsonlite)
for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  
  Sys.sleep(1)
}


data_sector = do.call(rbind, data_sector)
write.csv(data_sector, 'data/KOR_sector.csv')
KOR_sector = read.csv('data/KOR_sector.csv',row.names=1,
                      stringsAsFactors = FALSE)

###################################
KOR_ticker$종목코드 = 
  str_pad(KOR_ticker$종목코드, 6, side = c('left'), 0)
KOR_sector$CMP_CD = 
  str_pad(KOR_sector$CMP_CD, 6, 'left', 0)

## 데이터 합치기
# 티커 기준으로 데이터를 맞추자 -> left_join
data_market = KOR_ticker %>% left_join(KOR_sector, by = c('종목코드'='CMP_CD',
                                            '종목명'='CMP_KOR'))
head(data_market)

# 데이터 구조 확인하기 -> glimpse()
glimpse(data_market)

# 열이름 바꾸기 -> rename()
head(names(data_market),15)

data_market<-data_market %>% 
  rename(`배당수익률(%)`=`배당수익률`)

head(names(data_market),15)

# 고유한 값 확인 -> distinct()
data_market %>% 
  distinct(SEC_NM_KOR) %>% c()

# 데이터 변형
data_market = data_market %>% 
  mutate(`PBR` = as.numeric(PBR),
         `PER` = as.numeric(PER),
         `ROE` = PBR/PER,
         `ROE` = round(ROE,4),
         `size` = ifelse(`시가총액`>=
                           median(`시가총액`, na.rm=TRUE),
                         'big','small'))
data_market %>% select(종목명, ROE, size) %>% head()

# 순위 계산 -> row_numer()
data_market %>% 
  mutate(PBR_rank = row_number(PBR)) %>% 
  select(종목명, PBR, PBR_rank) %>% 
  arrange(PBR) %>% 
  head(5)

data_market %>% 
  mutate(PBR_rank = row_number(desc(ROE))) %>% 
  select(종목명, ROE, PBR_rank) %>% 
  arrange(desc(ROE)) %>% 
  head(5)

data_market %>% 
  mutate(PBR_rank = row_number(desc(ROE))) %>% 
  select(종목명, ROE, PBR_rank) %>% 
  arrange(PBR_rank) %>% 
  head(5)

# 분위수 계산 -> ntile()
data_market %>%
  mutate(PBR_tile = ntile(PBR, n=5)) %>% 
  select(PBR, PBR_tile) %>% 
  head()

# ggplot
library(ggplot2)

## 종목정보시각화
data_market %>% ggplot(aes(x=ROE, y=PBR))+
  geom_point()

data_market %>% ggplot(aes(x=ROE, y=PBR))+
  geom_point()+
  coord_cartesian(xlim = c(0,0.3), ylim = c(0,3))

data_market %>% ggplot(aes(x=ROE, y=PBR))+
  geom_point(aes(col=`시장구분`,shape=`시장구분`))+
  geom_smooth(method='lm',
              aes(col=`시장구분`))+
  coord_cartesian(xlim=c(0,0.3),ylim=c(0,3))

data_market %>% ggplot(aes(x=PBR))+
  geom_histogram(binwidth = 0.1)+
  coord_cartesian(xlim = c(0,10))

data_market %>% ggplot(aes(x=PBR))+
  geom_histogram(aes(y=..density..),
                 binwidth = 0.1, color='skyblue',
                 fill='skyblue')+
  coord_cartesian(xlim=c(0,10))+
  geom_density(color='red')+
  geom_vline(aes(xintercept = median(PBR, na.rm = TRUE)),
             color='blue')+
  geom_text(aes(label = median(PBR, na.rm = TRUE),
                x = median(PBR, na.rm = TRUE), y = 0.05),
            col='black',size = 6, hjust = -0.5)
# geom_boxplot
data_market %>% ggplot(aes(x=SEC_NM_KOR, y= PER))+
  geom_boxplot()+
  coord_flip()

data_market %>% filter(!is.na(SEC_NM_KOR)) %>% 
  group_by(SEC_NM_KOR) %>% 
  summarise(ROE_sector = median(ROE, na.rm=TRUE),
            PBR_sector = median(PBR, na.rm=TRUE)) %>% 
  ggplot(aes(x=ROE_sector,y=PBR_sector))+
  geom_point(aes(col=SEC_NM_KOR),size=3)+
  geom_text(aes(label = SEC_NM_KOR),
            color='black',size=3,vjust=1.3)+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# geom_bar
data_market %>% filter(!is.na(SEC_NM_KOR)) %>% 
  group_by(SEC_NM_KOR) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(SEC_NM_KOR,n),y=n))+
  geom_bar(stat='identity')+
  theme_classic()+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  geom_text(aes(label=n),color = 'black', size= 4, hjust=-0.3)

## 주가 및 수익률 시각화
# 주가 그래프 나타내기
library(quantmod)

getSymbols('SPY') # S&P 500 지수 추종하는 ETF인 SPY 데이터 다운로드
prices = Cl(SPY) # 종가만 추출

plot(prices,main='Price')

SPY %>% 
  ggplot(aes(x=Index, y = SPY.Close))+
  geom_line()

# 인터랙티브 그래프 나타내기
library(dygraphs)

dygraph(prices) %>% 
  dyRangeSelector()

library(highcharter)

highchart(type='stock') %>% 
  hc_add_series(prices) %>% 
  hc_scrollbar(enabled=FALSE)

library(plotly)

p = SPY %>% 
  ggplot(aes(x=Index, y=SPY.Close))+
  geom_line()

ggplotly(p)

# 샤이니 사용
prices %>% 
  fortify.zoo %>% 
  plot_ly(x = ~Index, y = ~SPY.Close) %>% 
  add_lines()

## 연도별 수익률
library(PerformanceAnalytics)

ret_yearly = prices %>% 
  Return.calculate() %>%  # 일별 수익률 구함
  apply.yearly(., Return.cumulative) %>%  # 연도별 수익률구할땐 일을 누적해야됨-> Cumulative
  round(4) %>% 
  fortify.zoo() %>% 
  mutate(Index = as.numeric(substring(Index, 1, 4))) # substring통해 연도추출

ret_yearly %>% 
  ggplot(aes(x=Index,y=SPY.Close))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label = paste0(round(SPY.Close*100,2),"%"),
                vjust = ifelse(SPY.Close>=0,-0.5,1.5)),
            position = position_dodge(width = 1),
            size = 3)+
  scale_x_continuous(breaks = ret_yearly$Index,
                     expand = c(0.01, 0.01))+ # x축에 모든 연도 표시되도록
  xlab(NULL)+ylab(NULL)
