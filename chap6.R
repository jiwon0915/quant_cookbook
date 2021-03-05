# 개별종목 주가 크롤링
library(stringr)

KOR_ticker = read.csv('data/KOR_ticker.csv',row.names=1)
print(KOR_ticker$종목코드[1])

KOR_ticker$종목코드 = str_pad(KOR_ticker$종목코드, 6, side = c('left'), pad = 0)

# 삼성전자 주가 크롤링 후 가공
library(xts)

ifelse(dir.exists('data/KOR_price'),FALSE,
       dir.create('data/KOR_price')) # KOR_price 폴더 생성

i = 1
name = KOR_ticker$종목코드[i] # 티커 입력

price = xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성, 인덱스는 sys.date이용해 현재날짜 입력
print(price)

library(httr)
library(rvest)
library(lubridate)
library(stringr)
library(readr)

from = (Sys.Date() - years(3)) %>%  str_remove_all('-')
to = Sys.Date() %>% str_remove_all('-') # 3년치 데이터 

url = paste0('https://fchart.stock.naver.com/siseJson.nhn?symbol=',
             name, '&requestType=1&startTime=',from,'&endTime=',to,'&timeframe=day')
data = GET(url)
data_html = data %>% read_html %>% 
  html_text() %>% 
  read_csv()
print(data_html)

# 우리에게 필요한 날짜, 종가 컬럼만 선택하자
library(timetk)

price = data_html[c(1,5)]
colnames(price) = c('Date','Price')
price = na.omit(price)
price$Date = parse_number(price$Date) # pasre_number() : 문자형 데이터에서 문자 제거후 숫자형으로 변경해줌
price$Date = ymd(price$Date)
price = tk_xts(price, date_var = Date) # 시계열 형태로 변경, date_var은 인덱스를 의미.

print(head(price))
print(tail(price))


# 전 종목 주가 크롤링(for문 써서)
library(xts)

ifelse(dir.exists('data/KOR_price'),FALSE,
       dir.create('data/KOR_price')) # KOR_price 폴더 생성

for (i in 1:nrow(KOR_ticker)){
  print(i)
  name = KOR_ticker$종목코드[i] # 티커 입력

  price = xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성, 인덱스는 sys.date이용해 현재날짜 입력
  
  from = (Sys.Date() - years(3)) %>%  str_remove_all('-')
  to = Sys.Date() %>% str_remove_all('-') # 3년치 데이터 

  url = paste0('https://fchart.stock.naver.com/siseJson.nhn?symbol=',
              name, '&requestType=1&startTime=',from,'&endTime=',to,'&timeframe=day')
  data = GET(url)
  data_html = data %>% read_html %>% 
    html_text() %>% 
    read_csv()

  price = data_html[c(1,5)]
  colnames(price) = c('Date','Price')
  price = na.omit(price)
  price$Date = parse_number(price$Date) 
  price$Date = ymd(price$Date)
  price = tk_xts(price, date_var = Date) 
  
  write.csv(data.frame(price),
            paste0('data/KOR_price/',name,'_price.csv'))
  
  Sys.sleep(2)
}    



