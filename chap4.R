# 금융 속보 크롤링
library(rvest)
library(httr)

url = 'https://finance.naver.com/news/news_list.nhn?mode=LSS2D&section_id=101&section_id2=258'
data = GET(url)

print(data)

data_title = data %>% 
  read_html(encoding = 'EUC-KR') %>% 
  html_nodes('dl') %>% 
  html_nodes('.articleSubject') %>% 
  html_nodes('a') %>% 
  html_attr('title')
print(data_title)

# 기업공시채널에서 오늘의 공시 불러오기
library(httr)
library(rvest)

Sys.setlocale("LC_ALL", "English")

url = "https://dev-kind.krx.co.kr/disclosure/todaydisclosure.do"
data = POST(url, body = 
              list(
                method = 'searchTodayDisclosureSub',
                currentPageSize = '15',
                pageIndex = '1',
                orderMode = '0',
                orderStat = 'D',
                forward = 'todaydisclosure_sub',
                chose = 'S',
                todayFlag = 'N',
                selDate = '2018-12-28'
              ))

data = read_html(data) %>% 
  html_table(fill = TRUE) %>% 
  .[[1]]

Sys.setlocale("LC_ALL", "Korean")

print(head(data))


Sys.setlocale("LC_ALL", "English")

url = "https://dev-kind.krx.co.kr/disclosure/todaydisclosure.do"
data = POST(url, body = 
              list(
                method = 'searchTodayDisclosureSub',
                currentPageSize = '15',
                pageIndex = '1',
                orderMode = '0',
                orderStat = 'D',
                forward = 'todaydisclosure_sub',
                chose = 'S',
                todayFlag = 'N',
                selDate = '2020-12-28'
              ))

data = read_html(data) %>% 
  html_table(fill = TRUE) %>% 
  .[[1]]

Sys.setlocale("LC_ALL", "Korean")

print(head(data))

# 네이버 금융에서 주식 티커 크롤링
library(httr)
library(rvest)

i = 0
ticker = list()
url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=1')
down_table = GET(url)

navi.final = read_html(down_table, encoding = 'EUC-KR') %>% 
  html_nodes(.,'.pgRR') %>% 
  html_nodes(.,"a") %>% 
  html_attr(.,"href")

print(navi.final)

navi.final = navi.final %>% 
  strsplit(.,"=") %>% 
  unlist() %>% 
  tail(.,1) %>% 
  as.numeric()

print(navi.final)

i = 0 # 코스피
j = 1 # page number
url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=',j)
down_table = GET(url)

Sys.setlocale("LC_ALL","English")

table = read_html(down_table, encoding = "EUC-KR") %>% 
  html_table(fill = TRUE)
Sys.setlocale("LC_ALL", "Korean")  

table = table[[2]]

View(table)

table[,ncol(table)] = NULL
table = na.omit(table)
View(table)

ticker_info = read_html(down_table, encoding = "EUC-KR") %>% 
  html_nodes(.,"tbody") %>% 
  html_nodes(., "td") %>% 
  html_nodes(., "a") %>% 
  html_attr(.,"href")

print(head(ticker_info))

# install.packages("stringr")
library(stringr)

ticker_info = sapply(ticker_info, function(x){
  str_sub(x, -6, -1)
})

print(head(ticker_info))
ticker_info = unique(ticker_info)

print(head(ticker_info))

table$N = ticker_info
colnames(table)[1] = '종목코드'

rownames(table) = NULL #  아까 na.omit 했던거때문에 행 이름 초기화

ticker[[j]] = table

## for 문 적용
data = list()

for(i in 0:1){
  library(rvest)
  library(httr)
  if (i == 0) print("코스피")
  if (i == 1) print("코스닥")
  url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=1')
  down_table = GET(url)
  
  navi.final = read_html(down_table, encoding = 'EUC-KR') %>% 
    html_nodes(.,'.pgRR') %>% 
    html_nodes(.,"a") %>% 
    html_attr(.,"href") >% 
    strsplit(.,"=") %>% 
    unlist() %>% 
    tail(.,1) %>% 
    as.numeric()
  
  ticker = list()
  
  for (j in 1:navi.final){
    print(paste0("현재페이지: ",j))
    url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=',j)
    down_table = GET(url)
    
    Sys.setlocale("LC_ALL","English")
    
    table = read_html(down_table, encoding = "EUC-KR") %>% 
      html_table(fill = TRUE)
    Sys.setlocale("LC_ALL", "Korean")  
    
    table = table[[2]]
    
    table[,ncol(table)] = NULL
    table = na.omit(table)
    
    ticker_info = read_html(down_table, encoding = "EUC-KR") %>% 
      html_nodes(.,"tbody") %>% 
      html_nodes(., "td") %>% 
      html_nodes(., "a") %>% 
      html_attr(.,"href")

    ticker_info = sapply(ticker_info, function(x){
      str_sub(x, -6, -1)
    })
    
    ticker_info = unique(ticker_info)
    
    table$N = ticker_info
    colnames(table)[1] = '종목코드'
    
    rownames(table) = NULL 
    
    ticker[[j]] = table
    
    Sys.sleep(0.5)
    
  }
  
  ticker = do.call(rbind, ticker)
  data[[i+1]] = ticker
}

data = do.call(rbind, data)
