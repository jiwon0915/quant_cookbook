# 재무제표 및 가치지표 크롤링
library(httr)
library(rvest)

ifelse(dir.exists('data/KOR_fs'),FALSE,
       dir.create('data/KOR_fs'))

Sys.setlocale("LC_ALL",'English')

url = paste0('http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A005930')

data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))

data = data %>% 
  read_html() %>% 
  html_table()

Sys.setlocale("LC_ALL",'Korean')

lapply(data, function(x){
  head(x,3)})

# 연간 기준 재무제표에 해당하는 1,3,5 테이블 선택
data_IS = data[[1]]
data_BS = data[[3]]
data_CF = data[[5]]

print(names(data_IS))
print(names(data_BS))
print(names(data_CF))

# 통일성 위해 전년동기 열 삭제
data_IS = data_IS[,1:(ncol(data_IS)-2)]

data_fs = rbind(data_IS,data_BS,data_CF)
data_fs %>% head()
# 계산에 참여한 계정 펼치기 -> 삭제해도 됨
data_fs[,1] = gsub('계산에 참여한 계정 펼치기','',
                   data_fs[,1])
data_fs = data_fs[!duplicated(data_fs[,1]),] # 중복되는 계정명은 다 삭제제

rownames(data_fs)

# 행 이름 초기화, 첫번째 열을 행이름으로 변경
rownames(data_fs)=NULL
rownames(data_fs) = data_fs[,1]
data_fs[,1]=NULL

# 분기 재무제표 들어간 경우 없앰
data_fs = data_fs[,substr(colnames(data_fs),6,7)==12]

sapply(data_fs, typeof)

# 숫자형으로 변경
library(stringr)

data_fs = sapply(data_fs,function(x){
  str_replace_all(x, ',','') %>% 
    as.numeric()
}) %>% 
  data.frame(.,row.names = rownames(data_fs))

print(head(data_fs))

sapply(data_fs, typeof)

write.csv(data_fs,'data/KOR_fs/005930_fs.csv')

# 가치 지표 계산하기
# PER(Earnings, 순이익), PBR(Book Value, 순자산)
# PCR(Cashflow, 영업활동현금흐름), PSR(Sales, 매출액)
# 이런 가치지표 계산할 때 분자는 주가, 분모는 재무제표 데이터를 사용함

# 먼저 재무제표 데이터 불러옴
ifelse(dir.exists('data/KOR_value'),FALSE,
       dir.create('data/KOR_value'))

value_type = c('지배주주순이익',
               '자본',
               '영업활동현금흐름',
               '매출액')
value_index = data_fs[match(value_type,rownames(data_fs)),ncol(data_fs)] # 최근년도 재무제표 데이터 선택
print(value_index)


# 주가 수집
library(readr)

url = 'http://comp.fnguide.com/SVO2/ASP/SVD_main.asp?pGB=1&gicode=A005930'
data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'
                      ))
price = read_html(data) %>% 
  html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>% 
  html_text() %>% 
  parse_number()

price

# PER = 주가/주당순이익으로 주당-> 발행주식수 필요
share = read_html(data) %>% 
  html_node(xpath = '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>% 
  html_text()

share = share %>% 
  strsplit(.,'/') %>% 
  unlist() %>% 
  .[1] %>% 
  parse_number()

# 가치지표 계산
data_value = price / (value_index*100000000/share)
names(data_value) = c('PER','PBR','PCR','PSR')
data_value[data_value<0]=NA

print(data_value)

write.csv(data_value, 'data/KOR_value/005930_value.csv')

# 전 종목 재무제표 및 가치지표 다운로드
library(stringr)
library(httr)
library(rvest)
library(stringr)
library(readr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6,side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_fs'), FALSE,
       dir.create('data/KOR_fs'))
ifelse(dir.exists('data/KOR_value'), FALSE,
       dir.create('data/KOR_value'))

for(i in 431 : nrow(KOR_ticker) ) {
  
  data_fs = c()
  data_value = c()
  name = KOR_ticker$'종목코드'[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    
    Sys.setlocale('LC_ALL', 'English')
    
    # url 생성
    url = paste0(
      'http://comp.fnguide.com/SVO2/ASP/'
      ,'SVD_Finance.asp?pGB=1&gicode=A',
      name)
    
    # 이 후 과정은 위와 동일함
    
    # 데이터 다운로드 후 테이블 추출
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                          AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36')) %>%
      read_html() %>%
      html_table()
    
    Sys.setlocale('LC_ALL', 'Korean')
    
    # 3개 재무제표를 하나로 합치기
    data_IS = data[[1]]
    data_BS = data[[3]]
    data_CF = data[[5]]
    
    data_IS = data_IS[, 1:(ncol(data_IS)-2)]
    data_fs = rbind(data_IS, data_BS, data_CF)
    
    # 데이터 클랜징
    data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                        '', data_fs[, 1])
    data_fs = data_fs[!duplicated(data_fs[, 1]), ]
    
    rownames(data_fs) = NULL
    rownames(data_fs) = data_fs[, 1]
    data_fs[, 1] = NULL
    
    # 12월 재무제표만 선택
    data_fs =
      data_fs[, substr(colnames(data_fs), 6,7) == "12"]
    
    data_fs = sapply(data_fs, function(x) {
      str_replace_all(x, ',', '') %>%
        as.numeric()
    }) %>%
      data.frame(., row.names = rownames(data_fs))
    
    
    # 가치지표 분모부분
    value_type = c('지배주주순이익', 
                   '자본', 
                   '영업활동으로인한현금흐름', 
                   '매출액') 
    
    # 해당 재무데이터만 선택
    value_index = data_fs[match(value_type, rownames(data_fs)),
                          ncol(data_fs)]
    
    # Snapshot 페이지 불러오기
    url =
      paste0(
        'http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp',
        '?pGB=1&gicode=A',name)
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
    
    # 현재 주가 크롤링
    price = read_html(data) %>%
      html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
      html_text() %>%
      parse_number()
    
    # 보통주 발행주식수 크롤링
    share = read_html(data) %>%
      html_node(
        xpath =
          '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
      html_text() %>%
      strsplit('/') %>%
      unlist() %>%
      .[1] %>%
      parse_number()
    
    # 가치지표 계산
    data_value = price / (value_index * 100000000/ share)
    names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
    data_value[data_value < 0] = NA
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    data_value <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(data_fs, paste0('data/KOR_fs/', name, '_fs.csv'))
  
  # 가치지표 저장
  write.csv(data_value, paste0('data/KOR_value/', name,
                               '_value.csv'))
  
  # 2초간 타임슬립 적용
  Sys.sleep(2)
}
