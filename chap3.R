url.aapl = "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG"
data.aapl = read.csv(url.aapl)

head(data.aapl)

# getSymbols()를 활용한 주가 다운로드
library(quantmod)
getSymbols('AAPL')
head(AAPL)
tail(AAPL)

chart_Series(Ad(AAPL))

data = getSymbols('AAPL',
                  from = '2000-01-01' , to = '2018-12-31',
                  auto.assign=FALSE) # auto.assign = FALSE일 경우 지정한 변수에 데이터 저장장
head(data)

ticker = c('FB','NVDA') # 페이스북, 엔비디아 주가
getSymbols(ticker)

head(FB)
head(NVDA)

# 국내 종목 주가 다운로드
# 국내 종목의 티커는 총 6자리 구성, 코스피는 그 뒤에 .KS, 코스닥은 .KQ
getSymbols('005930.KS',
           from = '2000-01-01', to = '2018-12-31')
tail(Ad(`005930.KS`)) # Ad = 수정주가 확인, ``은 마침표 때문에 붙임

tail(Cl(`005930.KS`)) # Cl = 종가

# 셀트리온 예시
getSymbols('068760.KQ',
           from = '2000-01-01', to = '2018-12-31')
tail(Cl(`068760.KQ`))

# FRED 데이터 다운로드
# FRED = Federal Reserve Economic Data 
getSymbols('DGS10', src = 'FRED') # 미 국채 10년물 금리에 해당하는 티커. src = 데이터 출처
chart_Series(DGS10) 
 
getSymbols('DEXKOUS', src = 'FRED') # 원달러환율
tail(DEXKOUS)
