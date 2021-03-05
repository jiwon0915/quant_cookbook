## 데이터 정리하기

# 주가 정리
# KOR_Price 폴더에 정리돼있는걸 하나로 합치자

library(stringr)
library(xts)
library(magrittr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names=1)
KOR_ticker$`종목코드` = 
  str_pad(KOR_ticker$종목코드, 6, side = c('left'), pad = '0')

price_list = list()

for (i in 1:nrow(KOR_ticker)){
  if(i%%100 == 0) print(i)
  name = KOR_ticker[i, '종목코드']
  price_list[[i]]=
    read.csv(paste0('data/KOR_price/',name,
                    '_price.csv'), row.names = 1) %>% 
    as.xts()
}

price_list = do.call(cbind,price_list) %>% na.locf() # 결측치에는 전일 데이터 사용
colnames(price_list) = KOR_ticker$종목코드

write.csv(data.frame(price_list),'data/KOR_price.csv')

# 재무제표 정리
# KOR_fs 폴더에 정리돼있는걸 하나로 합치자

library(stringr)
library(magrittr)
library(dplyr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names=1)
KOR_ticker$`종목코드`=
  str_pad(KOR_ticker$`종목코드`, 6, side = c('left'), pad= '0')

data_fs = list()

for(i in 1:nrow(KOR_ticker)){
  if(i%%50==0) print(i)
  name = KOR_ticker$종목코드[i]
  data_fs[[i]] = read.csv(paste0('data/KOR_fs/',name,
                                 '_fs.csv'), row.names=1)
}

# 한 기업당 총 237개의 항목에 대해 시계열 자료가 있음
# 그러나 재무제표 작성 항목은 각 업종별로 상이하므로, 이를 모두 고려하면 지나치게 데이터가 커짐.
# 따라서 대표적인 재무 항목을 정한 뒤 이를 기준으로 정리하자! -> 삼성전자

select_fs = lapply(data_fs, function(x){
  # 해당 항목이 있을 시 매출액 데이터 선택
  if('매출액' %in% rownames(x)){
    x[which(rownames(x)=='매출액'),]
    
    # 존재하지 않으면 NA 생성
  } else{
    data.frame(NA)
  }
})

select_fs = bind_rows(select_fs) # 리스트 내 데이터를 행으보 붂어줌
# bind row -> 열 개수 달라도 나머지 NA로 처리해서 묶는 rbind
print(head(select_fs))

# .NA 칼럼 삭제, 컬럼 순서 연도순으로 정리
select_fs = select_fs[!colnames(select_fs) %in% c('.','NA.')]
select_fs = select_fs[, order(names(select_fs))]

rownames(select_fs) = KOR_ticker[,'종목코드']

print(head(select_fs))

# 매출액 외 다른 재무항목에 대한 데이터 정리
fs_list = list()
fs_item = data_fs[[1]] %>% rownames()
for(i in 1:length(fs_item)){
  print(i)
  select_fs = lapply(data_fs, function(x){
    if (fs_item[i] %in% rownames(x)){
      x[which(rownames(x)==fs_item[i]),]
    } else{
      data.frame(NA)
    }
  })
  
  select_fs = bind_rows(select_fs)
  
  select_fs = select_fs[!colnames(select_fs) %in% c('.','.NA')]
  
  select_fs = select_fs[,order(names(select_fs))]
  
  rownames(select_fs) = KOR_ticker[,'종목코드']
  
  fs_list[[i]] = select_fs
}

names(fs_list) = fs_item

saveRDS(fs_list, 'data/KOR_fs.Rds')

## 가치지표 정리하기
# KOR_value 폴더 내 저장돼있음

library(stringr)
library(magrittr)
library(dplyr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$`종목코드` = 
  str_pad(KOR_ticker$`종목코드`,6,side = c('left'), pad='0')

data_value = list()
for(i in 1:nrow(KOR_ticker)){
  name = KOR_ticker[i,'종목코드']
  data_value[[i]] = 
    read.csv(paste0('data/KOR_value/',name,'_value.csv'),row.names = 1) %>% 
    t() %>% data.frame()
}

data_value = bind_rows(data_value)
print(head(data_value))

# NA삭제
data_value = data_value[colnames(data_value) %in%
                          c('PER','PBR','PCR','PSR')]
data_value = data_value %>% 
  mutate_all(list(~na_if(.,Inf))) # data_value가 Inf값이라면 na로 처리, muate_all-> 모든 컬럼에 대해

rownames(data_value) = KOR_ticker[,'종목코드']
print(head(data_value))

write.csv(data_value,'data/KOR_value.csv')
