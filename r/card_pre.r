card <- fread('card.csv',
              header = T, 
              stringsAsFactors = F,
              data.table = F,
              encoding = 'UTF-8')

## 한글 없애기 ##
data <- card %>% filter(! (selng_cascnt %in% grep('[ㄱ-힣]',unique(card$selng_cascnt), value = T)),
                        ! (salamt %in% grep('[ㄱ-힣]',unique(card$salamt), value = T))) %>% 
  mutate(selng_cascnt = as.numeric(selng_cascnt),
         salamt = as.numeric(salamt)) %>%
  select(- c(adstrd_code, mrhst_induty_cl_code))

rm(list = c('card'))

data$receipt_dttm=data$receipt_dttm %>% as.character() %>% as.Date('%Y%m%d')

## 음수 값 확인 - 양수만 넣기## 
data$selng_cascnt %>% summary()
data$salamt %>% summary()

data = data %>% filter(selng_cascnt > 0, salamt > 0) %>% 
  mutate(receipt_dttm = ymd(receipt_dttm),
         week = week(receipt_dttm))

data %>% glimpse()

#코로나 시기를 새로운 period변수로 나타내 줍니다.
index1 = which(data$receipt_dttm == '2020-02-22') %>% max() #기 
index2 = which(data$receipt_dttm == '2020-03-08') %>% max() #승
index3 = which(data$receipt_dttm == '2020-05-06') %>% max() #전-1
index4 = nrow(data) #전-2

data_period = data 
data_period$period = c(rep(1, index1),
                       rep(2, index2 - index1),
                       rep(3, index3 - index2),
                       rep(4, index4 - index3))

##이상치 및 결측치 처리##

data_period %>% is.na() %>% colSums()

mean_amount=data_period %>%
  group_by(mrhst_induty_cl_nm) %>% 
  summarise(N_amount=mean(selng_cascnt)) %>% 
  arrange(N_amount)

mean_amount %>%
  ggplot(aes(x=1, y=N_amount))+
  geom_violin( color = "#1E3269",size=0.3)+theme_bw() +theme(plot.margin = margin(60,60,60,60)) 

categories_new=mean_amount %>%
  filter(N_amount>=quantile(mean_amount$N_amount)[2]) %>% 
  arrange(desc(N_amount)) %>% select(mrhst_induty_cl_nm)%>% 
  ungroup()

categories_new <- as.data.frame(categories_new)


data_period <- data_period %>% 
  filter(mrhst_induty_cl_nm%in%
           as.matrix(categories_new,nrow = 1))