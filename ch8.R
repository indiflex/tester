Sys.sleep(10)
######### dplyr #########
library(dplyr)
data = read.csv('data/성적.csv')
data = dplyr::rename(data, eng=영어)
data = dplyr::rename(data, kor=국어, sci=과학, eng=영어)
class(data)
mpg = as.data.frame(ggplot2::mpg)
mpg = dplyr::rename(mpg, manu = manufacturer)
## filter ############
data
head(data)
colnames(data)
attach(data)
data %>% filter(group == 'C조')
data %>% filter(group == 'C조' & 수학 > 90)

t = data %>% filter(group %in% c('A조', 'C조'))
t

## select #########
data$math
data %>% select(-math, -영어)
data %>%
  filter(math>95) %>% 
  select(학번, 국어, 영어, math) %>%
  head

## arrange #######
data %>% arrange(math) %>% head
data %>% arrange(desc(math)) %>% head
data %>% arrange(math, 국어, 영어) %>% head

## mutate ########
data = data %>% mutate(subTotal = kor + eng + math)
data = data %>%
  mutate(subTotal = kor + eng + math,
         subMean = round((kor + eng + math) / 3))
data %>% head

data %>%
  mutate(kor_eng = kor + eng) %>%
  arrange(desc(kor_eng)) %>%
  head

## summarize ######
summary(data)
summarise(data)

data %>% summarize(t = mean(math))
mean(data$math)
data %>% group_by(반) %>% summarise(m = mean(math))

?summarise
unloadNamespace('ggplot2')
unloadNamespace('reshape2')
unloadNamespace('plyr')

data

data %>%
  group_by(반, 성별) %>%
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            medi_math = median(math),
            n_math = n())


data %>%
  group_by(cls) %>%
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            medi_math = median(math),
            n_math = n()) %>%
  arrange(desc(mean_math))

## join ###########
dfsum = cbind( data.frame(yno=1:4, year=2016:2019), 
               matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4))))
dfsum
sales = cbind( data.frame(no=1:12, year=2016:2019), 
               matrix(round(runif(48), 3) * 100000, ncol=12, dimnames = list(NULL, month.abb)) )
sales
inner_join(sales, dfsum, by=c('year' = 'year', 'no' = 'yno'))

topsale4 = sales[1:4,] %>% select(year, Jan, Apr, Jul, Oct)
top4 = sales[5:8,] %>% 
  select(1:4, year, Jan, Apr, Jul, Oct) %>% 
  rename(yno=no, Q1=Jan, Q2=Apr, Q3=Jul, Q4=Oct)
top4
bind_rows(dfsum, top4, .id = 'yyy')
bind_rows(dfsum, top4)
bind_cols(dfsum, top4) %>% select(-year1, -yno1)
cbind(dfsum, top4)

table(sales$year)

