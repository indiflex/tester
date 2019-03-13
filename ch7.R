Sys.sleep(10)

## R built-in functions #####
help(package=base)

min(data$수학)
max(data$수학)
boxplot(data$수학)
hist(data$수학)
hist(data$과학)
summary(data$수학)

data[data$학번 == 10337, ]
km = data[data$성별 == '남' & data$국어 > 90 & data$수학 > 90, c(1,2, 4:8)]
km
km[order(km$수학),]

summary(data)
data$국어 + data$영어 + data$수학
data$tot = data[,4] + data[,4] + data[,6]
data$tot = sum(data[, 4:6])
sum(data[, 4:6])
data[,4] + data$영어 + data$수학
data$avg = (data$국어 + data$영어 + data$수학) / 3
head(data, 20)
aggregate(data=data, avg~반, mean)
aggregate(data=data, cbind(국어,영어,수학)~반, mean)
aggregate(data=data, 수학~반, sum)

data$pass = ifelse(data$avg > 60, TRUE, FALSE)
data[data$pass, ]
library('ggplot2')

data$scout = ifelse(data$avg > 60, 
                    ifelse(data$성별 == '남', 'BoyScout', 'GirlScout'),
                    '')

data$pass
qplot(data$pass)
qplot(data$scout)

## ggplot2::mpg #####
library('ggplot2')
mpg = as.data.frame(ggplot2::mpg)
mpg
str(mpg)
View(mpg)
summary(mpg)

midwest = as.data.frame(ggplot2::midwest)
?midwest
install.packages('dplyr')
library('dplyr')
mpg = rename(mpg, manu=manufacturer)

colnames(mpg)


## Try This ##########
# 1
km2 = data[data$반 == '난' & data$성별 == '남' & data$국어 > 90 & data$수학 > 90, c(1, 4:8)]
km2
km2[order(-km$국어),]

# 2
data.avg = (data[,4] + data[,5] + data[,6] + data[,7] + data[,8])
head(data)
aggregate(data=data, cbind(국어,avg)~반, mean)

# 3


# 1 - 스카우트인 학생들만 대상으로 그리기
qplot(data[data$scout != '',]$scout)

# 2 - 

search()

attach(data)
'수학'
detach(data)
edit(data)




## Try This - 평가 ######
# 1 통합연비 높은 순 출력
mpg
mpg$cthw = (mpg$cty + mpg$hwy) / 2
mpg
mpg[order(-mpg$cthw),]

# 2 생산연도별 통합연비
t = aggregate(data=mpg, cthw~(year+fl), mean)
t
t[order(t$year), ]

aggregate(data=mpg, (cty+hwy)~(fl+year), mean)


# 3 data 특징 설명
library(ggplot2)
midwest = as.data.frame(ggplot2::midwest)
summary(midwest)
midwest
library(psych)
m1 = midwest[,5:ncol(midwest) - 1]
m1
mn = describe(m1)
mn
colnames(midwest)
plot(midwest[, 5:ncol(midwest) - 1])
aggregate(data=midwest, popdensity~state, mean)
aggregate(data=midwest, cbind(poptotal, popdensity)~state, mean)
aggregate(data=midwest, cbind(c(5,6))~state, mean)
aa = aggregate(data=midwest, cbind(poptotal, popdensity, popasian, percasian)~state, mean)
aa
hist(aa$poptotal)
qplot(aa$popdensity)
mpt = mean(midwest$poptotal)
mpt
aa[order(-aa$poptotal),]
boxplot(aa$poptotal)
# 전체 인구수와 인구밀도는 OH가 가장 많으나, 아시아계 인구수는 IL가 가장 많다.
# 아시아계 인구수가 가장 높은 주는 IL주이다.
# IL과 MI 주를 비교했을때 전체 인구수와 밀도는 비슷하나, 아시아계 인구수는 IL주가 2배 이상 많다!


# 4 
colnames(midwest)
as.data.frame(colnames(midwest))
colnames(midwest)[5] = 'total'
colnames(midwest)[10] = 'asian'
colnames(midwest)[c(5,10)] = c("total","asian")


# bad way
midwest$total = midwest$poptotal
midwest$total = NULL

# 5 
total_asian = sum(midwest$asian)
#midwest$asianpct = (midwest$asian/midwest$total) * 100
midwest$asianpct = (midwest$asian/total_asian) * 100
midwest
hist(midwest$asianpct)

sum(midwest$asianpct)

# 6
plot(ord_ap$asianpct)
boxplot(ord_ap$asianpct)
hist(midwest$asianpct)
hist(ord_ap[ord_ap$asianpct < 5,]$asianpct)
# 아시아계 비율은 대부분의 도시가 5% 이하이며 5%이상은 2개 도시이며, 이중 1개는 30% 이상이다.
hist(ord_ap[ord_ap$asianpct < 0.5,]$asianpct)
hist(ord_ap[ord_ap$asianpct < 0.05,]$asianpct)
boxplot(ord_ap[ord_ap$asianpct < 0.05,]$asianpct)
# 아시아계 비율은 대부분의 도시가 0.05% 이하이며, 0.01% 이하가 가장 많다.
boxplot(ord_ap[ord_ap$asianpct < 0.01,]$asianpct)
# 0.003 ~ 0.007% 에 집중되어 있다.

ord_ap = midwest[order(-midwest$asianpct), c(2:3, 10, 15, 29)]
head(ord_ap)   # IL주 COOK시에 약 33% 아시아계 인구가 살고있다.(전체 인구의 1/3이 이곳에 살고있다)
tail(ord_ap)   # menominee시는 아시아계 인구가 0 이다.


# 7 - mean of asianpct
mean_ap = mean(midwest$asianpct)
mean_ap
midwest$asianrate = ifelse(midwest$asianpct > mean_ap, 'lg', 'sm')
midwest

# 8 - qplot
table(midwest$asianrate)
qplot(midwest$asianrate)
# -----------------------------------------------------------------

describe(data[1:8], IQR=T)
summary(data)
boxplot(data$수학)




## rep & seq #######
rep(1, times=3)
for (i in c('boy', 'girl')) {
  print(c('I', 'am', i))  
}
rep(c('I', 'am'), times=2, length.out=7)
rep(2:4, 5)
rep(LETTERS[1:3], times=3)
rep(LETTERS[1:3], each=3)

-3:5
5.4:-4.5
seq(5.4, -4.5, by=-1)
seq(5.4, -4.5, by=-0.1)
seq(from=1, to=10, by=0.5)
seq(5.4, -4.5, by=-0.1, length.out=2)

chdish = data.frame(customer=rep(c('김일수', '김이수'), each=2), menu=rep(c('짜장', '짬뽕'), times=2))
chdish
expand.grid(chdish)

t = runif(n=30, min=10, max=20)
order(t)
plot(t[order(t)])

# runif & sample ##############
runif(20)

sample(1:5, size=30, replace = T)
data[sample(1:nrow(data), size=5, replace=F),]
data$col = sample(c('AA', 'BB'), size=nrow(data), replace=T)
data$col = sample(c('AA', 'BB'), size=nrow(data), replace=T, prob = c(0.5,0.5))
data
nrow[data$col == 'AA']
print(paste(nrow(data), nrow(data[data$col == 'AA',])))
cat(nrow(data), nrow(data[data$col == 'AA',]))
nrow(data[data$col == 'BB',])
sample(1:5)
data[sample(1:nrow(data), size=5, replace=F),]

set.seed(50); sample(1:100, 10)

set.seed(255)
smdt = data.frame(stuno = 1:5, Korean=sample(60:100, 5), English=sample((5:10) * 10, 5), Math=sample(50:100, 5))
smdt
# string functions ########
s = '123456789'
nchar(s)
substr(s, 1, 5)
s = "abc,efg,abc"
strsplit(s, ',')
sub('abc', 'ttt', s)
sub(pattern='abc/g', replacement='ttt', x=s)
gsub('abc', 'ttt', s)

toupper(s)


which(data$반 == '난')
which(data$학번 == 10110)
data
tail(data)
grep(pattern='^2', x=data$학번, value = T)

s = "first\tsecond\nthird"
cat(s)
print(s)

paste('aaa-bbb', 'ccc-ddd', sep='**')
paste(data[1:3, '반'], collapse='**')
paste(data[1:3, '반'], collapse='')
paste0(data[1:3, '반'])

outer(month.abb, 2011:2020, paste, sep='-')
outer(LETTERS, 2011:2020, paste, sep='-')
outer(LETTERS, 2011:2020, paste0)


# date function ########
dt = Sys.Date()
class(dt)
Sys.time()

as.Date('2019-03-04 09:00')
as.time('2019-03-04 09:00')

dt1 = as.POSIXct('2019-03-04 09:00')
class(dt1)
dt1
seq(dt1, as.POSIXct('2019-04-01 23:59'), by='2 hour')
strptime(dt1, format='%Y-%m-%d')
seq(dt1, as.POSIXct('2019-04-01 23:59'), by='min')

install.packages('lubridate')
library(lubridate)
ymd('20190228')
mdy('02282019')
dmy('02122019')
year(dt1)
minute(dt1)
quarter(dt1)
day(dt1) = 15
dt1
days_in_month(1:12)
ddays(10)
dhours(50)
duration(1000)
month(dt1)
round(as.POSIXct('2019-03-05 18:39:45'), 'min')
round(as.POSIXct('2019-03-05 18:39:45'), 'hour')


# for #######
switch(2, "111", "222", "333")

for (i in 1:3) { print(i) }
for (r in 1:nrow(data)) { print(data[r, c(1,3)]) }

for (r in 1:nrow(data)) { print (data[r, 'scout']) }
x = 10000
fn_for = function(n) {
  x = 0
  for (i in 1:n) {
    #x = x + i
    x <- x + i
  }
  print(paste("x =", x))
}

fn_for(100)
x

attributes(data)

data = read.csv('data/성적.csv')
sum = 0
for (i in 1:5) {
  if (i == 3)
    next
  print(i)
}
print(sum)

i = 0
while(i < 10) { print(i); i = i + 1 }

factorial = function(x) {
  f = 1
  
  if (x < 0)
    print("Input the positive number!!")
  else if (x == 0)
    print("The factorial of 0 is 1")
  else {
    for (i in 2:x)
      f = f * i
  }
  
  return(f)
}
factorial(3)
factorial(1)

i = 0
while(TRUE) { 
  i = i + 1
  if (i %% 2)
    next
  if (i > 10)
    break
  
  print(i)
}


data[-1, -1]

num = as.integer(readline(prompt = "Input the number: (exit: 0) "))
factorial(num)

# apply ###########
smdt
apply(smdt[, 2:4], MARGIN = 1, FUN = 'mean')
apply(smdt[, 2:4], MARGIN = 2, FUN = mean)
apply(smdt[, 2:4], MARGIN = 2, mean)
apply(smdt[, 2:4], MARGIN = 2, FUN = quantile)
apply(smdt[, 2:4], MARGIN = 2, FUN = quantile, probs=1:3/4)
quantile(data$수학)

sapply(smdt[, 2:4], FUN = mean)
sapply(smdt[, 2:4], FUN = mean, simplify = T)

vapply(smdt[, 2:4], FUN = mean, FUN.VALUE = "")


# reshape2 ########
library('reshape2')
data.frame(q=1:4, year=2016:2019)
set.seed(255)
round(runif(16), 3) * 1000
matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4)))

dfsum = cbind( data.frame(yno=1:4, year=2016:2019), 
               matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4))))

dfsum
melt(data=dfsum[,2:6], id.vars = "year")
colnames(melt(data=dfsum[,2:6], id.vars = "year"))
meltsum = melt(dfsum[,2:6], id.vars = "year", variable.name = 'Sales')
meltsum

dcast(meltsum, Sales~year, value.var="value")

smdt
library('reshape2')
dcast(smdt, stuno~Korean, value.var = 'Korean')

colnames(meltsum)
# array ############
dataArray = array(1:24, dim=c(3, 4, 2))
dataArray
dim(dataArray)
nrow(dataArray)
ncol(dataArray)
length(dataArray)
dimnames(dataArray)
dimnames(dataArray) = list( 1:3, c('c1', 'c2', 'c3', 'c4'), c('x','y'))
rownames(dataArray)
colnames(dataArray)

dataArray[1][1][1] = 222
dataArray[1,2,1] = 333
dataArray[1,2,2] = 555
dataArray
ad1 = dataArray[,,1]
ad2 = dataArray[,,2]
class(ad1)
ad2
attr(dataArray, "dim") = c(3,8)
attr(dataArray, "dim") = NULL
dataArray

# ----------------------------------------------------------
head(data)
data$group1 = NULL
smdt = smdt[-nrow(smdt), ]
data$group = rep(sample(c('A조','B조','C조')), times=160)
head(data, 20)
nrow(data[data$group=="A조",])
nrow(data[data$group=="B조",])
nrow(data[data$group=="C조",])

apply(smdt[,2:4], MARGIN = 2, FUN = mean)
smdt[nrow(smdt)+1,2:4] = apply(smdt[,2:4], MARGIN = 2, FUN = mean)
smdt[nrow(smdt),1]="계"
smdt$total = apply(smdt[,2:4],MARGIN = 1, FUN = sum)
smdt$avg = apply(smdt[,2:4],MARGIN = 1, FUN = mean)
smdt

# Try This - last #####
# 1 - data$group 컬럼에 A조~C조 랜덤으로 160명씩 고르게 분포시키시오.
data$group = sample(c(rep('A조', 160), rep('B조', 160), rep('C조', 160)))
sample(c(rep('A', 160), rep('B', 160), rep('C', 160)))

cat(nrow(data), nrow(data[data$group == 'A조',]), nrow(data[data$group == 'B조',]))
head(data)
data$group1 = rep(LETTERS[1:3], length.out=480)
dim(data[data$group=='A',])
cat(nrow(data), nrow(data[data$group1 == 'A',]), nrow(data[data$group1 == 'B',]))

# 2 - fibonacci
# fibo.R 참조
while(TRUE) {
  x = as.integer(readline(prompt = "Input the number: "))
  if (x <= 0) break
  
  p0 = 0
  p1 = 1
  ret = paste(p0, p1)
  while(num > 2) {
    p = p0 + p1
    p0 = p1
    p1 = p
    ret = paste(ret, p)
    
    num = num - 1
  }
  
  print(ret)
}


set.seed(255)
smdt = data.frame(stuno = 1:5, 
                  Korean=sample(60:100, 5),
                  English=sample((5:10) * 10, 5),
                  Math=sample(50:100, 5))
smdt

# 3-1 - apply 과목별 평균점수 행 추가
sa = sapply(smdt[, 2:4], mean)
sa
sa[1:3] = round(sa[1:3])
smdt
smdt[nrow(smdt) + 1, ] = c(100, sa)   # 계 자리에 임의로 100을 입력(numeric유지)
smdt[nrow(smdt) + 1, ] = c('계', sa)  # or rbind(c('계', sa))
smdt[nrow(smdt), ] = c(100, sa)

class(smdt$Korean)
class(smdt$Math)
smdt$Korean = as.integer(smdt$Korean)
smdt[, 2] = as.integer(smdt[,2])
smdt[, 2:4] = as.integer(smdt[,2:4])
class(smdt[,2:4])
for (i in 2:4)
  smdt[, i] = as.integer(smdt[, i])
colnames(smdt)

smdt$total = apply(smdt[, 2:4], MARGIN = 1, sum)
smdt$avg = apply(smdt[, 2:4], MARGIN = 1, mean)


smdt[nrow(smdt), ] = NULL
smdt = smdt[-nrow(smdt), ]
smdt
smdt[, 5:6] = round(smdt[, 5:6])
# 3-2 - 총점과 평균 컬럼 추가
smdt[-6,2:4]
smdt[6, 2] = as.integer(smdt[6, 2])
class(smdt[6,2])
apply(smdt[, 2:4], MARGIN=1, FUN = sum)
smdt$total = apply(smdt[, 2:4], MARGIN=1, FUN = 'sum')

smdt$avg = apply(smdt[, 2:4], MARGIN=1, mean)
smdt$avg = round(smdt$avg)
smdt$total = NULL
smdt$avg = NULL
smdt[6,1] = '계'
smdt


smdt$Korean = as.integer(smdt$Korean)
smdt$English = as.integer(smdt$English)
smdt$Math = as.integer(smdt$Math)
smdt$'total' = apply(smdt[, 2:4], MARGIN = 1, FUN = sum)

# 4
month.abb

dfsum
data.frame(no=1:12, year=2016:2019)

cbind( data.frame(no=1:12, year=2016:2019), 
       matrix(round(runif(48), 3) * 100000, ncol=12, dimnames = list(NULL, month.abb)) )

sales = cbind( data.frame(no=1:12, year=2016:2019), 
               matrix(round(runif(48), 3) * 100000, ncol=12, dimnames = list(NULL, month.abb)) )

sales
library(reshape2)
melt(data=sales[,2:nrow(sales)], id.vars = "year", variable.name = 'month', value.name = 'saleamt' )
melt(sales[,2:nrow(sales)], id.vars = "year", variable.name = 'month', value.name = 'saleamt')

# ----------------------------------------------------------
data
attach(data)
data$수학
mean(수학)
detach(data)

smdt
sales
dfsum

# dplyr #########
library(dplyr)
library(lubridate)
?lubridate

inner_join(sales, dfsum, by=c('year' = 'year'))
left_join(sales, dfsum, by=c('year' = 'year'))
right_join(sales, dfsum, by=c('year' = 'year'))
inner_join(sales, dfsum, by=c('year' = 'year', 'no' = 'yno'))
semi_join(sales, dfsum, by=c('year' = 'year', 'no' = 'yno'))
full_join(sales, dfsum, by=c('year' = 'year', 'no' = 'yno'))
anti_join(sales, dfsum, by=c('no' = 'yno'))


# order #############
t = c(5, 7, 2, 8, 20, 11, 19)
sort(-t)
order(t)
rev(t)
t[order(t)]
t[rev(t)]
t[order(t, decreasing = T)]
rev(t[order(t)])

smdt[order(smdt$avg, -smdt$Korean),]
smdt
sort(smdt$avg)

# missing values ########
t = c(1:5, NA, 7, NA, 9, 10)
t
is.na(t)
table(is.na(t))
t[!is.na(t)]
mean(t[!is.na(t)])
t = ifelse(is.na(t), 0, t)
t
is.infinite(t)
mean(t, na.rm = T)


m1=m2=m3=matrix(c(1:3, NA, 9:3, NA, 1:3), nrow=3)
m1[is.na(m1)] = 0
m1
mean(m2[,2], na.rm=T)
m2[,2]
m2[is.na(m2[,2]), 2]
