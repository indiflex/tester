## R built-in functions #####
help(package=base)

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
head(data)
aggregate(data=data, avg~반, mean)
aggregate(data=data, cbind(국어,영어,수학,avg)~반, mean)
?aggregate(data=data, )


## ggplot2::mpg #####
library(ggplot2)
mpg = as.data.frame(ggplot2::mpg)
mpg
str(mpg)
View(mpg)
summary(mpg)




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
