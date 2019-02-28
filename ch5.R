# data() #########

ap = data()$result
data("AirPassengers")
str(AirPassengers)
AirPassengers
class(AirPassengers)
head(AirPassengers)
AirPassengers[1:10]
colnames(ap)
class(ap)
items = ap[, 'Item']
str(items)
names(items)
str(ap)
df_ap = as.data.frame(ap)
str(df_ap)
items
dim(ap)
head(ap)

# trees
data("trees")
trees
head(trees)
class(trees)

month.name
month.abb
pi

letters = c('AA', 'BB')
letters
rm(letters)

# package #########
library('data.table')
start = Sys.time()
fread('data/성적.csv')
#read.csv('data/성적.csv')
Sys.time() - start

install.packages('data.table')
install.packages('ggplot2')
