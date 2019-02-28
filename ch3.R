# factor ######
f = factor(c(2,3), levels=1:3, labels=c('A', 'B', 'C'))
as.numeric(f)
as.vector(f)
c(f)
as.character(f)

# vector ####
v1 = c("A", "B", "O", "AB") 
head(v1, 3)
tail(v1, 2)
v1[2:4]
names(v1) = LETTERS[1:length(v1)]
v1

v2 = c(1, 2, 3)
tail(v2, 2)
v2[1] + v2[2]
summary(v2)
v2[6] = 6

append = function(iter = c(), val) {
  iter[length(iter) + 1] = val
  print(iter)
  iter
}           
v2 = append(v2, 56)
print(v2)
v2[ c(1,3,5) ] = 0
v2 = null
v2[-1]
v2 = 1:7
v2
v2[c(1:2, 4)]
v2[ -(1:3) ]
names(v2) = LETTERS[1:length(v2)]
v2['C']
names(v2) = NULL
rm(v2)

# matrix ####
m1 = matrix(c("A", 'F', 'G', 'D', 'C', 'B'), nrow=2,byrow=T)
m2= matrix(LETTERS[1:25], ncol=5)
length(m1)
m1 
m2
matrix(c(LETTERS, 1, 2, 3, 4), ncol=5, byrow=T)
dim(m2)
str(m2)
rownames(m1)
row.names(m1)
nrow(m1)
colnames(m1) = LETTERS[1:ncol(m1)]
rownames(m2) = 1:nrow(m2)
colnames(m2) = c('1분단', '2분단', '3분단', '4분단', '5분단')
colnames(m2) = c('group 1', 'group 2', 'group 3', 'dsaf asdf asdf', '5분단')
rownames(m2) = seq(1,nrow(m2))
 m2
m2[,2]
m2[,'5분단']

m = matrix(1:200, ncol=20, byrow=T)
m
m[, 1:5]

m3 = matrix(1:15, ncol=3, byrow=T)
m3 = matrix(1:25, ncol=25/5, byrow=T)
colnames(m3) = LETTERS[1:ncol(m3)]
rownames(m3) = LETTERS[1:nrow(m3)]
m3
cbind(m2, m3)
rbind(m2, m3)

ls(pattern="matrix")
ls()
a = 1
ls(pattern = 'm')
get(m2)
do.call(rbind, list(m2, m3))

# data frame ####
df1 = data.frame(column1=11:15, columnx=LETTERS[1:5])
class(df1)
data = read.csv('data/성적.csv')
class(data)
df1
str(df1)
summary(df1)
rownames(df1) = LETTERS[1:nrow(df1)]

h = head(data)
df2 = data[1:10, 1:6]
df2
df3 = data[1:10, c(1:3, 7,8)]
df3
cbind(df2, df3)
df3[, 4:5]
df4 = cbind(df2, df3[, 4:5])
cn = colnames(df4)
df4[, c(cn[1:4], '과학', '수학', '예체', '영어')]
  colnames(df4)
h
df100 = data[101:110, 1:6]
rbind(df2, df100)

# list ###############
lst1 = list(a=1:3, b=4:6)
lst1$a
lst1['a']
str(lst1)
lst2 = list(a=1:3, b=c(4, '5', 6))
str(lst2)
lst2
lst3 = list(1:3, LETTERS[1:3], c(2.5, 4.5, 3.7))
lst3
str(lst3)
names(lst3)
lst3[1]
lst3[2]
length(lst3[2])
unlist(lst1)
names(lst1)
ul = unlist(lst1)
ul
colnames(ul)
ul$'a1'
ncol(ul)
ul[3:5]
unlist(lst3)

# transform ####
s = "123"
i=as.numeric(s)
i
s
colnames(m2) = NULL
rownames(m2)
m2d=as.data.frame(m2)
m2
str(m2)
str(m2d)
head(m2d)
class(m2d)
m2d

tbl = table(data$수학)
tbl

# Try this #########
# 1
blood_type = function(n) {
  f = factor(n, levels=1:4, labels=c('A', 'B', 'O', 'AB'))
  return(as.vector(f))
}
blood_type(2)

# 2
append = function(v, val) {
  v[length(v) + 1] = val
  return(v)
}
v1 = 1:3
v1 = append(v1, 4)
v1

# 3
m10 = matrix(1:200, ncol=20, byrow=T)
m10
colnames(m10) = LETTERS[1:ncol(m10)]
rownames(m10) = letters[1:nrow(m10)]
colnames(m10)[10] = colnames(m10)[10] 10


