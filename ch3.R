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
colnames(m2) = c('group 1', 'group 2', 'group 3', 'dsaf asdf asdf', '5분단')
row.names(m2)