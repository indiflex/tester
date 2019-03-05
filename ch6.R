# read.delim - seperated file ####
sepdata = read.delim('../sep.txt', sep='#', stri)
sepdata
str(sepdata)
sepdata$name = as.character(sepdata$name)
class(sepdata$name)
tels = sepdata$tel
sepdata$tel = as.character(tels)
head(sepdata)
dim(sepdata)
class(sepdata)
View(sepdata)
summary(sepdata)

# read.table - tsv ##########
tsvdata = read.table("../tcv.tsv", sep='\t', header=T, stringsAsFactors = F)
tsvdata
class(tsvdata)
str(tsvdata)


# read.fwf - fixed width file ####
fwfdata = read.fwf('../temper.txt', header=F, width = c(15, 4, 69, 4, 1))

fwfdata$V1 = NULL
fwfdata$V3 = NULL
fwfdata

fwfdata = read.fwf('../fwf.txt', header=F, width=c(8,6,5,3, 4))
class(fwfdata)
str(fwfdata)
View(fwfdata)

mt = read.csv('../melontop100.csv', stringsAsFactors = F)
mt
mt[47,]
mt$제목
str(mt)

# xl.read.file - excel ###########
install.packages('readxl')
library('readxl')
mtx = read_excel('../meltop100.xlsx', sheet = 1, col_names = T)
mtx
class(mtx)
str(mtx)
dim(mtx)
mtx[101,1] = NULL
mtx = mtx[-101,]
mtx$..3
mtx$가수
colnames(mtx)
colnames(mtx) = c('rank', 'title', 'singer', 'like', 'likediff')
mean(mtx$like)
nrow(mtx)

save(mtx, file='data/meltop100.rda')
rm(mtx)
load('data/meltop100.rda')
View(mtx)

data$'성별'
nrow(data[data$'성별' == '여',])

## psych ###########
install.packages('psych')
library('psych')
describe(data)
describe(data, IQR=T)
data('iris')
describe(iris)
