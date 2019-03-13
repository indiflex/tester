Sys.sleep(10)

#### mac 한글 ###############
setFont = function() {
  theme_set(theme_gray(base_family="AppleGothic"))
  par(family = "AppleGothic")
}

setFont()

plot(x=1, y=2)
plot(x=1:10, y=1:10)
smdt
plot(x=smdt$stuno, y=smdt$Math)
plot(smdt$stuno, smdt$Korean)
plot(smdt$stuno, smdt$Korean, col='red')
plot(smdt$stuno, smdt$Korean, col='#0000FF', cex=3)
plot(smdt$stuno, smdt$Korean, col='yellow3')
prePar = par(col='red')
plot(smdt)
par(prePar)
plot(data$eng)
melt(smdt)
rownames(smdt)
colnames(smdt)

xl = c(-0.5, 5.5); yl = c(0, 100)

p1 = plot(x = smdt$stuno, y = smdt$Korean,
     col='#0000FF', cex=3, pch = 8,
     xlim = xl, ylim = yl,
     xlab = '학번', ylab = '국어, 수학',
     main = '우리반 국어 / 수학 성적')
par(new = T)
plot(x = smdt$stuno, y = smdt$Math,
     col='#ff0000', cex=3, pch = 21,
     xlim = xl, ylim = yl,
     xlab = '', ylab = '')

legend('center', legend=c('국어', '수학'), pch=c(8, 21), col=c('blue', 'red'), bg='gray')

?points
?pch

plot(x = smdt$stuno, y = smdt$Korean,
     col = '#0000FF',
     cex = 3,
     xlim = c(-0.5, 5.5),
     ylim = c(50, 100),
     pch = 8,       
     las=1,
     type='c',
     xlab = '학번', ylab = '국어',
     main = '그래프 타이틀')

plot(tan, -pi, pi * 3)
df = data.frame(x=smdt$stuno, y=smdt$Korean)
df = df[-nrow(df),]
df
table(data[data$eng > 90,]$반)
class(dfsum)
t = table(data[data$eng > 90,]$반)
rownames(t)
t= melt(data=dfsum[,2:6], id.vars = "year")
class(t)
data
t = data %>% filter(eng > 90) %>% select('반', '성별') %>% table
t = data %>% filter(수학 > 90) %>% select('반') %>% table
t = data %>% filter(eng > 90) %>% group_by(반) %>% select('반', '성별') %>% group_by(성별) %>% summarise(tot=n())
data %>% group_by(반)%>% summarise(m = mean(국어)) %>% select(반, m) %>% table
t = data %>% group_by(반)%>% summarise(m = mean(국어)) 
t
table(as.matrix(t))
class(t)
barplot(as.matrix(t),
        beside = T,
        border = 'dark blue',
        density = 30,
        angle = 15 + 10*1:2,
        xlab = '학급별 성별', ylab = '영어',
        legend=rownames(t),
        col=heat.colors(4))

barplot(smdt$Korean, main=list('타이틀', font=3))
barplot(data$eng)

data %>% filter(eng > 90) %>% summarise(n = n())
sum(data$eng)
length(data$eng)
data = read.csv('data/성적.csv')
data[data$학번 == '10323', ]$국어 = 105
boxplot(국어~성별, data=data, col='lightblue')

data %>% filter(국어 < 30)
hist(data$국어, col="gray", labels=T, breaks=10, freq=F)
hist(data$반, col="gray", labels=T, breaks=10, freq=F)

curve(sin, -2 * pi, 3 * pi, xname='x', xlab = 'TT', n=200, type='p', xlim=c(-10,10), ylim=c(0,10))
  
data
d = data %>% filter(국어 > 90) %>% select('반')
d
names(d)
pie(table(d), clockwise = T)
pie(table(d), clockwise = T, angle=45)
pie(table(d), clockwise = T, labels = c('A', 'B', 'C', 'D'))
pie(table(d), clockwise = T, radius = 1, init.angle=if(T) 90 else 0)
pie(table(d), clockwise = T, col=c('red', 'purple', 'green', 'cyan'), density = 10)

qqplot(data$학번, data$국어)
abline(data$국어)
text(6,2, "TTT", cex=.8)

dd = load('../hellor/data/data_eng.rda')
data

smdt
ggplot() + geom_point(data=smdt, aes(x=stuno, y=Korean), size=3)

ggplot() +
  geom_point(data=smdt,
             aes(x=stuno, y=avg),
             color='blue',
             size = 5)  

ggplot(data, aes(eng, kor)) +
  geom_point(aes(color=eng, size=kor), alpha=0.3)

ggplot(data[data$stuno >= 30000,], aes(stuno, kor)) +
  geom_point(aes(color=stuno, size=kor), alpha=0.3)

d = data %>% filter(stuno >= 30000)
ggplot(d, aes(cls, kor)) +
  geom_point(aes(color=cls, size=kor), alpha=0.3) +
  geom_smooth(method='loess')
  

# 꺽은선 #######
ggplot(data=d4, aes(x=displ)) +
  geom_line(data=d4 %>% filter(year==1999), aes(y=m1, color='1999 cty')) +
  geom_line(data=d4 %>% filter(year==1999), aes(y=m2, color='1999 hwy')) +
  geom_line(data=d4 %>% filter(year==2008), aes(y=m1, color='2008 cty')) +
  geom_line(data=d4 %>% filter(year==2008), aes(y=m2, color='2008 hwy')) +
    scale_colour_manual("", breaks = c("1999 cty", "1999 hwy", "2008 cty", "2008 hwy"),
                             values = c("gray", "pink", "blue", "darkblue")) +
  xlab("배기량(cc)") +
  scale_y_continuous("연비(M/h)", limits = c(0, 50)) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은선은 2008년)')

d4

ls = c("1999 cty", "1999 hwy", "2008 cty", "2008 hwy")

d2 = mpg %>% group_by(manu, displ) %>%
  summarise(m1 = mean(cty), m2 = mean(hwy))
d2
ggplot(d2, aes(x=displ)) + 
  geom_line(aes(y=m1, color='cty')) + 
  geom_line(aes(y=m2, color='hwy'), size=1) +
  xlab("배기량(cc)") +
  scale_y_continuous("연비(M/h)", limits = c(0, 50)) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은선은 2008년)')

ggplot(d2, aes(x=displ)) + 
  geom_line(aes(y=m1, color='cty')) + 
  geom_line(aes(y=m2, color='hwy'), size=1) +
  xlab("배기량(cc)") +
  scale_y_continuous("연비(M/h)", limits = c(0, 50)) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은선은 고속도로)') 

d3 = mpg %>% group_by(displ) %>% summarise(m1 = mean(cty), m2 = mean(cty + hwy) / 2, m3 = mean(cty/2 + hwy/2))
d3 = mpg %>% group_by(displ) %>% summarise(m = mean(cty + hwy) / 2)
d3 = mpg %>% filter(year == '1999') %>% group_by(displ) %>% summarise(m1 = mean(cty), m2 = mean(hwy))
d3 = mpg %>% group_by(displ)
d3
ggplot(d3) + geom_line(aes(displ, m, color=m, main='aa'), size=1) + labs(title='aaa', subtitle = 'bbb')
ggplot(d3) + geom_line(aes(displ, m1), color='red') + geom_line(aes(displ, m2), color='blue')

d4 = mpg %>% group_by(year, displ) %>% summarise(m1 = mean(cty), m2 = mean(hwy))
d4
ggplot(d4) + 
  geom_line(aes(displ, m1 + m2), color=c(m1, m2))

d1 = ggplot() +
  geom_line(data=d4 %>% filter(year==1999), aes(x=displ, y=m1), colour='gray') +
  geom_line(data=d4 %>% filter(year==1999), aes(x=displ, y=m2), colour='blue') +
  geom_line(data=d4 %>% filter(year==2008), aes(x=displ, y=m1), colour='darkgray', size=1) +
  geom_line(data=d4 %>% filter(year==2008), aes(x=displ, y=m2), colour='darkblue', size=1) +
  labs(title = '연도별 통합 연비', subtitle = '굵은선은 2008년')

ggplot(data=d4) +
  geom_line(data=d4 %>% filter(year==1999), aes(x=displ, y=m1), colour='gray') +
  geom_line(data=d4 %>% filter(year==1999), aes(x=displ, y=m2), colour='blue') +
  geom_line(data=d4 %>% filter(year==2008), aes(x=displ, y=m1), colour='darkgray', size=1) +
  geom_line(data=d4 %>% filter(year==2008), aes(x=displ, y=m2), colour='darkblue', size=1) +
  labs(title = '연도별 통합 연비', subtitle = '굵은선은 2008년')

d1 + theme(legend.title = element_text(face = 4, color = 'red', size = 15))

d1 + scale_color_discrete(name = "Y series", labels = c("Y2", "Y1"))
d1 + scale_color_manual(values = c('Y1' = 'darkblue', 'Y2' = 'red')) +
  labs(color = 'Y series')

d1 + scale_colour_manual(values=c("red","green","blue", "black"))

ggplot(data) + geom_line(aes(kor, math))

mpg %>% filter(manu == 'audi')
ggplot(mpg %>% filter(manu == 'audi')) + geom_line(aes(displ, cty))

t = mpg %>% group_by(cty) %>% summarise(m = mean(hwy))
ggplot(t) + geom_line(aes(cty, m))
ggplot(mpg) + geom_line(aes(cty, hwy))
ggplot(mpg) + geom_line(aes(displ, hwy))
mpg

ggplot(data[data$kor > 50,]) + geom_line(aes(kor, math))


## Try This : ggplot2 ########
# 1 
ggplot(data=d4, aes(x=displ)) +
  geom_line(data=d4 %>% filter(year==1999), aes(y=m1, color='1999 cty')) +
  geom_line(data=d4 %>% filter(year==1999), aes(y=m2, color='1999 hwy')) +
  geom_line(data=d4 %>% filter(year==2008), aes(y=m1, color='2008 cty')) +
  geom_line(data=d4 %>% filter(year==2008), aes(y=m2, color='2008 hwy')) +
  scale_colour_manual("", breaks = c("1999 cty", "1999 hwy", "2008 cty", "2008 hwy"),
                      values = c("gray", "pink", "blue", "darkblue")) +
  xlab("배기량(cc)") +
  scale_y_continuous("연비(M/h)", limits = c(0, 50)) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은선은 2008년)')