Sys.sleep(10)

### presp() : perspective #########

## outer()
x = 1:5
y = 1:3
z = outer(x, y, function(x,y) { x + y })   # dim(x) * dim(y)
z
persp(x, y, z, theta = 45, phi = 0, expand = 0.5)
for (i in 1:24) {
  persp(x, y, z, theta = 30, phi = i*15)
  Sys.sleep(0.1)
}

seq(-10, 10, length=30) -> x
y = x
f = function(x, y) {
  r = sqrt(x^2 + y^2)
  return (10 * sin(r) / r)
}
z = outer(x, y, f)
persp(x, y, z, theta = 45, phi = 30, expand = 0.5, col='lightblue')
persp(x, y, z, theta = 45, phi = 30, expand = 0.5, col='lightblue',
      ltheta = 120, shade=0.75, ticktype='detailed',
      xlab='X', ylab = 'Y', zlab = "Sinc(r)")

cars
data
pressure


# Choropleth Map #####
install.packages('ggiraphExtra')
library(ggiraphExtra)
str(USArrests)
USArrests
rownames(USArrests)
USAccDeaths

library(tibble)
chodata = rownames_to_column(USArrests, var = 'state')
chodata$state = tolower(chodata$state)
save(chodata, file='data/chodata.rda')
str(chodata)
head(chodata)
chodata$cd = substr(chodata$state, 1, 3)
chodata$cd = NULL

midwest = as.data.frame(ggplot2::midwest)
midwest %>% group_by(state) %>% summarise(s = sum(poptotal))

install.packages('maps')
usmap = map_data('state')
head(usmap)
library(ggplot2)
library(dplyr)
ggChoropleth(data=chodata,
             aes(fill=Murder, map_id=state),
             map = usmap, interactive = T)

ggChoropleth(data=chodata,
             aes(fill=c(Murder,Rape), map_id=state),
             map = usmap, interactive = T)



last_plot()

# non-interactive
load('data/chodata.rda')

head(chodata)

ggplot(chodata, aes(data = Murder, map_id = state)) +
  geom_map(aes(fill = Murder), map = usmap) + 
  expand_limits(x = usmap$long, y = usmap$lat) +
  xlab('경도') + ylab('위도') +
  labs(title="USA Murder", fill='범죄율') 


ggplot(chodata, aes(data = Murder, map_id = state)) +
  geom_map(aes(fill = Murder), map = usmap) + 
  expand_limits(x = usmap$long, y = usmap$lat) +
  scale_fill_gradient2('살인', low='red', high = "blue", mid = "green") +
  xlab('경도') + ylab('위도') +
  labs(title="USA Murder", fill='범죄율') 

# interactive #######

# tooltip html
tooltips = paste0(
  sprintf("<p><strong>%s</strong></p>", as.character(crimes$state)),
  '<table>',
  '  <tr>',
  '    <td>인구(만)</td>',
  sprintf('<td>%.0f</td>', chodata$UrbanPop * 10),
  '  </tr>',
  '  <tr>',
  '    <td>살인</td>',
  sprintf('<td>%.0f</td>', chodata$Murder),
  '  </tr>',
  '</table>'
)

# onclick
onclick = sprintf("alert(\"%s\")", as.character(crimes$state))
??geom_map_interactive
library(ggplot2)
library(ggiraphExtra)
library(ggiraph)

ggplot(chodata, aes(data = Murder, map_id = state)) +
  geom_map_interactive( 
    aes(fill = Murder,
        data_id = state,
        tooltip = tooltips,
        onclick = onclick), 
    map = usmap) +
  expand_limits(x = usmap$long, y = usmap$lat) +
  scale_fill_gradient2('살인', low='red', high = "blue", mid = "green") +
  labs(title="USA Murder") -> gg_map

# activate
ggiraph(code = print(gg_map))
girafe(ggobj = gg_map)

## 우리나라 단계 구분도 #################################
install.packages('stringi')
install.packages('devtools')
devtools::install_github("cardiomoon/kormaps2014")

library(kormaps2014)
str(korpop1)
head(changeCode(korpop1))
kdata = rename(korpop1, pop=총인구_명, area=행정구역별_읍면동)
head(kdata)
str(kdata)
head(kormap1)
str(kormap1)

ggChoropleth(data=kdata, 
             aes(fill = pop, map_id = code, tooltip = area),
             map = kormap1,
             interactive = T)

ggplot(kdata, aes(data = pop, map_id = code)) +
  geom_map(aes(fill = pop), map = kormap1) + 
  expand_limits(x = kormap1$long, y = kormap1$lat) +
  scale_fill_gradient2('인구', low='darkblue') +
  xlab('경도') + ylab('위도') +
  labs(title="시도별 인구") 

# 결핵환자수
kormaps2014::tbc
head(tbc)
class(tbc$year)
tbc %>% distinct(year)
tbc %>% filter(!is.na(NewPts)) %>% filter(year %in% 2006:2015)
tt = tbc %>% filter(year %in% 2006:2015) %>% group_by(code, name1) %>% summarise(pa = sum(NewPts))
tt$pa = ifelse(is.na(tt$pa), 0, tt$pa)
tt
ggChoropleth(data=tt, 
             aes(fill = pa, map_id = code, tooltip = name1),
             map = kormap1, title = '시도별 결핵환자수',
             interactive = T) 

tbc %>% filter(name1 == '경기')

ggplot(chodata, aes(data = Rape, map_id = state)) +
  geom_map_interactive( 
    aes(fill = Rape,
        data_id = state,
        tooltip = tooltip2,
        onclick = onclick2), 
    map = usmap) +
  expand_limits(x = usmap$long, y = usmap$lat) +
  scale_fill_gradient2('Rape', low='red', high = "blue", mid = "green") +
  labs(title="USA Murder") -> gg_map2

ggiraph(code = print(gg_map2))
girafe(ggobj = gg_map2)


##### Try This ###########
# 1
ggChoropleth(data=chodata,
             aes(fill=c(Murder, Assault, UrbanPop,Rape), map_id=state),
             map = usmap, interactive = T)

# 2
tooltip2 = paste0(
  sprintf("<p><strong>%s</strong></p>", as.character(chodata$state)),
  sprintf('<p> <strong>%.0f</strong> / %.0f 만</p>', chodata$Rape,  chodata$UrbanPop * 10)
)

onclick2 <- sprintf(
  "window.open(\"%s%s\")",
  "http://en.wikipedia.org/wiki/",
  as.character(crimes$state)
)

ggplot(chodata, aes(data = Rape, map_id = state)) +
  geom_map_interactive( 
    aes(fill = Rape,
        data_id = state,
        tooltip = tooltip2,
        onclick = onclick2), 
    map = usmap) +
  expand_limits(x = usmap$long, y = usmap$lat) +
  scale_fill_gradient2('Rape', low='red', high = "blue", mid = "green") +
  labs(title="USA Murder") -> gg_map2

ggiraph(code = print(gg_map2))
girafe(ggobj = gg_map2)


# 3 : 결핵환자수
kormaps2014::tbc
head(tbc)
class(tbc$year)
tbc %>% distinct(year)
tbc %>% filter(!is.na(NewPts)) %>% filter(year %in% 2006:2015)
tt = tbc %>% filter(year %in% 2006:2015) %>% group_by(code, name1) %>% summarise(pa = sum(NewPts))
tt$pa = ifelse(is.na(tt$pa), 0, tt$pa)
tt$pa[is.na(tt$pa)] = 0
tt
ggChoropleth(data=tt, 
             aes(fill = pa, map_id = code, tooltip = name1),
             map = kormap1, title = '시도별 결핵환자수',
             interactive = T) 

tbc %>% filter(name1 == '경기')


# interactive #########
# plotly
install.packages('plotly')
library(plotly)
last_plot()
ggplot(data=d4, aes(x=displ)) +
  geom_line(data=d4 %>% filter(year==1999), aes(y=m1, color='1999 cty'), size=1) +
  geom_line(data=d4 %>% filter(year==1999), aes(y=m2, color='1999 hwy'), size=1) +
  geom_line(data=d4 %>% filter(year==2008), aes(y=m1, color='2008 cty'), size=2) +
  geom_line(data=d4 %>% filter(year==2008), aes(y=m2, color='2008 hwy'), size=2) +
  scale_colour_manual("", breaks = c("1999 cty", "1999 hwy", "2008 cty", "2008 hwy"),
                      values = c("gray", "pink", "blue", "darkblue")) +
  xlab("배기량(cc)") + xlim(1, 8) +
  scale_y_continuous("연비(M/h)", limits = c(0, 50)) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은선은 2008년)') -> lp

ggplotly(lp)
ggplotly(korp)

ggplot(data, aes(eng, kor)) +
  geom_point(aes(color=eng, size=kor), alpha=0.3) -> t
ggplotly(t)


install.packages('dygraphs')
library(dygraphs)
library(xts)
xts(d4)
