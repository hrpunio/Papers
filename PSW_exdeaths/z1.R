library("scales")
library("ggthemes")
library("ggpubr")
library("tidyverse")
library("eurostat")
library('lubridate')

picWd <- 12
picHt <- 8
spanV <- 0.5
spanVV <- 0.25
windowLen <- 12
surl <- "© NI-KW (source: http://www.wsse.gda.pl/)"
mainColor <- "deeppink"
loessColor <- "steelblue"
mainBreaks <- "1 month"
NIKW <- "© NI-KW @ github.com/knsm-psw/GUS_mortality | https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/zgony-wedlug-tygodni,39,2.html"
unit1 <- 1000000

wojcodes <- c('PL21', 'PL22', 'PL41', 'PL42', 'PL43', 'PL51', 'PL52',
'PL61', 'PL62', 'PL63', 'PL71', 'PL72', 'PL81', 'PL82', 'PL84', 'PL91', 'PL92');

##
### Dane
##
zz <- get_eurostat('demo_r_mweek3', filters = list(
  ##sex = "T", 
  ##age = 'TOTAL'
  geo = wojcodes
)) %>%
  mutate (
    year = lubridate::year(time),
    week = lubridate::week(time)
  ) %>%
  rename( value = values)
##
##

## zgony łącznie wg tygodni/województw
zz1 <- zz %>% group_by(time, geo) %>%
   summarise( mdeaths=sum(value, na.rm=T), 
              year=last(year), 
              week = last (week),
              na.rm=TRUE)

zz2021 <- zz %>% filter(week < 15) %>% group_by(year, geo) %>%
   summarise( mdeaths=sum(value, na.rm=T), na.rm=TRUE) %>% 
  as.data.frame()

##############################################################################
### na 1mln wg województw                                                  ###
##############################################################################
nuts <- read.csv("nuts_teryt.csv", colClasses = c('teryt'="character"),
                 sep = ';',  header=T, na.string="NA" )
##ww <- read.csv("ludnosc_2019.csv",
##               colClasses = c('teryt'="character"),
##               sep = ';',  header=T, na.string="NA" )
pop0 <- get_eurostat("demo_r_d2jan", filters = list(
  sex = "T", 
  age = 'TOTAL',
  geo = wojcodes
)) %>% mutate (
  year = lubridate::year(time)
) %>% rename(pop = values)




##ww <- get("", sep = ';',  header=T, na.string="NA" )
##############################################################################
####
####
pop0 <- pop0 %>% left_join(nuts, by='geo')
#### aaproximation
#### Approximate pop for 2024 (bo nie ma)
pop0.2024 <- pop0 %>% filter (year == 2023) %>% mutate (year = 2024)
pop0 <- bind_rows(pop0, pop0.2024)

zx <- left_join(zz1, pop0, by = c('geo', 'year')) %>% 
  mutate(md1 = mdeaths/pop * unit1) %>% as.data.frame()
####
####
library("forecast")
library("zoo")
#
#install.packages("fable")
#
library('tsibble')
library('fable')
library('fabletools')

## 
covid.start <- as.Date("2020-07-31")

pre.covid <- zx %>% filter (  time.x <  covid.start & time.x > as.Date("2015-12-31") )
post.covid <- zx %>% filter (  time.x >=  covid.start  )

length(post.covid)
## |> is the base R "pipe" operator. It was new in version 4.1.0.
## ===============================================================
## https://robjhyndman.com/hyndsight/tsibbles/
## https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r

pre.covid.ts <- as_tsibble(pre.covid, key = geo, index = time.x)
post.covid.ts <- as_tsibble(post.covid, key = geo, index = time.x)
##post.covid.ts
##
## 7D
tsibble::interval(post.covid.ts)

## Post covid forecast length
post.covid.forecast.h <- post.covid.ts %>% tsibble::group_by_key() %>%
  mutate (n=n()) %>% tail(n=1) %>% 
  as.data.frame() %>%
  select(n) %>% unlist()

## Models
fitted_models = pre.covid.ts %>% 
  na.omit() %>%
  model(m1=TSLM(md1 ~ trend() ))

## Forecasts
h.post.covid <- fitted_models %>% fabletools::forecast(., h = post.covid.forecast.h) %>%
  as_tsibble() %>%
  select (geo, time=time.x, value=.mean)

e.pre.covid <- fitted_models %>% fitted() %>% select (geo, time=time.x, value=.fitted)
## merge tsibbles
trend <- dplyr::bind_rows(h.post.covid, e.pre.covid) %>%
  as_tsibble() %>%
  left_join(nuts, by='geo') %>%
  mutate (geo= sprintf ("%s|%s", geo, name))
##trend


geo.list <- nuts %>% select(geo) %>% filter (geo != 'PL') %>% unlist()

## https://rpubs.com/marydkeller/788244
#for (g in geo.list) {
#  fitted_models |> filter(geo == g ) |> report()
#}

# Extract list of slope parameters
zz <- fitted_models |> tidy() |> filter (term == 'trend()') %>%
  select (estimate)

# Slope parameters summary
summary(zz$estimate)

###
###  pic one

p1m <- zx %>%  filter (  year > 2015) %>% 
   ##mutate (date = sprintf ("%i-01-01", year)) %>%
   ##mutate (xlab = teryt) %>%
   mutate (geo= sprintf ("%s|%s", geo, name)) %>%
   ##mutate(xlab = replace(xlab, which( year < 2023), "")) %>%
   ggplot(aes(x=time.x, y=md1 )) +
   geom_line(size=.4) +
   ##geom_point(size=.4) +
   geom_vline(xintercept = covid.start, colour="firebrick", alpha=.25, size=0.8) +
   geom_vline(xintercept = as.Date('2024-01-01'), colour="forestgreen", alpha=.25, size=0.8) +
   geom_vline(xintercept = as.Date('2023-01-01'), colour="forestgreen", alpha=.25, size=0.8) +
   geom_vline(xintercept = as.Date('2022-01-01'), colour="forestgreen", alpha=.25, size=0.8) +
   geom_vline(xintercept = as.Date('2021-01-01'), colour="forestgreen", alpha=.25, size=0.8) +
   geom_vline(xintercept = as.Date('2020-01-01'), colour="forestgreen", alpha=.25, size=0.8) +
   geom_vline(xintercept = as.Date('2019-01-01'), colour="forestgreen", alpha=.25, size=0.8) +
   geom_vline(xintercept = as.Date('2018-01-01'), colour="forestgreen", alpha=.25, size=0.8) +
   geom_vline(xintercept = as.Date('2017-01-01'), colour="forestgreen", alpha=.25, size=0.8) +
   ##geom_text(aes(label=sprintf("%s", xlab), y= md1), hjust=-.5, size=2.5) +
   facet_wrap( ~geo, scales = "fixed", ncol = 1) +
   xlab(label="") +
   ylab(label="") +
   ylim(0, NA) +
   theme(plot.subtitle=element_text(size=9), legend.position="right")+
   geom_line(data=trend, aes(x=time, y=value), linewidth=.8, color='red', alpha=.35) +
   ggtitle("Total deaths per 1mln by province 2016--2024",
           subtitle= sprintf('red line = line trend 2016-01-01--%s (%s onwards = forecast)', 
                             covid.start, covid.start) )

ggsave(plot=p1m, "zgony_PL_w1m.png", width=picWd, height = 35)
p1m

### pic two (weekly)
###
p2m.last <- zx %>% filter (year == 2024) %>%
  na.omit() %>%
  mutate (xlab = teryt) %>%
  mutate (geo= sprintf ("%s|%s", geo, name))

p2m <- zx %>%  filter (year > 2015) %>% 
  na.omit() %>%
  mutate (xlab = teryt) %>%
  mutate (geo= sprintf ("%s|%s", geo, name)) %>%
  ##mutate(xlab = replace(xlab, which( year < 2023), "")) %>%
  ggplot(aes(x=week, y=md1, color=as.factor(year), groups=as.factor(year) )) +
  geom_line(size=.4) +
  geom_line(data=p2m.last, aes(x=week, y=md1), size=1, color='black') +
  geom_point(size=.4) +
  ##geom_vline(xintercept = covid.start, colour="firebrick", alpha=.25, size=0.8) +
  ##geom_text(aes(label=sprintf("%s", xlab), y= md1), hjust=-.5, size=2.5) +
  facet_wrap( ~geo, scales = "fixed", ncol = 1) +
  xlab(label="") +
  ylab(label="") +
  theme(plot.subtitle=element_text(size=9), legend.position="right")+
  ggtitle("Total deaths per 1mln by province 2016--2024",
          subtitle='Black line = 2024')

ggsave(plot=p2m, "zgony_PL_w2m.png", width=picWd, height = 33)
p2m

### Population
## year geo name pop

p3p <- pop0 %>%  filter (year > 2015) %>% 
  na.omit() %>%
  mutate (xlab = teryt) %>%
  mutate (geo= sprintf ("%s|%s", geo, name)) %>%
  ggplot(aes(x=year, y=pop )) +
  geom_line(size=.4) +
  geom_point(size=.4) +
  facet_wrap( ~geo, scales = "fixed", ncol = 2) +
  xlab(label="") +
  ylab(label="") +
  theme(plot.subtitle=element_text(size=9), legend.position="right")+
  ggtitle("Population by province 2016--2024")

ggsave(plot=p3p, "pop_PL_w3m.png", width=picWd, height = 13)
p3p


## ###
## ### Population balance as line trend slope
## ###
pop0 <- pop0 %>% filter (year > 2015) 

pop.ts <- as_tsibble(pop0, key = geo, index = year)
tsibble::interval(pop.ts)

fitted_models_pop = pop.ts %>% 
  na.omit() %>%
  model(m1=TSLM(pop ~ trend() ))
p.pop.fitted <- fitted_models_pop %>% fitted() %>% select (geo, year, value=.fitted)

pop.slope <- fitted_models_pop |> tidy() |> filter (term == 'trend()') %>%
  select (geo, estimate) %>%
  left_join(nuts, by='geo') %>% left_join(pop0.2024, by='geo') %>%
  mutate (balance1 = estimate/pop * unit1)

# Slope parameters summary
#summary(zz$estimate)

p1.pop.change <- pop.slope %>%
  mutate (geo= sprintf ("%s|%s", geo, name.x)) %>%
  ggplot(aes(x = reorder(geo, balance1 ))) +
  geom_bar(aes(y = balance1), stat="identity", alpha=.25, fill=default_violet ) +
  xlab(label="") +
  ylab(label="") +
  ggtitle("Population balance 2015--2023 per 1mln", subtitle='(line trend slope)') +
  ##
  theme(axis.text = element_text(size = 7)) +
  coord_flip()
p1.pop.change
ggsave(plot=p1.pop.change, "pop_PL_w4m.png", width=picWd, height = 13)


