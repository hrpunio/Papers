library("ggplot2")
library("dplyr")
library("scales")
library("ggthemes")
library("ggpubr")
library("tidyr")
library("readr")
library("stringr")
#################### Konferencja
picWd <- 12
picHt <- 8
spanV <- 0.5
spanVV <- 0.25
windowLen <- 12

xax.font.size <- 12

surl <- "© NI-KW (source: http://www.wsse.gda.pl/)"
mainColor <- "deeppink"
loessColor <- "steelblue"
mainBreaks <- "1 month"
NIKW <- "© NI-KW @ github.com/knsm-psw/GUS_mortality | https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/zgony-wedlug-tygodni,39,2.html"
unit1 <- 1000000
## kolory
farbe19 <- '#F8766D'
farbe20 <- '#00BFC4'
farbe21 <- '#C77CFF'

c25 <- c(
   "dodgerblue2", 
   "#E31A1C", # red
   "green4",
   "#6A3D9A", # purple
   "#FF7F00", # orange
   "black", 
   "gold1",
   "skyblue2", 
   #"#FB9A99", # lt pink
   "palegreen2",
   "#CAB2D6", # lt purple
   "#FDBF6F", # lt orange
   "gray70", 
   "khaki2",
   "maroon", 
   "orchid1", 
   "deeppink1", 
   ############
   #"blue1", 
   #"steelblue4",
   #"darkturquoise", 
   #"green1", 
   #"yellow4", 
   #"yellow3",
   #"darkorange4", 
   "brown"
)

firstYrWeek <- 1

#########################################################################
### Zgony COVID (tygodniowe) ###########################################
#############
zc <- read.csv("MZNw.csv", sep = ';',  header=T, na.string="NA" )
## dane dot województw
ww <- read.csv("ludnosc_2019.csv", sep = ';',  header=T, na.string="NA" )
###########
### zgony i liczba ludności
### ogółem PL 2000-2020
dd <- read.csv("pop_deaths_2000-2020.csv", sep = ';',  header=T, na.string="NA" )

#dd.p.1 <- dd %>%
#   ggplot(aes(x=year, y=pop)) +
#   geom_smooth(method = "lm", se=F, size=.8, alpha=.5) +
#   geom_point(size=.8) 
#dd.p.1
#
#
#dd.p.2 <- dd %>%
#   ggplot(aes(x=year, y=deaths)) +
#   geom_smooth(method = "lm", se=F, size=.8, alpha=.5) +
#   geom_point(size=.8) 
#dd.p.2
#
## trend
#
## ####
## wg województw
ddww <- read.csv("pop_by_woj_2000-2020.csv", sep = ';',  header=T, na.string="NA" )
##
ddww <- ddww %>% gather( key = "year", value = "pop", X1995:X2020) %>%
   mutate(year = as.numeric(str_sub(year, 2)))

#dd.p.3 <- ddww %>% filter ( teryt != 0 & year > 1999) %>% 
#   ggplot(aes(x=year, y=pop, color=name)) +
#   geom_line() +
#   geom_point(size=.4) +
#   scale_color_manual(values=c25) 
#
#
#dd.p.3
#
##
#dd.p.33 <- ddww %>% filter ( teryt != 0 & year > 1999) %>% 
#   ggplot(aes(x=year, y=pop )) +
#   geom_line() +
#   facet_wrap( ~name, scales = "free_y") +
#   geom_point(size=.4) +
#   scale_color_manual(values=c25) 
#dd.p.33
#
#ddww %>% group_by(name) %>% 
#   filter(year > 1999 & year < 2020) %>%
#   summarise( d = last(pop) - first(pop), 
#             dp = d/last(pop) * 100 )
#
#pop.pl <- ddww %>% filter ( teryt == 0 & year > 1999 & year < 2020) 
#
#last(pop.pl$pop)
#first(pop.pl$pop)
#
#diff2019 <- last(pop.pl$pop) - first(pop.pl$pop)
#diff2019
#diff2019 / last(pop.pl$pop) * 100
#
## tylko polska (łącznie)
#dd.p.4 <- ddww %>% filter ( teryt == 0 & year > 1999) %>% 
#   ggplot(aes(x=year, y=pop, color=name)) +
#   geom_line() +
#   geom_point(size=.4) +
#   scale_color_manual(values=c25) 
#
#dd.p.4
#
##  zc -- COVID deaths
##  #################################################################

zc.mean <- zc %>% 
   filter(woj != "Polska" & ((year == 2021 & dow < 15) | (year == 2020))) %>%
   group_by(woj) %>%
   summarise(cc = sum(newd))

aveCovid = sum(zc.mean$cc)
ww <- ww %>% filter (geo != 'Polska')

avePop = sum(ww$pop)
aveCP = aveCovid / avePop * 1000000

zc1m <- left_join( zc.mean, ww, by=c("woj"="geo")) %>%
   mutate(cc = cc / pop * 1000000, ccp = cc/aveCP * 100) %>%
   select(woj,cc,ccp,pop)

#### wykres słupkowy ###
#### 
covid_by_province <- zc1m %>%
   ggplot(aes(x = reorder(woj, ccp ))) +
   geom_bar(aes(y = cc), stat="identity", alpha=.25, fill=default_violet ) +
   xlab(label="") +
   ylab(label="") +
   ## remove title
   ##ggtitle("Deaths per 1mln 2020--2021 by province", subtitle='last week reported 14/2021') +
   ##
   geom_hline(yintercept = aveCP, color='red1', alpha=.4, size=.4) +
   theme(axis.text = element_text(size = xax.font.size)) +
   #theme(plot.title = element_text(hjust = 0.5)) +
   coord_flip()
   
covid_by_province_p <- zc1m %>%
   ggplot(aes(x = reorder(woj, ccp ))) +
   geom_bar(aes(y = ccp), stat="identity", alpha=.25, fill=default_violet ) +
   xlab(label="") +
   ylab(label="") +
   ggtitle("Deaths per 1mln 2020--2021 by province (as % of overall average)", 
           subtitle='last week reported 14/2021') +
   geom_hline(yintercept = 100, color='red1', alpha=.4, size=.4) +
   theme(axis.text = element_text(size = xax.font.size)) +
   #theme(plot.title = element_text(hjust = 0.5)) +
   coord_flip()


#p1 <- ggarrange(covid_by_province, covid_by_province_p,  ncol = 1, nrow=2 )
p1 <- covid_by_province
##ggsave(plot=p1, filename="COVID_by_woj.pdf", height = 5)

### [W1]
p1

## 

zc.total.PL <- zc %>% 
   filter(woj == "Polska" & ((year == 2021 & dow < 15) || (year==2020)))

zc.total.PL.p <- zc.total.PL %>% 
   ggplot(aes(x=as.Date(date), y=newd), color=farbe19) +
   geom_line(color=farbe19) +
   scale_x_date( labels = date_format("%y/%m/%d"), breaks ="6 weeks") +
   geom_point(size=.5, color=farbe19)

zc.total.PL.p
##ggsave(plot=zc.total.PL.p, filename="COVID_PL_total.pdf", height = 9)
## ### RYS[1]
p12 <- ggarrange(p1, zc.total.PL.p,  ncol = 1, nrow=2 )
ggsave(plot=p12, filename="COVID_by_woj.pdf", height = 6)

###################################################
##
## Total deaths
##################################
### dla całego okresu rocznie
wojcodes <- c('PL21', 'PL22', 'PL41', 'PL42', 'PL43', 'PL51', 'PL52',
'PL61', 'PL62', 'PL63', 'PL71', 'PL72', 'PL81', 'PL82', 'PL84', 'PL91', 'PL92');

## Województwa i Polska łącznie
z_PL_Woj <- read.csv("demo_r_mweek3_PL.csv.gz", sep = ';',  header=T, na.string="NA" )
n <- read.csv("nuts.csv", sep = ';',  header=T, na.string="NA" )

## Tylko PL (zgodny ogółem)
z_PL <- z_PL_Woj %>% filter(geo == "PL") %>% as.data.frame()
z_PL_yy <- z_PL  %>% filter(sex=="O" & age == "T") %>%
   group_by(year) %>% summarise(yy=sum(value))

## Tylko województwa
## Połącz 91 + 92 (tylko w ramce województwa)
z_Woj <- z_PL_Woj %>% filter(geo %in% wojcodes) %>%
   mutate(geo=recode(geo, PL91="PL92")) %>%
   group_by(year, week, sex, age, geo) %>% 
   summarise(value=sum(value)) %>% as.data.frame

## Dodaj nazwy
z_Woj <- left_join(z_Woj, n, by='geo')

## Tylko ogółem płeć/wiek
z_Woj_T <- z_Woj %>% filter (sex == 'O' & age == 'T' & geo %in% wojcodes ) %>% as.data.frame()

# wzg. województw rocznie
# 

z_Woj_T_sum <- z_Woj_T %>% group_by(year, geo) %>%
   summarise( mdeaths=sum(value, na.rm=T), na.rm=TRUE) %>%
   mutate (date = sprintf ("%i-01-01", year))  %>% as.data.frame()
## dodanie nazwy (bo wypadła)
z_Woj_T_sum <- left_join(z_Woj_T_sum, n, by='geo')

## 
z_Woj_T_sum <- left_join(z_Woj_T_sum, ddww, by=c('name', 'year'))

# na 1mln
z_Woj_T_sumr <- z_Woj_T_sum %>% 
   mutate(rm = mdeaths/pop * 1000000) %>% as.data.frame()

z_Woj_T_sumr_2019 <- z_Woj_T_sumr %>% filter (year == 2019) 

z_Woj_T_sumr_yy <- z_Woj_T_sumr %>% filter (year < 2021) %>%
   group_by(name) %>%
   summarise(rmy=mean(rm), dy=mean(mdeaths), dp = mean(pop))


p2.a <- z_Woj_T_sumr %>% filter ( year < 2021) %>% 
   ggplot(aes(x=as.Date(date), y=rm, color=name)) +
   geom_smooth(method = "loess", se=F, size=.5, span = spanV) +
   #geom_smooth(method = "lm", se=F, size=.8, alpha=.5) +
   #geom_line(size=.4) +
   geom_point(size=.5) +
   scale_color_manual(values=c25) +
   #geom_text(aes(label=sprintf("%i", week), y= cdeaths), size=2.5) +
   #facet_wrap( ~geo, scales = "free_y") +
   xlab(label="") +
   ylab(label="") +
   scale_x_date( labels = date_format("%Y"), breaks ="2 years") +
   ###ggtitle("Deaths per 1mln by province" ) +
   theme(plot.subtitle=element_text(size=9), legend.position="right")
   
p2.a

ggsave(plot=p2.a, "zgony_PL_x1r.pdf", height = 4)


p2.b <- z_Woj_T_sumr %>% filter ( year < 2021) %>% 
   ggplot(aes(x=as.Date(date), y=rm, color=geo)) +
   geom_smooth(method = "loess", se=F, size=.4, span = spanVV) +
   ##geom_smooth(method = "lm", se=F, size=.8, alpha=.5) +
   geom_point(size=.8) +
   scale_color_manual(values=c25) +
   #geom_text(aes(label=sprintf("%i", week), y= cdeaths), size=2.5) +
   #facet_wrap( ~geo, scales = "free_y") +
   xlab(label="") +
   ylab(label="") +
   scale_x_date( labels = date_format("%Y"), breaks ="1 year") +
   theme(plot.subtitle=element_text(size=9), legend.position="right")+
   ggtitle("Deaths per 1mln", subtitle="2015=100%" )

p2ab <- ggarrange(p2.a, p2.b, ncol = 1, nrow = 2)
ggsave(plot=p2ab, "zgony_PL_x11rr.pdf", width=picWd, height = picHt)
### [W2]
p2ab


## Polar diagram (PL ogółem)

z_PL_tt <- z_PL %>% filter (sex == 'O' & age == 'T' & year > 2014)  %>% as.data.frame()
pgr_polar <-   ggplot(z_PL_tt, aes(x= as.factor(week), y=value, group=as.factor(year), color=as.factor(year))) +
   geom_line(size=.5) +
   geom_point(size=2, alpha=.3) +
   scale_y_continuous(breaks=seq(2000, 20000, by=2000)) +
   xlab(label="week") +
   ylab(label="") +
   labs(caption="", color = "Year") +
   ##ggtitle("Total deaths weekly (Poland/2015--2021)") +
   theme(plot.subtitle=element_text(size=9), legend.position="right") +
   coord_polar(start=3*pi/2)
   
######### RYS[3]
pgr_polar   
ggsave(plot=pgr_polar, filename="COVID_PL_polar.pdf", height = 4)
##################

## ############################
## ############################
## średnia z lat 2015--2019 vs lata 2020/21

zz_0 <- z_Woj_T %>% filter(year > 2019) %>% as.data.frame()



zz15_19 <- z_Woj_T %>% filter(year < 2020) %>% 
   group_by(week, geo) %>%
   summarise( mdeaths=mean(value, na.rm=T), na.rm=TRUE) %>%
   as.data.frame()

## zgony + zgony covidowe
#zz_0 <- left_join(zz_0, n, by='geo') ##
zz_x <- left_join(zz_0, zc, by = c('year'='year', 'week'='dow', 'name' = 'woj'))

## cdeaths od 1.10.2020 ##
## 35 tydzień
start_2nd_wave <- as.numeric(format(as.Date("2020-08-31"), "%U"))

## zgony+zgony covidowe + średnia 2015-19
zz_xx <- left_join(zz15_19, zz_x, by = c('week'='week', 'geo' = 'geo')) %>%
   select(year, week, geo, value, newc, newd, mdeaths) %>%
   mutate (cdeaths = newd/(value - mdeaths ) * 100 ) %>% 
   filter (week < 53 & week > start_2nd_wave ) %>%
   as.data.frame()
# Dodaj nazwy
 zz_xx <- left_join(zz_xx, n, by='geo')

## Dodaj info o ludności
zz_xx_p <- left_join(zz_xx, ww, by=c('name'='geo'))

## Łącznie wojewódzkie
zz_xx_t <- zz_xx %>% group_by(name) %>%
   summarise(s_value = sum(value), s_nd = sum(newd), s_md = sum(mdeaths))

zz_xx_diff <- zz_xx_t %>% mutate (exdeaths = s_value - s_md) %>% 
   filter (name != 'Polska') %>%
   left_join(ww, by=c('name'='geo')) %>%
   mutate (exdp1m = exdeaths/pop * 1000000)

exd_total_PL <- sum(zz_xx_diff$exdeaths)
pop_total_PL <- sum(zz_xx_diff$pop)

exd_ave_PL <- exd_total_PL / pop_total_PL * 1000000

## bar chart

p3 <- zz_xx_diff %>%
   mutate(name = str_sub(name,1,9)) %>%
   ggplot(aes(x = reorder(name, exdp1m ))) +
   geom_bar(aes(y = exdp1m), stat="identity", alpha=.25, fill=default_violet ) +
   xlab(label="") +
   ylab(label="") +
   #ggtitle("Excessive deaths per 1mln 2020--2021 by province", 
   #        subtitle='last week reported 14/2021') +
   geom_hline(yintercept = exd_ave_PL, color='red1', alpha=.4, size=.4) +
   theme(axis.text = element_text(size = xax.font.size)) +
   coord_flip()
## RYS4 ###
p13 <- ggarrange(p3, covid_by_province,  ncol = 2, nrow=1 )
ggsave(plot=p13, file= "exd_by_province.pdf", height=4 )
## [W3]
#### RYS4L########
p13
######

upop <-  read.csv("miasta_2019.csv", sep = ';',  header=T, na.string="NA" )
dens <- read.csv("gestosc_2019.csv", sep = ';',  header=T, na.string="NA" )


upop.p <- upop %>% mutate(geo = str_sub(geo,1,9)) %>%
   ggplot(aes(x = reorder(geo, urbanpop ))) +
   geom_bar(aes(y = urbanpop), stat="identity", alpha=.25, fill=default_violet ) +
   geom_text(aes(label=sprintf("%.1f", urbanpop), y= urbanpop ), 
             vjust=0.25, hjust=1.25, size=2, color='brown3' ) +
   xlab(label="") +
   ylab(label="") +
   ggtitle("Urban population (%)" ) +
   geom_hline(yintercept = 100, color='red1', alpha=.4, size=.4) +
   theme(axis.text = element_text(size = xax.font.size)) +
   #theme(plot.title = element_text(hjust = 0.5)) +
   coord_flip()
upop.p

dens.p <- dens %>% mutate(geo = str_sub(geo,1,9)) %>%
   ggplot(aes(x = reorder(geo, urban_popdens))) +
   geom_bar(aes(y = urban_popdens), stat="identity", alpha=.25, fill=default_violet ) +
   geom_text(aes(label=sprintf("%.1f", urban_popdens), y= urban_popdens ), 
             vjust=0.25, hjust=1.25, size=2, color='brown3' ) +
   xlab(label="") +
   ylab(label="") +
   ggtitle("Population density (build-up areas)" ) +
   geom_hline(yintercept = 100, color='red1', alpha=.4, size=.4) +
   theme(axis.text = element_text(size = xax.font.size)) +
   #theme(plot.title = element_text(hjust = 0.5)) +
   coord_flip()
dens.p

pop_dens.p <- ggarrange(upop.p, dens.p, ncol = 2, nrow = 1)

ggsave(plot=pop_dens.p, file="pop_dens.pdf", height = 4)
pop_dens.p



###############
## Płeć łącznie
z_PL_sex_o <- z_PL %>% filter ( sex == 'O') %>% as.data.frame()

z0 <- z_PL_sex_o %>% filter ( year >= 2015  & year < 2020 ) %>% as.data.frame
z1 <- z_PL_sex_o %>% filter ( year == 2020 ) %>% as.data.frame
z2 <- z_PL_sex_o %>% filter ( year == 2021 ) %>% as.data.frame

## średnie w okresie 1 -- (n-1)
zz0 <- z0 %>% group_by(age,week) %>% 
   summarise( year = 't19', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz1 <- z1 %>% group_by(age,week) %>% 
   summarise( year = 't20', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz2 <- z2 %>% group_by(age,week) %>% 
   summarise( year = 't21', vv = mean(value, na.rm=TRUE)) %>% as.data.frame

### Połącz
zz1 <- bind_rows(zz0, zz1, zz2)


p4 <- ggplot(zz1, aes(x=week, y=vv, color=year)) +
   geom_smooth(method="loess", se=F, span=spanV, size=.4) +
   geom_point(size=.4, alpha=.5) +
   facet_wrap( ~age, scales = "free_y") +
   xlab(label="") +
   ylab(label="") +
   ##theme_nikw()+
   theme(plot.subtitle=element_text(size=9), legend.position="top")+
   scale_color_manual(name="Year: ", 
                      labels = c("2015--2019 average", "2020", "2021"), 
                      values = c("t19"=farbe19, "t20"=farbe20, "t21"=farbe21 )) +
   ggtitle("Deaths by age groups")

ggsave(plot=p4, "PL_deaths_by_agegrps.pdf", width=picWd)

###[W4]
### Deaths by age groups
p4

#######################################################
## wiek razem

z0 <- z_Woj %>% filter ( year >= 2015  & year < 2020 ) %>% as.data.frame
z1 <- z_Woj %>% filter ( year == 2020 ) %>% as.data.frame
z2 <- z_Woj %>% filter ( year == 2021 ) %>% as.data.frame

## średnie w okresie 1 -- (n-1)
zz0 <- z0 %>% group_by(name,week) %>% summarise( year = 't19', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz1 <- z1 %>% group_by(name,week) %>% summarise( year = 't20', 
                                                 vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz2 <- z2 %>% group_by(name,week) %>% summarise( year = 't21', 
                                                 vv = mean(value, na.rm=TRUE)) %>% as.data.frame
### Połącz
zz1 <- bind_rows(zz0, zz1, zz2)
### Zamień na procenty

zz1p <- zz1 %>% pivot_wider(names_from = year, values_from = vv) %>%
   mutate (t20p = t20/t19 * 100, t21p = t21 / t19 * 100) %>%
   select(name, week, t20p, t21p) %>%
   pivot_longer(cols=c(t20p, t21p), names_to="year", values_to="vv") %>%
   as.data.frame()

lastWeek <- last(zz1$week)
## nie działa dla dwóch lat
## firstWeek <- lastWeek - windowLen
firstWeek <- firstYrWeek
zz1 <- zz1 %>% filter ( week >= firstWeek  ) %>% as.data.frame

p5 <- ggplot(zz1p, aes(x=week, y=vv, group=year, color=year)) +
   geom_smooth(method="loess", se=F, span=spanV, size=.4) +
   geom_point(size=.4, alpha=.5) +
   #facet_wrap( ~name, scales = "free_y") +
   facet_wrap( ~name, scales = "fixed") +
   xlab(label="") +
   ylab(label="") +
   geom_hline(yintercept = 200, color="firebrick", alpha=.3, size=0.4) +
   geom_hline(yintercept = 150, color="green1", alpha=.3, size=0.4) +
   theme(plot.subtitle=element_text(size=9), legend.position="top")+
   scale_color_manual(name="Year: ", labels = c("2020", "2021"), 
                      values = c("t20p"=farbe20, "t21p"=farbe21 ))
   #ggtitle("Deaths by province")

##
p5 <- ggplot(zz1p, aes(x=week, y=vv, group=year, color=year)) +
   #geom_smooth(method="loess", se=F, span=spanVV, size=.4) +
   geom_line(size=.4, alpha=.5) +
   geom_point(size=.8, alpha=.5) +
   #facet_wrap( ~name, scales = "free_y") +
   facet_wrap( ~name, scales = "fixed") +
   xlab(label="") +
   ylab(label="") +
   geom_hline(yintercept = 200, color="firebrick", alpha=.3, size=0.4) +
   geom_hline(yintercept = 150, color="green1", alpha=.3, size=0.4) +
   ## 47 tydzień
   geom_vline(xintercept = 47, colour="green") +
   scale_x_continuous(breaks=seq(1, 52, by=5)) +
   theme(plot.subtitle=element_text(size=9),
         axis.text = element_text(size = 7),
         legend.position="top")+
   scale_color_manual(name="Year: ", labels = c("2020", "2021"), 
                      values = c("t20p"=farbe20, "t21p"=farbe21 ))
   #ggtitle("Deaths by province")

ggsave(plot=p5, "zgony_PL_by_woj_O.pdf", height = 5)
p5


ggsave(plot=p4, "zgony_PL_by_woj_O.pdf", width=picWd, height = picHt)
###
##[W5]
p5
###
zc.mean <- zc %>% select (year, dow, woj, newd) %>%
   filter ((year == 2021 & dow < 15) | (year == 2020 & dow > 35)) %>%
   group_by(woj) %>%
   summarise( meand=mean(newd, na.rm=T)) %>% as.data.frame()
## na 1mln
#wojpop <- read.csv("ludnosc_2019.csv", sep = ';',  header=T, na.string="NA" )

zc.mean <- left_join(zc.mean, ww, by=c("woj"="geo"))
zc.mean <- zc.mean %>% mutate(meand = meand/pop * 1000000)

zc.pp <- left_join(zc, zc.mean, by="woj") %>%
   filter ((year == 2021 & dow < 15) | (year == 2020 & dow > 35)) %>%
   select (woj, dow, year, newd, meand) %>%
   mutate (vv=newd/meand * 100, year=sprintf("c%i", year - 2000)) %>%
   #select(woj, dow, year, vv, newd, meand) %>%
   select(woj, dow, year, vv) %>%
   rename(name=woj, week=dow) %>% as.data.frame()

## dodaj do info o zgonach
zz1zz <- bind_rows(zz1p, zc.pp)  %>%  filter (name != 'Polska')

p6 <- ggplot(zz1zz, aes(x=week, y=vv, group=year, color=year)) +
   #geom_smooth(method="loess", se=F, span=spanV, size=.4) +
   geom_line(size=.4, alpha=.5) +
   geom_point(size=.8, alpha=.5) +
   facet_wrap( ~name, scales = "free_y") +
   #facet_wrap( ~name, scales = "fixed") +
   xlab(label="") +
   ylab(label="") +
   #geom_hline(yintercept = 200, color="firebrick", alpha=.3, size=0.4) +
   #geom_hline(yintercept = 150, color="green1", alpha=.3, size=0.4) +
   ##theme_nikw()+
   scale_y_log10()+
   scale_x_continuous(breaks=seq(1, 52, by=5)) +
   geom_vline(xintercept = 47, colour="green") +
   ##labs(caption=source) +
   theme(plot.subtitle=element_text(size=9),
         axis.text = element_text(size = 7),
         legend.position="top")
   #scale_color_manual(name="Year: ", labels = c("2020", "2021", "c20", "c21"), 
   #                   values = c("t20p"=farbe20, "t21p"=farbe21, "c19"=farbe19 )) +
   ##ggtitle("Deaths in Poland by province (weekly)")

## [W6]
ggsave(plot=p6, "zgony_PL_by_woj_Ozz.pdf", height = 6) 
p6




##################################################
##
#csvw <- read.csv("MZNw.csv", sep = ';',  header=T, na.string="NA" )

## 36 tydzień =  1.09 początek drugiej fali
zc.mean <- zc %>% select (year, dow, woj, newd) %>%
   filter ((year == 2021 & dow < 15) | (year == 2020 & dow > 35)) %>%
   group_by(woj) %>%
   summarise( meand=mean(newd, na.rm=T), na.rm=TRUE) %>% as.data.frame()
## na 1mln
#wojpop <- read.csv("ludnosc_2019.csv", sep = ';',  header=T, na.string="NA" )

zc.mean <- left_join(zc.mean, ww, by=c("woj"="geo"))
zc.mean <- zc.mean %>% mutate(meand = meand/pop * 1000000)

zc.pp <- left_join(zc, zc.mean, by="woj") %>%
   filter ((year == 2021 & dow < 15) | (year == 2020 & dow > 35)) %>%
   select (woj, dow, year, newd, meand) %>%
   mutate (vv=newd/meand * 100, year=sprintf("c%i", year - 2000)) %>%
   #select(woj, dow, year, vv, newd, meand) %>%
   select(woj, dow, year, vv) %>%
   rename(name=woj, week=dow) %>% as.data.frame()

## dodaj do info o zgonach
zz1zz <- bind_rows(zz1p, zc.pp)  %>%  filter (name != 'Polska')

p6 <- ggplot(zz1zz, aes(x=week, y=vv, group=year, color=year)) +
   geom_smooth(method="loess", se=F, span=spanV, size=.4) +
   geom_point(size=.4, alpha=.5) +
   #facet_wrap( ~name, scales = "free_y") +
   facet_wrap( ~name, scales = "fixed") +
   xlab(label="") +
   ylab(label="") +
   #geom_hline(yintercept = 200, color="firebrick", alpha=.3, size=0.4) +
   #geom_hline(yintercept = 150, color="green1", alpha=.3, size=0.4) +
   ##theme_nikw()+
   scale_y_log10()+
   ##labs(caption=source) +
   theme(plot.subtitle=element_text(size=9), legend.position="top")
   #scale_color_manual(name="Year: ", labels = c("2020", "2021", "c20", "c21"), 
   #                   values = c("t20p"=farbe20, "t21p"=farbe21, "c19"=farbe19 )) +
   #ggtitle("Deaths in Poland by province (weekly)")

## [W6]
ggsave(plot=p6, "zgony_PL_by_woj_Ozz.pdf", width=picWd, height = picHt)
## 
p6
###########################
###########################


zz_0 <- z_Woj_T %>% filter(year > 2019) %>% as.data.frame()
##zz_0 <- left_join(zz_0, n, by='geo') ##??##

zz_15 <- z_Woj_T  %>% filter(year < 2020) %>% 
   group_by(week, geo) %>%
   summarise( mdeaths=mean(value, na.rm=T), na.rm=TRUE) %>%
   as.data.frame()

## cdeaths od 1.10.2020 ##
zz_xx <- left_join(zz_15, zz_x, by = c('week'='week', 'geo' = 'geo')) %>%
   select(year, week, geo, value, newc, newd, mdeaths) %>%
   mutate (cdeaths = newd/(value - mdeaths ) * 100 ) %>% 
   ##mutate(date=as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")) %>%
   mutate(date= as.Date(sprintf("%i-%i-1", year, week), "%Y-%U-%u") ) %>%
   filter (as.Date(date) > "2020-09-30" ) %>%
   as.data.frame()
zz_xx <- left_join(zz_xx, n, by='geo')

p7 <- zz_xx %>% filter (geo != "Polska") %>%
   mutate(cdeaths = replace(cdeaths, which( cdeaths > 100 | cdeaths < -100), NA)) %>%
   ggplot(aes(x=as.Date(date), y=cdeaths, color=name)) +
   geom_smooth(method="loess", size=.4, se = F, span=spanV) +
   geom_point(size=.8) +
   xlab(label="yy/ww") +
   ylab(label="") +
   scale_x_date( labels = date_format("%y/%W"), breaks ="2 weeks") +
   ##coord_cartesian(ylim = c(0, 100)) +
   theme(plot.subtitle=element_text(size=7), legend.position="right")
   ##ggtitle("COVID-related deaths/all deaths ratios" )


p7

### RYS8
####
ggsave(plot=p7, "zgony_PL_by_province_1.pdf", height = 3.5)
###

