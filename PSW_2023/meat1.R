##---
##title: "Meat and animal products consumption trends 2010--2020. Exploratory analysis"
##author: "Tomasz Przechlewski"
##description: (c) Tomasz Przechlewski / CC-BY license
##date: "May 2023"
##output:
##  slidy_presentation:
##    css: scrollable_slides.css
##    smaller: yes
##  html_document:
##    df_print: paged
##  ioslides_presentation: default
##---

library("ggplot2")
library("dplyr")
library("tidyr")
library("knitr")
library("ggpubr")

printprc <- function(x) { sprintf ("%.1f%%", x)}

printprc(2.222)

items.of.interest <- c( 'Meat', 'Animal Products', 'Grand Total', 'Population')
units.of.interest <- c('FoodS (kcal/c/d)', 'ProtSQ (g/c/d)', 
                       'FoodSQ (kg/c/y)', 'TotalPop', 'Import', 'Export',
                       'DomSQ')
## years_c not used
# years_c <- c('2010', '2011', '2012', '2013', '2020')
## Lata które są rejestrowane podwójnie
years_overlap <- c('2010', '2011', '2012', '2013')
## Lata wybrane do wykresu
years_sel <- c('1961', '1981', '2001', '2020')

## 159 == China
## China mainland vs China, mainland
fao.aggregates <- c("001", "002", "014", "017", "015", "018", "011", "019",
"021", "013", "029", "005", "142", "143", "030", "034", "035", "145", "150",
"151", "154", "039", "155", "009", "053", "054", "097", "199", "432", "722",
"901", "902", "159")

LastYr <- 2020

## Dane 2010--
## China, mainland vs China mainland
o0 <- read.csv(file='FoodBalance2023.csv',sep=';', 
               colClasses=c( CodeM49 ="character"),
               header=T) %>%
  ##filter (YearCode %in% years_c ) %>%
  filter (Item %in% items.of.interest) %>%
  filter (Element %in% units.of.interest)

## Spożycie mięsa ogółem 1000 ton w  roku 2020
o0.total.2020.world <- o0 %>% filter (Area == 'World') %>%
  filter (Item == 'Meat' & Element =='DomSQ' & YearCode == LastYr) %>%
  select(Value) %>%  unlist() %>% unname() 

## Dane historyczne
## Dane --2013
o0h <- read.csv(file='FoodBalance2023H_M.csv',sep=';', 
               colClasses=c( CodeM49 ="character"),
               header=T) %>%
  ##filter (YearCode %in% years_c ) %>%
  filter (Item %in% items.of.interest) %>%
  filter (Element %in% units.of.interest)

## Tylko kraje o liczbie ludności > 3mln (2020)
## około 130 krajów
## Polecenie pull zamienia na listę
big.countries.2020 <- o0 %>% filter(YearCode == LastYr ) %>%
   filter (Item == 'Population' & Value > 3000) %>%
    filter (! CodeM49 %in% fao.aggregates) %>%
  pull(CodeM49)

o1 <- o0 %>% filter (CodeM49 %in% big.countries.2020) 
o1h <- o0h %>% filter (CodeM49 %in% big.countries.2020) 

## Mają być tylko 2010--2013
o1xx <- o1 %>% filter (YearCode %in% years_overlap)
o1hxx <- o1h %>% filter (YearCode %in% years_overlap)

o_old_vs_new.0 <- o1xx %>% 
  left_join(o1hxx, by=c('CodeM49', 'Item', 'Element', 'YearCode') ) %>%
  select(CodeM49, Area=Area.x, Item, Element, YearCode, 
         Value=Value.x, ValueOld=Value.y)

o_old_vs_new <- o_old_vs_new.0 %>%
  group_by(Area, Item, Element) %>%
    summarise(v = mean(Value), z = mean(Value - ValueOld)) %>%
  ungroup() %>%
  mutate(zp=z/v * 100 ) %>%
  filter (Item == 'Meat') %>%
  filter (Element == 'FoodSQ (kg/c/y)' | Element == 'Import' | Element == 'Export')

## # Kraje o 10% i większej różnicy 
## # -------------------------------
o_big_diff <- o_old_vs_new %>%
  filter (zp > 10 | zp < -10) %>%
  filter (Element == 'FoodSQ (kg/c/y)' )

##
o_big_diff_names <- o_big_diff %>% select (Area) %>% unlist %>% unname()

## Łączymy stare + nowe
## pomijamy kraje o dużej różnicy
#
o1h <- o1h %>% filter (YearCode < 2010)

oo <- bind_rows(o1, o1h) %>%
  filter (! Area %in% o_big_diff_names )

## liczba krajów
nn0 <- nlevels(as.factor(oo$Area))
nn_levels_0 <- levels(as.factor(oo$Area))

## Łączna konsumpcja mięsa na świecie (szereg czasowy)
o0h <- o0h %>% filter (YearCode < 2010)


oo.total.meat <- bind_rows(o0, o0h) %>% filter (Area == 'World') %>%
  filter (Item == 'Meat' & Element =='DomSQ') %>%
    arrange(Value)
##
## Meat FoodS (kcal/c/d)
oo.total.meat.foodS <- bind_rows(o0, o0h) %>% filter (Area == 'World') %>%
  filter (Item == 'Meat' & Element =='FoodS (kcal/c/d)') %>%
    arrange(Value)

##
## Łączna konsumpcja białka (g/c/d)
oo.total.prot <- bind_rows(o0, o0h) %>% filter (Area == 'World') %>%
  filter (Item == 'Grand Total' & Element =='ProtSQ (g/c/d)') %>%
    arrange(Value)

##
## Łączna konsumpcja wszystkiego
oo.total.tot <- bind_rows(o0, o0h) %>% filter (Area == 'World') %>%
  filter (Item == 'Grand Total' & Element =='FoodS (kcal/c/d)') %>%
     arrange(Value)
### Indeksy
## FoodSuply kcal/pc 
oo.total.tot <- oo.total.tot %>%
  mutate (pt = Value / first(Value) * 100)

## ProtSupply
oo.total.prot <- oo.total.prot %>%
  mutate (pp = Value / first(Value) * 100)

## MeatSupply 1000 t
oo.total.meat <- oo.total.meat %>%
  mutate (pm = Value / first(Value) * 100)

## mięso per capita
oo.total.meat.foodS <- oo.total.meat.foodS  %>%
  mutate (pm = Value / first(Value) * 100)

#################################################################################
## Średnia zmiana 1961-2010
yearsPass <- 2010 - 1961
YC0 <- c(1961, 2010)
YY <- oo.total.meat.foodS %>% filter (YearCode %in% YC0)

meat.min.0 <- YY %>%  summarise(index = first(Value))  %>% unlist %>% unname()
meat.max.0 <- YY %>%  summarise(index = last(Value))  %>% unlist %>% unname()
                                
meat.index.0 <- YY %>%
  summarise(index = ((last(Value)/first(Value) * 100 ) - 100) /yearsPass)  %>%
  unlist %>% unname()

YY <- oo.total.prot %>% filter (YearCode %in% YC0)

prot.min.0 <- YY %>%  summarise(index = first(Value))  %>% unlist %>% unname()
prot.max.0 <- YY %>%  summarise(index = last(Value))  %>% unlist %>% unname()

prot.index.0 <- YY %>%
  summarise(index = ((last(Value)/first(Value) * 100 ) - 100) /yearsPass)  %>%
  unlist %>% unname()

YY <- oo.total.tot %>% filter (YearCode %in% YC0)

tot.min.0 <- YY %>%  summarise(index = first(Value))  %>% unlist %>% unname()
tot.max.0 <- YY %>%  summarise(index = last(Value))  %>% unlist %>% unname()

tot.index.0 <- YY %>%
  summarise(index = ((last(Value)/first(Value) * 100 ) - 100) /yearsPass)  %>%
  unlist %>% unname()

### okres 2020--2010
yearsPass <- 2020 - 2010
YC0 <- c(2010, 2020)

YY <- oo.total.meat.foodS %>% filter (YearCode %in% YC0)

meat.min.1 <- YY %>%  summarise(index = first(Value))  %>% unlist %>% unname()
meat.max.1 <- YY %>%  summarise(index = last(Value))  %>% unlist %>% unname()

meat.index.1 <- YY %>%
  summarise(index = ((last(Value)/first(Value) *100) - 100) / yearsPass) %>%
  unlist %>% unname()


YY <- oo.total.prot %>% filter (YearCode %in% YC0)

prot.min.1 <- YY %>%  summarise(index = first(Value))  %>% unlist %>% unname()
prot.max.1 <- YY %>%  summarise(index = last(Value))  %>% unlist %>% unname()

prot.index.1 <- YY %>%
  summarise(index = ((last(Value)/first(Value) * 100 ) - 100) /yearsPass)  %>%
  unlist %>% unname()

YY <- oo.total.tot %>% filter (YearCode %in% YC0)

tot.max.1 <- YY %>%  summarise(index = first(Value))  %>% unlist %>% unname()
tot.min.1 <- YY %>%  summarise(index = last(Value))  %>% unlist %>% unname()

tot.index.1 <- YY %>%
  summarise(index = ((last(Value)/first(Value) * 100 ) - 100) /yearsPass)  %>%
  unlist %>% unname()

### Konsumpcja w tonach/ udział konsumpcji krajów z próby
### w całości
yearsPass <- 2020 - 2010
YC0 <- c(2010, 2020)

## oo.total.meat = łącznie świat
YY <- oo.total.meat %>% filter (YearCode %in% YC0)
meat.min.supply <- YY %>%  summarise(index = first(Value))  %>% unlist %>% unname()
### konsumpcja mięsa (Domestic Supply w 2020)
meat.max.supply <- YY %>%  summarise(index = last(Value))  %>% unlist %>% unname()
## Teraz łącznie dla badanych krajów 

oo.total.meat.sample <- oo %>% filter (YearCode == 2020) %>%
  filter (Item == 'Meat' & Element =='DomSQ') %>%
  summarise(total = sum(Value)) %>% unlist %>% unname()

### ile światowej konsumpcji mięsa jest w próbie (ramka oo)
## około 92.3%
how.much.meat.in.sample <- oo.total.meat.sample / meat.max.supply * 100


#################################
## Porównanie dynamiki (wykres)
p.total.cons.xx <- oo.total.meat.foodS %>% left_join(oo.total.prot, by='YearCode' ) %>%
  left_join(oo.total.tot, by='YearCode') %>%
  select(YearCode, meat=pm, protein=pp, total=pt) %>%
  pivot_longer(c(meat, protein, total), names_to = 'P', values_to = 'V') %>%
  ggplot(aes(x=YearCode, y=V, color=P)) +
  geom_point() +
  geom_line() +
  geom_text(aes( x=2009, y=105, label="2010"), size=3, angle=90,
            color='hotpink4') +
  geom_vline(xintercept = 2010, colour="hotpink4") +
  ##facet_wrap(~class, scales = "free") +
  ylab("%") +
  xlab("") +
  labs(colour="") +
  theme(legend.position="top") +
  ggtitle("Meat/Protein/Total consumption (1961=100%)")

## konsumpcja mięsa, białka oraz łącznie 1961--2020
p.total.cons.xx
ggsave(p.total.cons.xx, file="meat-prot-tot-consumption.png")


##
## Grupy dochodowe BŚ
##
gni <- read.csv(file='API_NY.GNP.PCAP.CD_DS2_en_csv_v2_5340976.csv', 
               sep=',', 
               ##colClasses=c( CodeM49 ="character"),
               header=T) %>%
  select (ISO3=Country.Code, starts_with('X')) %>%
  pivot_longer(cols = starts_with('X'), names_prefix = 'X', names_to = 'year',
               values_to = 'GNI') %>%
  mutate (year = as.numeric(year), GNI=GNI/1000)


## gni.pl <- gni %>% filter (ISO3 == 'POL')

gni0 <- gni %>%
  drop_na() %>%
  group_by(ISO3) %>%
  summarise(ISO3 = last(ISO3), year = last(year), GNI = last (GNI))


## UNSD nie zawiera taiwanu
## unsd potrzebny do dodania ISO3
#unsd <- read.csv(file='m49toIso.csv', sep=';',
#                 colClasses=c( M49 ="character"), header=T) %>%
#  select (CountryName, CodeM49=M49, ISO3)
unsd <- read.csv(file='m49toIso.csv', sep=';',
                 colClasses=c( M49 ="character"), header=T) %>%
  select (CountryName, CodeM49=M49, ISO3, UE)
unsd.ue <- unsd %>% filter (UE == "Y") %>%
  select (CodeM49) %>% unlist %>% unname()

## Podstawowa baza danych
## oo - wszystkie kraje bez krajów małych i `mało wiarygodnych' (ok 103)
o2 <- oo %>% 
  left_join(unsd, by='CodeM49' ) %>%
  left_join(gni, by=c('ISO3'='ISO3', 'YearCode'='year')) %>%
  ##mutate( class=IncomeGroup)
  mutate (fbs=case_when(YearCode < 2011  ~ "<2011", 
                        YearCode >= 2011 ~ "2010>",
                        TRUE ~ NA_character_)) %>%
  ##
  mutate( class=case_when( GNI < 1.085 ~ "low",
                           GNI < 4.255 ~ "lower-middle",
                           GNI < 13.205 ~ "upper-middle",
                           GNI >= 13.205 ~ "high",
                           TRUE ~ NA_character_ ) ) %>%
  drop_na(class)

## dwa kraje odpadły
nn1 <- nlevels(as.factor(o2$Area))
nn_levels_1 <- levels(as.factor(o2$Area))

oo.total.meat.sample <- o2 %>% filter (YearCode == 2020) %>%
  filter (Item == 'Meat' & Element =='DomSQ') %>%
  summarise(total = sum(Value)) %>% unlist %>% unname()

### ile światowej konsumpcji mięsa jest w próbie (ramka oo)
## około 92.3%
how.much.meat.in.sample <- oo.total.meat.sample / meat.max.supply * 100
##
## ostatecznie 90,7%
##nn0[! (nn_levels_0 %in% nn_levels_1)  ]
nn_diff <- setdiff(nn_levels_0, nn_levels_1)
##nn_diff


### ###################################################
### Podsumowania
### ###################################################

##
## Spożycie mięsa ogółem 1000 ton w  roku 2020 (już było)
#o2.total.2020.sample <- o2 %>% 
#  filter (Item == 'Meat' & Element =='DomSQ' & YearCode == LastYr) %>%
#  select(Value) %>% summarise( t = sum(Value)) %>%
#  unlist() %>% unname() 
## 303022
#sampleShare <- o2.total.2020.sample  / o0.total.2020.world *100

#############################
oo.total.meat.by.class <- o2 %>% 
  ##filter (Area == 'World') %>%
  filter (YearCode > 1979) %>%
  filter (Item == 'Meat' & Element =='DomSQ') %>%
  group_by(class, YearCode) %>%
  summarise(Value=sum(Value)) %>%
  group_by(class) %>%
  arrange(class, YearCode)  %>%
  mutate (ValueP=Value/first(Value) * 100) 
  

##
## Meat consumption by income class
p.total.meat.by.class <- oo.total.meat.by.class %>%
  ggplot(aes(x=YearCode, y=Value, color=class)) +
  geom_point() +
  geom_smooth(method='loess', se=F) +
  ggtitle("Meat consumption (1000 tonnes)") +
  ylab("1000 tonnes") +
  xlab("") +
  annotate("text", x = 1995, y = 170000, colour = "black", size=4, 
           label="Lines fitted with loess regression") +
  labs(colour="income class:") +
  theme(legend.position="top")

p.total.meat.by.class
ggsave(p.total.meat.by.class, file="total_meat_cons_by_gdi_class.png")

##
o2.high <- o2 %>% group_by(Area) %>%
  summarise(Area=last(Area), class=last(class)) %>%
  filter (class=='high')
N.high <- nrow(o2.high)

o2.lm <- o2 %>% group_by(Area) %>%
  summarise(Area=last(Area), class=last(class)) %>%
  filter (class=='lower-middle')
N.lm <- nrow(o2.lm)

o2.um <- o2 %>% group_by(Area) %>%
  summarise(Area=last(Area), class=last(class)) %>%
  filter (class=='upper-middle')
N.um <- nrow(o2.um)

o2.low <- o2 %>% group_by(Area) %>%
  summarise(Area=last(Area), class=last(class)) %>%
  filter (class=='low')
N.low <- nrow(o2.low)

## There is `r N.high` countries in high-income group etc...

## ============================================================
## Population

p.pop <- o2 %>%
  filter (Item == 'Population') %>%
  group_by(class, YearCode) %>%
  summarise(population = sum(Value, na.rm = T)) %>%
  group_by(YearCode) %>%
  mutate(T = sum(population)) %>%
  group_by(class, YearCode) %>%
  summarise(p = sum(population) / T * 100 ) 

## sprawdzenie
##  p.pop.total <- p.pop %>%
##  group_by(YearCode) %>%
##  summarise(p=sum(p))
##

p.pop.p1 <- p.pop %>% 
  filter (YearCode %in% years_sel) %>%
  ##mutate(class = as.factor(class)) %>%
  mutate(class = dplyr::recode(class, "high" = "h", 
                         "upper-middle" = "um", 
                         "lower-middle" = "lm",
                         "low" = "l" )) %>%
  ggplot(aes(x = class, y = p)) +
  facet_wrap(~YearCode) +
  ggtitle("World population by income classes (% of total)") +
  geom_bar(stat='identity',  fill = "navyblue")  +
  xlab ("income class") +
  xlab ("%") +
  geom_text(aes(label = sprintf ("%.1f", p)), size=3, color='white', vjust=1.5)
  ##coord_flip()

p.pop.p1
ggsave(p.pop.p1, file="pop_by_class.png")

##
## Meat Consumption 

p0.meat.cons <- o2 %>% 
  filter (Item == 'Meat' & Element =='FoodSQ (kg/c/y)' ) %>%
  filter (YearCode %in% years_sel) %>%
  mutate(class = dplyr::recode(class, 
                        "high" = "h", "upper-middle" = "um", 
                        "lower-middle" = "lm",
                        "low"='l')) %>%
  ggplot(aes(x=class, y=Value)) +
  facet_wrap(~YearCode) +
  geom_jitter(size=3, alpha=.3, position=position_jitter(0.05)) +
  geom_boxplot(outlier.shape = NA, alpha=.3, fill='yellow') +
  ylab("kg/capita/year") +
  xlab("income class") +
  ggtitle("Meat consumption (kg/capita/year)", subtitle="")

p0.meat.cons
ggsave(p0.meat.cons, file="meat_consumption_by_class.png")

### Not included in paper
p1.meat.cons <- o2 %>% 
  filter (Item == 'Meat' & Element =='FoodSQ (kg/c/y)' ) %>%
  group_by(Area) %>%
  arrange (YearCode) %>%
  mutate (class=last(class)) %>%
  filter (class == 'lower-middle') %>%
  filter (YearCode > 1995) %>%
  ggplot(aes(x=YearCode, y=Value )) +
  facet_wrap(~Area, scales='free_y') +
  geom_smooth(method='loess', size=.25) +
  ylab("kg/capita/year") +
  xlab("income class") +
  theme(legend.position = "none") +
  ggtitle("Meat consumption (kg/capita/year)", subtitle="")
p1.meat.cons
ggsave (p1.meat.cons, file='lower-middle-meat-consumption-pc.png')


## ###### Regression #######################################
##library("plm") ## or install.packages("plm") # first
## ##########################################################


###########################################################################
## Meat (kilograms)
##########################################################################

oom.0 <- o2 %>% filter (Item == 'Meat' & Element =='FoodSQ (kg/c/y)' ) %>%
  select (CodeM49, Area, Meat=Value, year=YearCode, GNI, class, fbs) %>%
    mutate (class = as.factor(class))

oom <- oom.0 %>% filter (year > 1995)


## Model Engla #######################################################
model.0.engel <- lm(data=oom, log(Meat) ~ log(GNI)  )
summary <- summary(model.0.engel)

lmr.meat.0 <- summary(model.0.engel)$r.squared
lmc_meat.0 <- coef(model.0.engel);
f_meat <- function(x) lmc_meat.0[1] + x * lmc_meat.0[2]

## EKC (logs)
model.0.ekc <- lm(data=oom, log(Meat) ~ log(GNI) + I(log(GNI)^2) )
summary(model.0.ekc)
lmr.meat.1 <- summary(model.0.ekc)$r.squared

### which is better?
anova(model.0.engel, model.0.ekc)
##library("AER")
##library("car")
aic1 <- AIC(model.0.engel, model.0.ekc) ## Engel is better
model.0.engel_aic <- aic1["model.0.engel","AIC"]
model.0.ekc_aic <- aic1["model.0.ekc","AIC"]
##
anova(model.0.ekc)

## EKC cont.
lmc <- coef(model.0.ekc);
lmc[3]
##
apex <- lmc[2] / (-2* lmc[3])
apex.y <- lmc[1] + lmc[2] * apex + lmc[3] * apex^2

##
## wykresy
p1.oom <- oom %>%
  ggplot(aes(x=GNI, y=Meat, color=as.factor(fbs))) +
  geom_point(alpha=.5) +
  ggtitle("Meat intake (kcal/c/d)")+
  ylab("kcal") +
  labs(colour="") +
  theme(legend.position="top") +
  scale_color_manual(values = c("<2011" = "black", "2010>" = "orange"))
p1.oom
#
#p1.oom.l <- oom %>%
#  ggplot(aes(x=log(GNI), y=log(Meat))) +
#  geom_point() +
#  ##facet_wrap(~class, scales = "free") +
#  ggtitle("Meat consumption (kcal/c/d)", subtitle="") 
#p1.oom.l

#
p.kuznets.meat.0 <- ggplot(oom, aes(log(GNI), log(Meat)) ) + 
  geom_point(color='red', alpha=.4) +
  stat_smooth(method = lm, se=F, formula = y ~ poly(x, 2)) +
  stat_function(fun=f_meat, colour="black", size=1, linetype="dashed") +
  annotate("point", x = apex, y = apex.y, colour = "black", size=4) +
  annotate("text", x = apex, y = apex.y, colour = "black", size=4, 
           label=sprintf ("%.1f", exp(apex)), vjust=2, hjust=0.75)

##p.kuznets.meat.0

##
## EKC (non-log version)
##
model.1.ekc <- lm(data=oom, Meat ~ GNI + I(GNI^2) )
summary(model.1.ekc)
lmr.meat.2 <- summary(model.1.ekc)$r.squared
#
lmc <- coef(model.1.ekc);
lmc[3]
##
apex <- lmc[2] / (-2* lmc[3])
apex.y <- lmc[1] + lmc[2] * apex + lmc[3] * apex^2

## Arithmetic scale
p.kuznets.meat.1 <- ggplot(oom, aes(GNI, Meat) ) + 
  geom_point(color="red", alpha=.4) +
  stat_smooth(method = lm, se=F, formula = y ~ poly(x, 2)) +
  annotate("point", x = apex, y = apex.y, colour = "black", size=4) +
  annotate("text", x = apex, y = apex.y, colour = "black", size=4,
           label=sprintf ("%.1f", apex), vjust=2, hjust=0.75)

##p.kuznets.meat.1


###
### Protein (czyli białko) ######################################
###

oop.0 <- o2 %>% filter (Item == 'Grand Total' & Element =='ProtSQ (g/c/d)' ) %>%
  select (CodeM49, Area, Value, year=YearCode, GNI, class) %>%
  mutate (class = as.factor(class))
oop <- oop.0 %>% filter (year > 1995)

#p1.oom <- oop %>%
#  ggplot(aes(x=GNI, y=Value)) +
#  geom_point() +
#  ggtitle("Protein (g/c/d)", subtitle="")
#p1.oom
#
#p1.oom.l <- oop %>%
#  ggplot(aes(x=log(GNI), y=log(Value))) +
#  geom_point() +
#  ##facet_wrap(~class, scales = "free") +
#  ggtitle("Protein (g/c/d)", subtitle="") 
#p1.oom.l

## EKC (logs)
model.0.prot <- lm(data=oop, log(Value) ~ log(GNI) + I(log(GNI)^2) )
summary(model.0.prot)
lmr.prot.0 <- summary(model.0.prot)$r.squared

lmc <- coef(model.0.prot);
apex <- lmc[2] / (-2* lmc[3])
apex.y <- lmc[1] + lmc[2] * apex + lmc[3] * apex^2

p.kuznets.prot.0 <- ggplot(oop, aes(log(GNI), log(Value)) ) + 
  geom_point(color='red', alpha=.4) +
  stat_smooth(method = lm, se=F, formula = y ~ poly(x, 2)) +
  annotate("point", x = apex, y = apex.y, colour = "black", size=4) +
  annotate("text", x = apex, y = apex.y, colour = "black", size=4, 
           label=sprintf ("%.1f", exp(apex)), vjust=2, hjust=0.75)

###p.kuznets.prot.0

### EKC (non-logs)
model.1.prot <- lm(data=oop, Value ~ GNI + I(GNI^2) )
summary(model.1.prot)
lmr.prot.1 <- summary(model.1.prot)$r.squared

lmc <- coef(model.1.prot);
apex <- lmc[2] / (-2* lmc[3])
apex.y <- lmc[1] + lmc[2] * apex + lmc[3] * apex^2

p.kuznets.prot.1 <- ggplot(oop, aes(GNI, Value) ) + 
  geom_point(color='red', alpha=.4) +
  stat_smooth(method = lm, se=F, formula = y ~ poly(x, 2)) +
  annotate("point", x = apex, y = apex.y, colour = "black", size=4) +
  annotate("text", x = apex, y = apex.y, colour = "black", size=4, 
           label=sprintf ("%.1f", apex), vjust=2)
##p.kuznets.prot.1

###########################################################################
### Łącznie p.c.
###########################################################################

oot.0 <- o2 %>% filter (Item == 'Grand Total' & Element =='FoodS (kcal/c/d)' ) %>%
  select (CodeM49, Area, Value, year=YearCode, GNI, class, fbs) %>%
  mutate (class = as.factor(class))
oot <- oot.0 %>% filter (year > 1995)

p1.oot <- oot %>%
  ggplot(aes(x=GNI, y=Value, color=as.factor(fbs))) +
  geom_point(alpha=.5) +
  ggtitle("Total intake (kcal/c/d)") +
  ylab("kcal") +
  labs(colour="") +
  theme(legend.position="top") +
  scale_color_manual(values = c("<2011" = "black", "2010>" = "orange"))
p1.oot

#
#p1.oot.l <- oot %>%
#  ggplot(aes(x=log(GNI), y=log(Value))) +
#  geom_point() +
#  ##facet_wrap(~class, scales = "free") +
#  ggtitle("Total intake (kcal/c/d)", subtitle="") 
#p1.oot.l
#
## 

## Engel
model.tot.0.engel <- lm(data=oot, formula = log(Value) ~ log(GNI))

summary(model.tot.0.engel)
lmr.total.0 <- summary(model.tot.0.engel)$r.squared

lmc_total.0 <- coef(model.tot.0.engel);
f_total <- function(x) lmc_total.0[1] + x * lmc_total.0[2]

## EKC (logs)
model.tot.ekc <- lm(data=oot, log(Value) ~ log(GNI) + I(log(GNI)^2) )
summary(model.tot.ekc)
lmr.total.1 <- summary(model.tot.ekc)$r.squared

aic2 <- AIC(model.tot.0.engel, model.tot.ekc) ## Engel is better
model.1.engel_aic <- aic2["model.tot.0.engel","AIC"]
model.1.ekc_aic <- aic2["model.tot.ekc","AIC"]

lmc <- coef(model.tot.ekc);
apex <- ( lmc[2] / (-2 * lmc[3]) )
apex.y <- lmc[1] + lmc[2] * apex + lmc[3] * apex^2

p.kuznets.tot.0 <- ggplot(oot, aes(log(GNI), log(Value)) ) + 
  geom_point(color='red', alpha=.4) +
  ylab("log(Total intake)") +
  stat_smooth(method = lm, se=F, formula = y ~ poly(x, 2)) +
  stat_function(fun=f_total, colour="black", size=1, linetype="dashed") +
  annotate("point", x = apex, y = apex.y, colour = "black", size=4) +
  annotate("text", x = apex, y = apex.y, colour = "black", size=4, 
           label=sprintf ("%.1f ", exp(apex)), vjust=2, hjust=0.75)
##p.kuznets.tot.0

## EKC (linear model)
model.tot.ekc.lin <- lm(data=oot, Value ~ GNI + I(GNI^2) )
summary(model.tot.ekc.lin)
lmr.total.2 <- summary(model.tot.ekc.lin)$r.squared

lmc <- coef(model.tot.ekc.lin);
apex <- lmc[2] / (-2* lmc[3])
apex.y <- lmc[1] + lmc[2] * apex + lmc[3] * apex^2

p.kuznets.tot.1 <- ggplot(oot, aes(GNI, Value) ) +
  geom_point(color='red', alpha=.4) +
  ##geom_point(alpha=.4) +
  ylab("Total intake") +
  stat_smooth(method = lm, se=F, formula = y ~ poly(x, 2)) +
  annotate("point", x = apex, y = apex.y, colour = "black", size=4) +
  annotate("text", x = apex, y = apex.y, colour = "black", size=4, 
           label=sprintf ("%.1f ", apex), vjust=2)

##p.kuznets.tot.1

## Łączny wykres

p.ekc.engel.4 <- ggarrange(p.kuznets.meat.0, p.kuznets.meat.1,
          p.kuznets.tot.0, p.kuznets.tot.1,
          ncol = 2, nrow = 2)
p.ekc.engel.4 
ggsave(p.ekc.engel.4, file='ekc_engel_4.png')

p.meat.total.xy <- ggarrange(p1.oom, p1.oot, ncol=2)
ggsave(p.meat.total.xy, file='meat_total_cons_xy.png')


######################################################################
# Co poniżej nic nie wnosi
######################################################################
## ## ##
## 9 największych konsumentów
## ## ##
xbig.names <- c('156', '840', '076', '643', '484', '392', '276', '356', '704')
oom.x <- oom %>%  filter (CodeM49 %in% xbig.names )


model.0.engel <- lm(data=oom.x, log(Meat) ~ log(GNI)  )
summary(model.0.engel)
car::outlierTest(model.0.engel)
oom[1853,]

## EKC
model.0.ekc <- lm(data=oom.x, log(Meat) ~ log(GNI) + I(log(GNI)^2) )
summary(model.0.ekc)

anova(model.0.ekc)

#
## wykresy
#
#p1.oom.l.x <- oom.xbig %>%
#  ggplot(aes(x=log(GNI), y=log(Meat))) +
#  ##ggplot(aes(x=GNI, y=Meat)) +
#  geom_point() +
#  ##geom_line() +
#  geom_smooth(method="loess") +
#  facet_wrap(~Area, scales = "fixed", ncol=2) +
#  #facet_wrap(~Area, scales = "free", ncol=2) +
#  ##facet_wrap(~CodeM49, scales = "free", ncol=2) +
#  ggtitle("Meat consumption (kcal/c/d)", subtitle="")
#  ##stat_function(fun=f, colour="red", size=2)
#p1.oom.l.x



