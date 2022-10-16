## ---------------------------------------------------------------------------------------------------------------
## Ethisches Konsumverhalten am Beispiel von Fairtrade

## Philipp Penner 
## Matr.-Nr.: 4322128
## Datum: 31.03.2020

## Datensatz: Umweltbewusstsein in Deutschland 2016 (Welle 1) - Version 1.0.0 (12.04.2017)
## Datenzugang: https://doi.org/10.4232/1.12764 (Gesis-Archiv - SPSS Datensatz - SAV Format)
## ---------------------------------------------------------------------------------------------------------------

## Workspace leeren
rm(list = ls())

## ---------------------------------------------------------------------------------------------------------------

## Pakete laden              Version
Pakete <- c("tidyverse",     ## 1.3.0
            "naniar",        ## 0.5.0
            "haven",         ## 2.2.0
            "survey",        ## 3.37
            "car",           ## 3.0-6
            "gmodels",       ## 2.18.1
            "psych",         ## 1.9.12.31
            "BaylorEdPsych", ## 0.5
            "mfx",           ## 1.2-2
            "epiDisplay",    ## 3.5.0.1
            "effects"        ## 4.1-4
            )

lapply(Pakete, require, character.only = T)

## ---------------------------------------------------------------------------------------------------------------

## Datenimport und -aufbereitung (Vorbereitung der Analyse)

## Datensatz einlesen
Daten <- read_sav("Daten/Umweltbewusstsein_DE_2016_Welle1_v1-0-0.sav")

## Variablenset erstellen (UB = Umweltbewusstseinsstudie)
UB <- Daten %>%
  dplyr::select(f17_9,  ## Beim Kauf achte ich auf die Nachhaltigkeit der Produkte (u.a. fairen Arbeitsbedingungen).
                f613_5, ## Fairen Handel zwischen reichen Ländern und Entwicklungsländern sicherstellen.
                f624_3, ## Ich kaufe möglichst nur Produkte, die unter fairen Arbeitsbedingungen hergestellt wurden.
                f821,   ## Geschlecht (m = 1 / w = 2) 
                f8210,  ## Monatliche Nettoeinkommen eines Haushaltes insgesamt (in 11 Einkommensgruppen).
                nf822,  ## Alter der Befragten in Jahren +Alterskategorien ebenfalls vorhanden).
                f823,   ## Bildungsabschluss (höchster Schulabschluss oder Hochschulabschluss). 
                QCL_1,  ## Soziale Milieus (sechs gesellschaftliche Segmente, 17 Items zur Identifikation)
                weight  ## Gewichtungsvariable (Gewichtung der Stichprobe nach Region, Geschlecht, Alter und Bildung)
                )

## Irrelevante Daten zur Übersichtlichkeit entfernen
rm(Daten)

## Fehlende Angaben bzw. nicht brauchbare Werte als "NA" definieren
UB <- UB %>%
  mutate(f17_9  = na_if(f17_9, 5),  ## "weiß nicht"
         f613_5 = na_if(f613_5, 5), ## "weiß nicht"
         f624_3 = na_if(f624_3, 5), ## "weiß nicht"
         f8210  = na_if(f8210, 12), ## "keine Angabe"
         f823   = na_if(f823, 1),   ## "noch Schüler/Schülerin"
         f823   = na_if(f823, 7),   ## "anderen Schulabschluss"
         f823   = na_if(f823, 8)    ## "keine Angabe"
         )

## Variablen für Analyse umkodieren / dichotomisieren

## Zielvariable Kaufwahrscheinlichkeit dichotomisieren als neue Variable "kauf"
## Umkodieren der Kategorien 1 und 2 zu 1 = Ja / Kategorien 3 und 4 zu 0 = Nein
UB <- UB %>%
  mutate(kauf = car::recode(UB$f624_3,
                            "1:2 = '1.ja'; 3:4 = '0.nein'; else = NA", as.factor = T))

summary(UB$kauf)   ## Variable "kauf" beinhaltet 79 fehlende Werte,
round(79/2030*100) ## was rund 4% der gesamten Stichprobe entspricht

## Geschlecht von m = 1 / w = 2 zu m = 0 / w = 1
## Neue Variable "weiblich" (weiblich = 1)
UB$weiblich <- dummy.code(UB$f821, group = 2)
prop.table(table(UB$weiblich))*100 ## 49% männlich / 51% weiblich


## Milieu als Faktorvariable definieren und damit automatisch eine Dummy-Variable erzeugen
## Neue Variable "milieu" (Milieu 1 automatisch als Referenzkategorie gesetzt)
## Bedeutet: Das erste Milieu geht mit 0 und die anderen Kategorien jeweils mit 1 in die Berechnung ein
## Zudem inhaltliche Label der sozialen Milieus zuordnen für die Übersichtlichkeit
UB <- UB %>%
  mutate(milieu = car::recode(UB$QCL_1,
                            "1 = '1.traditionell';
                             2 = '2.gehoben';
                             3 = '3.bürgerlich';
                             4 = '4.prekär'; 
                             5 = '5.kritisch-kreativ';
                             6 = '6.jung' ", 
                             as.factor = T))

## Wie ist die Variable "milieu" als Faktor genau aufgebaut. Wichtig für spätere Modelinterpretation
contrasts(UB$milieu)

## Referenzkategorie der Variable milieu ändern (Vorbereitung für logistische Regression)
UB$milieu <- relevel (UB$milieu, ref = "4.prekär")
## Struktur der Variable nach der Änderung der Referenzkategorie
## Viertes Milieu (Prekäre) als Referenz gesetzt und Wert 0 zugewiesen
contrasts(UB$milieu)

## Bildung als Faktorvariable definieren und damit automatisch eine Dummy-Variable erzeugen
## Neue Variable "milieu" ("ohne Abschluss" automatisch als Referenzkategorie gesetzt)
UB$bildung <- factor(UB$f823) 

## Einkommen als Faktorvariable definieren und damit automatisch eine Dummy-Variable erzeugen
## Neue Variable "einkommen" (unterste Einkommensgruppe automatisch als Referenzkategorie gesetzt)
UB$einkommen <- factor(UB$f8210) 
contrasts(UB$einkommen)

## ---------------------------------------------------------------------------------------------------------------

## Anwendungsvoraussetzungen der binär logitischen Regression prüfen

## 1. Keine extreme Ungleichverteilung der abhängigen Variable
prop.table(table(UB$kauf))*100 ## Ergebnis: 19% Nein / 81% Ja

## 2. Geringe Multikollinearität !nichtwichtig, da nur ein Prädiktor!


## 3. Ausreichend Fälle in den Kategorien der Prädiktorvariable in Verbindung mit Zielvariable
CrossTable(UB$milieu, UB$kauf, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F, digits = 2) 
## Ergebnis: Kategorie 4 als kleinste Gruppe mit 157 Fällen
## Empfohlene Mindestanzahl von 25 Fällen pro Kategorie in der Prädiktorvariable erfüllt 
## Kategorie 1 und 5 zeigen mit 33 und 15 sehr wenige Fälle der Ausprägung 0
## Die Ausprägung 1 ist dagegen in allen Kategorien deutlich besser besetzt

## ---------------------------------------------------------------------------------------------------------------

## Binär logistisches Regressionsmodell
## Abhängige Variable: Kauf von Fairtrade Ja (1) / Nein (0)
## Unabhängige Variable(n):


## Testmodelle
m1 <- lm(f624_3 ~ weiblich ,UB, weights = weight)
summary(m1)

m2 <- lm(f624_3 ~ f8210 ,UB, weights = weight)
summary(m2)

## Logistische Modelle
## Ausschluss von Fällen mit fehlenden Werten ("na.action=na.omit" als Standard gesetzt)

## Geschlecht
mlogit1 <- glm(kauf ~ weiblich, 
               family = binomial, 
               data = UB)
summary(mlogit1)

## Geschlecht gewichtet
mlogit1.w <- glm(kauf ~ weiblich, 
               family = binomial, 
               data = UB,
               weights = weight)
summary(mlogit1.w)


## Geschlecht und Milieu
mlogit2.w <- glm(kauf ~ weiblich + milieu, 
               family = binomial, 
               data = UB,
               weights = weight)
summary(mlogit2.w)

mlogit.bildung.w <- glm(kauf ~ bildung, 
                 family = binomial, 
                 data = UB,
                 weights = weight)
summary(mlogit.bildung.w)

mlogit.einkommen.w <- glm(kauf ~ einkommen, 
                        family = binomial, 
                        data = UB,
                        weights = weight)
summary(mlogit.einkommen.w)

mlogit.alter.w <- glm(kauf ~ nf822, 
                          family = binomial, 
                          data = UB,
                          weights = weight)
summary(mlogit.alter.w)

mlogit.alles.w <- glm(kauf ~ nf822 + einkommen + bildung + weiblich + milieu, 
                      family = binomial, 
                      data = UB,
                      weights = weight)
summary(mlogit.alles.w)


## ---------------------------------------------------------------------------------------------------------------
## Milieu

## Nullmodell - Milieu gewichtet
mlogit3.w.null <- glm(kauf ~ 1, 
                 family = binomial, 
                 data = UB,
                 weights = weight)
summary(mlogit3.w.null)

## Milieu ungewichtet
mlogit3.unw <- glm(kauf ~ milieu, 
                 family = binomial, 
                 data = UB)
summary(mlogit3.unw)

## Milieu gewichtet
mlogit3.w <- glm(kauf ~ milieu, 
                 family = binomial, 
                 data = UB,
                 weights = weight)
summary(mlogit3.w)

## Milieu gewichtet (quasibinomial)
mlogit3.w.q <- glm(kauf ~ milieu, 
                 family = quasibinomial, 
                 data = UB,
                 weights = weight)
summary(mlogit3.w.q)

## Odds Ratio
logistic.display(mlogit3.w)

## Modellvergleich und Modellgüte

## Log-Likelihood
logLik(mlogit3.w.null)   ## -948.0162 (df=1)
logLik(mlogit3.w)        ## -905.3995 (df=6)

## Likelihood-Ratio-Test
lrtest(mlogit3.w.null, mlogit3.w)

## AIC - Akaike Information Criterion
AIC(mlogit3.w.null, mlogit3.w)

## BIC - Bayesian Information Criterion 
BIC(mlogit3.w.null, mlogit3.w)

## Pseudo R2 (unter anderem McFadden)
PseudoR2(mlogit3.w)      ## McFadden 0.0463
PseudoR2(mlogit3.w.null) ## McFadden -2.22x10^-16
PseudoR2(mlogit1.w)
PseudoR2(mlogit2.w)
PseudoR2(mlogit.bildung.w)
PseudoR2(mlogit.einkommen.w)
PseudoR2(mlogit.alter.w)
PseudoR2(mlogit.alles.w)

## McKelvey mal ansehen!


## Vorhergesagte Wahrscheinlichkeiten
milieu.label <- data.frame(milieu=factor(c("1.traditionell","2.gehoben","3.bürgerlich",
                                           "4.prekär","5.kritisch-kreativ","6.jung")))
round(predict(mlogit3.w, milieu.label, type="response"), digits = 2)

## Graphische Analyse
plot(allEffects(mlogit3.w),
     main="Vorhergesagte Wahrscheinlichkeiten",
     xlab="Soziale Milieu",
     ylab="Kaufwahrscheinlichkeit")
 
## ---------------------------------------------------------------------------------------------------------------

## Bildung und Geschlecht gewichtet
mlogit4.w <- glm(kauf ~ weiblich + bildung, 
                 family = binomial, 
                 data = UB,
                 weights = weight)
summary(mlogit4.w)

## Einkommen kategorial bzz. Faktorvariable
mlogit5.w <- glm(kauf ~ einkommen, 
                 family = binomial, 
                 data = UB,
                 weights = weight)
summary(mlogit5.w)

## Einkommen als quasimetrisch
mlogit6.w <- glm(kauf ~ f8210, 
                 family = binomial, 
                 data = UB,
                 weights = weight)
summary(mlogit6.w)



## ---------------------------------------------------------------------------------------------------------------


## Spielereien

## Odds ratio berechnen
## paket mfx

odds_ratio <- logitor(kauf ~ milieu, data=UB) ## Achtung ungwichtete Daten!
odds_ratio




## Übersicht
hist(UB$f17_9)
hist(UB2$f613_5)
hist(UB2$f624_3)

## NA Analyse
gg_miss_var(UB)

## log grafik
fit = glm(kauf ~ f8210, data=UB, family=binomial)
newdat <- data.frame(f8210=seq(min(UB$f8210), max(UB$f8210),len=100))
newdat$kauf = predict(fit, newdata=newdat, type="response")
plot(kauf ~ f8210, data=UB, col="red4")
lines(kauf ~ f8210, newdat, col="green4", lwd=2)

ggplot(UB, aes(x=weiblich, y=kauf)) + 
  geom_point(shape=1, position=position_jitter(width=.05,height=.05)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

## Odds Ratio Experimente
require(MASS)
exp(cbind(coef(mlogit3.unw), confint(mlogit3.unw))) 
exp(cbind(coef(mlogit3.w), confint(mlogit3.w))) 



## Milieu gewichtet
mlogit3.w.test <- glm(kauf ~ milieu.test, 
                 family = binomial, 
                 data = UB,
                 weights = weight)
summary(mlogit3.w.test)



