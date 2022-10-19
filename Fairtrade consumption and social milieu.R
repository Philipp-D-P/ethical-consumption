## Convert R Markdown to R script
## knitr::purl("Fairtrade consumption and social milieu.Rmd")


## ------------------------------------------------------------------------------
rm(list = ls())


## ----setup, include=F----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
Packages <- c("tidyverse",     ## 1.3.0
            "naniar",        ## 0.5.0
            "haven",         ## 2.2.0
            "survey",        ## 3.37
            "car",           ## 3.0-6
            "gmodels",       ## 2.18.1
            "psych",         ## 1.9.12.31
            "mfx",           ## 1.2-2
            "epiDisplay",    ## 3.5.0.1
            "effects",       ## 4.1-4
            "DescTools",     ## 0.99.37
            "margins"        ## 0.3.23
            )

lapply(Packages, require, character.only = T)


## ----eval=F, message=FALSE-----------------------------------------------------
## Packages <- c("tidyverse",     ## 1.3.0
##             "naniar",        ## 0.5.0
##             "haven",         ## 2.2.0
##             "survey",        ## 3.37
##             "car",           ## 3.0-6
##             "gmodels",       ## 2.18.1
##             "psych",         ## 1.9.12.31
##             "mfx",           ## 1.2-2
##             "epiDisplay",    ## 3.5.0.1
##             "effects",       ## 4.1-4
##             "DescTools",     ## 0.99.37
##             "margins"        ## 0.3.23
##             )
## 
## lapply(Packages, require, character.only = T)


## ------------------------------------------------------------------------------
Data <- read_sav("Umweltbewusstsein_DE_2016_Welle1_v1-0-0.sav")


## ------------------------------------------------------------------------------
Data <- Data %>%
  dplyr::select(f624_3, ## If possible, only buy products that have been produced under fair working conditions.
                f821,   ## gender (m = 1 / w = 2) 
                f8210,  ## Total monthly net income of a household (in 11 income groups).
                nf822,  ## Age of respondents in years.
                f823,   ## Educational attainment (highest school degree or university degree). 
                QCL_1,  ## Social milieus (six social segments, 17 items for identification).
                weight  ## Weighting variable (weighting by region, gender, age and education).
                )


## ------------------------------------------------------------------------------
Data <- Data %>%
  mutate(f624_3 = na_if(f624_3, 5), ## "don't know"
         f8210  = na_if(f8210, 12), ## "not specified"
         f823   = na_if(f823, 1),   ## "still student"
         f823   = na_if(f823, 7),   ## "Other school diploma"
         f823   = na_if(f823, 8)    ## "not specified"
         )


## ------------------------------------------------------------------------------
Data <- Data %>%
  mutate(buy = car::recode(Data$f624_3,
                            "1:2 = 1; 3:4 = 0; else = NA", as.factor = T))


## ------------------------------------------------------------------------------
Data$female <- dummy.code(Data$f821, group = 2)


## ------------------------------------------------------------------------------
Data <- Data %>%
  mutate(milieu = car::recode(Data$QCL_1,
                            "1 = 1;
                             2 = 2;
                             3 = 3;
                             4 = 4; 
                             5 = 5;
                             6 = 6", 
                             as.factor = T))


## ------------------------------------------------------------------------------
round(prop.table(table(Data$milieu))*100, digits = 1)


## ------------------------------------------------------------------------------
contrasts(Data$milieu)


## ------------------------------------------------------------------------------
Data$milieu <- relevel (Data$milieu, ref = "4")


## ------------------------------------------------------------------------------
contrasts(Data$milieu)


## ------------------------------------------------------------------------------
Data$education <- factor(Data$f823)


## ------------------------------------------------------------------------------
Data$age <- Data$nf822


## ------------------------------------------------------------------------------
Data$income <- Data$f8210


## ------------------------------------------------------------------------------
summary(Data)


## ---- fig.width=10,fig.height=6------------------------------------------------
variables <-data.frame(Data$female, Data$education, Data$income, Data$age, Data$milieu, Data$buy)
gg_miss_var(variables)


## ------------------------------------------------------------------------------
prop.table(table(Data$buy))*100


## ----include=FALSE-------------------------------------------------------------
## Regression vor Berechnung der vif-Werte, damit Berechnung möglich wird
m6.w <- glm(buy ~ female + education + income + age + milieu,
            family = binomial,
            data = Data,
            weights = weight)
summary(m6.w)


## ------------------------------------------------------------------------------
CrossTable(Data$milieu, Data$buy, prop.r=T, prop.c=F, prop.t=F, prop.chisq=F, digits = 2) 


## ------------------------------------------------------------------------------
vif(m6.w)


## ------------------------------------------------------------------------------
m.null <- glm(buy ~ 1,
              family = binomial,
              data = Data)
summary(m.null)


## ----warning=FALSE-------------------------------------------------------------
m1.w <- glm(buy ~ female,
            family = binomial,
            data = Data,
            weights = weight)
summary(m1.w)


## ----warning=FALSE-------------------------------------------------------------
m2.w <- glm(buy ~ education,
            family = binomial,
            data = Data,
            weights = weight)
summary(m2.w)


## ----warning=FALSE-------------------------------------------------------------
m3.w <- glm(buy ~ income,
            family = binomial,
            data = Data,
            weights = weight)
summary(m3.w)


## ----warning=FALSE-------------------------------------------------------------
m4.w <- glm(buy ~ age,
            family = binomial,
            data = Data,
            weights = weight)
summary(m4.w)


## ----warning=FALSE-------------------------------------------------------------
m5.w <- glm(buy ~ milieu,
            family = binomial,
            data = Data,
            weights = weight)
summary(m5.w)


## ----warning=FALSE-------------------------------------------------------------
m6.w <- glm(buy ~ female + education + income + age + milieu,
            family = binomial,
            data = Data,
            weights = weight)
summary(m6.w)


## ------------------------------------------------------------------------------
logLik(m.null)
logLik(m5.w) 
logLik(m6.w)


## ------------------------------------------------------------------------------
lrtest(m.null, m5.w)


## ----warning=FALSE-------------------------------------------------------------
AIC(m.null, m5.w, m6.w)


## ----warning=FALSE-------------------------------------------------------------
BIC(m.null, m5.w, m6.w)


## ----echo=TRUE, warning=FALSE--------------------------------------------------
round((PseudoR2(m1.w, "McKelveyZavoina")*100), digits = 1)   ## gender
round((PseudoR2(m2.w, "McKelveyZavoina")*100), digits = 1)   ## education
round((PseudoR2(m3.w, "McKelveyZavoina")*100), digits = 2)   ## income
round((PseudoR2(m4.w, "McKelveyZavoina")*100), digits = 1)   ## age
round((PseudoR2(m5.w, "McKelveyZavoina")*100), digits = 1)   ## milieu


## ----warning=FALSE-------------------------------------------------------------
round((PseudoR2(m6.w, "McKelveyZavoina")*100), digits = 1) ## Overall model 


## ------------------------------------------------------------------------------
round(11.9/21.2*100, digits = 1)


## ----warning=FALSE-------------------------------------------------------------
logistic.display(m5.w)
logistic.display(m6.w)


## ------------------------------------------------------------------------------
m5.w.marginal <- margins(m5.w)
summary(m5.w.marginal)

m6.w.marginal <- margins(m6.w) 
summary(m6.w.marginal)


## ---- fig.width=10,fig.height=6------------------------------------------------
m5.w.marginal.sum <- summary(m5.w.marginal)

ggplot(data = m5.w.marginal.sum) +
  geom_point(aes(factor, AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


## ---- fig.width=10,fig.height=6------------------------------------------------
m6.w.marginal.sum <- summary(m6.w.marginal)

ggplot(data = m6.w.marginal.sum) +
  geom_point(aes(factor, AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

