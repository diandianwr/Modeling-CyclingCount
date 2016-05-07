---
fontsize: 10pt
geometry: margin=1in
output: pdf_document
---

```r
library(MASS)
library(knitr)
library(texreg)
setwd("~/Google Drive/WeiweiThesis")

bc.5sites <- read.csv("data/bc.5sites.csv",header = T)

modelfb <- count ~ temperatureMaxSq + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw
modeli90 <- I90 ~ temperatureMaxSq + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 
modelbgt <- BGT ~ temperatureMaxSq + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 
modelell <- elliott ~ temperatureMaxSq + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 
modelsb <- Spokanebrdg ~ temperatureMaxSq + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 


mod_fb <- glm(modelfb, data=bc.5sites, family=poisson()) 
mod_bgt <- glm(modelbgt, data=bc.5sites, family=poisson()) 
mod_i90 <- glm(modeli90, data=bc.5sites, family=poisson()) 
mod_ell <- glm(modelell, data=bc.5sites, family=poisson()) 
mod_sb <- glm(modelsb, data=bc.5sites, family=poisson()) 


bc.long <- read.csv("data/bc.long.csv",header = T)
modellong <- bc.count ~  temperatureMaxSq + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw +location

mod_long <- glm(modellong, data=bc.long, family=poisson())


result <- cbind(data.frame(coef(mod_fb)),coef(mod_bgt),coef(mod_i90),coef(mod_ell),coef(mod_sb),coef(mod_long)[c(1:8)])

names(result) <-c("fremontb","BGT","elliott","I90","seattleb","5sites")
round(result,digits = 4)
```

```
##                   fremontb     BGT elliott     I90 seattleb  5sites
## (Intercept)         5.4922  2.0884  2.2996  3.7013   3.8258  3.8664
## temperatureMaxSq   -0.0003 -0.0006 -0.0006 -0.0004  -0.0004 -0.0004
## temperatureMax      0.0551  0.0997  0.0870  0.0708   0.0612  0.0670
## holidayTRUE        -0.5525  0.3246  0.1363 -0.3070  -0.4639 -0.2658
## precipProbability  -0.4410 -0.8018 -0.8711 -0.6312  -0.5674 -0.5853
## WkndTRUE           -0.7488  0.3568  0.2994 -0.4278  -0.6201 -0.3247
## daylight            0.0362  0.0932  0.0800  0.0587   0.0519  0.0561
## uwTRUE              0.1191  0.0755  0.1228  0.1143   0.1241  0.1129
```

```r
# summary(mod_fb)
# summary(mod_sb)
# summary(mod_ell)
# summary(mod_i90)
# summary(mod_bgt)
# summary(mod_long)
```


```r
#2 model summary
texreg(list(mod_fb,mod_sb,mod_ell,mod_i90,mod_bgt),
       dcolumn=FALSE
       ,model.names=c("Fremont Bridge","Seattle Bridge","elliott","I90","BGT")
       )
```


\begin{table}
\begin{center}
\begin{tabular}{l c c c c c }
\hline
 & Model 1 & Model 2 & Model 3 & Model 4 & Model 5 \\
\hline
(Intercept)       & $5.49^{***}$  & $3.83^{***}$  & $3.70^{***}$  & $2.30^{***}$  & $2.09^{***}$  \\
                  & $(0.02)$      & $(0.04)$      & $(0.04)$      & $(0.05)$      & $(0.04)$      \\
temperatureMaxSq  & $-0.00^{***}$ & $-0.00^{***}$ & $-0.00^{***}$ & $-0.00^{***}$ & $-0.00^{***}$ \\
                  & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      \\
temperatureMax    & $0.06^{***}$  & $0.06^{***}$  & $0.07^{***}$  & $0.09^{***}$  & $0.10^{***}$  \\
                  & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      \\
holidayTRUE       & $-0.55^{***}$ & $-0.46^{***}$ & $-0.31^{***}$ & $0.14^{***}$  & $0.32^{***}$  \\
                  & $(0.01)$      & $(0.01)$      & $(0.01)$      & $(0.01)$      & $(0.01)$      \\
precipProbability & $-0.44^{***}$ & $-0.57^{***}$ & $-0.63^{***}$ & $-0.87^{***}$ & $-0.80^{***}$ \\
                  & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.01)$      & $(0.00)$      \\
WkndTRUE          & $-0.75^{***}$ & $-0.62^{***}$ & $-0.43^{***}$ & $0.30^{***}$  & $0.36^{***}$  \\
                  & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      \\
daylight          & $0.04^{***}$  & $0.05^{***}$  & $0.06^{***}$  & $0.08^{***}$  & $0.09^{***}$  \\
                  & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      \\
uwTRUE            & $0.12^{***}$  & $0.12^{***}$  & $0.11^{***}$  & $0.12^{***}$  & $0.08^{***}$  \\
                  & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      & $(0.00)$      \\
\hline
AIC               & 85717.93      & 29145.00      & 42591.48      & 33898.75      & 50548.60      \\
BIC               & 85754.67      & 29181.74      & 42627.88      & 33935.38      & 50585.09      \\
Log Likelihood    & -42850.96     & -14564.50     & -21287.74     & -16941.37     & -25266.30     \\
Deviance          & 78679.17      & 23026.06      & 36464.50      & 27994.75      & 44399.89      \\
Num. obs.         & 730           & 730           & 699           & 720           & 707           \\
\hline
\multicolumn{6}{l}{\scriptsize{$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$}}
\end{tabular}
\caption{Statistical models}
\label{table:coefficients}
\end{center}
\end{table}


