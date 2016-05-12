library(MASS)
library(dplyr)
library(ggplot2)
library(mgcv)
library(GGally)
library(reshape2)
library(stargazer)
library(forecast)
library(tseries)
library(gridExtra)


setwd("~/Google Drive/WeiweiThesis")
bc.df <- read.csv("data/bc.df.csv",header = T)


bc.df<- bc.df%>%
  mutate(Wknd=as.factor(Wknd),holiday=as.factor(holiday), 
         uw=as.factor(uw))
covariates <- bc.df %>%
  mutate(Wknd=as.numeric(Wknd),holiday=as.numeric(holiday), 
         uw=as.numeric(uw),season = as.numeric(season))


#function
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(size = 0.5,alpha=0.2) + 
    geom_smooth(fill="red", color="red",span=1, ...) 
  p
}

#------------------- select interested variable-------------

# # bc.df$temperatureMaxSq <- bc.df$temperatureMax^2

# bc.df$daylight <- bc.df$daylight / 60 / 60
# bc.df$rain <- ifelse(bc.df$icon == "rain",1,0)
# bc.df$ppp <- pmax(bc.df$precipProbability-0.72,0)
#------------------- exploratory analysis----------------------
##describe y 
plot(bc.df$count,type = "l")
hist(bc.df$count) #histgram of y

pdf("WIPThesis/figures/daylight.pdf", width = 7,height = 4)
plot(bc.df$daylight)
dev.off()
# pdf("figures/resid_time.pdf", width = 10,height = 10)
# ggplot(bc.df,aes(y = resid(mod_log), x = X))+geom_line(color="blue")
# dev.off()

##describe y ~ x
describ<- bc.df %>%
  select(count,precipscale,precipProbability,precipIntensity,precipIntensityMax,humidity,visibility,cloudCover,
         temperaturescale,temperatureMax,temperatureMin,temperature, dewPoint,windSpeed,daylight,
         icon,holiday,dow,Wknd,uw,season,X,Date)

#precip
pdf("WIPThesis/figures/matrix1.pdf", width = 10,height = 10)
g <- ggpairs(describ,columns =c(1,3:8), 
            lower = list(continuous = my_fn))
g+theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8,face="bold"))
dev.off()

###temperature
pdf("WIPThesis/figures/matrix2.pdf", width = 10,height = 10)
g <- ggpairs(describ,columns =c(1,10:15), 
            lower = list(continuous = my_fn))
g+theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8,face="bold"))
dev.off()

##catagorical variables
pdf("WIPThesis/figures/holiday_cat.pdf",width=5,height=5)
ggplot(describ, aes(x = holiday, y = count)) +geom_boxplot()
dev.off()

pdf("WIPThesis/figures/uw_cat.pdf",width=5,height=5)
ggplot(describ, aes(x = uw, y = count)) +geom_boxplot()
dev.off()

pdf("WIPThesis/figures/wknd_cat.pdf",width=5,height=5)
ggplot(describ, aes(x = Wknd, y = count)) +geom_boxplot()
dev.off()

pdf("WIPThesis/figures/dow_cat.pdf",width=5,height=5)
ggplot(describ, aes(x = dow, y = count)) +geom_boxplot()
dev.off()

pdf("WIPThesis/figures/icon_cat.pdf",width=5,height=5)
ggplot(describ, aes(x = icon, y = count)) +geom_boxplot()
dev.off()

pdf("WIPThesis/figures/season_cat.pdf",width=5,height=5)
ggplot(na.omit(describ), aes(x = factor(season,levels = c("spring", "summer", "fall","winter")), y = count)) +geom_boxplot()
dev.off()


#------------------- initial model fit-----------------------
#1 y transformation
#no transformation
#max tempmax/probability r square 
modellm <- count ~  temperatureMaxSq + holiday +
  precipProbability+ Wknd + daylight + uw
mod_lm <- lm(modellm, data=bc.df)
# par(mfrow=c(1,4))
# plot(mod_lm)
# summary(mod_lm)
#log y
modellog <- log(count) ~  temperatureMaxSq + holiday +
  precipProbability  + Wknd + daylight + uw 
mod_log <- lm(modellog, data=bc.df)

# square root y
modelsqrt <- sqrt(count) ~ temperatureMaxSq+holiday +
  precipProbability  +Wknd+ daylight + uw
mod_sqrt <- lm(modelsqrt, data=bc.df)

#possion model
mod_pos <- glm(modellm, data=bc.df, family=poisson()) 

# #  log-linked GLM gaussian
# mod_glmlog <- glm(modellm,data=bc.df,family = gaussian(link = "log"))

#negtive binominal
mod_nb <- glm.nb(modellm, link=log, data=bc.df)

#------------- commpare y transformation residual plot------
pdf("WIPThesis/figures/ytrans.pdf",width=18,height=18)
par(mfrow=c(4,4))
plot(mod_lm,main = "no-transformation")
plot(mod_log,main = "log(y)")
plot(mod_sqrt,main = "sqrt(y)")
plot(mod_pos,main = "poisson")
dev.off()

pdf("WIPThesis/figures/boxcox.pdf",width=4,height=4)
boxcox(modellm)
dev.off()

##----------------- Compare the r-squared/mse/avsfited for different transformations---------------
# mod_lm 
rsquare <- summary(mod_lm)$r.squared
mse <- sqrt(mean((mod_lm$fitted.values-bc.df$count)^2))
# mod_log
y.hat.log <- exp(mod_log$fitted.values)
mss.log <- sum((y.hat.log-bc.df$count)^2)
tss.log <- sum((bc.df$count-mean(bc.df$count))^2)
rsquare[2] <- 1-mss.log/tss.log
mse[2] <- sqrt(mean((y.hat.log-bc.df$count)^2))

# mod_sqrt
y.hat.sqrt <- (mod_sqrt$fitted.values)^2
mss.sqrt <- sum((y.hat.sqrt-bc.df$count)^2)
tss.sqrt <- sum((bc.df$count-mean(bc.df$count))^2)
rsquare[3] <-1- mss.sqrt/tss.sqrt
mse[3] <- sqrt(mean((y.hat.sqrt-bc.df$count)^2))

# mod_pos
y.hat.pos <- mod_pos$fitted.values
mss.pos <- sum((y.hat.pos-bc.df$count)^2)
tss.pos <- sum((bc.df$count-mean(bc.df$count))^2)
rsquare[4] <- 1-mss.pos/tss.pos
mse[4] <- sqrt(mean((y.hat.pos-bc.df$count)^2))

ycompare <- round(as.data.frame(rbind(rsquare,mse)),digits = 2)
colnames(ycompare) <- c("standard linear model","Exponential model:log(y)","Quadratic model:sqrt(y)","poisson model")
write.csv(ycompare,"WIPThesis/figures/ycomparetable.csv")

#compare actual vs fitted
f1<-ggplot(data.frame(actual=bc.df$count, fitted=fitted.values(mod_lm)),
             aes(x=actual, y=fitted)) +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_point() + theme_bw() + labs(title = "standard linear model")+
  ylim(0,7000)+ ylab("fitted value")+xlab("actual observation")

f2 <-ggplot(data.frame(actual=bc.df$count, fitted=exp(fitted.values(mod_log))),
            aes(x=actual, y=fitted)) +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_point() + theme_bw()+labs(title = "log(y)")+
  ylim(0,7000)+ ylab("fitted value")+xlab("actual observation")

f3<-ggplot(data.frame(actual=bc.df$count, fitted=(fitted.values(mod_sqrt))^2),
           aes(x=actual, y=fitted)) +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_point() + theme_bw() + labs(title = "sqrt(y)")+
  ylim(0,7000)+ ylab("fitted value")+xlab("actual observation")
f4<-ggplot(data.frame(actual=bc.df$count, fitted=fitted.values(mod_pos)),
           aes(x=actual, y=fitted)) +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_point() + theme_bw() + labs(title = "possion model")+
  ylim(0,7000)+ ylab("fitted value")+xlab("actual observation")v

pdf("WIPThesis/figures/prediction/actualvfit4.pdf",width=15,height=4)
grid.arrange(f1,f2,f3,f4,ncol = 4, nrow= 1)
dev.off()


#----------- 2 nonparametric smooth splines to fit the model.Explore non- linear relationship between x ~y------------

#gam() is used to fit generalized additive models
# modelgam <- log(count) ~ s(temperatureMin) +temperatureMax + holiday +
#   s(precipProbability)  + Wknd+ daylight +uw

modelgam <- sqrt(count) ~ s(temperatureMax)  + holiday +
  s(precipProbability)  + Wknd+ daylight +uw
mod_gam <- gam(modelgam, data = bc.df)

pdf("WIPThesis/figures/gam.pdf",width=12,height=7)
par(mfrow=c(1,2))
plot(mod_gam)
dev.off()

# pdf("WIPThesis/figures/gam_resid.pdf",width=12,height=7)
# par(mfrow=c(1,2))
# plot(fitted(mod_gam),resid(mod_gam))
# l <- loess.smooth(fitted(mod_gam),resid(mod_gam), span = 2/3, degree = 1)
# lines(l,col = 'red',lwd=2)
# qqnorm(resid(mod_gam))
# qqline(resid(mod_gam,col=2))
# dev.off()


#---------3 ------------------model specification-----------------------


# 1 non-linear temperaturemax
modelsqrt1 <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability  +Wknd+ daylight + uw
mod_sqrt1 <- lm(modelsqrt1, data=bc.df)

#2  non-linear precipprobalility
modelsqrt2 <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability +ppp +Wknd+ daylight + uw
mod_sqrt2 <- lm(modelsqrt2, data=bc.df)

# 3 catagorical variable: weather icon
modelicon <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax  + holiday +
  precipProbability  + Wknd+ daylight+ icon +uw
model3 <- lm(modelicon, data=bc.df)

#4 change binary variable: weekday vs weekend to day of week

modeldow <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax  + holiday +
  precipProbability  +
  Sat + Mon + Tues + Wed + Thurs + Fri+ 
  daylight +uw
model4<- lm(modeldow, data=bc.df)

#5  change daylight to season
modelseason <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax  + holiday +
  precipProbability  + Wknd+ season +uw
bc.df$season <- factor(bc.df$season, levels = c("spring", "summer", "fall","winter"))
bc.df$season <- relevel(bc.df$season,ref = "winter")
model5 <- lm(modelseason, data=bc.df)

#6  explore interaction term
modelWkndinter <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax  + holiday +
  precipProbability*Wknd + daylight +uw 
model6 <- lm(modelWkndinter, data=bc.df)

##7  General trend over time
modelx <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability  +Wknd+ daylight + uw+ X
model7 <- lm(modelx, data=bc.df)

#find interaction terms using summary model result see if it signi
model <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax+
  precipProbability*uw+Wknd+holiday+ X
summary(lm(model, data=bc.df))
#-----Compare model results side by side using stargazer-------
stargazer(model0,model1,model2,title="Results",align=TRUE,
          dep.var.labels="log(Bike Counts)", 
          covariate.labels=c("TempMaxSq","Holiday","PrecipProb",
                             "Weekend", "daylight","spring","summer","winter",
                             "cloudy","fog","partly-cloudy-day","partly-cloudy-night",
                             "rain","wind","UW","const."),
          omit.stat = c("f"),
          no.space = T,single.row = T)


stargazer(model0,model3, model4, title="Results",align=TRUE,
          dep.var.labels="log(Bike Counts)", 
          covariate.labels=c("TempMaxSq","Holiday","PrecipProb",
                             "Weekend","Mon","Sat","Sun","Thu","Tue","Wed",
                             "daylight","uw","PrecipProb:Wknd","const."),
          omit.stat=c("f"),
          no.space = T,single.row = T)

# calculate AIC and BIC
model.list <- list(mod_sqrt1,mod_sqrt2,model3,model4,model5,model6,model7)
model.aic <- lapply(model.list,AIC)
model.bic <- lapply(model.list,BIC)




