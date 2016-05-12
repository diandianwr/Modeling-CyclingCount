library(gridExtra)
library(ggplot2)
library(reshape2)
library(dplyr)
setwd("~/Google Drive/WeiweiThesis")
bc.df <- read.csv("data/bc.df.csv",header = T)


#prefered model specification 
modelsqrt1 <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability  +dow+ daylight + uw
mod_sqrt1 <- lm(modelsqrt1, data=bc.df)

square <- function(x){
  x <- x^2
}

#-------------------prediction and cross validate------------

#prediction 


bc.df.train <- bc.df[1:792,]
bc.df.test <- bc.df[793:1157,]

modelsqrt <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability  +dow+ daylight + uw

modellog <- log(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability  +dow+ daylight + uw

modellm <- count ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability  +dow+ daylight + uw

mod_lm_train <- lm(modellm,data=bc.df.train)
pred_lm_count <- predict(mod_lm_train,newdata=bc.df.test)
msepred <- sqrt(mean((pred_lm_count-bc.df.test$count)^2))

mod_log_train <- lm(modellog, data=bc.df.train)
pred_log_count <- predict(mod_log_train,newdata=bc.df.test)
msepred[2] <- sqrt(mean((exp(pred_log_count)-bc.df.test$count)^2))

mod_sqrt_train <- lm(modelsqrt,data=bc.df.train)
pred_sqrt_count <- predict(mod_sqrt_train,newdata=bc.df.test)
msepred[3] <- sqrt(mean(((pred_sqrt_count)^2-bc.df.test$count)^2))


mod_pos_train <- glm(modellm, data=bc.df.train, family=poisson()) 
pred_pos_count <- predict(mod_pos_train,newdata=bc.df.test,type="response")
msepred[4] <- sqrt(mean((pred_pos_count-bc.df.test$count)^2))

pred4 <- round(msepred,digits = 2)
names(pred4) <- c("standard linear model","Exponential model:log(y)","Quadratic model:sqrt(y)","poisson model")
write.csv(pred4,"WIPThesis/figures/prediction/pred4mse.csv")

#----------prediction plot-------------

ggplot(data.frame(actual=bc.df.test$count, predicted=(pred_sqrt_count)^2),
            aes(x=actual, y=predicted)) +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_point() + theme_bw()+ ylim(0,6000)

ggsave("WIPThesis/figures/prediction/actualvspred.pdf",width=5,height=5)

#acutual predict point


simin <- predict(mod_sqrt_train,newdata=bc.df.test,interval = "prediction")

simin <- apply(simin, 2,square)
simin <- as.data.frame(cbind(as.data.frame(simin),bc.df.test$X,bc.df.test$count))
colnames(simin)[c(1,4,5)]<- c("predict","TimeIndex","actual")
simin$id <- seq.int(nrow(simin))   #add id 

ggplot(simin, aes(x=id, y=predict, ymin=lwr,
                  ymax=upr)) +
  geom_line(size = 0.3,alpha =0.7) + geom_ribbon(alpha=0.5,fill = "grey")  +
  xlab("Time Index") + ylab("Bicycles")+
  geom_point(aes(x=id, y=actual),alpha= 0.5,size= 0.8,colour = "blue")+
  scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1'))+
  theme_bw()+  scale_x_continuous(breaks=seq(0, 365,50))
  
ggsave(file="predbytime_point_arima.pdf", path="WIPThesis/figures/prediction", width=15, height=5, units=("in"))

#acutual predict line

simin2 <- melt(simin,id.vars=c("id"),measure.vars=c("actual","predict"),
     variable.name="compare",value.name = "count")
simin3 <- left_join(simin2,simin,by ="id")

ggplot(simin3, aes(x=id, y=count)) +
  geom_line(aes(colour = compare)) + 
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.2) +
  xlab("Time Index") + ylab("Bicycles of 2015")+theme_bw()+
  scale_x_continuous(breaks=seq(0, 365,50))
#   scale_color_discrete(name="Compare",
#                       breaks=c("Actual", "Forcast"))
ggsave(file="predbytime.pdf", path="WIPThesis/figures/prediction", width=15, height=5, units=("in"))
  

#forcast month
bc.df$Date <- as.POSIXct(bc.df$Date)
bc.df$month <- strftime(bc.df$Date, "%Y%m")

fit1314 <- predict(mod_sqrt_train,interval = "prediction")
pred15 <- predict(mod_sqrt_train,newdata=bc.df.test,interval = "prediction")
predm <- rbind(fit1314[-1,],pred15)  #start from 201211
predm  <- apply(predm, 2,square)
predm  <- as.data.frame(cbind(as.data.frame(predm),bc.df[-1,]$month,bc.df[-1,]$count))
colnames(predm)[c(1,4,5)]<- c("predict","month","actual")

predm<- predm %>%
  filter(month!="201211"&month!="201212")%>%
  group_by(month)%>%
  summarise(predict = sum(predict), lwr = sum(lwr),upr = sum(upr),actual = sum(actual))

predm$m <- seq.int(nrow(predm))

ggplot() +
  geom_line(data=predm[24:36,], aes(x=m, y=predict,colour = "red")) +
  geom_ribbon(data=predm[24:36,],aes(x=m,ymin=lwr,ymax=upr),alpha=0.2)+
  geom_line(data =predm[1:24,], aes(x=m, y=actual))+
  scale_x_continuous(breaks =seq(0,36,1))+
  xlab("Month Index(from Jan,2013 to Dec,2015 )") + ylab("monthly cycling count")+theme_bw()

ggsave(file="pred_month.pdf", path="WIPThesis/figures/prediction", width=15, height=5, units=("in"))

# fp1<-ggplot(data.frame(actual=bc.df.test$count, predicted=exp(pred_log_count)),
#             aes(x=actual, y=predicted)) +
#   geom_abline(intercept=0, slope=1, color="red") +
#   geom_point() + theme_bw()+labs(title = "log")+ ylim(0,7000)
# 
# fp2<-ggplot(data.frame(actual=bc.df.test$count, predicted=pred_lm_count),
#             aes(x=actual, y=predicted)) +
#   geom_abline(intercept=0, slope=1, color="red") +
#   geom_point() + theme_bw()+labs(title = "lm")+ ylim(0,7000)
# 
# fp4<-ggplot(data.frame(actual=bc.df.test$count, predicted=pred_pos_count),
#             aes(x=actual, y=predicted)) +
#   geom_abline(intercept=0, slope=1, color="red") +
#   geom_point() + theme_bw()+labs(title = "poisson")+ ylim(0,7000)
# 
# pdf("WIPThesis/figures/prediction/actualvspred.pdf",width=5,height=5)

# grid.arrange(fp1,fp2,fp3,fp4,ncol = 4, nrow= 1)
# dev.off()


#arima prediction

#--------------------------- time series---------------------------
#correlation test

# pacf(residuals(mod_gam),main="PACF of response residuals")
# acf(residuals(mod_gam),main="ACF of response residuals")

pdf("WIPThesis/figures/residual_acfpcf.pdf",width=8,height=10)
par(mfrow=c(1,3))
plot(residuals(mod_sqrt1),type="l",main="response residuals of the Quadratic model:sqrt(y) Model")
dev.off()

#stationary testing
kpss.test(resid(mod_gam))
adf.test(resid(mod_gam))

kpss.test(resid(mod_sqrt1))
adf.test(resid(mod_sqrt1))#stationary

auto.arima(resid(mod_sqrt1),stationary = T)#ARIMA(2,0,1)
# auto.arima(resid(mod_sqrt1)) #Use ARIMA(1,1,1)

# auto.arima(resid(mod_lm),stationary = T)#ARIMA(1,0,0)
# auto.arima(resid(mod_lm)) #ARIMA(4,1,5) 

# gam.log.fit=gamm(modelgam,data=bc.df,correlation=corARMA(p=1,q=2))

covari <- bc.df %>%
  select(temperatureMaxSq,temperatureMax,holiday,precipProbability,dow, uw, daylight) %>%
  mutate(dow=as.numeric(dow),holiday=as.numeric(holiday), uw=as.numeric(uw))

# arima.sqrt.model1 <- arima(sqrt(bc.df$count), order=c(2,0,1), xreg=covari)
arima.sqrt.model2 <- arima(sqrt(bc.df$count), order=c(1,0,1), xreg=covari)

# pdf("WIPThesis/figures/test.pdf",width=15,height=10)
# par(mfrow=c(2,2))
# acf(residuals(model4),main="ACF of response residuals")
# pacf(residuals(model4),main="PACF of response residuals")
# acf(resid(arima.sqrt.model1))
# pacf(resid(arima.sqrt.model1))
# dev.off()



stargazer(mod_sqrt1,arima.sqrt.model2,title="model0 vs timeseries(ARIMA) model",align=TRUE,
          dep.var.labels="sqrt(Bike Counts)", 
          # covariate.labels=c("Temp. Max Sq.","Temp. Low","Temp. Median","holiday","Precip. Prob.",
          #                    "Weekend","Prep. Low", "Mon","Sat","Sun","Thu","Tue","Wed",
          #                    "daylight","cloudy","fog","part","part","rain","wind",
          #                    "PrecipProb:Wknd","const."), 
          no.space = T,single.row = T)

#-------- model results----

#fitted vs actual
# pd0<-ggplot(data.frame(actual=bc.df$count, fitted=(fitted.values(mod_sqrt1))^2),
#             aes(x=actual, y=fitted)) +
#   geom_abline(intercept=0, slope=1, color="red") +
#   geom_point() + theme_bw() + labs(title = "sqrt")+
#   ylim(0,6000)
# 
# # pdts1<- ggplot(data.frame(actual=bc.df$count, fitted=fitted.values(arima.sqrt.model1)^2),
# #                aes(x=actual, y=fitted)) +
# #   geom_abline(intercept=0, slope=1, color="red") +
# #   geom_point() + theme_bw() + labs(title = "arima(2,0,1)_sqrt")+
# #   ylim(0,7000)
# 
# pdts2<- ggplot(data.frame(actual=bc.df$count, fitted=fitted.values(arima.sqrt.model2)^2),
#                aes(x=actual, y=fitted)) +
#   geom_abline(intercept=0, slope=1, color="red") +
#   geom_point() + theme_bw() + labs(title = "arima(1,0,1)_sqrt")+
#   ylim(0,7000)

msets <- sqrt(mean((fitted.values(mod_sqrt1)^2-bc.df$count)^2))
msets[2] <- sqrt(mean((fitted.values(arima.sqrt.model2)^2-bc.df$count)^2))
# msets[3] <- sqrt(mean((fitted.values(arima.sqrt.model1)^2-bc.df$count)^2))
# 
# pdf("WIPThesis/figures/prediction/actualvsfited_arima.pdf",width=15,height=5)
# grid.arrange(pd0,pdts2, ncol=3, nrow =1)
# dev.off()

msets <- round(msets,digits = 2)
names(msets) <- c("Quadratic model:sqrt(y)","arima(1,0,1)_sqrt")
write.csv(msets,"WIPThesis/figures/prediction/msets.csv")

covari <- bc.df %>%
  mutate(temperatureMaxSq=temperatureMax^2) %>%
  select(temperatureMaxSq,temperatureMax,holiday,precipProbability,Wknd, uw, daylight) %>%
  mutate(holiday=as.numeric(holiday), uw=as.numeric(uw),Wknd=as.numeric(Wknd))


#predict vs actual
arima.sqrt.train1 <- arima(sqrt(bc.df[1:792,]$count), order=c(1,0,1), xreg=covari[1:792,])
pred_arima_sqrt_count1 <- predict(arima.sqrt.train1,n.ahead = 365, newxreg=covari[793:1157,])


# arima.sqrt.train2 <- arima(sqrt(bc.df[1:792,]$count), order=c(1,1,1), xreg=covari[1:792,])
# pred_arima_sqrt_count2 <- predict(arima.sqrt.train2,n.ahead = 427, newxreg=covari[793:1157,])


ggplot(data.frame(actual=bc.df[793:1157,]$count, predicted=(pred_arima_sqrt_count1$pred)^2),
             aes(x=actual, y=predicted)) +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_point() + theme_bw() + labs(title = "arima(1,0,1)_sqrt")+ ylim(0,6000)

#prediction plot
simin<- as.data.frame(as.numeric(pred_arima_sqrt_count1$pred))
simin$lwr<- as.numeric(pred_arima_sqrt_count1$pred) - 2*as.numeric(pred_arima_sqrt_count1$se)
simin$upr<- as.numeric(pred_arima_sqrt_count1$pred) + 2*as.numeric(pred_arima_sqrt_count1$se)
#repeat prediction plot above

# fpts2<-ggplot(data.frame(actual=bc.df[793:1157,]$count, predicted=(pred_arima_sqrt_count2$pred)^2),
#              aes(x=actual, y=predicted)) +
#   geom_abline(intercept=0, slope=1, color="red") +
#   geom_point() + theme_bw() + labs(title = "arima(1,1,1)_sqrt")+ ylim(0,7000)

# pdf("WIPThesis/figures/prediction/actualvspred_arima.pdf",width=15,height=10)
# grid.arrange(fp3,fpts2,fpts1,ncol = 3, nrow= 1)
# dev.off()

# pdf("WIPThesis/figures/predict.pdf",width=6,height=4)
# plot(bc.df[793:1157,]$count,type = "l")
# lines(pred_count,col ="red")
# dev.off()

#-------------------trend------------------
pdf("WIPThesis/figures/prediction/trend.pdf",width=8,height=5)
plot(bc.df$X,resid(model7),type = "l")
abline(h=0,col=3,lwd=0.5)
trend <-lm(resid(mod_gam)~bc.df$X)
abline(trend, col="red")
dev.off()

#--------------------