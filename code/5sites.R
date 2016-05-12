# multiple site


#------------data process-------
# library(tidyr)
# library(reshape2)
# # setwd("~/Google Drive/WeiweiThesis")
# bc.df <- read.csv("data/bc.df.csv",header = T)
# othersites <- read.csv("data/4sites.csv",header = T)
# names(othersites)<- c("Date","BGT","elliott","I90","Spokanebrdg")
# othersites <- othersites%>%
#   separate(Date, c("Date", "x"),sep =" ")%>%
#   mutate(Date=as.Date(Date,"%m/%d/%y"))
# 
# bc.df <- bc.df %>%
#   mutate(Date=as.Date(Date))
# bc.5sites <- inner_join(bc.df,othersites,by="Date")
# bc.5sites <-bc.5sites%>%
#   mutate(total = count + BGT +elliott +I90 +Spokanebrdg)
# write.csv(bc.5sites,"data/bc.5sites.csv")
# 
# bc.long <- melt(bc.5sites,id.vars=c("Date"),measure.vars=c("count","BGT","elliott","I90","Spokanebrdg"),
#                 variable.name="location",
#                 value.name="bc.count")
# 
# bc.long <- left_join(bc.long,bc.df,by="Date")
# 
# write.csv(bc.long,"data/bc.long.csv")
# 
#--------------------------
bc.5sites <- read.csv("data/bc.5sites.csv",header = T)
# bc.5sites<- bc.5sites%>%
#   mutate(Wknd=as.factor(Wknd),holiday=as.factor(holiday), 
#          uw=as.factor(uw))
bc.5sites$season <-factor(bc.5sites$season,levels = c("spring", "summer", "fall","winter"))
# 
# for(level in unique(bc.5sites$season)){
#   bc.5sites[paste(level, sep = "_")] <- ifelse(bc.5sites$season == level, 1, 0)
# }
#-------------distribution-----------
pdf("WIPThesis/figures/5sites/distribution5.pdf",width=15,height=4)
par(mfrow=c(1,5))
hist(bc.5sites$count,main = "Fremont Bridge",xlab = "Cycling Count") #histgram of y
hist(bc.5sites$BGT,main ="Burke-Gilman Trail",xlab = "Cycling Count") #histgram of y
hist(bc.5sites$elliott, main = "Elliott Bay",xlab = "Cycling Count") #histgram of y
hist(bc.5sites$I90,main = "I-90 Bridge",xlab = "Cycling Count") #histgram of y
hist(bc.5sites$Spokanebrdg,main = "Seattle Bridge",xlab = "Cycling Count") #histgram of y
dev.off()

#-------------compare transformation -----------

modellm <- count ~  I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 
mod_lm <- lm(modellm, data=bc.5sites)

modellog <- log(count) ~  I(temperatureMax^2) +temperatureMax+ holiday +
  precipProbability  + Wknd + daylight + uw 
mod_log <- lm(modellog, data=bc.5sites)

# square root y
modelsqrt <- sqrt(count) ~ I(temperatureMax^2)+temperatureMax+holiday +
  precipProbability  +Wknd+ daylight + uw 
mod_sqrt <- lm(modelsqrt, data=bc.5sites)

#possion model
mod_pos <- glm(modellm, data=bc.5sites, family=poisson()) 

pdf("WIPThesis/figures/5sites/resid_count.pdf",width=18,height=18)
par(mfrow=c(4,4))
plot(mod_lm,main = "no-transformation")
plot(mod_log,main = "log(y)")
plot(mod_sqrt,main = "sqrt(y)")
plot(mod_pos,main = "poisson")
dev.off()
summary(mod_pos)

#count sqrt posson
#BGT poisson log
#i90 poisson
#ellliott not sure
#seattle bridge square root
# 5 sites poisson

#------------fit 5 models in 5 locations--------

modelfb <- count ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 
modeli90 <- I90 ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 
modelbgt <- BGT ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 
modelell <- elliott ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw 
modelsb <- Spokanebrdg ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw


mod_fb <- glm(modelfb, data=bc.5sites, family=poisson()) 
mod_bgt <- glm(modelbgt, data=bc.5sites, family=poisson()) 
mod_i90 <- glm(modeli90, data=bc.5sites, family=poisson()) 
mod_ell <- glm(modelell, data=bc.5sites, family=poisson()) 
mod_sb <- glm(modelsb, data=bc.5sites, family=poisson()) 

result <- cbind(data.frame(coef(mod_fb)),coef(mod_bgt),coef(mod_i90),coef(mod_ell),coef(mod_sb))

names(result) <-c("fremontb","BGT","I90","elliott","seattleb")
round(result,digits = 4)

library(texreg)
texreg(list(mod_fb,mod_sb,mod_ell,mod_i90,mod_bgt),
       caption="Location Comparison",dcolumn=FALSE,caption.above = T,
       custom.coef.names = c("const.","TempMaxSq","TempMax","Holiday","PrecipProb","Weekend",
                             "UW","Daylight"),
       custom.model.names =c("Fremont Bridge","Seattle Bridge","Elliott",
                              "I-90","BGT"))

# stargazer(model0,model1,model2,title="Results",align=TRUE,
#           dep.var.labels="log(Bike Counts)", 
#           covariate.labels=c("TempMaxSq","Holiday","PrecipProb",
#                              "Weekend", "daylight","spring","summer","winter",
#                              "cloudy","fog","partly-cloudy-day","partly-cloudy-night",
#                              "rain","wind","UW","const."),
#           omit.stat = c("f"),
#           no.space = T)
#---------combine 5 sites long-type ------------

bc.long <- read.csv("data/bc.long.csv",header = T)
hist(bc.long$bc.count,breaks= 40) #histgram of y
month <-summarise(group_by(bc.5sites,month),
                  monthly = mean(count),sd= sd(count),
                  monthly = mean(),sd= sd(count),
                  monthly = mean(count),sd= sd(count),
                  monthly = mean(count),sd= sd(count),
                  monthly = mean(count),sd= sd(count))
month$month_index<- row_number(month$month)


ggplot(month,aes(x=month_index, y= monthly))+
  geom_point(size = 0.7)+theme_bw()+geom_line()+
  geom_errorbar(aes(ymin = monthly-sd,ymax = monthly + sd),width =.1)+
  scale_colour_brewer(palette = "Set1")

levels(bc.long$location)<- c("BGT","Fremont Bridge","Elliott", "I90","Seattle Bridge")
ggplot(bc.long,aes(x=X, y= bc.count,colour=location))+
  geom_line(size = 0.3,alpha=0.7)+theme_bw()+
  scale_colour_brewer(palette = "Set1")

ggsave(file="5location_time.pdf", path="WIPThesis/figures/5sites", width=12, height=4, units=("in"))

# 
# bc.5sites$Date <- as.POSIXct(bc.5sites$Date)
# bc.5sites$month <- strftime(bc.5sites$Date, "%Y%m")
# 


modellong <- bc.count ~  I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + daylight + uw +location+X

mod_long <- glm(modellong, data=bc.long, family=poisson()) 

xhypo <- cfMake2(modellong, bc.long,X=1:700,
                 Wknd = FALSE,uw=TRUE,holiday = FALSE,
                 location=c("count","BGT","elliott","I90","Spokanebrdg"))

yhat <-predict(mod_long,newdata = xhypo$x,type = "response")

p_trend2 <- as.data.frame(cbind(yhat,xhypo$x))


ggplot(p_trend2, aes(x=X, y=yhat)) +
  geom_line(size =0.5) +
  xlab("TimeIndex") + ylab("Bicycles") + theme_bw()+
  facet_grid(location~.,scales = "free")
  # scale_fill_brewer(palette = "Set1")

ggsave(file="Sim_trend3.pdf", path="WIPThesis/figures/5sites", width=5, height=5, units=("in"))

#trendlong
xhypo <- cfMake2(modellong, bc.long,X=1:700,
                 location=c("count","BGT","elliott","I90","Spokanebrdg"))
yhat <- loglinsimev(xhypo, simbetas(mod_long)) # Run the sim
p_trendlong <- fortify.longsim(yhat, xhypo)

ggplot(p_trendlong, aes(x=X, y=pe,ymin=lower,
                        ymax=upper,fill = location)) +
  geom_line(aes(colour = location),size =0.5) + geom_ribbon(alpha=0.7) +
  xlab("TimeIndex") + ylab("Bicycles") + theme_bw()+
  facet_grid(location~.,scales = "free")+
  scale_fill_brewer(palette = "Set1")



#-----------simulations------
library(simcf)
simbetas <- function(nbmodel, nsim=10000) {
  mvrnorm(nsim, coef(nbmodel), vcov(nbmodel))
}

mod <- list(mod_fb,mod_sb,mod_ell,mod_i90,mod_bgt)
model <- list(modelfb, modelsb,modelell,modeli90, modelbgt)

set.seed(123456)                        # To reproduce results
beta <-lapply(mod, simbetas)

location <- c("Fremont Bridge","Seattle Bridge","Elliott",
              "I-90","BGT")

#daylight
p_daylight<- data.frame()
for (i in 1:5) {
  xhypo <- cfMake2(model[[i]], bc.5sites, daylight=seq(8, 16, by=0.01))
  yhat <- loglinsimev(xhypo, beta[[i]],ci=0.99) # Run the sim
  yhat <- fortify.longsim(yhat, xhypo)
  yhat$location <-rep(location[i],nrow(yhat))
  p_daylight <- rbind(p_daylight,yhat)
}

ggplot(p_daylight, aes(x=daylight, y=pe, ymin=lower,
                   ymax=upper,fill = location)) +
  geom_line(aes(colour = location),size =0.5) + geom_ribbon(alpha=0.7) +
  # geom_text(aes(label = location))+
  xlab("Daylight(hours)") + ylab("Bicycles") + theme_bw()+
  scale_fill_brewer(palette = "Set1")
ggsave(file="Sim_daylight.pdf", path="WIPThesis/figures/5sites", width=5.5, height=5, units=("in"))


#precip
p_precip<- data.frame()
for (i in 1:5) {
  xhypo <- cfMake2(model[[i]], bc.5sites, precipProbability=seq(0, 1, by=.01))
  yhat <- loglinsimev(xhypo, beta[[i]]) # Run the sim
  yhat <- fortify.longsim(yhat, xhypo)
  yhat$location <-rep(location[i],nrow(yhat))
  p_precip <- rbind(p_precip,yhat)
}

ggplot(p_precip, aes(x=precipProbability, y=pe, ymin=lower,
                     ymax=upper,fill = location)) +
  geom_line(aes(colour = location),size =0.5) + geom_ribbon(alpha=0.8) +
  # scale_y_log   10()+
  xlab("Preciptation Probability") + ylab("Bicycles") + theme_bw()+
  scale_fill_brewer(palette = "Set1")
ggsave(file="Sim_precip.pdf", path="WIPThesis/figures/5sites", width=5.5, height=5, units=("in"))

#temperture max

p_temp<- data.frame()
for (i in 1:5) {
  xhypo <- cfMake2(model[[i]], bc.5sites,temperatureMax=29:90)
  yhat <- loglinsimev(xhypo, beta[[i]],ci=0.99) # Run the sim
  yhat <- fortify.longsim(yhat, xhypo)
  yhat$location <-rep(location[i],nrow(yhat))
  p_temp <- rbind(p_temp,yhat)
}

ggplot(p_temp, aes(x=temperatureMax, y=pe, ymin=lower,
                     ymax=upper,fill = location)) +
  geom_line(aes(colour = location),size =0.5) + geom_ribbon(alpha=0.7) +
  # scale_y_log10()+
  xlab("TemperatureMax") + ylab("Bicycles") + theme_bw()+
  # guides(fill = F,colour = F)+
  scale_fill_brewer(palette = "Set1")

ggsave(file="Sim_temp.pdf", path="WIPThesis/figures/5sites", width=5.5, height=5, units=("in"))

#weekenK

p_Wknd<- data.frame()
for (i in 1:5) {
  xhypo <- cfMake2(model[[i]], bc.5sites,Wknd=c(T,F))
  yhat <- loglinsimev(xhypo, beta[[i]],ci=0.99) # Run the sim
  yhat <- fortify.longsim(yhat, xhypo)
  yhat$location <-rep(location[i],nrow(yhat))
  p_Wknd <- rbind(p_Wknd,yhat)
}
p_Wknd$Wknd<- as.factor(p_Wknd$Wknd)

levels(p_Wknd$Wknd)<- c("Weekday", "Weekend")
ggplot(p_Wknd, aes(x=Wknd, y=pe, ymin=lower,
                   ymax=upper,colour = location)) +
  geom_point(size =3) + geom_errorbar(width=0.2) +facet_grid(.~location)+
  scale_colour_brewer(palette = "Set1")+ylab("Bicycles") +theme_bw(base_size = 16)+
  xlab("")
# +scale_x_discrete("Wknd",lables =c("FALSE"="Weekday", "TRUE"="Weekend"))

ggsave(file="Sim_wknd.pdf", path="WIPThesis/figures/5sites", width=12, height=4, units=("in"))

#holiday
p_ho<- data.frame()
for (i in 1:5) {
  xhypo <- cfMake2(model[[i]], bc.5sites,holiday=c(T,F))
  yhat <- loglinsimev(xhypo, beta[[i]],ci=0.99) # Run the sim
  yhat <- fortify.longsim(yhat, xhypo)
  yhat$location <-rep(location[i],nrow(yhat))
  p_ho <- rbind(p_ho,yhat)
}

p_ho$holiday<- as.factor(p_ho$holiday)

ggplot(p_ho, aes(x=holiday, y=pe, ymin=lower,
                   ymax=upper,colour = location)) +
  geom_point(size =3) + geom_errorbar(width=0.2) +facet_grid(.~location)+
  scale_colour_brewer(palette = "Set1")+ylab("Bicycles") +theme_bw(base_size = 16)

ggsave(file="Sim_ho.pdf", path="WIPThesis/figures/5sites", width=12, height=4, units=("in"))


p_uw<- data.frame()
for (i in 1:5) {
  xhypo <- cfMake2(model[[i]], bc.5sites,uw=c(T,F))
  yhat <- loglinsimev(xhypo, beta[[i]],ci=0.99) # Run the sim
  yhat <- fortify.longsim(yhat, xhypo)
  yhat$location <-rep(location[i],nrow(yhat))
  p_uw <- rbind(p_uw,yhat)
}

p_uw$uw<- as.factor(p_uw$uw)
levels(p_uw$uw)<- c("Not in Session","In Session")

ggplot(p_uw, aes(x=uw, y=pe, ymin=lower,
                   ymax=upper,colour = location)) +
  geom_point(size =3) + geom_errorbar(width=0.2) +
  facet_grid(.~location)+scale_colour_brewer(palette = "Set1")+
  xlab("UW") + ylab("Bicycles") +theme_bw(base_size = 16)
ggsave(file="Sim_UW.pdf", path="WIPThesis/figures/5sites", width=12, height=4, units=("in"))


#tend x(not right due to missing data)

p_trend<- data.frame()
for (i in 1:5) {
  xhypo <- cfMake2(model[[i]], bc.5sites,X=1:730)
  yhat <- loglinsimev(xhypo, beta[[i]],ci=0.99) # Run the sim
  yhat <- fortify.longsim(yhat, xhypo)
  yhat$location <-rep(location[i],nrow(yhat))
  p_trend <- rbind(p_trend,yhat)
}

ggplot(p_trend, aes(x=X, y=pe, ymin=lower,
                   ymax=upper,fill = location)) +
  geom_line(aes(colour = location),size =0.5) + geom_ribbon(alpha=0.7) +
  xlab("TimeIndex") + ylab("Bicycles") + theme_bw()+
  scale_fill_brewer(palette = "Set1")

ggsave(file="Sim_trend.pdf", path="WIPThesis/figures/5sites", width=4, height=8, units=("in"))


#season

modelfb <- count ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd  + uw +season
modeli90 <- I90 ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + uw +season
modelbgt <- BGT ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + uw +season
modelell <- elliott ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd + uw  +season
modelsb <- Spokanebrdg ~ I(temperatureMax^2) + temperatureMax + holiday +
  precipProbability+ Wknd  + uw +season

p_season<- data.frame()
for (i in 1:5) {
  xhypo <- cfMake2(model[[i]], bc.5sites, season = c("spring", "summer", "fall","winter"),
                   uw=T,holiday = F,Wknd = F)
  # yhat <- loglinsimev(xhypo, beta[[i]],ci=0.99) # Run the sim
  yhat <- predict(mod[[i]], newdata = xhypo$x, type = "response")
  yhat <- as.data.frame(cbind(yhat,xhypo$x[,-1]))
  yhat$season<- as.character(yhat$season)
  yhat$location <-(rep(location[i],nrow(yhat)))
  p_season <- rbind(p_season,yhat)
}

p_season$season <-factor(p_season$season,levels = c("spring", "summer", "fall","winter"))

ggplot(p_season, aes(x=season, y=yhat,colour = location)) +
  geom_point(size =3)+
  facet_grid(.~location)+ scale_colour_brewer(palette = "Set1")+
  xlab("Season") + ylab("Bicycles")+ theme_bw(base_size = 12)
ggsave(file="Sim_season.pdf", path="WIPThesis/figures/5sites", width=12, height=4, units=("in"))

