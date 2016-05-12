library(simcf)

square <- function(x){
  x <- x^2
}

simbetas <- function(nbmodel, nsim=10000) {
  ## Takes estimated model and (optionally) number of simulations;
  ## returns matrix of simulated betas drawn from multivariate
  ## normal distribution
  mvrnorm(nsim, coef(nbmodel), vcov(nbmodel))
}

bc.df <- read.csv("data/bc.df.csv",header = T)
bc.df<- bc.df%>%
  mutate(Wknd=as.factor(Wknd),holiday=as.factor(holiday), 
         uw=as.factor(uw))
covariates <- bc.df %>%
  mutate(Wknd=as.numeric(Wknd),holiday=as.numeric(holiday), 
         uw=as.numeric(uw),season = as.numeric(season))

#---------------model fit-------
#---------1 weather factor-----------

# #non-linear temperaturemax
modelsqrt1 <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability  +Wknd+ daylight + uw
mod_sqrt1 <- lm(modelsqrt1, data=bc.df)

#non-linear precipprobalility
modelsqrt2 <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability +ppp +Wknd+ daylight + uw
mod_sqrt2 <- lm(modelsqrt2, data=bc.df)

#simulation
mod1_sb <- simbetas(mod_sqrt1)             # Simulate our betas
model <- modelsqrt1


## cf1- effect of max temperature
mod1_cf1 <- cfMake2(model, bc.df, temperatureMax=29:90)

mod1_res1 <- linearsimev(mod1_cf1, mod1_sb) # Run the sim
mod1_res1 <- lapply(mod1_res1,square)
mod1_tidy1 <- fortify.longsim(mod1_res1, mod1_cf1)


m1c1 <- ggplot(mod1_tidy1, aes(x=temperatureMax, y=pe, ymin=lower,
                               ymax=upper)) +
  geom_line() + geom_ribbon(alpha=0.3) + 
  xlab("Daily Max Temp") + ylab("Bicycles") + theme_bw()

ggsave(file="temperture.pdf", path="WIPThesis/figures/sim", width=7, height=4.5, units=("in"))


## cf2 - Effect of max precipitation

mod1_cf2 <- cfMake2(model, bc.df, precipProbability=seq(0, 1, by=.01))
mod1_cf2$x$ppp <-pmax(mod1_cf2$x$precipProbability-0.72,0)

mod1_res2 <- linearsimev(mod1_cf2, mod1_sb) # Run the sim
mod1_res2 <- lapply(mod1_res2,square)

mod1_tidy2 <- fortify.longsim(mod1_res2, mod1_cf2)

m1c2 <- ggplot(mod1_tidy2, aes(x=precipProbability, y=pe, ymin=lower,
                               ymax=upper)) +
  geom_line() + geom_ribbon(alpha=0.3)+
  xlab("Daily PrecipProbability (inches)") + ylab("Bicycles") + theme_bw()

ggsave(file="precip.pdf", path="WIPThesis/figures/sim", width=7, height=4.5, units=("in"))


# add catagorical variable: weather icon
modelicon <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax  + holiday +
  precipProbability  + Wknd+ daylight+ icon +uw
model2 <- lm(modelicon, data=bc.df)

#---------2 tempero factor-----------

#----------change binary variable: weekday vs weekend to day of week --------
#Wknd

modelknd <- lm(modelsqrt1, data=covariates)

baseline1 <- c(59.3,1.03 , 0.288,1,12.1,0.627)     
baseline0 <- c(59.3,1.03 , 0.288,0,12.1,0.627)     

# Set as necessary
xhypo1 <- matrix(baseline1, nrow=2, ncol=6, byrow= TRUE)  # Set ncol to # of x's
xhypo0 <- matrix(baseline0, nrow=2, ncol=6, byrow= TRUE)  # Set ncol to # of x's

xhypo <- as.data.frame(rbind(xhypo1,xhypo0))
names(xhypo) <- c("temperatureMax","holiday","precipProbability","Wknd","daylight","uw")                                          # Set by user

xhypo <- cfMake2(modelsqrt1, covariates,Wknd = c(1,0))


sim <- predict(modelknd, newdata = xhypo$x, interval = "prediction")

sim <- apply(sim, 2,square)
sim <- as.data.frame(cbind(sim,
                           # xhypo["holiday"],
                           xhypo$x["Wknd"]))
sim$Wknd <- as.factor(sim$Wknd)
levels(sim$Wknd)<- c("Weekday", "Weekend")


limits <- aes(ymax = upr, ymin=lwr)
ggplot(sim, aes(x=factor(Wknd), y=fit)) +
  geom_point() + geom_errorbar(limits,width=0.2) +
  xlab("") + ylab("Bicycles") + theme_bw()

ggsave(file="Wknd.pdf", path="WIPThesis/figures/sim", width=7, height=4.5, units=("in"))


# dow
bc.df <- cbind(bc.df, dcast(data=bc.df, X ~ dow, length)[,-1])

modeldow <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax  + holiday +
  precipProbability  +
  Sat + Mon + Tues + Wed + Thurs + Fri+ 
  daylight +uw
model4<- lm(modeldow, data=bc.df)

#simulation
mod1_sb <- simbetas(model4)             # Simulate our betas

nonrefdays <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
mod1_cf3 <- cfMake2(modeldow, bc.df, Mon = c(0,1), Tues = c(0,1),
                    Wed = c(0,1), Thurs = c(0,1), Fri = c(0,1),
                    Sat = c(0,1))
mod1_cf3 <- mutex_dummy(mod1_cf3, nonrefdays)

mod1_res3 <- linearsimev(mod1_cf3, mod1_sb) # Run the sim
mod1_res3 <- lapply(mod1_res3,square)

## Need to convert dummy vars to long format manually.
mod1_tidy3 <- fortify.longsim(mod1_res3, mod1_cf3)
mod1_tidy3$Sun <- apply(subset(mod1_tidy3, select=nonrefdays), 1, sum) == 0
mod1_tidy3 <-
  melt(mod1_tidy3,
       id.vars = setdiff(names(mod1_tidy3), c(nonrefdays, "Sun")),
       variable.name = "Day")
mod1_tidy3 <- subset(mod1_tidy3, value==1)
mod1_tidy3$Day <- factor(mod1_tidy3$Day, levels=c("Sun", nonrefdays))

m1c3 <- ggplot(mod1_tidy3, aes(x=Day, y=pe, ymin=lower,
                               ymax=upper)) +
  geom_point() + geom_errorbar(width=0.2) +
  xlab("Day of the week") + ylab("Bicycles") + theme_bw()

ggsave(file="dow.pdf", path="WIPThesis/figures/sim", width=7, height=4.5, units=("in"))


# ----------daylight vs season----------

## cf4 - Effect of season (measured continuously via daylight hours)
mod1_cf4 <- cfMake2(modelsqrt1, bc.df2, daylight=seq(8, 16, by=0.01),
                    uw=c(TRUE,FALSE))
mod1_res4 <- linearsimev(mod1_cf4, simbetas(mod_sqrt1)) # Run the sim
mod1_res4 <- lapply(mod1_res4,square)

mod1_tidy4 <- fortify.longsim(mod1_res4, mod1_cf4)

levels(as.factor(mod1_tidy4$uw))
mod1_tidy4$uw <-factor(mod1_tidy4$uw)
levels(mod1_tidy4$uw) <- c("Not in Session","In Session")
m1c4 <- 
  ggplot(mod1_tidy4, aes(x=daylight, y=pe, ymin=lower,
                         ymax=upper, fill=uw)) +
  geom_line() + geom_ribbon(alpha=0.3) + 
  scale_fill_discrete(breaks=c("In Session","Not in Session"))+
  xlab("Daylight (hours)") + ylab("Bicycles") + theme_bw()

ggsave(file="daylight.pdf", path="WIPThesis/figures/sim", width=7.5, height=4.5, units=("in"))

# ## cf4 - Effect of season (measured via seanson)
modelseason <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax  + holiday +
  precipProbability  + Wknd+ season +uw
model5 <- lm(modelseason, data=bc.df)

xhypos <- cfMake2(modelseason, bc.df,season = c("spring", "summer", "fall","winter"),
                    uw="TRUE",holiday = "FALSE",Wknd = "FALSE")

sims <- predict(model5, newdata = xhypos$x, interval = "prediction", level = 0.1)

sims <- as.data.frame(cbind(apply(sims, 2,square),xhypo$x))

ggplot(sims, aes(x=season, y=fit)) + 
  # facet_wrap(~Wknd)+
  geom_point() + geom_errorbar(limits,width=0.2) +
  xlab("Season") + ylab("Bicycles") + theme_bw()
ggsave(file="season.pdf", path="WIPThesis/figures/sim", width=7, height=4.5, units=("in"))

# factor(season,levels = c("spring", "summer", "fall","winter")
#-----------------holiday factors----------
#holiday

xhypoh <- cfMake2(modelsqrt1, bc.df,holiday = c("TRUE","FALSE"),
                  uw="TRUE",Wknd = "FALSE")
simh <- predict(mod_sqrt1, newdata = xhypoh$x, interval = "prediction", level = 0.1)

simh <- apply(simh, 2,square)
simh <- as.data.frame(cbind(simh,xhypoh$x))

ggplot(simh, aes(x=holiday, y=fit)) +
  # facet_grid(season~uw)+
  geom_point() + geom_errorbar(limits,width=0.2) +
  xlab("Holiday") + ylab("Bicycles") + theme_bw()
ggsave(file="holiday.pdf", path="WIPThesis/figures/sim", width=7, height=4.5, units=("in"))


#------------multi effect---------

xhypom <- cfMake2(modelseason, bc.df, 
                  season = c("spring", "summer", "fall","winter"),
                  uw="TRUE",holiday = c("TRUE","FALSE"), Wknd = c("TRUE","FALSE"))

simm <- predict(model5, newdata = xhypom$x, interval = "prediction", level = 0.1)

simm <- as.data.frame(cbind(apply(simm, 2,square),xhypom$x))
levels(simm$Wknd)<- c("Weekend", "Weekday")

ggplot(simm, aes(x=season, y=fit, colour = holiday)) + 
  facet_wrap(~Wknd)+
  geom_point() + geom_errorbar(limits,width=0.2)+ 
  ylab("Bicycles")+theme_bw()

ggsave(file="multi.pdf", path="WIPThesis/figures/sim", width=10, height=4, units=("in"))

# -----------3 explore interaction term --------------

xhypointer <- cfMake2(modelWkndinter, bc.df, precipProbability=seq(0, 1, by=.01)
,Wknd = c("TRUE","FALSE"),
                  uw="TRUE",holiday = "FALSE")
simin <- predict(model6, newdata = xhypointer$x, interval = "prediction", level = 0.1)

simin <- apply(simin, 2,square)
simin <- as.data.frame(cbind(simin,xhypointer$x))

levels(simin$Wknd)<- c("Weekend", "Weekday")

ggplot(simin, aes(x=precipProbability, y=fit, ymin=lwr,
                       ymax=upr,fill= Wknd)) +
  geom_line() + geom_ribbon(alpha=0.3) +
  xlab("Preciptation Probability") + ylab("Bicycles")+
 theme_bw()+scale_y_continuous(breaks=seq(600, 3600, 300))+
  scale_fill_discrete(name="",
                      breaks=c("Weekday", "Weekend"))
#   annotate("text", x = 2.5, xend = 4, y = 15, yend = 25,
#            colour = "blue")

ggsave(file="precip*Wkn.pdf", path="WIPThesis/figures/sim", width=7.5, height=4.5, units=("in"))


##---------- cf5 - General trend over time---------
bc.df2 <- bc.df %>%
  mutate(Wknd=as.logical(Wknd),holiday=as.logical(holiday), 
         uw=as.logical(uw))

modelx <- sqrt(count) ~ I(temperatureMax^2) +temperatureMax + holiday +
  precipProbability  +Wknd+ daylight + uw+ X
model7 <- lm(modelx, data=bc.df)

mod1_cf5 <- cfMake2(modelx, bc.df2, X=1:1157) # factor has to be logic using bc.df2
mod1_res5 <- linearsimev(mod1_cf5, simbetas(model7)) # Run the sim
mod1_res5 <- lapply(mod1_res5,square)

mod1_tidy5 <- fortify.longsim(mod1_res5, mod1_cf5)

m1c5 <- ggplot(mod1_tidy5, aes(x=X, y=pe, ymin=lower,
                         ymax=upper)) +
  geom_line() + geom_ribbon(alpha=0.3) +
  xlab("Day of study period(index of time)") + ylab("Bicycles")+ 
  theme_bw()+scale_y_continuous(breaks=seq(2300, 2700, 50))

ggsave(file="trend.pdf", path="WIPThesis/figures/sim", width=7, height=4.5, units=("in"))

