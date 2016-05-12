setwd("~/Google Drive/WeiweiThesis")

#function
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(size = 0.5,alpha=0.2) + 
    geom_smooth(fill="red", color="red",span=1, ...) 
  p
}

bcraw.df <- read.csv("data/weatherbike3yr.csv",stringsAsFactors=F)
# bcraw.df <- read.csv("data/weatherbike2yr.csv",stringsAsFactors=F)

##------------------ data processing--------
bcraw.df$dow <- as.factor(as.character(wday(
  bcraw.df$Date, label=TRUE, abbr=TRUE)))         # Day of Week

bcraw.df$daylight <- bcraw.df$sunsetTime - bcraw.df$sunriseTime # Secs of daylight
bcraw.df$daylighth <- bcraw.df$daylight / 60 / 60

## Days of the week, recoded as dummy vars
bcraw.df <- cbind(bcraw.df, dcast(data=bcraw.df, X ~ dow, length)[,-1])
bcraw.df$Wknd <- (bcraw.df$dow == "Sat" | bcraw.df$dow == "Sun")
bcraw.df$TTh <- (bcraw.df$dow == "Tues" | bcraw.df$dow == "Wed" | bcraw.df$dow == "Thurs")

## Beyond heret of holidays (as observed by UW). Manually scraped from:
## http://www.washington.edu/admin/hr/holidays/holidays.html and
## http://www.washington.edu/admin/hr/holidays/holidays-archive.html
bcraw.df$holiday <- ymd(bcraw.df$Date) %in% 
  mdy(c("1/2/12", "1/16/12", "2/20/12", "5/28/12", "7/4/12", "9/3/12", "11/12/12", "11/22/12", "11/23/12", "12/25/12", 
        "1/1/13", "1/21/13", "2/18/13", "5/27/13", "7/4/13", "9/2/13", "11/11/13", "11/28/13", "11/29/13", "12/25/13", 
        "1/1/14", "1/20/14", "2/17/14", "5/26/14", "7/4/14", "9/1/14", "11/11/14", "11/27/14", "11/28/14", "12/25/14",
        "1/1/15", "1/19/15", "2/16/15", "5/25/15", "7/3/15", "9/7/15", "11/11/15", "11/26/15", "11/27/15", "12/25/15"))

## UW in regular (non-summer) session? Manually scraped from UW
## academic calendar "Dates of Instruction" archives. Includes finals
## week, though that may be unjustified.
uwstartdates <- mdy(c("1/3/2012", "3/26/2012", "9/24/2012", 
                      "1/7/2013", "4/1/2013", "9/25/2013", 
                      "1/6/2014", "3/31/2014", "9/24/2014",
                      "1/5/2015", "3/30/2015", "9/30/2015"))
uwstopdates <- mdy(c("3/16/2012", "6/8/2012", "12/14/2012", 
                     "3/22/2013", "6/14/2013", "12/13/2013", 
                     "3/21/2014", "6/13/2014", "12/12/2014",
                     "3/20/2015", "6/12/2015", "12/18/2015"))
uwdur <- uwstopdates - uwstartdates
uwinst <- unlist(mapply(function(s,d) as.character(s + ddays(1:d)),
                        uwstartdates, uwdur))
bcraw.df$uw <- ymd(bcraw.df$Date) %in% ymd(uwinst)


##season 
library(zoo)
yq <- as.yearqtr(as.yearmon(bcraw.df$Date, "%Y-%m-%d") + 1/12)
bcraw.df$season <- factor(format(yq, "%q"), levels = 1:4, 
                          labels = c("winter", "spring", "summer", "fall"))


bc.df <- bcraw.df %>%
  mutate(count = daily.count)%>%
  mutate(precipscale = as.factor(ifelse(precipProbability<0.7,0,1)))%>%
  mutate(temperaturescale = ifelse(temperatureMax<40,"low",
                                   ifelse(temperatureMax<75,"median","high")),
         temperature = (temperatureMax+temperatureMin)/2)%>%
  mutate(dow = as.factor(dow),Wknd = as.factor(Wknd),holiday = as.factor(holiday))



write.csv(bc.df,"data/bc.df.csv")