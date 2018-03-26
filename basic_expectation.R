###########################
# Author: Rebecca Stubbs
# March 25, 2018
#

# Underlying quetsions: 
# "What is 'rescue weather'?"
# "Do hoiday weekends actually matter, statistically?"
# "Days of the week-- what are the worst ones?"

# City of Boulder, County of Boulder, Boulder ZIP, Nederland ZIP.
# Daily weather summaries 
# https://www.ncdc.noaa.gov/cdo-web/

rm(list=ls())
library(data.table)
library(lme4)
library(ggplot2)
library(MapSuite)
library(extrafont)

repo<-"/Users/stubbsrw/Documents/git_code/RMR_Predict/"

# Load and Clean Data
  missions<-list()
  for(y in 1998:2017){
  rmrg<-fread(paste0(repo,"rmr_data/rmrg_",y,".csv"))
  rmrg<-rmrg[,1:5] # keep only rows 1-5
  names(rmrg)<-c("MissionNumber","Type","DateTime","Synopsis","Resolution")
  rmrg<-rmrg[!(MissionNumber%in%c("\n",""))] # Elim rows with no mission number
  missions[[y]]<-rmrg
  }
  missions<-rbindlist(missions)
  rm(rmrg)
  

# Generate the days that missions were *not* observed
  missions[,date:=as.Date(DateTime)]
  missions[,N:=1]
  
  # First, generate all dates within the time window
  no_missions<-data.table(date=seq(as.Date("1998-01-01"), as.Date("2017-12-31"), by="days"))
  no_missions<-no_missions[!(date%in%missions$date),] # Exclude dates where missions happened
  no_missions[,N:=0] # Set mission count for these days to 0
  missions<-rbind(missions,no_missions,fill=T) # Combine data sets
  rm(no_missions) 
  
# Calculate the number of call-outs each day:
  # <-- TODO: Calculate N for certain hours of the day/ eg "After Work", "After Dark"
  mission_summary<-missions[,list(count=sum(N)),by=date]
  
# Calculate better date/time fields
  mission_summary[,weekday:=weekdays(date)]
  mission_summary[,month:=months(date)]
  mission_summary[,year:=year(date)]
  
# Fit Model:
  # Random effects on year, month, and weekday
  mod<-glmer(count ~ 1+ (1|year)+(1|month)+(1|weekday),
        family=poisson, data=mission_summary)
  
# Report out the Random Effect Values:
  re_values<-function(mod,dataset,cat_var){
    re_table<-data.table(lvl=levels(as.factor(dataset[[cat_var]])),
                         ranef(mod)[[cat_var]])
    setnames(re_table,"(Intercept)","Intercept_shift")
    re_table[,exp_intercept_shift:=exp(Intercept_shift)]
    }
 

  values<-rbind(re_values(mod,mission_summary,"weekday"),
        re_values(mod,mission_summary,"year"),
        re_values(mod,mission_summary,"month"))
  
  
# Predcit based on year, month, and day-of-week RE:
  predictions<-CJ(year=1998:2017,
              month=unique(mission_summary$month),
              weekday=unique(mission_summary$weekday))
  predictions[,month:=factor(month,
                            levels=c("January","February","March",
                                     "April","May","June","July",
                                     "August","September","October",
                                     "November","December"))]
  
  predictions[,preds:=predict(mod,predict_dataset)] # Add predicted values to the data set
  predictions[,exp_preds:=exp(preds)] # Predictions are in log-space, we need to exponentiate them
  
  # Categorize into weekend/weekday
  predictions[weekday%in%c("Saturday","Sunday"),weekday_type:="Week End"]
  predictions[!weekday%in%c("Saturday","Sunday"),weekday_type:="Work Week"]
  
# View expected values for each month, by workweek/weekend, for each year:

  p<-ggplot(predictions, aes(x=month, y=exp_preds,color=year,shape=weekday_type)) + geom_point() +
    scale_color_gradientn(colors=wpal("cool_blue_bright")) + theme_bw(base_size=14,base_family="CMU Sans Serif") +
    ggtitle("Expected Number of RMR Missions/Incidents Per Day", 
            subtitle="For Weekend and Weekdays, in each Month from 1998-2017") +
    ylab("Expected Number of Missions Per Day")+xlab("Month")+ scale_shape_manual(values=c(16,3))+
    guides(color=guide_colourbar(title="Year", title.position="top", barwidth=.3,barheight = 10,
                label=TRUE, ticks=FALSE),
           shape=guide_legend(title=""))+theme(legend.position="right",
                                               plot.title = element_text(color="red", size=20))
  
  # Save graphic 
  ggsave(paste0(repo,"basic_expectation.png"),p, width = 12, height = 8, units = "in")
  