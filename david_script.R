#start off with composite plots on daily spread with mobility change


library(covid19.analytics)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)


CovidJHU<-covid19.data(case="ts-confirmed")
mobility<- read.csv("Global_Mobility_Report.csv")
worlddatan<-read.csv("worlddatan.csv")

Mobility5C <- mobility %>% filter((country_region == 'New Zealand' |
                                     country_region == 'United Kingdom' |
                                     country_region  == 'Spain' |
                                     country_region == 'Italy' |
                                     country_region == 'Sweden')& date<="2020-07-01")%>%
  filter(sub_region_1=="")%>% mutate(date= ymd(date))

mobilityAJ<-Mobility5C %>% filter(date>="2020-01-31" & date<="2020-07-01") %>%
  select(-c(3,4,5,6,7))


MobilityUK<-Mobility5C%>%filter((country_region == 'United Kingdom'))


#head(MobilityUK)


Covid5C <- CovidJHU %>% 
  pivot_longer(col = -c("Province.State","Country.Region","Lat", "Long"),
               names_to = "date", values_to = "cumulative_cases") %>% 
  # turn the Date column into actual Date type
  mutate(date = ymd(date)) %>% 
  # just get selected countries
  filter((Country.Region == 'New Zealand' |
            Country.Region == 'United Kingdom' |
            Country.Region == 'Spain' |
            Country.Region == 'Italy' |
            Country.Region == 'Sweden'|
            Country.Region == 'US')& Province.State=="") %>% 
  # Calculate the number of new cases per day  #THIS NEEDS CHANGED IT IS WRONG, MUST DIVIDE BY INDIVIDUAL POPULATIONS
  mutate(incident_cases = c(0, diff(cumulative_cases)))%>%
  mutate(cases_100000 = incident_cases/100000)%>%
  # only keep data before August, and remove row with no reported cases 
  filter(date<="2020-07-01", incident_cases > 0)

#colnames(Covid5C)

CovidUK<-Covid5C%>%filter((Country.Region == 'United Kingdom'))

#selecting maxium values by date

CovidAJ<-Covid5C %>% filter(date>="2020-01-31" & date<="2020-07-01")%>%
  rename(country_region=Country.Region)

#Joining mobility and covid data

CovMob<-left_join(CovidAJ,mobilityAJ, by=c("date", "country_region"))%>% group_by(country_region)

#Selecting only UK data
CovMobUK<-CovMob%>% filter((country_region == 'United Kingdom'))

# Set color palette  
cols = hcl(c(15, 15+180), 100, 65)
scl = 1000

#selecting UK data 
R <- ggplot()+geom_line(data=CovidUK, aes(x = date , y= cases_100000, colour = "cases_100000"))+
  geom_line(data=MobilityUK, aes(x = date , y=retail_and_recreation_percent_change_from_baseline/scl, 
                                 colour="retail"))+
  scale_y_continuous("Cases/1000000",sec.axis = sec_axis(~.*scl, name = "% change in Mobility"))+
  ggtitle("Retail mobility and cases/1000000 ") +
  theme_bw()+ 
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1])) +
  scale_colour_manual(values=cols) +
  labs(colour="")



P <- ggplot()+geom_line(data=CovidUK, aes(x = date , y= cases_100000, colour = "cases_100000"))+
  geom_line(data=MobilityUK, aes(x = date , y=parks_percent_change_from_baseline/scl, 
                                 colour="Park"))+
  scale_y_continuous("Cases/1000000",sec.axis = sec_axis(~.*scl, name = "% change in Mobility"))+
  ggtitle("Park mobility and cases/1000000 ") +
  theme_bw()+ 
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1])) +
  scale_colour_manual(values=cols) +
  labs(colour="")



G <- ggplot()+geom_line(data=CovidUK, aes(x = date , y= cases_100000, colour = "cases_100000"))+
  geom_line(data=MobilityUK, aes(x = date , y=grocery_and_pharmacy_percent_change_from_baseline/scl, 
                                 colour="Grocery"))+
  scale_y_continuous("Cases/1000000",sec.axis = sec_axis(~.*scl, name = "% change in Mobility"))+
  ggtitle("Grocery mobility and cases/1000000 ") +
  theme_bw()+ 
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1])) +
  scale_colour_manual(values=cols) +
  labs(colour="")

head(MobilityUK)
Q <- ggplot()+geom_line(data=CovidUK, aes(x = date , y= cases_100000, colour = "cases_100000"))+
  geom_line(data=MobilityUK, aes(x = date , y=transit_stations_percent_change_from_baseline/scl, 
                                 colour="Transit"))+
  scale_y_continuous("Cases/1000000",sec.axis = sec_axis(~.*scl, name = "% change in Mobility"))+
  ggtitle("Transit mobility and cases/1000000 ") +
  theme_bw()+ 
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1])) +
  scale_colour_manual(values=cols) +
  labs(colour="")

head(MobilityUK)

#workplaces_percent_change_from_baseline

W <- ggplot()+geom_line(data=CovidUK, aes(x = date , y= cases_100000, colour = "cases_100000"))+
  geom_line(data=MobilityUK, aes(x = date , y=workplaces_percent_change_from_baseline/scl, 
                                 colour="Workplace"))+
  scale_y_continuous("Cases/1000000",sec.axis = sec_axis(~.*scl, name = "% change in Mobility"))+
  ggtitle("Workplace mobility and cases/1000000 ") +
  theme_bw()+ 
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1])) +
  scale_colour_manual(values=cols) +
  labs(colour="")

#residential_percent_change_from_baseline

R2 <- ggplot()+geom_line(data=CovidUK, aes(x = date , y= cases_100000, colour = "cases_100000"))+
  geom_line(data=MobilityUK, aes(x = date , y=residential_percent_change_from_baseline/scl, 
                                 colour="Residential"))+
  scale_y_continuous("Cases/1000000",sec.axis = sec_axis(~.*scl, name = "% change in Mobility"))+
  ggtitle("Residential mobility and cases/1000000 ") +
  theme_bw()+ 
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1])) +
  scale_colour_manual(values=cols) +
  labs(colour="")

#composite plot

K<-ggarrange(R, R2, Q, P, G, W)
annotate_figure(K,top=text_grob("United Kingdom Composite Plots", color="red", face="bold", size=14))



#UK scatter plots for correlation
R3 <- ggplot(CovMobUK, aes(x = retail_and_recreation_percent_change_from_baseline, y = cases_100000)) +
  geom_point(color = "firebrick") +
  geom_smooth(method = "lm", se =FALSE)+
  labs(x = "Retail Mobility", y = "Cases/100000") +
  theme(axis.title.x = element_text(color = "sienna",
                                    size = 15, vjust = -0.35),
        axis.title.y = element_text(color = "orangered",
                                    size = 15, vjust = 0.35))+
  stat_cor(label.x = -50, label.y = 0.08) +
  stat_regline_equation(label.x = -50, label.y = 0.087)


#composite scatter plots for correlation for retails
R3all <- ggplot(CovMob, aes(x = retail_and_recreation_percent_change_from_baseline, y = cases_100000)) +
  geom_point(color = "firebrick") +
  geom_smooth(method = "lm", se =FALSE)+
  labs(x = "Retail Mobility", y = "Cases/100000") +
  theme(axis.title.x = element_text(color = "sienna",
                                    size = 15, vjust = -0.35),
        axis.title.y = element_text(color = "orangered",
                                    size = 15, vjust = 0.35))+
  stat_cor(label.x = -50, label.y = 0.08) +
  stat_regline_equation(label.x = -50, label.y = 0.087)+ 
  theme_bw()+
  facet_wrap(~country_region)

# composite scatter plots for correlations for parks
Pall <- ggplot(CovMob, aes(x = parks_percent_change_from_baseline, y = cases_100000)) +
  geom_point(color = "firebrick") +
  geom_smooth(method = "lm", se =FALSE)+
  labs(x = "Park Mobility", y = "Cases/100000") +
  theme(axis.title.x = element_text(color = "sienna",
                                    size = 15, vjust = -0.35),
        axis.title.y = element_text(color = "orangered",
                                    size = 15, vjust = 0.35))+
  stat_cor(label.x = -50, label.y = 0.08) +
  stat_regline_equation(label.x = -50, label.y = 0.087)+ facet_wrap(~country_region)


z1<- ggarrange(R3all,Pall)
