# working with dates and times in R - lubridate and tidyquant packages
# analyzing Climate Change, including time series analysis

remove(list=ls())
library(tidyverse)  # contains lubridate
library(tidyquant)  # for calculating moving averages

# database: (remove hashtag)
# browseURL("https://docs.google.com/spreadsheets/d/1yKedemxKYMiBd8nwzZI5GI06enIZKRUvmbbLJk10uAI/edit?gid=304223944")

# read the daily weather data from the KNMI station Lauwersoog - 1 jan 1991 - 31 sep 2024
dat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRYSmvT7qFqBPa-XIxFIzIXpZOYlWpY-MqyMVVwh_Q1nN7pzxSGaPKlWRhPfCtomR59bkLuOgaUFRa1/pub?gid=2037414920&single=true&output=csv", show_col_types = FALSE) %>%
     mutate(date=lubridate::ymd(YYYYMMDD),   # make variable of type Date from string
            day=lubridate::day(date), 
            month=lubridate::month(date),
            year=lubridate::year(date),
            metyear_dec=ifelse(month==12,year+1,year),  # calculate meteorological year, which starts at 1 December of prevbious year
            metyear_nov=ifelse(month %in% c(11,12),year+1,year),  # winter for Hellman index, which starts at 1 November of prevbious year
            RH=ifelse(RH==-1,0,RH/10),     # RH = Daily precipitation amount (in 0.1 mm) (-1 for <0.05 mm) -> convert to mm/day
            TG=TG/10) %>% # TG = Daily mean temperature in (0.1 degrees Celsius) -> convert to 0C
     select(date,metyear_nov,metyear_dec,year,month,day,RH,TG)
head(dat,n=10)
tail(dat,n=10) # show the last 10 records of the tibble

# aggregate the dates per month
dat_month<-dat %>% 
  dplyr::group_by(year,metyear_nov,metyear_dec,month) %>%
  summarize(totrain_mo_mm=sum(RH,na=T), #monthly total rainfall in mm
            avgtemp_mo_oC=mean(TG,na.rm=T)) %>%             #monthly average temperature in oC
  mutate(mdate=ymd(paste(year,"-",month,"-","15",sep="")))  # months characterized by their 15th day to keep them in time series format
tail(dat_month,20) # note the definition of the metyear_nov and metyear_dec variables, these months "belong" to the next year

# plot monthly average temperature with 5-year running average
dat_month |> 
  ggplot(aes(x=mdate,y=avgtemp_mo_oC)) +
  geom_line() + tidyquant::geom_ma(ma_fun=SMA,n=61,col="red",linetype="solid",) +
  coord_x_date(xlim=c("2001-01-01","2024-08-31")) + # zoom to this
  ggtitle("Monthly temperature at Lauwersoog") +
  theme(text = element_text(size=10))

      
dat_month2 <- dat_month |>
  dplyr::mutate(rain_SMA_13 = rollapply(totrain_mo_mm, width=13, FUN=sum, align = "right", fill = NA))
# plot monthly rainfall with 1-year running average
dat_month |> ggplot() +
  geom_line(aes(x=mdate,y=totrain_mo_mm)) + 
  geom_line(data=dat_month2,aes(x=mdate,y=rain_SMA_13),col="red") 
  
 
dat_month |> 
  ggplot(aes(x=mdate,y=totrain_mo_mm)) +
  geom_line() +
 tidyquant::geom_ma(ma_fun=SMA,n=13,col="red",linetype="solid") +
  coord_x_date(xlim=c("2014-01-01","2024-08-31")) + # zoom to this
  ggtitle("Monthly rainfall at Lauwersoog") +
  theme(text = element_text(size=10)) +
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")  # show only years on x-axis 
  

# additive time series decomposition of the monthly mean temperatures: what is seasonal and what is a longterm trend?
monthly_temps <- dat_month |>
  dplyr::ungroup() |> 
  dplyr::filter(year!=2024) |>
  dplyr::select(avgtemp_mo_oC)
plot(monthly_temps$avgtemp_mo_oC)

monthly_temps_ts <- ts(monthly_temps,frequency=12,start=c(1992,1))
plot(monthly_temps_ts)
monthly_temps_decomp <- decompose(monthly_temps_ts)
plot(monthly_temps_decomp)

# note the trend component is the same as the 12 month running average from the previous plot
# adjust the time series for the seasonal component, and plot it

  
# note again that really cold monthly temperatures (after seasonal correction) become rare, especially since since 2010
# while really warm months (after seasonal correction) become more common from 1991 -2016, while declining a bit again since
# to make the plots and do this in tidyverse style, you may want to explore the timetk package
# https://cran.r-project.org/web/packages/timetk/timetk.pdf


# average temperature of the coldest month per meteorological year
dat_metyear_dec <- dat_month |>
  dplyr::group_by(metyear_dec) |>
  dplyr::summarise(minmonth=min(avgtemp_mo_oC,na.rm=T))
  # note that 2010 and 2011 were the last winters with a (on average) sub-zero month, 
# potentially relevant for cockle recruitment (crabs stay on mudflats eating spatfall)
dat_metyear_dec |> ggplot(aes(x=metyear_dec,y=minmonth)) +
  geom_line(linewidth=.7) +
  geom_point(size=3) 
# Hellmann index (sum of all below-zero daily average temperatures from 1 November of previous year until 31 march)
# see https://nl.wikipedia.org/wiki/Koudegetal
dat_Hellman <- dat |>
  dplyr::filter(month %in% c(11,12,1,2,3),TG<0) |>
  dplyr::group_by(metyear_nov) |>
  dplyr::summarise(Hellman=sum(TG,na.rm=T))
tail(dat_Hellman,20)
dat_Hellman |> ggplot(aes(x=metyear_nov,y=Hellman)) +
  geom_line(linewidth=.7) +
  geom_point(size=3)
# 1997 was the last "Elfstedentocht"
# 2014 -2024 (our transect study) is characterized by only warm winters

# Warmth index
# sum of all daily average temperatures above 18.0 degrees Celsius from 1 April until 30 September
# see https://nl.wikipedia.org/wiki/Warmtegetal
dat_WarmthIndex <- dat |>
  dplyr::filter(month %in% c(4:8),TG>18) |>
  dplyr::group_by(year) |>
  dplyr::summarise(WarmthIndex=sum(TG,na.rm=T))
tail(dat_warmth,20)
dat_warmth |> ggplot(aes(x=year,y=warmth)) +
  #geom_line(linewidth=.7) +
  geom_point(size=3) +
  geom_smooth(method="lm")


library(quantreg)
# see for calculation https://www.knmi.nl/nederland-nu/klimatologie/lijsten/warmtegetallen
#ADD THE quantile regresion lines quantreg package in this plot, 10,50, 90% quqntiles
dat_warmth |> ggplot(aes(x=year,y=warmth)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  geom_quantile(quantiles=c(0.1,0.5,0.9),col="red",size=1)
# note that the 90% quantile is increasing, while the 10% quantile is decreasing, 
#indicating more extreme warm years
# the 50% quantile is increasing, indicating a general warming trend






