# multivariate analysis of vegetation composition in relation to environmental factors

# clear everything in memory (of R)
remove(list=ls())
library(data.table)
library(lubridate)

# read the tidal data
tidedat1<-fread("https://docs.google.com/spreadsheets/d/e/2PACX-1vQiUfnswG4ivv00ukfbRIL-fhUZN5tBwKmvcBIR9nh6Zi8lOhTCxQ1bci5dv2mOb5ovfy4zJeHDa0TN/pub?gid=1879878831&single=true&output=csv") |>
  dplyr::mutate(Year=lubridate::year(datetime),Month=lubridate::month(datetime),
                sealevel=sealevel/100) 
tidedat1


# calculate an annual cumulative transgression frequency of each elevation
tidedat2<-tidedat1 |>
  dplyr::group_by(Year,sealevel) |>
  dplyr::summarize(sealevel_freq=n()) |>
  dplyr::group_by(Year) |>
  dplyr::mutate(transgression_freq=-cumsum(sealevel_freq))
tidedat2
  
tidedat3<-tidedat2 |> dplyr::filter(Year==2023) |> 
ggplot(aes(x=sealevel,y=transgression_freq))+
  geom_line() +
  labs(x="Sea level (m)",y="Cumulative transgression frequency",color="Year")
