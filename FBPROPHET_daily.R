setwd("C:/Users/gwatson5/Documents/PROJECTS/Channel Forecast/Output/Final/Mar 2020")
#Forecast using FBProphet
#library(data.table)
library(readxl)
library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(prophet)
library(anytime)

#Read in data
my_date_lookup <- read.csv("~/PROJECTS/Channel Forecast/my_date_lookup.csv")
input_data <- read.csv("March_2020_OTA_complete_input.csv")
#input_data$Week_Number <- NULL ; input_data$Year <- NULL 
names(input_data) <- c("Brand", "OpAreaLvl3", "StayDate", "BkChannel", "2020_Comp", "RmNts")
Brand_Cat_Lookup <- read.csv("~/PROJECTS/Channel Forecast/Brand_Cat_Lookup.csv")
holidays <- read_excel("~/PROJECTS/Channel Forecast/Forecast Holidays.xlsx", sheet = "Holidays")

#Add Brand Cat
input_data <- merge(input_data, Brand_Cat_Lookup, by = 'Brand', all.x = T)

#Filter for model tweaking
#Add comparability filter here
input_data <- input_data %>% 
filter(`2020_Comp` == 1)

#Group RmNts by Brand_Cat
input_data <- input_data %>%
  group_by(Brand_Cat, OpAreaLvl3, BkChannel, StayDate) %>%
  summarise(RmNts = sum(RmNts))


input_data <- input_data[input_data$Brand_Cat %in% c('All Suites', 'Focused Service', 'Full Service', 'Luxury & Lifestyle') & input_data$OpAreaLvl3 != 'Unknown Operational Area (Lvl 3)',]


#format data for prophet modeling
p.data <- input_data
p.data$Identifier <- paste(p.data$Brand_Cat, p.data$OpAreaLvl3, p.data$BkChannel, sep = "-")
p.data$StayDate <- as.Date(anytime(p.data$StayDate), '%Y/%m/%d')

p.data <- p.data %>%
  ungroup() %>%
  select(Identifier, StayDate, RmNts) %>%
  rename('ds' = StayDate, 'y' = RmNts)


#breakdown data by category and store in mylist
ids <- as.vector(unique(p.data$Identifier))
mylist = list()

for(i in ids){
  mylist[[i]] <- as.data.frame(p.data[p.data$Identifier == i,])
}
length(ids)

#Create model for each level and store output in forecast_list
forecast_list = list()
iter = 1
for(j in mylist){
  
  pmodel <- prophet(j, daily.seasonality = F, weekly.seasonality = F, yearly.seasonality = T, seasonality.mode = 'multiplicative', holidays = holidays)
  future <- make_future_dataframe(pmodel, periods = 306)
  forecast <- predict(pmodel, future)
  forecast_list[[iter]] <- data.frame(forecast$ds, forecast$yhat, forecast$yhat_upper, forecast$yhat_lower, j[1, 'Identifier'])
  names(forecast_list[[iter]]) <- c('ds', 'yhat', 'yhat_upper', 'yhat_lower', 'Identifier')
  print(iter)
  print(j[1, 'Identifier'])
  print(plot(pmodel, forecast))
  iter = iter + 1
  #prophet_plot_components(m, forecast)
}

p.complete_forecast <- bind_rows(forecast_list)
p.complete_forecast <- separate(p.complete_forecast, col = Identifier, into = c('Brand_Cat', 'OpAreaLvl3', 'BkChannel'), sep = '-')

#combine forecast and actuals
#Create Actuals to be used for comparision
p.Actuals <- input_data %>% 
  select(Brand_Cat, OpAreaLvl3, BkChannel, RmNts, StayDate)

p.Actuals$StayDate <- as.Date(anytime(p.Actuals$StayDate), '%Y/%m/%d')
p.Actuals <- select(p.Actuals, Brand_Cat, OpAreaLvl3, BkChannel, StayDate, RmNts)

p.complete_forecast$ds <- as.Date(anytime(p.complete_forecast$ds), '%Y/%m/%d')
p.complete_forecast <- merge(p.complete_forecast, p.Actuals, by.x = c("Brand_Cat", "OpAreaLvl3", "BkChannel", "ds"), 
                             by.y = c("Brand_Cat", "OpAreaLvl3", "BkChannel", "StayDate"), all.x = T)

setwd('C:/Users/gwatson5/Documents/PROJECTS/Channel Forecast/Output/Final/Mar 2020')
#fwrite(p.complete_forecast, 'p.OTA_comp_forecast_0309.csv')

######################################################OTA MIX######################################################
mtest <- p.complete_forecast %>%
  mutate(Ota_flag = ifelse(BkChannel == 'OTA', 1, 0)) %>%
  group_by(Brand_Cat, OpAreaLvl3, Ota_flag, ts_group=floor_date(ds, "day")) %>%
  summarize(yhat=sum(yhat), RmNts = sum(RmNts))

ota <- filter(mtest, Ota_flag == 1)
Noota <- filter(mtest, Ota_flag == 0)

d2 <- merge(ota, Noota, by = c( "Brand_Cat", "OpAreaLvl3", "ts_group"))

mix_summary <- d2 %>%
  rename("OTA_RmNts" = RmNts.x) %>%
  rename("Non_OTA_RmNts" = RmNts.y) %>%
  rename("OTA_yhat" = yhat.x) %>%
  rename("Non_OTA_yhat" = yhat.y) %>%
  select(- c(Ota_flag.x, Ota_flag.y))

#################################################fwrite(mix_summary, 'p.2020Comparable_Mix_Summary.csv')#write to working directory














