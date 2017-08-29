#demand forecasting project
#created: 3/13/17
#author: Mike Vlah (vlahm13@gmail.com)

#install packages if necessary
package_list = c('imputeTS','plotrix','dplyr','forecast','foreach',
                 'doParallel','httr','jsonlite','forecastHybrid','shiny','stringr',
                 'rvest','lubridate')#,'quantmod')
new_packages = package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

#load packages
for(i in package_list) library(i, character.only=TRUE)

#load data
sales_data2 = tbl_df(read.csv('data/sales.csv', stringsAsFactors=FALSE))
past_weather = tbl_df(read.csv('data/past_weather.csv', stringsAsFactors=FALSE,
                               colClasses=c(time='Date')))
past_colnames = colnames(past_weather)

#if past weather needs to be updated, add in missing days
last_historic = past_weather$time[nrow(past_weather)]
#fetch missing past data if necessary
api_key = Sys.getenv('DKSKY')
if(last_historic+1 != Sys.Date()){
    recent_vec = seq(last_historic+1, Sys.Date(), 'day') #adding one to correct weird unix time translations
    recents = as.numeric(as.POSIXct(recent_vec, origin='1970-01-01', tz='PST')) #convert to unix time
    for(i in 1:length(recents)){
        URL = paste0('https://api.darksky.net/forecast/', api_key, '/',
                     '47.6062,-122.3321,',recents[i],
                     '?exclude=currently,minutely,hourly,alerts,flags')
        out = fromJSON(content(GET(URL), as="text", encoding='UTF-8'))
        out$daily$data$time = as.Date(as.POSIXct(out$daily$data$time,
                                                 origin="1970-01-01", tz='PST'))
        past_weather = merge(past_weather, out$daily$data, all=TRUE)
    }
    past_weather = tbl_df(past_weather[,past_colnames])
    past_weather = past_weather[!duplicated(past_weather$time, fromLast=TRUE),]
    #update past weather data
    write.csv(past_weather, 'data/past_weather.csv', row.names=FALSE)
}

#clean
past_weather = past_weather[-(1:21),]

#fetch forecasts
URL = paste0('https://api.darksky.net/forecast/', api_key, '/',
             '47.6062,-122.3321?exclude=currently,minutely,hourly,alerts,flags')
future_weather = fromJSON(content(GET(URL), as="text", encoding='UTF-8'))
future_weather = tbl_df(future_weather$daily$data[,past_colnames])
future_weather$time = as.Date(as.POSIXct(future_weather$time, origin="1970-01-01", tz='PST'))
weather = rbind(past_weather, future_weather)

#fetch event data
source('../06_webScrape.R')

#create holiday data
thxgvng = c(24,23,22,28,26,25,24,23,28,27,26,25,23,22,28,27,25,24,23,22,27,26,25,24,22,28,27,26,24,23,22,28,26,25)
thxgvng = as.Date(paste(seq(2016, 2050, 1), 11, thxgvng, sep='-'))
thxgvng = as.character(append(thxgvng, thxgvng - 1))
xmas = as.Date(paste(seq(2016, 2050, 1), 12, 25, sep='-'))
xmas = as.character(append(xmas, xmas - 1))
nyd = as.character(seq(as.Date('2016-01-01'), as.Date('2050-01-01'), 'year'))
vday = as.character(seq(as.Date('2016-02-14'), as.Date('2050-02-14'), 'year'))

holidays = c(thxgvng, xmas, nyd, vday)
hday = rep(1, length(holidays))

#process sales data part 1
sales2 = sales_data2 %>%
    select(datetime=Time.From, sales=Net.Sales) %>% #grab columns of interest
    mutate(date=as.Date(datetime, format='%m/%d/%Y')) %>% #reformat date
    select(-datetime)
sales2 = sales2[-c(1:10,nrow(sales2)),] #remove totals

#create regular sequence of dates along sample period
date_seq = data.frame(date=seq.Date(sales2$date[1], sales2$date[nrow(sales2)],
                                    by='day'))

#process sales data part 2
sales = sales2 %>%
    full_join(date_seq) %>% #fill in missing dates with NA
    arrange(date) %>% #sort by date, ascending
    filter(date >= as.Date('2016-11-01')) %>%
    select(sales, date, everything()) #reorder columns

#merge weather data
sales = weather %>%
    mutate(windy=factor(ifelse(windSpeed >= 7, 'y', 'n')),
           midtemp=colMeans(rbind(temperatureMin, temperatureMax), na.rm=TRUE),
           tempfac=cut(midtemp, c(-50,35,55,75,120),
                       labels=c('cold','cool','warm','hot')),
           overall=factor(ifelse(icon %in% c('partly-cloudy-night','partly-cloudy-day'),
                                 'cloudy',icon)),
           cloudy=factor(ifelse(cloudCover < .6 | is.na(cloudCover), 'n', 'y')),
           humid=factor(ifelse(humidity < .6, 'n', 'y')),
           date=time,
           rain=precipProbability,
           rainfac=cut(precipProbability, c(-1,.2,.6,10), labels=c('no','lo','hi'))) %>%
    select(-icon,-summary,-time,-temperatureMin,-temperatureMax,-cloudCover,
           -windSpeed,-humidity,-precipProbability) %>%
    left_join(sales) %>%
    select(date,sales,overall,tempfac,midtemp,rainfac,rain,humid,cloudy,windy,everything())

sales$rainfac[!(sales$precipType=='rain')] = 'no'
sales$rain[sales$precipType=='snow'] = 0

#merge event data and turn sales data into ts object
seahawks = tbl_df(cbind(seahawks, seahawks_home))
huskies = tbl_df(cbind(huskies, huskies_home))
holidays = tbl_df(cbind(holidays, hday))
sales = seahawks %>%
    full_join(huskies, by=c('seahawks'='huskies')) %>%
    full_join(holidays, by=c('seahawks'='holidays')) %>%
    mutate(date=as.Date(seahawks)) %>%
    select(-seahawks) %>%
    right_join(sales, by='date') %>%
    mutate(seahawks=replace(seahawks_home, is.na(seahawks_home), 'none'),
           huskies=replace(huskies_home, is.na(huskies_home), 'none'),
           holiday=replace(hday, is.na(hday), 'none')) %>%
    mutate(seahawks=factor(seahawks, labels=c('a','h','0')),
           huskies=factor(huskies, labels=c('a','h','0')),
           sales=ts(sales, start=c(1,2), frequency=7),
           holiday=factor(holiday, labels=c('1','0'))) %>%
    select(-seahawks_home, -huskies_home, -hday)

#extract additional time info and replace some zeros
sales = sales %>%
    mutate(dayofweek=factor(weekdays(date, abbreviate=TRUE)), #extract day of week
           dayofmonth=factor(substr(date,9,10)), #extract day of month
           sales=replace(sales, which(date < '2017-01-09' & dayofweek=='Mon'), NA)) %>% #early mondays should be NA
    select(date, sales, dayofweek, dayofmonth, everything())

contrasts(sales$huskies) = matrix(c(1,0,0,0,1,0), nrow=3, dimnames=list(NULL,c('a','h')))
contrasts(sales$seahawks) = matrix(c(1,0,0,0,1,0), nrow=3, dimnames=list(NULL,c('a','h')))
contrasts(sales$holiday) = matrix(c(1,0), nrow=2, dimnames=list(NULL,'y'))

#replace holidays with NA (so they will be treated like normal days. no need to incorporate
#major as predictors, since we won't be opening on those days anymore
sales$sales[which(as.character(sales$date) %in% holidays$holidays)] <- NA
#same with the first day of business
sales$sales[1] <- NA

#merge S&P discretionary spending trend
# sales = getSymbols('XLY', src = "yahoo", from=sales$date[1]-25,
#                to=Sys.time(), auto.assign = FALSE) %>%
#     as.data.frame() %>% cbind(rownames(.), .) %>% tbl_df %>%
#     mutate(date = as.Date(`rownames(.)`)) %>%
#     select(date, XLY.Close) %>%
#     full_join(sales, by='date') %>%
#     arrange(date) %>%
#     mutate(market = lag(XLY.Close, 13)) %>%
#     mutate(market = rollapply(market, 3, mean, fill='extend',
#                               na.rm=TRUE, align='center')) %>%
#     select(-XLY.Close) %>%
#     filter(!is.na(dayofweek))

#checked cross correlation (significant at lag of -12, -13, -14)
# ccf(drop(scale(sp[1:102])), drop(scale(as.vector(sales$sales)[1:102])),
    # na.action=na.pass)

#impute missing values with linear interpolation (required for some forecasting methods)
interp = na.seasplit(sales$sales, algorithm='mean')
