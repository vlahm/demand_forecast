#demand forecasting project
#created: 4/9/17
#author: Mike Vlah (vlahm13@gmail.com)

this_year = year(Sys.Date())
last_year = this_year-1

#scrape seahawks data ####
seahawks = seahawks_home = NULL

for(i in c(last_year, this_year)){

    #fetch webpage
    url = read_html(paste0('http://www.seahawks.com/schedule?season=', i))

    #extract dates
    dates = url %>%
        html_nodes(".date-month-date") %>%
        html_text() %>%
        as.Date(format='%b %d')

    #if upcoming season is far away, skip
    if(!length(dates) | (i == this_year & month(Sys.Date()) < 7)){
        next
    }

    #add years to the dates
    year(dates) = i
    year(dates)[which(month(dates) %in% 1:3)] = i+1

    #find which games are home/away
    schedule = url %>%
        html_nodes(".visitor .club-name") %>%
        html_text() %>%
        replace(., which(.=='Seahawks'), 0) %>%
        replace(., which(.!=0), 1) %>%
        as.numeric()

    #build vectors of games and dates
    seahawks_home = append(seahawks_home, schedule)
    seahawks = append(seahawks, dates)
}

#sort dates and home/away
seahawks_home = seahawks_home[order(seahawks, na.last=NA)]
seahawks = as.character(sort(seahawks))


#scrape huskies data ####
huskies = huskies_home = NULL

for(i in c(last_year, this_year)){

    #fetch webpage
    url = read_html(paste0('http://www.fbschedules.com/ncaa-',
                           substr(i,3,4), '/', i, '-washingt',
                           'on-huskies-football-schedule.php'))

    #extract dates
    dates = url %>%
        html_nodes("td.cfb1") %>%
        html_text()
    dates = as.Date(str_match(dates, '\\w*\\s*(.*)')[,2], format='%b. %e')

    #if upcoming season not yet available, skip
    if(!length(dates)){
        next
    }

    #add years to the dates and remove NAs (dates not set in stone yet)
    year(dates) = i
    year(dates)[which(month(dates) %in% 1:3)] = i+1
    dates = dates[!is.na(dates)]

    #find which games are home/away
    schedule = url %>%
        html_nodes("td.cfb2") %>%
        html_text() %>%
        grepl('Husky Stadium', .) %>%
        as.numeric()

    #build vectors of games and dates
    huskies_home = append(huskies_home, schedule)
    huskies = append(huskies, dates)
}

#sort dates and home/away
huskies_home = huskies_home[order(huskies, na.last=NA)]
huskies = as.character(sort(huskies))
