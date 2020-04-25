##################################
#                                #
# Feature Engineering: Calendar  #
#                                #
##################################

## Source dependencies
# source("modules/main.R")

# library(data.table)


## Crate extra calendar features

# calendar <- fread("data/raw/calendar.csv", na.strings = c("", "_"))
# calendar$date <- as.Date(calendar$date)
# calendar <- create_calendar_features(calendar)
# saveRDS(calendar, "data/raw/calendar.rds")



create_calendar_features <- function(calendar){
  
  # 1. Sesonal Features
  
  calendar[, `:=`(day_month = lubridate::mday(date), day_quarter = lubridate::qday(date),day_year = yday(date))       # Day
           ][,`:=`(week_month = ceiling(day_month/7), week_quarter = ceiling(day_quarter/7), week_year = week(date),  # Week
                   weekend = ifelse(wday %in% c(1,2), T, F),                                                          # Weekend
                   month_quarter = ifelse(month%%3 == 0, 3, month%%3),                                                # Month
                   quart = quarter(date),                                                                             # Quarter
                   semester = ifelse(month<=6, 0, 1)                                                                  # Semester 
           )][]  
  
  
  
  # 2. Fourier Transformation for seasonal features
  
  calendar[, `:=`(wday_s1 = round(sin(wday * (2*pi/7)), digits = 5),
                  wday_c1 = round(cos(wday * (2*pi/7)), digits = 5),
                  wday_s2 = round(sin(wday * (4*pi/7)), digits = 5),
                  wday_c2 = round(cos(wday * (4*pi/7)), digits = 5),
                  wday_s3 = round(sin(wday * (6*pi/7)), digits = 5),
                  wday_c3 = round(cos(wday * (6*pi/7)), digits = 5),
                  
                  month_s1 = round(sin(month * (4*pi/12)), digits = 5), 
                  month_c1 = round(cos(month * (4*pi/12)), digits = 5),
                  month_s2 = round(sin(month * (4*pi/12)), digits = 5), 
                  month_c2 = round(cos(month * (4*pi/12)), digits = 5),
                  month_s3 = round(sin(month * (6*pi/12)), digits = 5), 
                  month_c3 = round(cos(month * (6*pi/12)), digits = 5)
                  )]
  
  
  # 3. Radial Basis functions
  a <- 0.5
  
  calendar[, `:=`(wday_rbf_1 = exp( (-1/(2*a)) * (wday-1)^2),
                  wday_rbf_2 = exp( (-1/(2*a)) * (wday-2)^2),
                  wday_rbf_3 = exp( (-1/(2*a)) * (wday-3)^2),
                  wday_rbf_4 = exp( (-1/(2*a)) * (wday-4)^2),
                  wday_rbf_5 = exp( (-1/(2*a)) * (wday-5)^2),
                  wday_rbf_6 = exp( (-1/(2*a)) * (wday-6)^2),
                  wday_rbf_7 = exp( (-1/(2*a)) * (wday-7)^2),
                  
                  month_rbf_1  = exp( (-1/(2*a)) * (month-1)^2), 
                  month_rbf_2  = exp( (-1/(2*a)) * (month-2)^2), 
                  month_rbf_3  = exp( (-1/(2*a)) * (month-3)^2), 
                  month_rbf_4  = exp( (-1/(2*a)) * (month-4)^2), 
                  month_rbf_5  = exp( (-1/(2*a)) * (month-5)^2), 
                  month_rbf_6  = exp( (-1/(2*a)) * (month-6)^2), 
                  month_rbf_7  = exp( (-1/(2*a)) * (month-7)^2), 
                  month_rbf_8  = exp( (-1/(2*a)) * (month-8)^2), 
                  month_rbf_9  = exp( (-1/(2*a)) * (month-9)^2), 
                  month_rbf_10 = exp( (-1/(2*a)) * (month-10)^2), 
                  month_rbf_11 = exp( (-1/(2*a)) * (month-11)^2), 
                  month_rbf_12 = exp( (-1/(2*a)) * (month-12)^2)
                  )]
  
  
  
  # 4. Calendar Events -----
  
  # Day has event
  calendar[, has_event := !is.na(event_type_1)]
  
  # Events with duration
  
  # (1) NBAFinals start - NBAFinals end (How many days)
  nba_start <- which(calendar$event_name_1 == "NBAFinalsStart")
  nba_end <-  which(calendar$event_name_1 == "NBAFinalsEnd")
  nba_duration <- nba_end - nba_start
  # nba_duration
  
  for(i in 1:length(nba_start)){
    calendar$event_name_1[nba_start[i]:nba_end[i]] <- "NBAFinals"
  }
  
  
  # Add pre-events
  
  # (1) NewYearsEve
  new_year <- which(calendar$event_name_1 == "NewYear")
  
  for(i in 1:length(new_year)){
    calendar$event_name_1[(new_year[i]-2):(new_year[i]-1)] <- "new_years_eve"
  }
  
  # (2) Christmas
  christmas <- which(calendar$event_name_1 == "Christmas")
  
  for(i in 1:length(christmas)){
    calendar$event_name_1[(christmas[i]-5):(christmas[i]-1)] <- "christmas"
  }
  
  # (3) IndependenceDay 
  independeceday <- which(calendar$event_name_1 == "IndependenceDay")
  
  for(i in 1:length(independeceday)){
    calendar$event_name_1[(independeceday[i]-2):(independeceday[i]-1)] <- "independeceday_before"
  }
  
  
  # (4) Thanksgiving 
  thanksgiving <- which(calendar$event_name_1 == "Thanksgiving")
  
  for(i in 1:length(thanksgiving)){
    calendar$event_name_1[(thanksgiving[i]-2):(thanksgiving[i]-1)] <- "thanksgiving_before"
  }
  
  # (5) SuperBowl
  superbowl <- which(calendar$event_name_1 == "SuperBowl")
  
  for(i in 1:length(superbowl)){
    calendar$event_name_1[(superbowl[i]-2):(superbowl[i]-1)] <- "superbowl_before"
  }
  
  
  ## Encode events as integers
  
  cols <- c("event_name_1", "event_type_1")  #, "event_name_2", "event_type_2")
  
  calendar[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]
  calendar[is.na(event_name_1), `:=`(event_name_1=0)][is.na(event_type_1), `:=`(event_type_1=0)]
  
  calendar[, `:=`(date = NULL, 
                  weekday = NULL, 
                  event_name_2 = NULL,
                  event_type_2 = NULL,
                  d = as.integer(substring(d, 3)))]
  
  gc()
  return(calendar)
  
}

