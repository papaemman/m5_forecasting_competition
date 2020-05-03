##################################
#                                #
# Feature Engineering: Calendar  #
#                                #
##################################


# library(data.table)
# library(RcppRoll)
# library(dplyr)


## Crate extra calendar features

# calendar <- fread("data/raw/calendar.csv", na.strings = c("", "_"))
# calendar$date <- as.Date(calendar$date)
# calendar <- create_calendar_features(calendar)
# saveRDS(calendar, "data/raw/calendar_full.rds")



create_calendar_features <- function(calendar){
  
  
  ## 01. Sesonal Features -----
  
  calendar[, `:=`(day_month = lubridate::mday(date), day_quarter = lubridate::qday(date), day_year = yday(date))       # Day
           ][,`:=`(week_month = ceiling(day_month/7), week_quarter = ceiling(day_quarter/7), week_year = week(date),   # Week
                   weekend = ifelse(wday %in% c(1,2), T, F),                                                           # Weekend
                   month_quarter = ifelse(month%%3 == 0, 3, month%%3),                                                 # Month
                   quart = quarter(date),                                                                              # Quarter
                   semester = ifelse(month<=6, 0, 1)                                                                   # Semester 
           )]  
  
  
  
  ## 02. Fourier Transformation for seasonal features (wday, month)  ----
  
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
  
  
  ## 03. Radial Basis functions ----
  a <- 1
  
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
  
  
  
  ## 04. Calendar Events (processing) -----
  
  
  # # 1. Events with duration
  # calendar$event_name_1_v2 <- calendar$event_name_1
  # calendar$event_type_1_v2 <- calendar$event_type_1
  # 
  # 
  # # NBAFinals start - NBAFinals end (How many days)
  # nba_start <- which(calendar$event_name_1_v2 == "NBAFinalsStart")
  # nba_end <- which(calendar$event_name_1_v2 == "NBAFinalsEnd")
  # nba_duration <- (nba_end - nba_start) + 1
  # 
  # for(i in 1:length(nba_start)){
  #   calendar$event_name_1_v2[nba_start[i]:nba_end[i]] <- "NBAFinals"
  #   calendar$event_type_1_v2[nba_start[i]:nba_end[i]] <- "Sporting"
  # }
  # 
  # 
  # 
  # # 3. Add pre-events
  # 
  # # (1) NewYearsEve
  # new_year <- which(calendar$event_name_1 == "NewYear")
  # 
  # for(i in 1:length(new_year)){
  #   calendar$event_name_1[(new_year[i]-2):(new_year[i]-1)] <- "new_years_eve"
  # }
  # 
  # # (2) Christmas
  # christmas <- which(calendar$event_name_1 == "Christmas")
  # 
  # for(i in 1:length(christmas)){
  #   calendar$event_name_1[(christmas[i]-5):(christmas[i]-1)] <- "christmas"
  # }
  # 
  # # (3) IndependenceDay 
  # independeceday <- which(calendar$event_name_1 == "IndependenceDay")
  # 
  # for(i in 1:length(independeceday)){
  #   calendar$event_name_1[(independeceday[i]-2):(independeceday[i]-1)] <- "independeceday_before"
  # }
  # 
  # 
  # # (4) Thanksgiving 
  # thanksgiving <- which(calendar$event_name_1 == "Thanksgiving")
  # 
  # for(i in 1:length(thanksgiving)){
  #   calendar$event_name_1[(thanksgiving[i]-2):(thanksgiving[i]-1)] <- "thanksgiving_before"
  # }
  # 
  # # (5) SuperBowl
  # superbowl <- which(calendar$event_name_1 == "SuperBowl")
  # 
  # for(i in 1:length(superbowl)){
  #   calendar$event_name_1[(superbowl[i]-2):(superbowl[i]-1)] <- "superbowl_before"
  # }
  # 
  
  
  ## 05. One-hot endoding for event_type_1 and event_type_1_v2 -----
  calendar[, `:=`(sporting_event = as.numeric(event_type_1 == "Sporting"),
                  cultural_event = as.numeric(event_type_1 == "Cultural"),
                  national_event = as.numeric(event_type_1 == "National"),
                  religious_event = as.numeric(event_type_1 == "Religious"))]
  
  
  calendar[is.na(sporting_event), `:=`(sporting_event=0)
           ][is.na(cultural_event), `:=`(cultural_event=0)
             ][is.na(national_event), `:=`(national_event=0)
               ][is.na(religious_event), `:=`(religious_event=0)]
  
  # calendar %>% select(date,weekday, event_name_1, event_type_1, sporting_event, cultural_event, national_event, religious_event, has_event) %>% View()
  
  
  ## 06. Label encoding (Encode events as integers) ----
  
  cols <- c("event_name_1", "event_type_1")   # "event_name_1_v2", "event_type_1_v2", "event_name_2", "event_type_2")
  calendar[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]
  
  calendar[is.na(event_name_1), `:=`(event_name_1=0)
           ][is.na(event_type_1), `:=`(event_type_1=0)]
  
  
  
  ## 07. Lead features ----
  
  # Day has event
  calendar[, `:=`(has_event = !is.na(event_type_1))
           ][,`:=`(has_event_lead_t1 = lead(has_event, n = 1),
                   has_event_lead_t2 = lead(has_event, n = 2),
                   has_event_lead_t3 = lead(has_event, n = 3))]
  
  # Count week's events
  count_week_events <- calendar %>% group_by(wm_yr_wk) %>% summarise(week_events = sum(has_event))
  calendar <- merge(calendar, count_week_events, by = "wm_yr_wk")
  
  
  calendar[,`:=`(sporting_event_lead_1 = lead(sporting_event,1),
                 cultural_event_lead_1 = lead(cultural_event,1),
                 national_event_lead_1 = lead(national_event,1),
                 religious_event_lead_1 = lead(religious_event,1),
                 
                 sporting_event_lead_3 = roll_sum(x = sporting_event, n = 3, align = "left", fill = NA),
                 cultural_event_lead_3 = roll_sum(x = cultural_event, n = 3, align = "left", fill = NA),
                 national_event_lead_3 = roll_sum(x = national_event, n = 3, align = "left", fill = NA),
                 religious_event_lead_3 = roll_sum(x = religious_event, n = 3, align = "left", fill = NA)
                 )]
  
  # calendar %>% select(date,weekday, event_name_1, event_type_1, sporting_event,sporting_event_lead_1, sporting_event_lead_3,cultural_event, national_event, religious_event, has_event) %>% View()
  
  ## 08. Drop unused columns ----
  calendar[, `:=`(date = NULL, 
                  weekday = NULL,
                  d = as.integer(substring(d, 3)))]
  
  calendar[is.na(calendar)] <- 0
  gc()
  return(calendar)
  
}

