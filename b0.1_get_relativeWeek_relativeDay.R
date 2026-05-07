
# create a function to produce relative weeks
get_relativeWeek <- function(date, season_start_month = 9, season_start_date = 1){
  new_year = year(date) + case_when(month(date) < season_start_month ~ 0, 
                                    (month(date) == season_start_month) & (day(date) < season_start_date) ~ 0, 
                                    T ~ 1)
  # ifelse(month(date) <= 8, 0, 1)
  new_year_week1start = ISOweek::ISOweek2date(paste0(new_year, "-W01-1"))
  relWeek = floor(as.numeric(date - new_year_week1start)/7) + 1
  relWeek
}


get_relativeDay <- function(date, season_start_month = 9, season_start_date = 1){
  new_year = year(date) + case_when(month(date) < season_start_month ~ 0, 
                                    (month(date) == season_start_month) & (day(date) < season_start_date) ~ 0, 
                                    T ~ 1)
  new_year_day0 <- as.Date(paste0(new_year-1, "-12-31"))
  relDay = as.numeric(date - new_year_day0)
  relDay
}