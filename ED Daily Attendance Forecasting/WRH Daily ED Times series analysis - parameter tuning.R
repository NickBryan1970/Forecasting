rm(list=ls())

library(prophet)
library(forecast)
library(dplyr)
library(ggplot2)

read_sql_analyst_Server <- function(sql_query) {
  # create a connection to MLCSU analyst server using user credentials
  
  connection <- DBI::dbConnect(odbc::odbc(),
                               Driver = "SQL Server",
                               Server = "MLCSU-BI-SQL",
                               Database = "emergency_care",
                               Trusted_Connection = "True"
  )
  
  # use the input variable sql_query to get data from the analyst server
  DBI::dbGetQuery(connection, sql_query)
}

# example_sql_query <- "
#'                     SELECT TOP (1000) *
#'                     FROM EAT_Reporting.dbo.tbInpatientEpisodes
#'                    "

ECDS_sql <- "SELECT
a.ProviderSiteCode
,CAST(a.ArrivalDate as date) 'ArrivalDate'
,CASE WHEN c.EventName LIKE '%Christmas Day%' then 1 else 0 END AS 'Christmas'
,CASE WHEN c.EventName LIKE '%New Years Day%' then 1 else 0 END AS 'New_Years_Day'
,CASE WHEN c.EventName LIKE '%Good Friday%' then 1 else 0 END AS 'Good_Friday'
,CASE WHEN c.EventName LIKE '%Easter Monday%' then 1 else 0 END AS 'Easter'
,CASE WHEN c.EventName LIKE '%Early May bh%' then 1 else 0 END AS 'May_BH'
,CASE WHEN c.EventName LIKE '%Spring bh%' then 1 else 0 END AS 'Spring_BH'
,CASE WHEN c.EventName LIKE '%Summer bh%' then 1 else 0 END AS 'Summer_BH'
,CASE WHEN c.EventName LIKE '%Platinum Jubilee%' then 1 else 0 END AS 'Jubilee'
,CASE WHEN c.EventName LIKE '%State Funeral%' then 1 else 0 END AS 'State_Funeral'
,CASE WHEN c.EventName LIKE '%Ambulance s%' then 1 else 0 END AS 'Strike_Ambulance'
,CASE WHEN c.EventName LIKE '%Junior doctors strike%' then 1 else 0 END AS 'Strike_Junior_Doctor'
,CASE WHEN c.EventName LIKE '%Coronation%' then 1 else 0 END AS 'Coronation'
,CASE WHEN c.EventName LIKE '%Consultant strike%' then 1 else 0 END AS 'Strike_consultant'


,count(*) 'activity'


--from ICB_HW.[ECDS].[CoreAttendance] a
from emergency_Care.[ECDS].[VwECDSCoreAttendanceWithExtraDates] a
left join [ICB_HW].[ICS].[tbCalEvents] c on CAST(a.ArrivalDate as Date) = c.[period]

where 
1=1
and CAST(a.ArrivalDate as Date) >= '2021-04-01'
and CAST(a.ArrivalDate as Date) <= CAST(getdate() - 3 as date)
and a.ProviderCode IN ('RWP00')
and a.DepartmentType = '01'
and a.ProviderSiteCode = 'RWP50'
AND isnull(a.ValidationErrors,'') not like '%1A%'
AND SUBSTRING(a.attendanceIdentifier, 3,1) = 'W'  -- excludes the EM OP clinic patients
--AND CAST(a.ArrivalDate as Date) = '2022-03-29'


GROUP BY
a.ProviderSiteCode

--,a.ValidationErrors
,CAST(a.ArrivalDate as Date)
,c.EventName

--HAVING
--count(*) < 


ORDER BY
CAST(a.ArrivalDate as Date)"

df <- read_sql_analyst_Server(ECDS_sql)




df$Date <- as.Date(df$ArrivalDate)

#pick the variables

df <- df %>% select(Date,
                    activity,
                    Christmas,
                    New_Years_Day,
                    Good_Friday,
                    Easter,
                    May_BH,
                    Spring_BH,
                    Summer_BH,
                    Jubilee,
                    State_Funeral,
                    Strike_Ambulance,
                    Strike_Junior_Doctor,
                    Strike_consultant,
                    Coronation)


# change variable name
colnames(df)[1] = 'ds'    # date name needs to be in this format for facebook prophet
colnames(df)[2] = 'y'

df$ds == '2022-06-24'

# replace errors in activity levels

#df$y[df$ds == as.Date("2022-06-24")] <- 210 
#df$y[df$ds == as.Date("2022-06-25")] <- 225
#df$y[df$ds == as.Date("2022-06-26")] <- 246 

#as.Date("2022-06-24")
# create bubble plot

ggplot(df, aes(x=ds, y = y)) +
  geom_line() +
  xlab('Date')+
  ylab('Activity')+
  theme(text = element_text(size = 10)) +
  scale_x_date (date_labels = "%Y  %b")


# isolate the days associated with christmas
# i.e. easter = 1

Easter_dates <- subset(df, df$Easter == 1)
Easter_dates <- Easter_dates$ds
easter <- tibble(holiday = 'easter', 
                       ds = Easter_dates,
                       lower_window = -1,
                       upper_window = +1)

good_friday_dates <- subset(df, df$Good_Friday == 1)
good_friday_dates <- good_friday_dates$ds
good_friday <- tibble(holiday = 'good_friday', 
                   ds = good_friday_dates,
                   lower_window = -1,
                   upper_window = +1)


jubilee_dates <- subset(df, df$Jubilee == 1)
jubilee_dates <- jubilee_dates$ds
jubilee <- tibble(holiday = 'jubilee', 
                      ds = good_friday_dates,
                      lower_window = 0,
                      upper_window = +2)

state_funeral_dates <- subset(df, df$State_Funeral == 1)
state_funeral_dates <- state_funeral_dates$ds
state_funeral <- tibble(holiday = 'state_funeral', 
                  ds = state_funeral_dates,
                  lower_window = 0,
                  upper_window = +2)

coronation_dates <- subset(df, df$Coronation == 1)
coronation_dates <- coronation_dates$ds
coronation <- tibble(holiday = 'coronation', 
                      ds = coronation_dates,
                      lower_window = 0,
                      upper_window = +2)

strike_amb_dates <- subset(df, df$Strike_Ambulance == 1)
strike_amb_dates <- strike_amb_dates$ds
strike_ambulance <- tibble(holiday = 'strike_ambulance', 
                        ds = strike_amb_dates,
                        lower_window = 0,
                        upper_window = 0)

strike_juniorDr_dates <- subset(df, df$Strike_Junior_Doctor == 1)
strike_juniorDr_dates <- strike_juniorDr_dates$ds
strike_juniorDr <- tibble(holiday = 'strike_juniorDr', 
                           ds = strike_juniorDr_dates,
                           lower_window = 0,
                           upper_window = 0)

strike_cons_dates <- subset(df, df$Strike_consultant == 1)
strike_cons_dates <- strike_cons_dates$ds
strike_cons <- tibble(holiday = 'strike_cons', 
                          ds = strike_cons_dates,
                          lower_window = 0,
                          upper_window = 0)

# -------- merge holidays 

holidays <- bind_rows(easter, good_friday, jubilee, state_funeral, strike_ambulance, strike_juniorDr, strike_cons, coronation)

## --------- training and test set ----------


# ------------- facebook prophet model

m <- prophet(holidays = holidays,
             yearly.seasonality = TRUE,
             weekly.seasonality = TRUE,
             daily.seasonality = FALSE,
             seasonality.mode = 'multiplicative',
             seasonality.prior.scale = 10,       
             changepoint.prior.scale = 0.05,
             interval.width = 0.95)
             #mcmc.samples = 300)

# - add regressors

m <- add_regressor(m, 'Christmas')
m <- add_regressor(m, 'New_Years_Day')
m <- add_regressor(m, 'May_BH')
m <- add_regressor(m, 'Spring_BH')
m <- add_regressor(m, 'Summer_BH')

m <- fit.prophet(m, df)

# ----- regressor coefficients

#regressor_coefficients(m)

# --------- Cross validation ---------------

# cv
df.cv <- cross_validation(model = m,
                          horizon = 31,
                          units = 'days',
                          period = 7,
                          initial = 400)
accuracy(df.cv$yhat, df.cv$y)

# ----------- Parameter tuning ------------

# parameter grid, scales can be extended to fully exploit tuning, however the more parameters used the greater the time to process
prophetGrid <- expand.grid(changepoint_prior_scale = c(0.05, 0.1),
                           seasonality_prior_scale = c(5, 10, 15),
                           holidays_prior_scale = c(5, 10),
                           seasonality.mode = c('multiplicative','additive'))


#results vector

results = vector(mode = 'numeric',
                 length = nrow(prophetGrid))

# parameter tuning

for (i in 1:nrow(prophetGrid)){
  
  # fetch the parameters
  
  parameters <- prophetGrid[i,]
  
  # build model
  m <- prophet(yearly.seasonality = TRUE,
               weekly.seasonality = TRUE,
               daily.seasonality = FALSE,
               holidays = holidays,
               seasonality.mode = parameters$seasonality.mode,
               seasonality.prior.scale = parameters$seasonality_prior_scale,
               holidays.prior.scale = parameters$holidays_prior_scale,
               changepoint.prior.scale = parameters$changepoint_prior_scale)
  
  m <- add_regressor(m, 'Christmas')
  m <- add_regressor(m, 'New_Years_Day')
  m <- add_regressor(m, 'May_BH')
  m <- add_regressor(m, 'Spring_BH')
  m <- add_regressor(m, 'Summer_BH')
  
  m <- fit.prophet(m, df)
  # cross validation
  
  df.cv <- cross_validation(model = m,
                            horizon = 31,
                            units = 'days',
                            period = 14,
                            initial = 400)
  # store results
  results[i] <- accuracy(df.cv$yhat, df.cv$y)[1, 2]
  print(i)
}

warnings()

prophetGrid <- cbind(prophetGrid, results)
best_params <- prophetGrid[prophetGrid$results == min(results),]

best_params
