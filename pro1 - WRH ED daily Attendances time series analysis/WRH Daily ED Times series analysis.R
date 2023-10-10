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
,CASE WHEN c.dEventName LIKE '%Christmas Day%' then 1 else 0 END AS 'Christmas'
,CASE WHEN c.dEventName LIKE '%New Years Day%' then 1 else 0 END AS 'New_Years_Day'
,CASE WHEN c.dEventName LIKE '%Good Friday%' then 1 else 0 END AS 'Good_Friday'
,CASE WHEN c.dEventName LIKE '%Easter Monday%' then 1 else 0 END AS 'Easter'
,CASE WHEN c.dEventName LIKE '%Early May bh%' then 1 else 0 END AS 'May_BH'
,CASE WHEN c.dEventName LIKE '%Spring bh%' then 1 else 0 END AS 'Spring_BH'
,CASE WHEN c.dEventName LIKE '%Summer bh%' then 1 else 0 END AS 'Summer_BH'
,CASE WHEN c.dEventName LIKE '%Platinum Jubilee%' then 1 else 0 END AS 'Jubilee'
,CASE WHEN c.dEventName LIKE '%State Funeral%' then 1 else 0 END AS 'State_Funeral'
,CASE WHEN c.dEventName LIKE '%Ambulance s%' then 1 else 0 END AS 'Strike_Ambulance'
,CASE WHEN c.dEventName LIKE '%Junior doctors strike%' then 1 else 0 END AS 'Strike_Junior_Doctor'
,CASE WHEN c.dEventName LIKE '%Coronation%' then 1 else 0 END AS 'Coronation'
,CASE WHEN c.dEventName LIKE '%Consultant strike%' then 1 else 0 END AS 'Strike_consultant'


,count(*) 'activity'


--from ICB_HW.[ECDS].[CoreAttendance] a
from emergency_Care.[ECDS].[VwECDSCoreAttendanceWithExtraDates] a
left join [ICB_HW].[ICS].[tbCalEvents] c on CAST(a.ArrivalDate as Date) = c.dDateYMDHyphen

where 
1=1
and CAST(a.ArrivalDate as Date) >= '2021-04-01'
and CAST(a.ArrivalDate as Date) <= CAST(getdate() - 3 as date)
and a.ProviderCode IN ('RWP00')
and a.DepartmentType = '01'
and a.ProviderSiteCode = 'RWP50'
AND isnull(a.ValidationErrors,'') not like '%1A%'

--AND CAST(a.ArrivalDate as Date) = '2022-03-29'


GROUP BY
a.ProviderSiteCode

--,a.ValidationErrors
,CAST(a.ArrivalDate as Date)
,c.dEventName

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

df$y[df$ds == as.Date("2022-06-24")] <- 210 
df$y[df$ds == as.Date("2022-06-25")] <- 225
df$y[df$ds == as.Date("2022-06-26")] <- 246 

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

training = df %>% 
  filter(ds < '2023-04-01') %>%
  select(ds, y, Christmas, New_Years_Day ,May_BH, Spring_BH, Summer_BH)

test = df %>% 
  filter(ds >= '2023-04-01') %>%
  select(ds, y, Christmas, New_Years_Day ,May_BH, Spring_BH, Summer_BH)

# ------------- facebook prophet model

m <- prophet(holidays = holidays,
             yearly.seasonality = TRUE,
             weekly.seasonality = TRUE,
             daily.seasonality = FALSE,
             seasonality.mode = 'additive',
             seasonality.prior.scale = 15,       
             changepoint.prior.scale = 0.05,
             holidays.prior.scale = 10,
             interval.width = 0.95)
             #mcmc.samples = 300)

# - add regressors

m <- add_regressor(m, 'Christmas')
m <- add_regressor(m, 'New_Years_Day')
m <- add_regressor(m, 'May_BH')
m <- add_regressor(m, 'Spring_BH')
m <- add_regressor(m, 'Summer_BH')

m <- fit.prophet(m, training)

# ----- regressor coefficients

regressor_coefficients(m)

# ----- create future data frame to hold future data

future <- make_future_dataframe(m, 
                                periods = nrow(test))

future[,2:6]<- df %>% select (Christmas, New_Years_Day ,May_BH, Spring_BH, Summer_BH)




# -----forecasting
forecast <- predict(m, future)
# many variables but look at yhat for prediction

# Events

# forecast %>%
#   select(ds, easter) %>%
#   filter(abs(easter) > 0) %>%
#   filter(ds > '2023-04-01')
# print(Easter_dates)
# 
# forecast %>%
#   select(ds, good_friday) %>%
#   filter(abs(good_friday) > 0) %>%
#   filter(ds > '2023-04-01')
# print(good_friday_dates)
# 
# 
# forecast %>%
#   select(ds, jubilee) %>%
#   filter(abs(jubilee) > 0) %>%
#   filter(ds > '2023-04-01')
# print(jubilee_dates)
# 
# forecast %>%
#   select(ds, strike_ambulance) %>%
#   filter(abs(strike_ambulance) > 0) %>%
#   filter(ds > '2023-04-01')
# print(strike_amb_dates)
# 
# 
# forecast %>%
#   select(ds, strike_juniorDr) %>%
#   filter(abs(strike_juniorDr) > 0) %>%
#   filter(ds > '2023-04-01')
# print(strike_juniorDr_dates)
# 
# forecast %>%
#   select(ds, strike_cons) %>%
#   filter(abs(strike_cons) >0) %>%
#   filter(ds > '2023-04-01')
# print(strike_cons_dates)

# -- vizualisation

plot(m, forecast)
prophet_plot_components(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m) 





# ---- Accuracy
# ---- isolate the test period

predictions = tail(forecast$yhat, nrow(test))
prediction_upper <- tail(forecast$yhat_upper, nrow(test))
prediction_lower <- tail(forecast$yhat_lower, nrow(test))


accuracy(predictions, test$y)


# ---------- save the forecast

prophet = as.data.frame(predictions)
colnames(prophet)[1] = 'Prophet'

op <- cbind(predictions, test$y)
colnames(op)[1] = 'prophet'    
colnames(op)[2] = 'actual'



d <- round((op[, 'actual'] - op[, 'prophet']), 2)  
  
percent_diff <- d / op[, 'actual']
abs_percent_diff <- abs(percent_diff) *100
MAPE <- mean(abs_percent_diff)


op <- data.frame(test$ds, test$y, predictions, prediction_lower, prediction_upper )
colnames(op)[1] = 'date' 
colnames(op)[2] = 'actual'
colnames(op)[3] = 'prophet'
colnames(op)[4] = 'prophet_lower'
colnames(op)[5] = 'prophet_upper'

p = ggplot() + 
  geom_line(data = op, aes(x = date, y = actual), color = "black") +
  geom_line(data = op, aes(x = date, y = prophet_upper), color = "red") +
  geom_line(data = op, aes(x = date, y = prophet_lower), color = "red") +
  geom_line(data = op, aes(x = date, y = prophet), color = "blue") +
  xlab('Dates') +
  ylab('WRH Activity')

print(p)

op

write.csv(op, 
          file = 'C:/Users/nick.bryan/OneDrive - Midlands and Lancashire CSU/Home/projects/R/Repository/pro1 - WRH ED daily Attendances time series analysis/wrh_ed_atts_prophet.csv',
          row.names = FALSE)


