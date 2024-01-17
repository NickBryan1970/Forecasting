# Forecasting

Before running the r scripts you will need to create a table to hold the events (use tbEvents.xlsx' to build a table)
in each r script edit the 'events' table join so that the sql points to the events table you have built (its at about row 91)
left join [ICB_HW].[ICS].[tbCalEvents] c on CAST(d.DateDate as DATE) = c.period

The where criteria will need to be adapted as well - i.e if you want to predict ed attendances for a different acute then you will
need to update the providerCode / providerSiteCode (the where clause is around line 48)

where 
1=1
and CAST(a.ArrivalDate as Date) >= '2021-04-01'
and CAST(a.ArrivalDate as Date) <= CAST(getdate() - 3 as date)
and a.ProviderCode IN ('RWP00')
and a.DepartmentType = '01'
and a.ProviderSiteCode = 'RWP50'
AND isnull(a.ValidationErrors,'') not like '%1A%'
AND SUBSTRING(a.attendanceIdentifier, 3,1) = 'W'

in the production version of the script you will also need to edit the destination table to one of your choice
dbWriteTable(
  conn = SQLConnection,
  #SQL('defaults.tbl_DEFINE_YOUR_OWN_TABLE'),  # define your own table for output
  value = df3,
  row.names = NULL,
  overwrite = TRUE,
  append = FALSE
)
