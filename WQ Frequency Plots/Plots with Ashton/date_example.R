library(lubridate)

dateData1 <- c("1999-01-27", "2002/03/16")
lubridate::ymd(dateData1)

dateData2 <- c("15 January, 2018", "30 Mar 15", "151175", "16-4-2001")
lubridate::dmy(dateData2)

dateData3 <- c(dateData1, dateData2)
lubridate::ymd(dateData3)
