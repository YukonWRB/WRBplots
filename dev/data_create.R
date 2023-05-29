#Shortcut script for (re)creating sysdata.rda

level_returns_max <- utils::read.csv("dev/level_returns_max.csv")

#NOTE: the name of *_returns tables be formulated so that the first part of the name matches the parameter as it's spelled in the local database, followed by _returns. If in doubt, check the parameter names using DB_browse_ts.

data <- list(level_returns_max = level_returns_max)

usethis::use_data(data, internal=TRUE, overwrite=TRUE)
