# CREATION OF THE DATASET GATHERING THE MEETINGS OF THE BANKS

##### ECB ###################################################

# initial table : a data frame with just the dates

# dates  = all the days between Jan2008 and Dec2024
# meeting = will be a binary variable (1=meeting, 0=no)

dates <- seq(from = as.Date("2008-01-01"), to = as.Date("2024-12-31"), by = "day")
meeting <- rep(0, length(dates)) # 0 as default value
ECB_release <- data.frame(Date = dates, Meeting = meeting)

# adding the dates of the meetings (corresponding to the article's table)
# listing the dates to update
dates_to_update <- as.Date(c("2011-08-04", "2011-10-06", "2011-12-08", "2014-06-05", "2014-09-04", "2014-10-02", "2015-01-22", "2015-03-05", "2015-09-03", "2016-03-10", "2016-04-21", "2016-06-02", "2016-12-08", "2017-10-26"))
# updating
ECB_release$Meeting[ECB_release$Date %in% dates_to_update] <- 1
# Afficher un aperçu des résultats
ECB_release[ECB_release$Meeting == 1, ] # checking if i have all the edited values i wanted

# csv en question
write.csv(ECB_release, "ECB_release.csv", row.names = FALSE)
head(ECB_release)



#################### FED ###############################################


# approx the same thing, just a difference in the dates of the meetings !

# initial table : a data frame with just the dates
dates <- seq(from = as.Date("2008-01-01"), to = as.Date("2024-12-31"), by = "day")
meeting <- rep(0, length(dates)) # 0 as default value
FED_release <- data.frame(Date = dates, Meeting = meeting)

# listing the dates to update
dates_to_update <- as.Date(c("2008-11-25", "2008-12-01", "2008-12-16", "2009-01-28", "2009-03-18", "2010-08-27", "2010-09-21", "2010-10-12", "2010-10-15", "2010-11-03", "2011-08-26", "2012-08-22", "2012-09-13", "2012-12-12"))
# updating
FED_release$Meeting[FED_release$Date %in% dates_to_update] <- 1
# Afficher un aperçu des résultats
FED_release[FED_release$Meeting == 1, ] # checking if i have all the edited values i wanted

# csv en question
write.csv(FED_release, "FED_release.csv", row.names = FALSE)
head(FED_release)



################### CREATING A MERGED ONE ###############################


# could be useful
# one column  = dates
# meeting_ecb = binary, 1 if meeting at ecb on this day
# meeting_fed = binary, 1 if meeting at fed on this day

names(ECB_release)[names(ECB_release) == "Meeting"] <- "Meeting_ECB"
names(FED_release)[names(FED_release) == "Meeting"] <- "Meeting_FED"

# merge based on the date
calendar <- merge(ECB_release, FED_release, by = "Date", all = TRUE)
head(calendar)

# create the csv
write.csv(calendar, "calendar_meetings.csv", row.names = FALSE)





