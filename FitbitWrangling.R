###### Fitbit insights

        rm(list = ls(all = TRUE))
  
        library(fitbitScraper)
        library(dplyr)
        library(ggraptR)

        
# Authenticate--------------------------------------------------------------------------------------------------------------

        cookie = login("your_email", "your_password", rememberMe = TRUE)


# Select dates--------------------------------------------------------------------------------------------------------------

        startDate <- "2015-10-01" # Octover 2015 - complete month with scales
        endDate <- "2016-04-30" # April 2016
        
        
# Activity data-------------------------------------------------------------------------------------------------------------        
        
        # get_activity_data(cookie, end_date = endDate)
        
        # Not run - do not have a complete set of activity data
        # Use steps and calorie burned for now
        
        
# Steps---------------------------------------------------------------------------------------------------------------------

    ### Get data 
    
        dfSteps <- get_daily_data(cookie, what = "steps", startDate, endDate)


    ### Get date from start datetime
    
        dfSteps$Date <- as.Date(substr(dfSteps$time, 1, 10))


    ### Select data and rename    
    
        dfSteps <- dfSteps[ , c("Date", "steps")]
        colnames(dfSteps) <- c("Date", "Steps")
    
        
# Distance------------------------------------------------------------------------------------------------------------------       

    ### Get data        
    
        dfDistance <- get_daily_data(cookie, what = "distance", startDate, endDate)


    ### Apply colnames
    
        colnames(dfDistance) <- c("Date", "Distance")    


    ### Coerce as date
    
        dfDistance$Date <- as.Date(substr(dfDistance$Date, 1, 10)) # Avoided timezone when using as.Date on POSIXct

        
# Floors--------------------------------------------------------------------------------------------------------------------       

    ### Get data
    
        dfFloors <- get_daily_data(cookie, what = "floors", startDate, endDate)


    ### Apply colnames
    
        colnames(dfFloors) <- c("Date", "Floors")    


    ### Coerce as date
    
        dfFloors$Date <- as.Date(substr(dfFloors$Date, 1, 10)) # Avoided timezone when using as.Date on POSIXct
        
        
# Minutes Very--------------------------------------------------------------------------------------------------------------
# Very active minutes        

    ### Get data    
    
        dfMinutesVery <- get_daily_data(cookie, what = "minutesVery", startDate, endDate)


    ### Apply colnames
    
        colnames(dfMinutesVery) <- c("Date", "VeryActiveMinutes")    


    ### Coerce as date
    
        dfMinutesVery$Date <- as.Date(substr(dfMinutesVery$Date, 1, 10)) # Avoided timezone when using as.Date on POSIXct
        
        
# Calories burned-----------------------------------------------------------------------------------------------------------

    ### Get data    
    
        dfCaloriesBurned <- get_daily_data(cookie, what = "caloriesBurnedVsIntake", startDate, endDate)
    
        ## Select columns
        dfCaloriesBurned <- dfCaloriesBurned[ , c("time", "caloriesBurned")]
    
    
    ### Apply colnames
    
        colnames(dfCaloriesBurned) <- c("Date", "CaloriesBurned")    
    
    
    ### Coerce as date
    
        dfCaloriesBurned$Date <- as.Date(substr(dfCaloriesBurned$Date, 1, 10)) # Avoided timezone when using as.Date on POSIXct

        
# Time in heart rate zone---------------------------------------------------------------------------------------------------

    ### Get data    
    
        dfTimeInHeartRateZone <- get_daily_data(cookie, what = "getTimeInHeartRateZonesPerDay", startDate, endDate)
        # Warning message:
        #     In names(df)[1:3] <- c("time", "zone1", "zone2", "zone3") :
        #     number of items to replace is not a multiple of replacement length
            
        
# Resting heart rate--------------------------------------------------------------------------------------------------------

    ### Get data
    
        dfRestingHeart <- get_daily_data(cookie, what = "getRestingHeartRateData", startDate, endDate)
    
    
    ### Apply colnames
    
        colnames(dfRestingHeart) <- c("Date", "RestingHeart")    
    
    
    ### Coerce as date
    
        dfRestingHeart$Date <- as.Date(substr(dfRestingHeart$Date, 1, 10)) # Avoided timezone when using as.Date on POSIXct
        
        
# Sleep---------------------------------------------------------------------------------------------------------------------

#### Sleep        
        
    ### Get data
    
        dfSleep <- as.data.frame(get_sleep_data(cookie, start_date = startDate, end_date = endDate))


    ### Keep key columns
    
        dfSleep <- dfSleep[ , c("df.date", "df.startDateTime", "df.endDateTime", "df.sleepDuration", "df.awakeDuration", 
                                "df.restlessDuration", "df.minAsleep")]

        ## Rename colnames
        # Date is sleep date attempt
        colnames(dfSleep) <- c("Date","SleepStartDatetime", "SleepEndDatetime", "SleepDuration", 
                               "SleepAwakeDuration", "SleepRestlessDuration", "SleepMinAsleep")


    ### Combine the split sleep sessions
    
        ## Index the Dates that are duplicated along with their original
        duplicatedDates <- unique(dfSleep$Date[which(duplicated(dfSleep$Date))])
        dfSleep$Combine <- ""
        dfSleep$Combine[which(dfSleep$Date %in% duplicatedDates)] <- 
            dfSleep$Date[which(dfSleep$Date %in% duplicatedDates)]
        
        ## Subset the combine indexed rows
        dfSubset <- dfSleep[which(dfSleep$Combine != ""), ]

        ## Aggregate rows marked to combine
        dfSubset <- 
            dfSubset %>%
            group_by(Combine) %>%
            summarise(Date = unique(Date),
                      SleepStartDatetime = min(SleepStartDatetime), # Earliest datetime
                      SleepEndDatetime = max(SleepEndDatetime), # Latest datetime
                      SleepDuration = sum(SleepDuration),
                      SleepAwakeDuration = sum(SleepAwakeDuration),
                      SleepRestlessDuration = sum(SleepRestlessDuration),
                      SleepMinAsleep = sum(SleepMinAsleep),
                      SleepSessions = n() # Number of split sleep sessions
            )


    ### Get the minutes awake between split sessions    
    
        ## Calculate sleep duration using start and end time using floor (round down)
        dfSubset$AwakeBetweenDuration <- floor((as.POSIXct(dfSubset$SleepEndDatetime) - as.POSIXct(dfSubset$SleepStartDatetime)) * 60)
        dfSubset$AwakeBetweenDuration <- dfSubset$AwakeBetweenDuration - dfSubset$SleepDuration
        
        ## Set any duration less than five minutes to zero
        dfSubset$AwakeBetweenDuration[which(dfSubset$AwakeBetweenDuration < 5)] <- 0


    ### Remove rows of combined sleep sessions
    
        ## Remove any row with AwakenBetweenDuration greater than five hours
        dfSubset <- dfSubset[-which(dfSubset$AwakeBetweenDuration > 60*5), ]


    ### Replace split sessions in dfSleep with dfSubset
    
        ## Prepare dfSubset for merging
        
        ## Remove duplicate dates from dfSleep
        dfSleep <- dfSleep[-which(nchar(dfSleep$Combine) > 0), ]
        
        ## Add new columns
        dfSleep$SleepSessions <- 1
        dfSleep$AwakeBetweenDuration <- 0
        
        ## Remove Combine column
        dfSubset <- dfSubset[ , -which(colnames(dfSubset) == "Combine")]
        dfSleep <- dfSleep[ , -which(colnames(dfSleep) == "Combine")]
        
        ## Bind dfSubset
        dfSleep <- rbind.data.frame(dfSleep, dfSubset)

        
    ### Coerce as date
        
        dfSleep$Date <- as.Date(dfSleep$Date)
        
        
# Weight--------------------------------------------------------------------------------------------------------------------
# Must read per month 

    ### Get month start and end dates
    
        ## Get unique Year-Month
        firstDays <- unique(substr(seq(as.Date(startDate), as.Date(endDate), "days"), 1, 7))
    
        ## Get first day of each month
        dummyYearMonth <- substr((as.Date(endDate) + 15), 1, 7)
        firstDays <- c(firstDays, dummyYearMonth)
        
        ## Append first day
        firstDays <- paste(firstDays, "-01", sep = "")  
    
        ## Get last day of each month         
        lastDays <- as.Date(firstDays) - 1
      
        ## Trim arrays
        firstDays <- firstDays[1:length(firstDays)-1]
        lastDays <- as.character(lastDays[2:length(lastDays)])

                    
    ### Iterate through each month to get weight
        
        ## Initialise data frame
        dfWeight <- data.frame(time = as.character(),
                               weight = as.numeric(),
                               stringsAsFactors = FALSE)
        dfWeight$time <- as.POSIXct(dfWeight$time)
                
        ## Iterate
        for (i in 1:length(firstDays)) {
            
            ## Select dates
            selectStartDate <- firstDays[i]
            selectEndDate <- lastDays[i]            

            ## Get monthly weights
            dfWeightAdd <- get_weight_data(cookie, start_date = selectStartDate, end_date = selectEndDate)
            
            ## Bind
            dfWeight <- rbind.data.frame(dfWeight, dfWeightAdd)
            rm(dfWeightAdd)
        }
            
        ## Remove duplicate weights - keep the latest recorded weight
        
            ## Sort datetime in descending order (latest weights first)     
            dfWeight <- dfWeight[order(dfWeight$time, decreasing = TRUE), ]
            rownames(dfWeight) <- NULL
            
            ## Remove duplicated datetimes
            if (sum(duplicated(dfWeight$time)) > 0) {
                dfWeight <- dfWeight[-which(duplicated(dfWeight$time)), ]
            }
            rownames(dfWeight) <- NULL
            
            ## Create Date
            dfWeight$Date <- as.Date(substr(dfWeight$time, 1, 10)) # Avoided timezone when using as.Date on POSIXct
            
            ## Remove duplicates where weight was recorded earliest in the day
            if (sum(duplicated(dfWeight$Date)) > 0) {
                dfWeight <- dfWeight[-which(duplicated(dfWeight$Date)), ]
            }
            
        ## Remove weights beyond desired date range
        dfWeight <- dfWeight[-which(as.Date(startDate) > dfWeight$Date), ]
        dfWeight <- dfWeight[-which(as.Date(endDate) < dfWeight$Date), ]
            
        ## Subset and rename columns
        dfWeight <- dfWeight[ , c("Date", "weight")]
        colnames(dfWeight) <- c("Date", "Weight")

    
# Create tidy data----------------------------------------------------------------------------------------------------------            
        
        ## Join dfSteps and dfDistance
        df <- full_join(dfSteps, dfDistance, by = "Date")
        
        ## Join df and dfFloors
        df <- full_join(df, dfFloors, by = "Date")
        
        ## Join df and dfMinutesVery
        df <- full_join(df, dfMinutesVery, by = "Date")
        
        ## Join df and dfCaloriesBurned
        df <- full_join(df, dfCaloriesBurned, by = "Date")
        
        ## Join df and dfRestingHeart
        df <- full_join(df, dfRestingHeart, by = "Date")

        ## Join df and dfSleep
        df <- full_join(df, dfSleep, by = "Date")
        
        ## Join df and dfWeight
        df <- full_join(df, dfWeight, by = "Date")
        
        
# Visualise-----------------------------------------------------------------------------------------------------------------
        
        ## Install and load package
        devtools::install_github('cargomoose/raptR', force = TRUE)
        library("ggraptR")
        
        ## Launch        
        ggraptR()
        