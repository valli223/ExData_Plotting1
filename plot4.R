plot4 <- function() {
  
  ## Read the Source file
  ### Subset the data on the fly while reading to avoid huge memory occupation
  library(dplyr)
  
  ## 1. Using fread() function for faster reading
  ## 2. mutate the Date column to Date instead of character
  ## 3. Filter based on the Date column
  dt <- tbl_df(fread("./data/household_power_consumption.txt", sep = ";", 
                     header = TRUE, na.strings = "?"  )) %>%
    mutate(d = as.Date(Date, "%d/%m/%Y")) %>% 
    filter(d >= "2007-02-01" & d <= "2007-02-02") %>% 
    mutate(full_date = as.POSIXct(paste(as.character(d), Time))) %>%
    select(full_date, everything())
  
  str(dt)
  
  ## Initialize a new PNG graphic device for the required output
  png("./data/plot4.png")
  
  par(mfcol = c(2,2))
  
  ## Plot the Global Active Power values for these two days
  plot(Global_active_power ~ full_date, data = dt, 
       type = "l",
       xlab = "",
       ylab = "Global Active Power" ,
       scales = list(x = list(format = "%a", tick.number = 2))
  )
  
  ## Plot the Energy Sub Metering values for these two days
  plot(Sub_metering_1 ~ full_date, data = dt, 
       type = "l",
       xlab = "",
       ylab = "Energy Sub Metering " ,
       scales = list(x = list(format = "%a", tick.number = 2)),
       col = "green"
  )
  
  ## Add all the sub metering plots to the same plot
  with(dt, lines(Sub_metering_2 ~ full_date, col = "red"))
  with(dt, lines(Sub_metering_3 ~ full_date, col = "blue"))
  legend("topright", pch = NA, lty = 1, col = c("green", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  ## Plot the Voltage readings for these two days
  plot(Voltage ~ full_date, data = dt, 
       type = "l",
       xlab = "datetime",
       ylab = "Voltage" ,
       scales = list(x = list(format = "%a", tick.number = 2))
  )
  
  ## Plot the Global Reactive Power values for these two days
  plot(Global_reactive_power ~ full_date, data = dt, 
       type = "l",
       xlab = "datetime",
       ylab = "Global Reactive Power" ,
       scales = list(x = list(format = "%a", tick.number = 2))
  )
  
  dev.off()
}