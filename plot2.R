plot2 <- function() {
  
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
    select(full_date, Global_active_power)
  
  # str(dt)
  
  ## Initialize a new PNG graphic device for the required output
  png("./data/plot2.png")
  
  ## Plot the Global Active Power values for these two days
  plot(Global_active_power ~ full_date, data = dt, 
          type = "l",
          xlab = "",
          ylab = "Global Active Power (kilowatts)" ,
          scales = list(x = list(format = "%a", tick.number = 2))
          )
  
  dev.off()
}