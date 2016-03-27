plot1 <- function() {
  
  ## Read the Source file
  ### Subset the data on the fly while reading to avoid huge memory occupation
  library(dplyr)
  
  ## 1. Using fread() function for faster reading
  ## 2. mutate the Date column to Date instead of character
  ## 3. Filter based on the Date column
  dt <- tbl_df(fread("./data/household_power_consumption.txt", sep = ";", 
              header = TRUE, na.strings = "?"  )) %>%
          mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
          filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% 
          select(Global_active_power)
  
  # str(dt)
  ## Initialize a new PNG graphic device for the required output
  png("./data/plot1.png")
  
  ## Plot a histogram of the Global Active Power values for these two days
  hist(dt$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)",
       main = "Histogram of Global Active Power consumption")
  
  dev.off()
}