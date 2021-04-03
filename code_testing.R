download.file(url = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip',
              destfile = 'project1_data.zip',
              method = 'curl')
unzip(zipfile = "project1_data.zip", exdir = getwd())
file.remove("project1_data.zip")
Raw_Data <- read.csv("activity.csv")
file.remove("activity.csv")
Clean_Data <- na.omit(Raw_Data)
Clean_Data$date <- as.Date.character(Clean_Data$date)

Raw_Data$date <- as.Date.character(Raw_Data$date)

Daily_Summary <- aggregate(list(Steps = Raw_Data$steps), by = list(date = Raw_Data$date), FUN = sum, na.rm = TRUE)

Daily_Summary_Clean <- aggregate(list(Steps = Clean_Data$steps), by = list(date = Clean_Data$date), FUN = sum)

paste("After removing the NAs, the remaining observations amount to ", round(100*(nrow(Clean_Data)/nrow(Raw_Data)), digits = 2),"%", sep = "")

#Clean_Data$date <- format(as.Date(Clean_Data$date), "%d-%b-%Y")

####
library(lubridate)
library(ggplot2)

plot1 <- ggplot(Daily_Summary, aes(date, Steps))
plot1 <- plot1 + geom_bar(stat = "identity") + geom_hline(aes(yintercept = mean(Steps), linetype = "mean"), colour = "red") + labs(linetype = "Legend") + ylab("Steps") + xlab("Date") + ggtitle("Daily Steps") + theme(axis.text.x = element_text(angle = 90))

print(plot1)
####

par(mar = c(6, 5, 1.5, 2.1), mgp = c(3.75, 0.5, 0))
barplot(height = Daily_Summary$Steps, names = format(as.Date(Daily_Summary$date), "%d-%b-%Y"), main = "Daily Steps", cex.main = 0.8, ylab = "Steps", xlab = "date", cex.axis = 0.7, cex.lab = 0.8, cex.names = 0.6, las = 2)
abline(h = mean(Daily_Summary$Steps), col = "red", lty = 5, lwd = 2)
legend("top", c("Mean"), lty = 2, lwd = 1, cex = 0.7, col = c("red"))
########

Daily_Summary <- aggregate(list(Steps = Clean_Data$steps, Interval = Clean_Data$interval), by = list(date = Clean_Data$date), FUN = sum)

Intervals_Average <- aggregate(list(Avg_Steps = Raw_Data$steps), by = list(Interval = Raw_Data$interval), FUN = function(x) mean(x, na.rm = TRUE))

plot(Intervals_Average$Interval, Intervals_Average$Avg_Steps, type = "l", main = "Average Activity Pattern", ylab = "Avg_Steps", xlab = "Intervals", cex.axis = 0.7, xaxt = "n")
axis(side = 1, seq(0, 2355, 15), las = 2, cex.axis = 0.7)
abline(h = max(Intervals_Average$Avg_Steps), col = "red", lty = 1, lwd = 2)
abline(h = mean(Intervals_Average$Avg_Steps), col = "blue", lty = 5, lwd = 2)
abline(h = median(Intervals_Average$Avg_Steps), col = "black", lty = 2, lwd = 2)
legend("left", c(paste("Max = ", round(max(Intervals_Average$Avg_Steps), digits = 0), sep = ""), paste("Mean = ", round(mean(Intervals_Average$Avg_Steps), digits = 0), sep = ""), paste("Median = ", round(median(Intervals_Average$Avg_Steps), digits = 0), sep = "")), lty = c(1, 5, 2), lwd = 1, cex = 0.7, col = c("red", "blue", "black"))

paste("(1) The maximum number of steps, ", round(max(Intervals_Average$Avg_Steps), digits = 0), " daily average, are taken during the ", Intervals_Average$Interval[which.max(Intervals_Average$Avg_Steps)], "th interval", sep = "")

Intervals_Average$Interval[which.max(Intervals_Average$Avg_Steps)]

Round_Intervals_Average <- aggregate(list(Round_Intervals_Average = Raw_Data$steps), by = list(Interval = Raw_Data$interval), FUN = function(x) mean(x, na.rm = TRUE))
Round_Intervals_Average$Avg_Steps <- round(Round_Intervals_Average$Avg_Steps, digits = 0)

avg_step_vector <- round(Intervals_Average$Avg_Steps[match(Raw_Data$interval, Intervals_Average$Interval)], digits = 0)

Filled_Dataframe <- Raw_Data

#Filled_Dataframe$steps <- round(Intervals_Average$Avg_Steps[match(Raw_Data$interval, Intervals_Average$Interval)], digits = 0)

for (i in 1:length(Filled_Dataframe$steps)) {
  
  if(is.na(Filled_Dataframe$steps[i])) {
    
    Filled_Dataframe$steps[i] <- round(Intervals_Average$Avg_Steps[match(Filled_Dataframe$interval, Intervals_Average$Interval)], digits = 0)
    
  }
  
}

Full_Dataframe <- Filled_Dataframe
Full_Dataframe$weekday <- as.numeric(strftime(as.Date(Full_Dataframe$date, "%d-%b-%Y"), "%u"))
Full_Dataframe$day_or_end <- "weekday"


for (i in 1:length(Full_Dataframe$day_or_end)) {
  
  if(Full_Dataframe$weekday[i] == 6 | Full_Dataframe$weekday[i] == 7) {
    
    Full_Dataframe$day_or_end[i] <- "weekend"
    
  }
  
}


Intervals_Average_Full <- aggregate(list(Avg_Steps = Full_Dataframe$steps), by = list(Interval = Full_Dataframe$interval, Week_Day_or_End = Full_Dataframe$day_or_end), FUN = function(x) mean(x, na.rm = TRUE))

coplot(Avg_Steps ~ Interval | Week_Day_or_End, Intervals_Average_Full, show.given = TRUE, type = "l")

abline(h = max(Intervals_Average_Full$Avg_Steps), col = "red", lty = 1, lwd = 2)