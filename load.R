fileUrl<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
zipFile <-'activity.zip'   
fileName<-'activity.csv'  
bin <- getBinaryURL(fileUrl,ssl.verifypeer=FALSE)
con <- file(zipFile, open = "wb")
writeBin(bin, con)
close(con)
setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d"))  
data <- read.csv(unz(zipFile, fileName), header=TRUE, 
                 na.strings="NA",colClasses=c("date"="myDate")) 
summary_data<-
    data %>%
    group_by(date) %>% 
    summarise(total.steps=sum(steps,na.rm=TRUE))
meanTotal<-
     summary_data %>%
        summarize(mean(total.steps))
library(ggplot2)
library(scales)
ggplot(summary_data, aes(x=date)) + geom_histogram(binwidth=30,colour="white")+
    scale_x_date(labels =date_format("%Y-%b"),
                 breaks = seq(min(summary_data$date)-5,
                              max(summary_data$date)+5, 30))+
    ylab("Total Steps per day") +xlab("Date") + 
    theme_bw() + theme(axis.text.x =element_text(angle=90))
sprintf("%s%.2f","Mean total number of steps taken per day=",meanTotal)

library(lubridate)
newdata<-mutate(data,datetime=as.POSIXct(date+dminutes(interval)))