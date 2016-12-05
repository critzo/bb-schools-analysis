# Initial Data Analysis
require(ggplot2)
library(dplyr)

## Load cleaned data 
raw_and_calc_metrics_withmeta_cleaned = read.csv("<your system path to>/bb-schools-analysis/raw-data/ACPS-raw-data_metrics-calculated_with-metadata_cleaned.csv")

## Numerical Summaries

### Fig. 1 - Calculate the 25th and 75th Percentile Values for Each Metric
quantile(raw_and_calc_metrics_withmeta_cleaned$downloadThroughput, c(.25,.75),na.rm=TRUE)
quantile(raw_and_calc_metrics_withmeta_cleaned$uploadThroughput, c(.25,.75),na.rm=TRUE)

quantile(RoundTripTime$min_rtt, c(.25,.75))
quantile(PacketRetransmissionRate$packet_retransmission_rate, c(.25,.75))

### Fig 2 - Display Initial Summary Data for Each Metric
DL_initial_summary <- subset(DownloadThroughput, select=c(downloadThroughput,school,room,grade_level))
summary(DL_initial_summary)

UL_initial_summary <- subset(UploadThroughput, select=c(uploadThroughput,school,room,grade_level))
summary(UL_initial_summary)

RTT_initial_summary <- subset(RoundTripTime, select=c(min_rtt,school,room,grade_level))
summary(RTT_initial_summary)

PRR_initial_summary <- subset(PacketRetransmissionRate, select=c(packet_retransmission_rate,school,room,grade_level))
summary(PRR_initial_summary)

## Graphical Summaries

### Fig. 3 - Histogram of All Download Throughput Measurements
hist(DownloadThroughput$downloadThroughput, main=paste('Histogram of All Download Speed Measurements'), xlab=paste('Download Throughput, Mbps'))

### Fig. 4 - Kernel Density of All Download Throughput Measurements 
plot(density(DownloadThroughput$downloadThroughput),main=paste('Kernel Density of All Download Speed Measurements'))

### Fig. 5 - Indexed, Sorted Values of All Download Throughput Measurements
plot(sort(DownloadThroughput$downloadThroughput),pch=".",main=paste('Indexed, Sorted Values of All Download Speed Measurements'), ylab="Download Throughput", xlab="All Download Tests, Sorted by Download Speed")

### Fig. 6 - Histogram of All Upload Throughput Measurements
hist(UploadThroughput$uploadThroughput, main=paste('Histogram of All Upload Speed Measurements'), xlab=paste('Upload Throughput, Mbps'))

### Fig. 7 - Kernel Density of All Upload Throughput Measurements 
plot(density(UploadThroughput$uploadThroughput),main=paste('Kernel Density of All Upload Speed Measurements'))

### Fig. 8 - Indexed, Sorted Values of All Upload Throughput Measurements
plot(sort(UploadThroughput$uploadThroughput),pch=".",main=paste('Indexed, Sorted Values of All Upload Speed Measurements'), ylab="Upload Throughput", xlab="All Upload Tests, Sorted by Upload Speed")


### Fig. 9 - Histogram of All Minimum Round Trip Time Measurements
hist(RoundTripTime$min_rtt, main=paste('Histogram of All Round Trip Time Measurements'), xlab=paste('Minimum Round Trip Time, milliseconds'))

### Fig. 10 - Kernel Density of All Minimum Round Trip Time Measurements 
plot(density(RoundTripTime$min_rtt),main=paste('Kernel Density of All Round Trip Time Measurements'))

### Fig. 11 - Indexed, Sorted Values of All Minimum Round Trip Time Measurements
plot(sort(RoundTripTime$min_rtt),pch=".",main=paste('Indexed, Sorted Values of All Round Trip Time Measurements'), ylab="Round Trip Time", xlab="All Round Trip Time Measurements, Sorted by Round Trip Time")

### Fig. 12 - Histogram of All Packet Retransmission Rate Measurements
hist(PacketRetransmissionRate$packet_retransmission_rate, main=paste('Histogram of All Packet Retransmission Rate Measurements'), xlab=paste('Packet Retransmission Rate'))

### Fig. 13 - Kernel Density of All Packet Retransmission Rate Measurements 
plot(density(PacketRetransmissionRate$packet_retransmission_rate),main=paste('Kernel Density of All Packet Retransmission Rate Measurements'))

### Fig. 14 - Indexed, Sorted Values of All Packet Retransmission Rate Measurements
plot(sort(PacketRetransmissionRate$packet_retransmission_rate),pch=".",main=paste('Indexed, Sorted Values of All Packet Retransmission Rate Measurements'), ylab="Packet Retransmission Rate", xlab="All Packet Retransmission Rate Measurements, Sorted by Packet Retransmission Rate")

### Fig. 15 - Distribution of All Download Speed Measurements by Hour of the Day
print(qplot(DownloadThroughput$hour,DownloadThroughput$downloadThroughput,main="Distribution of All Download Speed Meaurements by Hour of the Day",xlab="Hour of the Day",ylab="Download Speed (Mbps)"))

### Fig. 16 - Distribution of Download Speeds by Hour of the Day, Faceted by School Type
ggplot(DownloadThroughput,aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(grade_level~.)+ggtitle("Download Speed Measurements by Grade Level, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Fig. 17 - Distribution of Download Speeds by Hour of the Day, Faceted by Room and School Type
ggplot(DownloadThroughput,aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~grade_level)+ggtitle("Download Speed Measurements by Hour, Faceted by Grade Level and Room")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Fig. 18 - Distribution of Download Speeds by Hour of the Day, Classrooms Only, Faceted by School Type
ggplot(filter(DownloadThroughput, room != 'help desk'),aes( x=hour,y=downloadThroughput))+geom_point()+facet_grid(grade_level~.)+ggtitle("Download Speed Measurements by Classroom, by Hour")+labs(x="Day of the Week",y="Download Throughput (Mbps)")

### Fig. 19 - Download Speed Measurements, High School IT Help Desk, by Hour
ggplot(filter(DownloadThroughput, room == 'help desk'),aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements, High School IT Help Desk, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Fig. 20 - Comparison of Elementary and High School Classrooms - Download Speeds by Hour
ggplot(filter(DownloadThroughput, room != 'help desk'),aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements by Classroom, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Fig. 21 - Distribution of All Upload Speed Measurements by Hour of the Day
ggplot(UploadThroughput,aes( x=hour,y=uploadThroughput))+geom_point()+ggtitle("Upload Speed Measurements by Classroom, by Hour")+labs(x="Hour of the Day",y="Upload Throughput (Mbps)")

### Fig. 22 - Distribution of Upload Speed Measurements by Hour of the Day, Classrooms Only, Faceted by School Type
ggplot(filter(UploadThroughput, room != 'help desk'),aes( x=hour,y=uploadThroughput))+geom_point()+facet_grid(grade_level~.)+ggtitle("Upload Speed Measurements by Classroom, by Hour")+labs(x="Hour of the Day",y="Upload Throughput (Mbps)")

### Fig. 23 - Distribution of Upload Speed Measurements by Hour of the Day, Help Desk
ggplot(filter(UploadThroughput, room == 'help desk'),aes( x=hour,y=uploadThroughput))+geom_point()+ggtitle("Upload Speed Measurements, Help Desk, by Hour")+labs(x="Hour of the Day",y="Upload Throughput (Mbps)")

### Fig. 24 - Distribution of Upload Speeds by Hour of the Day, Faceted by School Type
ggplot(UploadThroughput,aes( x=hour,y=uploadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~grade_level)+ggtitle("Upload Speed Measurements by Hour, Faceted by Grade Level and Room")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

# Numerical Data Summaries, limiting to classroom measurements only
summary(filter(DL_initial_summary,room != 'help desk'))
summary(filter(UL_initial_summary,room != 'help desk'))
summary(filter(RTT_initial_summary,room != 'help desk'))
summary(filter(PRR_initial_summary,room != 'help desk'))

##-- scratch pad below 

### Elementary and High School Classrooms only, by Weekday
ggplot(filter(DownloadThroughput, room != 'help desk'), aes(y=downloadThroughput, x=weekday)) + geom_bar(stat="identity")+facet_wrap(~ grade_level)


