# Initial Data Analysis

## Numerical Summaries

### Calculate the 25th and 75th Percentile Values for Each Metric
quantile(DownloadThroughput$downloadThroughput, c(.25,.75))
quantile(UploadThroughput$uploadThroughput, c(.25,.75))
quantile(RoundTripTime$min_rtt, c(.25,.75))
quantile(PacketRetransmissionRate$packet_retransmission_rate, c(.25,.75))

### Display Initial Summary Data for Each Metric
DL_initial_summary <- subset(DownloadThroughput, select=c(downloadThroughput,school,room,grade_level))
summary(DL_initial_summary)

UL_initial_summary <- subset(UploadThroughput, select=c(uploadThroughput,school,room,grade_level))
summary(UL_initial_summary)

RTT_initial_summary <- subset(RoundTripTime, select=c(min_rtt,school,room,grade_level))
summary(RTT_initial_summary)

PRR_initial_summary <- subset(PacketRetransmissionRate, select=c(packet_retransmission_rate,school,room,grade_level))
summary(PRR_initial_summary)

## Graphical Summaries

### Histogram of All Download Speed Measurements
hist(DownloadThroughput$downloadThroughput, main=paste('Histogram of All Download Speed Measurements'), xlab=paste('Download Throughput, Mbps'))

### Kernel Density of All Download Speed Measurements
plot(density(DownloadThroughput$downloadThroughput),main=paste('Kernel Density of All Download Speed Measurements'))

### Indexed, Sorted Values of All Download Speed Measurements
plot(sort(DownloadThroughput$downloadThroughput),pch=".",main=paste('Indexed, Sorted Values of All Download Speed Measurements'), ylab="Download Throughput", xlab="All Download Tests, Sorted by Download Speed")

### Distribution of All Download Speed Measurements by Hour of the Day
require(ggplot2)
print(qplot(DownloadThroughput$hour,DownloadThroughput$downloadThroughput,main="Distribution of All Download Speed Meaurements by Hour of the Day",xlab="Hour of the Day",ylab="Download Speed (Mbps)"))

### Comparison of School Type - Download Speeds by Hour of the Day
ggplot(DownloadThroughput,aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(grade_level~.)+ggtitle("Download Speed Measurements by Grade Level, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Comparison of Schools - Download Speeds by Hour of the Day
ggplot(DownloadThroughput,aes( x=hour,y=downloadThroughput))+geom_point()+facet_grid(~school)+ggtitle("Download Measurements by Hour, Comparing by School")+labs(y="Download Throughput (Mbps)", x="Hour of the Day")

### Comparison of Classrooms and Help Desk - Download Speeds by Hour of the Day
ggplot(DownloadThroughput,aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements by Classroom, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Comparison of Elementary and High School Classrooms - Download Speeds by Hour
ggplot(filter(DownloadThroughput, room != 'help desk'),aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements by Classroom, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Elementary and High School Classrooms only, by Weekday
ggplot(filter(DownloadThroughput, room != 'help desk'), aes(y=downloadThroughput, x=weekday)) + geom_bar(stat="identity")+facet_wrap(~ grade_level)

### Comparison of Elementary and High School Download Speed Measurements by Day of the Week
ggplot(filter(DownloadThroughput, room != 'help desk'),aes( x=weekday,y=downloadThroughput))+geom_point()+facet_grid(grade_level~.)+ggtitle("Download Speed Measurements by Classroom, by Hour")+labs(x="Day of the Week",y="Download Throughput (Mbps)")

### Download Speed Measurements, High School IT Help Desk, by Hour
ggplot(filter(DownloadThroughput, room == 'help desk'),aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements, High School IT Help Desk, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

