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

### 
plot(DownloadThroughput$hour,DownloadThroughput$downloadThroughput, main="All Download Speed Measurements by Hour of the Day", xlab="Hour of the Day", ylab="Download Speed (Mbps)")
require(ggplot2)
print(qplot(DownloadThroughput$hour,DownloadThroughput$downloadThroughput,main="Distribution of All Download Speed Meaurements by Hour of the Day",xlab="Hour of the Day",ylab="Download Speed (Mbps)"))
