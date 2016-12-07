# Initial Data Analysis
require(ggplot2)
library(dplyr)

## Load cleaned data 
all_cleaned_data = read.csv("<your system path to>/bb-schools-analysis/raw-data/ACPS-raw-data_metrics-calculated_with-metadata_cleaned.csv")

## Create separate data frames for classroom and non-classroom measurements, elementary and high school measurements 
classroom_measurements <- subset(filter(all_cleaned_data,room!='help desk'))
non_classroom_measurements <- subset(filter(all_cleaned_data,room=='help desk'))
elem_classroom_measurements <- subset(filter(all_cleaned_data,room!='help desk' & grade_level=='elementary'))
highschool_classroom_measurements <- subset(filter(all_cleaned_data,room!='help desk' & grade_level=='high school'))

## Numerical Summaries

### Fig. 1 - Total number of measurements, 25th and 75th Percentile Values

#### Download measurements       
length(which(!is.na(all_cleaned_data$downloadThroughput)))
quantile(all_cleaned_data$downloadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(classroom_measurements$downloadThroughput)))
quantile(classroom_measurements$downloadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(elem_classroom_measurements$downloadThroughput)))
quantile(elem_classroom_measurements$downloadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(highschool_classroom_measurements$downloadThroughput)))
quantile(highschool_classroom_measurements$downloadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(non_classroom_measurements$downloadThroughput)))
quantile(non_classroom_measurements$downloadThroughput, c(.25,.75),na.rm=TRUE)

# All     25th      75th      | Classrooms  25th      75th      | Non-Classrooms  25th      75th  
# 7608    79.59180  91.66299  | 6663        77.36369  90.85208  | 945             185.4724  286.9072
#                             |---------------------------------|
#                             | Elementary                      |
#                             | 4717        78.70964  91.02981  |
#                             |                                 |
#                             | High School                     |
#                             | 1946        74.82725  89.55848  |

#### Upload measurements
length(which(!is.na(all_cleaned_data$uploadThroughput)))
quantile(all_cleaned_data$uploadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(classroom_measurements$uploadThroughput)))
quantile(classroom_measurements$uploadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(elem_classroom_measurements$uploadThroughput)))
quantile(elem_classroom_measurements$uploadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(highschool_classroom_measurements$uploadThroughput)))
quantile(highschool_classroom_measurements$uploadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(non_classroom_measurements$uploadThroughput)))
quantile(non_classroom_measurements$uploadThroughput, c(.25,.75),na.rm=TRUE)

# All     25th      75th      | Classrooms  25th      75th      | Non-Classrooms  25th      75th  
# 9088    63.03321  68.05603  | 7947        62.98748  63.48879  | 1141            219.6216  293.9745
#                             |---------------------------------|
#                             | Elementary                      |
#                             | 5686        62.92426  63.38304  |
#                             |                                 |
#                             | High School                     |
#                             | 2261        63.33436  68.51997  |

#### Average RTT measurements
length(which(!is.na(all_cleaned_data$avgRTT)))
quantile(all_cleaned_data$avgRTT, c(.25,.75),na.rm=TRUE)

length(which(!is.na(classroom_measurements$avgRTT)))
quantile(classroom_measurements$avgRTT, c(.25,.75),na.rm=TRUE)

length(which(!is.na(elem_classroom_measurements$avgRTT)))
quantile(elem_classroom_measurements$avgRTT, c(.25,.75),na.rm=TRUE)

length(which(!is.na(highschool_classroom_measurements$avgRTT)))
quantile(highschool_classroom_measurements$avgRTT, c(.25,.75),na.rm=TRUE)

length(which(!is.na(non_classroom_measurements$avgRTT)))
quantile(non_classroom_measurements$avgRTT, c(.25,.75),na.rm=TRUE)

# All     25th      75th      | Classrooms  25th      75th      | Non-Classrooms  25th      75th  
# 7608    4.918611  8.219872  | 6663        5.272273  9.633781  | 945             3.465868  4.348356
#                             |---------------------------------|
#                             | Elementary                      |
#                             | 4717        6.661709 10.220621  |
#                             |                                 |
#                             | High School                     |
#                             | 1946        4.634802 5.829468   |

#### Minimum RTT measurements
length(which(!is.na(all_cleaned_data$minRTT)))
quantile(all_cleaned_data$minRTT, c(.25,.75),na.rm=TRUE)

length(which(!is.na(classroom_measurements$minRTT)))
quantile(classroom_measurements$minRTT, c(.25,.75),na.rm=TRUE)

length(which(!is.na(elem_classroom_measurements$minRTT)))
quantile(elem_classroom_measurements$minRTT, c(.25,.75),na.rm=TRUE)

length(which(!is.na(highschool_classroom_measurements$minRTT)))
quantile(highschool_classroom_measurements$minRTT, c(.25,.75),na.rm=TRUE)

length(which(!is.na(non_classroom_measurements$minRTT)))
quantile(non_classroom_measurements$minRTT, c(.25,.75),na.rm=TRUE)

# All     25th      75th      | Classrooms  25th      75th      | Non-Classrooms  25th      75th  
# 16696   3         4         | 14610       3         5         | 2086            3         4
#                             |---------------------------------|
#                             | Elementary                      |
#                             | 10403       3         8         |
#                             |                                 |
#                             | High School                     |
#                             | 4207        3         4         |

#### Packet Retransmission Rate
length(which(!is.na(all_cleaned_data$packetRetransRate)))
quantile(all_cleaned_data$packetRetransRate, c(.25,.75),na.rm=TRUE)

length(which(!is.na(classroom_measurements$packetRetransRate)))
quantile(classroom_measurements$packetRetransRate, c(.25,.75),na.rm=TRUE)

length(which(!is.na(elem_classroom_measurements$packetRetransRate)))
quantile(elem_classroom_measurements$packetRetransRate, c(.25,.75),na.rm=TRUE)

length(which(!is.na(highschool_classroom_measurements$packetRetransRate)))
quantile(highschool_classroom_measurements$packetRetransRate, c(.25,.75),na.rm=TRUE)

length(which(!is.na(non_classroom_measurements$packetRetransRate)))
quantile(non_classroom_measurements$packetRetransRate, c(.25,.75),na.rm=TRUE)

# All     25th      75th      | Classrooms  25th      75th      | Non-Classrooms  25th      75th  
# 7608    4.918611  8.219872  | 6663  0.002073605  0.004796193  | 945         0.0008766026  0.0020511029 
#                             |---------------------------------|
#                             | Elementary                      |
#                             | 4717   0.002438402 0.005120240       |
#                             |                                 |
#                             | High School                     |
#                             | 1946   0.001590311 0.002965130     |

#### Network Limited Ratio
length(which(!is.na(all_cleaned_data$NetworkLimRatio)))
quantile(all_cleaned_data$NetworkLimRatio, c(.25,.75),na.rm=TRUE)

length(which(!is.na(classroom_measurements$NetworkLimRatio)))
quantile(classroom_measurements$NetworkLimRatio, c(.25,.75),na.rm=TRUE)

length(which(!is.na(elem_classroom_measurements$NetworkLimRatio)))
quantile(elem_classroom_measurements$NetworkLimRatio, c(.25,.75),na.rm=TRUE)

length(which(!is.na(highschool_classroom_measurements$NetworkLimRatio)))
quantile(highschool_classroom_measurements$NetworkLimRatio, c(.25,.75),na.rm=TRUE)

length(which(!is.na(non_classroom_measurements$NetworkLimRatio)))
quantile(non_classroom_measurements$NetworkLimRatio, c(.25,.75),na.rm=TRUE)

# All     25th      75th      | Classrooms  25th      75th      | Non-Classrooms  25th      75th  
# 7608    0.9755999 0.9821918 | 6663        0.9759522 0.9822473 | 945             0.9737537 0.9818442
#                             |---------------------------------|
#                             | Elementary                      |
#                             | 4717       0.9763309 0.9823665   |
#                             |                                 |
#                             | High School                     |
#                             | 1946       0.9748452 0.9819561  |

#### Client Limited Ratio
length(which(!is.na(all_cleaned_data$ClientLimRatio)))
quantile(all_cleaned_data$ClientLimRatio, c(.25,.75),na.rm=TRUE)

length(which(!is.na(classroom_measurements$ClientLimRatio)))
quantile(classroom_measurements$ClientLimRatio, c(.25,.75),na.rm=TRUE)

length(which(!is.na(elem_classroom_measurements$ClientLimRatio)))
quantile(elem_classroom_measurements$ClientLimRatio, c(.25,.75),na.rm=TRUE)

length(which(!is.na(highschool_classroom_measurements$ClientLimRatio)))
quantile(highschool_classroom_measurements$ClientLimRatio, c(.25,.75),na.rm=TRUE)

length(which(!is.na(non_classroom_measurements$ClientLimRatio)))
quantile(non_classroom_measurements$ClientLimRatio, c(.25,.75),na.rm=TRUE)

# All     25th        75th          | Classrooms  25th      75th      | Non-Classrooms  25th      75th  
# 7608  0.0006143714  0.0012038161  | 6663  0.0006625815  0.0012654174| 945         0.000339913   0.001013628
#                             |---------------------------------|
#                             | Elementary                      |
#                             | 4717    0.0006715249 0.0014013503    |
#                             |                                 |
#                             | High School                     |
#                             | 1946    0.0006375036 0.0011517160 |

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


