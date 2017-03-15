# Initial Data Analysis - OTI Measuring Broadband in Schools

## Project Setup

### Load required R packages
require(ggplot2)
library(dplyr)

### Import all cleaned data 
all_cleaned_data = read.csv("raw-data/ACPS-raw-data_metrics-calculated_with-metadata_cleaned.csv")

## Numeric Summaries

### Numeric Summary - All Locations
summary(subset(all_cleaned_data,select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

### Numeric Summary - QoS Limited Measurements - Classrooms (Except HS B323)
summary(subset(filter(all_cleaned_data, room != 'help desk' & room != 'B323'),select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

### Numeric Summary - Non-QoS Limited Measurements - High School Help Desk
summary(subset(filter(all_cleaned_data, room == 'help desk'),select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

## Graphical Summaries

### Histograms of Download Throughput Measurements: All, Classrooms, Non-classrooms
hist(all_cleaned_data$downloadThroughput, main=paste('Histogram of All Download Speed Measurements'), xlab=paste('Download Throughput, Mbps'))
hist(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$downloadThroughput, main=paste('Histogram of Classroom Download Speed Measurements'), xlab=paste('Download Throughput, Mbps'))
hist(filter(all_cleaned_data, room == 'help desk')$downloadThroughput, main=paste('Histogram of Non-Classroom Download Speed Measurements'), xlab=paste('Download Throughput, Mbps'))

### Kernel Density Plots of Download Throughput Measurements: All, Classrooms, Non-classrooms 
plot(density(all_cleaned_data$downloadThroughput, na.rm=TRUE), main=paste('Kernel Density of All Download Speed Measurements'))
plot(density(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$downloadThroughput, na.rm=TRUE), main=paste('Kernel Density of Classroom \nDownload Speed Measurements'))
plot(density(filter(all_cleaned_data, room == 'help desk')$downloadThroughput, na.rm=TRUE), main=paste('Kernel Density of Non-Classroom \nDownload Speed Measurements'))

### Indexed, Sorted Values Plots of Download Throughput Measurements: All, Classrooms, Non-classrooms
plot(sort(all_cleaned_data$downloadThroughput),pch=".",main=paste('Indexed, Sorted Values of All Download Speed Measurements'), ylab="Download Throughput", xlab="All Download Tests, Sorted by Download Speed")
plot(sort(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$downloadThroughput),pch=".",main=paste('Indexed, Sorted Values of Classroom \nDownload Speed Measurements'), ylab="Download Throughput", xlab="Classroom Download Tests, Sorted by Download Speed")
plot(sort(filter(all_cleaned_data, room == 'help desk')$downloadThroughput),pch=".",main=paste('Indexed, Sorted Values of Non-Classroom \nDownload Speed Measurements'), ylab="Download Throughput", xlab="Non-classroom Download Tests, Sorted by Download Speed")

### Histograms of All Upload Throughput Measurements: All, Classrooms, Non-classrooms
hist(all_cleaned_data$uploadThroughput, main=paste('Histogram of All Upload Speed Measurements'), xlab=paste('Upload Throughput, Mbps'))
hist(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$uploadThroughput, main=paste('Histogram of Classroom \nUpload Speed Measurements'), xlab=paste('Upload Throughput, Mbps'))
hist(filter(all_cleaned_data, room == 'help desk')$uploadThroughput, main=paste('Histogram of Non-classroom \nUpload Speed Measurements'), xlab=paste('Upload Throughput, Mbps'))

### Kernel Density Plots of Upload Throughput Measurements: All, Classrooms, Non-classrooms
plot(density(all_cleaned_data$uploadThroughput, na.rm=TRUE),main=paste('Kernel Density of All \nUpload Speed Measurements'))
plot(density(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$uploadThroughput, na.rm=TRUE),main=paste('Kernel Density of Classroom \nUpload Speed Measurements'))
plot(density(filter(all_cleaned_data, room == 'help desk')$uploadThroughput, na.rm=TRUE),main=paste('Kernel Density of Non-classroom \nUpload Speed Measurements'))

### Indexed, Sorted Values Plots of Upload Throughput Measurements: All, Classrooms, Non-classrooms
plot(sort(all_cleaned_data$uploadThroughput),pch=".",main=paste('Indexed, Sorted Values of All Upload Speed Measurements'), ylab="Upload Throughput", xlab="All Upload Tests, Sorted by Upload Speed")
plot(sort(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$uploadThroughput),pch=".",main=paste('Indexed, Sorted Values of Classroom \nUpload Speed Measurements'), ylab="Upload Throughput", xlab="Classroom Upload Tests, Sorted by Upload Speed")
plot(sort(filter(all_cleaned_data, room == 'help desk')$uploadThroughput),pch=".",main=paste('Indexed, Sorted Values of Non-Classroom \nUpload Speed Measurements'), ylab="Upload Throughput", xlab="Non-classroom Upload Tests, Sorted by Upload Speed")

### Histogram of Minimum Round Trip Time Measurements: All, Classrooms, Non-classrooms
hist(all_cleaned_data$minRTT, main=paste('Histogram of All \nMin. Round Trip Time Measurements'), xlab=paste('Minimum Round Trip Time (ms)'))
hist(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$minRTT, main=paste('Histogram of Classroom \nMin. Round Trip Time Measurements'), xlab=paste('Minimum Round Trip Time (ms)'))
hist(filter(all_cleaned_data, room == 'help desk')$minRTT, main=paste('Histogram of Non-classroom \nMin. Round Trip Time Measurements'), xlab=paste('Minimum Round Trip Time (ms)'))

### Kernel Density of Minimum Round Trip Time Measurements: All, Classrooms, Non-classrooms
plot(density(all_cleaned_data$minRTT),main=paste('Kernel Density of All \nMin. Round Trip Time Measurements'))
plot(density(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$minRTT),main=paste('Kernel Density of Classroom \nMin. Round Trip Time Measurements'))
plot(density(filter(all_cleaned_data, room == 'help desk')$minRTT),main=paste('Kernel Density of Non-classroom \nMin. Round Trip Time Measurements'))

### Indexed, Sorted Values of Minimum Round Trip Time Measurements: All, Classrooms, Non-classrooms
plot(sort(all_cleaned_data$minRTT),pch=".",main=paste('Indexed, Sorted Values of All \nMin. Round Trip Time Measurements'), ylab="Min. Round Trip Time", xlab="All Min. Round Trip Time Measurements, Sorted")
plot(sort(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$minRTT),pch=".",main=paste('Indexed, Sorted Values of Classroom \nMin. Round Trip Time Measurements'), ylab="Min. Round Trip Time", xlab="Min. RTT Measurements, Sorted")
plot(sort(filter(all_cleaned_data, room == 'help desk')$minRTT),pch=".",main=paste('Indexed, Sorted Values of Non-classroom \nMin. Round Trip Time Measurements'), ylab="Min. Round Trip Time", xlab="Min. RTT Measurements, Sorted")

### Histogram of Average Round Trip Time Measurements: All, Classrooms, Non-classrooms
hist(all_cleaned_data$avgRTT, main=paste('Histogram of All \nAvg. Round Trip Time Measurements'), xlab=paste('Average Round Trip Time (ms)'))
hist(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$avgRTT, main=paste('Histogram of Classroom \nAvg. Round Trip Time Measurements'), xlab=paste('Average Round Trip Time (ms)'))
hist(filter(all_cleaned_data, room == 'help desk')$avgRTT, main=paste('Histogram of Non-classroom \nAvg. Round Trip Time Measurements'), xlab=paste('Average Round Trip Time (ms)'))

### Kernel Density of Average Round Trip Time Measurements: All, Classrooms, Non-classrooms
plot(density(all_cleaned_data$avgRTT, na.rm=TRUE),main=paste('Kernel Density of All \nAvg. Round Trip Time Measurements'))
plot(density(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$avgRTT, na.rm=TRUE),main=paste('Kernel Density of Classroom \nAvg. Round Trip Time Measurements'))
plot(density(filter(all_cleaned_data, room == 'help desk')$avgRTT, na.rm=TRUE),main=paste('Kernel Density of Non-classroom \nAvg. Round Trip Time Measurements'))

### Indexed, Sorted Values of Average Round Trip Time Measurements: All, Classroms Classrooms, Non-classrooms
plot(sort(all_cleaned_data$avgRTT),pch=".",main=paste('Indexed, Sorted Values of All \nAvg. Round Trip Time Measurements'), ylab="Avg. Round Trip Time", xlab="Avg. Round Trip Time Measurements, Sorted")
plot(sort(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$avgRTT),pch=".",main=paste('Indexed, Sorted Values of Classroom \nAvg. Round Trip Time Measurements'), ylab="Avg. Round Trip Time", xlab="Avg. RTT Measurements, Sorted")
plot(sort(filter(all_cleaned_data, room == 'help desk')$avgRTT),pch=".",main=paste('Indexed, Sorted Values of Non-classroom \nAvg. Round Trip Time Measurements'), ylab="Avg. Round Trip Time", xlab="Avg. RTT Measurements, Sorted")

### Histogram of Packet Retransmission Rate Measurements: All, Classrooms, Non-classrooms
hist(all_cleaned_data$packetRetransRate, main=paste('Histogram of All Packet Retransmission Rate Measurements'), xlab=paste('Packet Retransmission Rate'))
hist(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$packetRetransRate, main=paste('Histogram of Classroom \nPacket Retransmission Rate Measurements'), xlab=paste('Packet Retransmission Rate'))
hist(filter(all_cleaned_data, room == 'help desk')$packetRetransRate, main=paste('Histogram of Non-classroom \nPacket Retransmission Rate Measurements'), xlab=paste('Packet Retransmission Rate'))

### Kernel Density of Packet Retransmission Rate Measurements: All, Classrooms, Non-classrooms 
plot(density(all_cleaned_data$packetRetransRate,na.rm=TRUE),main=paste('Kernel Density of All \nPacket Retransmission Rate Measurements'))
plot(density(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$packetRetransRate,na.rm=TRUE),main=paste('Kernel Density of Classroom \nPacket Retransmission Rate Measurements'))
plot(density(filter(all_cleaned_data, room == 'help desk')$packetRetransRate,na.rm=TRUE),main=paste('Kernel Density of Non-classroom \nPacket Retransmission Rate Measurements'))

### Indexed, Sorted Values of Packet Retransmission Rate Measurements: All, Classrooms, Non-classrooms
plot(sort(all_cleaned_data$packetRetransRate),pch=".",main=paste('Indexed, Sorted Values of All \nPacket Retransmission Rate Measurements'), ylab="Packet Retransmission Rate", xlab="All Packet Retransmission Rate Measurements, Sorted")
plot(sort(filter(all_cleaned_data, room != 'help desk' & room != 'B323')$packetRetransRate),pch=".",main=paste('Indexed, Sorted Values of Classroom \nPacket Retransmission Rate Measurements'), ylab="Packet Retransmission Rate", xlab="Packet Retransmission Rate Measurements, Sorted")
plot(sort(filter(all_cleaned_data, room == 'help desk')$packetRetransRate),pch=".",main=paste('Indexed, Sorted Values of Non-classroom \nPacket Retransmission Rate Measurements'), ylab="Packet Retransmission Rate", xlab="Packet Retransmission Rate Measurements, Sorted")


## Charts Referenced in Measuring Broadband in Schools Paper

### Figure 1 - Download Speed Measurements by Hour, Faceted by Grade Level and Room
ggplot(all_cleaned_data,aes(x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~grade_level)+ggtitle("Download Speed Measurements by Hour, Faceted by Grade Level and Room")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Figure 2 - High School Classroom B323 - Download Speeds by Hour
ggplot(filter(all_cleaned_data, room == 'B323'),aes(x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements, \nHigh School Classroom B323, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Figure 3 - Upload Speed Measurements by Hour of the Day, Classrooms Only, Faceted by School Type
ggplot(filter(all_cleaned_data, room != 'help desk' & uploadThroughput <= 100),aes( x=hour,y=uploadThroughput))+ylim(0,100)+geom_smooth(se=F)+geom_point()+facet_grid(grade_level~.)+ggtitle("Upload Speed Measurements by Classroom, by Hour, \nLimited to Measurements Below 100 Mbps")+labs(x="Hour of the Day",y="Upload Throughput (Mbps)")

### Figure 4 - QoS Limited Download Speed Measurements by Hour
ggplot(filter(all_cleaned_data, room != 'help desk' & room != 'B323'),aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+ggtitle("QoS Limited Download Speed Measurements by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Figure 5 - Non-QoS Limited Download Speed Measurements by Hour
ggplot(filter(all_cleaned_data, room == 'help desk'),aes(x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+ggtitle("Non-QoS Limited Download Speed Measurements by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Figure 6 - QoS Limited Upload Speed Measurements by Hour
ggplot(filter(all_cleaned_data, room != 'help desk' & room != 'B323'),aes( x=hour,y=uploadThroughput))+geom_smooth(se=F)+geom_point()+ggtitle("QoS Limited Upload Speed Measurements by Hour")+labs(x="Hour of the Day",y="Upload Throughput (Mbps)")

### Figure 7 - Non-QoS Limited Upload Speed Measurements by Hour
ggplot(filter(all_cleaned_data, room == 'help desk'),aes( x=hour,y=uploadThroughput))+geom_smooth(se=F)+geom_point()+ggtitle("Non-QoS Limited Upload Speed Measurements by Hour")+labs(x="Hour of the Day",y="Upload Throughput (Mbps)")

### Figure 8 - QoS Limited Download Measurements Indexed by MinRTT <= 20ms
qplot(downloadThroughput,minRTT,data=filter(all_cleaned_data,minRTT <=20 & downloadThroughput <= 100))+ggtitle("QoS Limited Download Measurements \n Indexed by MinRTT <= 20ms")+labs(x="Download Speed (mbps)", y="Minimum Round Trip Time (ms)")

### Figure 9 - Non-QoS Limited Download Measurements Indexed by MinRTT <= 20ms
qplot(downloadThroughput,minRTT,data=filter(all_cleaned_data, room == 'help desk' & minRTT <= 20))+ggtitle("Non-QoS Limited Download Measurements \n Indexed by MinRTT <= 20ms")+labs(x="Download Speed (mbps)", y="Minimum Round Trip Time (ms)")

### Figure 10 - QoS Limited Upload Measurements Indexed by MinRTT <= 20ms
qplot(uploadThroughput,minRTT,data=filter(all_cleaned_data,minRTT <=20 & uploadThroughput <= 75 & uploadThroughput >= 45))+ggtitle("QoS Limited Upload Measurements Indexed by Minimum RTT <= 20ms")+labs(x="Upload Speed (mbps)\n >= 45 mbps", y="Minimum Round Trip Time (ms) \n <= 20 ms")

### Figure 11 - Non-QoS Limited Upload Measurements Indexed by MinRTT <= 20ms
qplot(uploadThroughput,minRTT,data=filter(all_cleaned_data, room == 'help desk' & minRTT <= 20))+ggtitle("Non-QoS Limited Upload Measurements \n Indexed by MinRTT <= 20ms")+labs(x="Upload Speed (mbps)", y="Minimum Round Trip Time (ms)")

### Figure 12 - Average RTT / Minimum RTT for QoS Limited Download Measurements
qplot(avgRTT,minRTT,data=filter(all_cleaned_data,minRTT <= 20 & avgRTT <=15 & downloadThroughput <= 100))+ggtitle("Average RTT / Minimum RTT\n for QoS Limited Download Measurements")+labs(x="Average RTT <= 15 ms", y="Minimum RTT <= 20 ms")

### Add "Transit" column and populate with known M-Lab upstream transit providers by IP
#### Add 'transit' column to the all_cleaned_data dataframe
all_cleaned_data$transit <- 0

#### Create an index of the unique server IPs
transit_lookup_index <- c("216.156.197.139","38.90.140.139","66.198.10.139","173.205.4.11","173.205.4.24","4.35.238.203")

#### Create an index of transit providers connected to the server IPs
transit_lookup_values <- c("XO","Cogent","Tata","GTT","GTT","Level3")

#### Use a match function to populate the 'transit' column for all rows
all_cleaned_data$transit <- transit_lookup_values[match(all_cleaned_data$server_ip, transit_lookup_index)]

### Figure 13 - QoS Limited Download Speeds and MinRTT, Faceted by M-Lab Server/Upstream Transit Provider
qplot(downloadThroughput,minRTT,data=filter(all_cleaned_data, room != 'help desk' & room != 'B323'))+facet_wrap(~transit)+ggtitle("QoS Limited Download Speeds and MinRTT\n Faceted by M-Lab Server/Upstream Transit Provider")

### Figure 14 - Non-QoS Limited Download Speeds and MinRTT, Faceted by M-Lab Server/Upstream Transit Provider
qplot(downloadThroughput,minRTT,data=filter(all_cleaned_data, room == 'help desk'))+facet_wrap(~transit)+ggtitle("Non-QoS Limited Download Speeds and MinRTT\n Faceted by M-Lab Server/Upstream Transit Provider")+labs(x='Download Throughput', y='Min RTT')

### Figure 15 - QoS Limited Upload Speeds and MinRTT, Faceted by M-Lab Server/Upstream Transit Provider
qplot(uploadThroughput,minRTT,data=filter(all_cleaned_data, room != 'help desk' & room != 'B323'))+facet_wrap(~transit)+ggtitle("QoS Limited Upload Speeds and MinRTT\n Faceted by M-Lab Server/Upstream Transit Provider")+labs(x='Upload Throughput', y='Min RTT')

### Figure 16 - Non-QoS Limited Upload Speeds and MinRTT, Faceted by M-Lab Server/Upstream Transit Provider
qplot(uploadThroughput,minRTT,data=filter(all_cleaned_data, room == 'help desk'))+facet_wrap(~transit)+ggtitle("Non-QoS Limited Upload Speeds and MinRTT\n Faceted by M-Lab Server/Upstream Transit Provider")+labs(x='Upload Throughput', y='Min RTT')
