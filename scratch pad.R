



### Fig. 1 - Total number of measurements, 25th and 75th Percentile Values

#### Download measurements       
length(which(!is.na(all_cleaned_data$downloadThroughput)))
quantile(all_cleaned_data$downloadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(classroom_measurements$downloadThroughput)))
quantile(classroom_measurements$downloadThroughput, c(.25,.75),na.rm=TRUE)
ggplot(filter(classroom_measurements),aes(x=grade_level, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + ggtitle("Distribution of Classroom Download Measurements \nby Grade Level, All Values ") + labs(x="Grade Level",y="Download Throughput (Mbps)")
ggplot(filter(classroom_measurements, downloadThroughput <= 91.66299 & downloadThroughput >=79.59180),aes(x=grade_level, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + ggtitle("Distribution of Classroom Download Measurements \nby Grade Level 25th-75th Percentile ") + labs(x="Grade Level",y="Download Throughput (Mbps)")

length(which(!is.na(elem_classroom_measurements$downloadThroughput)))
quantile(elem_classroom_measurements$downloadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(highschool_classroom_measurements$downloadThroughput)))
quantile(highschool_classroom_measurements$downloadThroughput, c(.25,.75),na.rm=TRUE)

length(which(!is.na(non_classroom_measurements$downloadThroughput)))
quantile(non_classroom_measurements$downloadThroughput, c(.25,.75),na.rm=TRUE)
ggplot(filter(non_classroom_measurements, downloadThroughput <= 91.66299 & downloadThroughput >=79.59180),aes(x=grade_level, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + ggtitle("Distribution of Non-Classroom Download \nMeasurements, 25th-75th Percentile ") + labs(x="Non-classroom Measurements",y="Download Throughput (Mbps)")
ggplot(non_classroom_measurements,aes(x=grade_level, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + ggtitle("Distribution of Non-Classroom \nDownload Measurements") + labs(x="Non-classroom Measurements",y="Download Throughput (Mbps)")

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
ggplot(filter(classroom_measurements, uploadThroughput <= 63.03321 & uploadThroughput >= 68.05603),aes(x=grade_level, y=uploadThroughput)) + geom_point(color="darkblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + ggtitle("Distribution of Classroom Upload Measurements \nby Grade Level 25th-75th Percentile ") + labs(x="Grade Level",y="Upload Throughput (Mbps)")
ggplot(filter(classroom_measurements),aes(x=grade_level, y=uploadThroughput)) + geom_point(color="darkblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + ggtitle("Distribution of Classroom Upload Measurements \nby Grade Level All Values ") + labs(x="Grade Level",y="Upload Throughput (Mbps)")
ggplot(filter(classroom_measurements, uploadThroughput <= 100 & uploadThroughput >= 68.05603),aes(x=grade_level, y=uploadThroughput)) + geom_point(color="darkblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + ggtitle("Distribution of Classroom Upload Measurements \nby Grade Level, 25th Percentile - 100 Mbps ") + labs(x="Grade Level",y="Upload Throughput (Mbps)")

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
#                             | 4717   0.002438402 0.005120240  |
#                             |                                 |
#                             | High School                     |
#                             | 1946   0.001590311 0.002965130  |

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
#                             | 4717       0.9763309 0.9823665  |
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
#                                   |---------------------------------|
#                                   | Elementary                      |
#                                   | 4717  0.0006715249 0.0014013503 |
#                                   |                                 |
#                                   | High School                     |
#                                   | 1946  0.0006375036 0.0011517160 |

### Fig 2 - Numerical Summaries for Each Metric - All Data; Aggregate by Classrooms, Classroom Level, Non-Classrooms

#### Numerical Summaries - All Measurements
summary(subset(all_cleaned_data,select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

# downloadThroughput uploadThroughput     minRTT           avgRTT       packetRetransRate NetworkLimRatio ClientLimRatio 
# Min.   :  0.328    Min.   :  1.36   Min.   : 1.000   Min.   : 2.703   Min.   :0.000     Min.   :0.502   Min.   :0.000  
# 1st Qu.: 79.592    1st Qu.: 63.03   1st Qu.: 3.000   1st Qu.: 4.919   1st Qu.:0.002     1st Qu.:0.976   1st Qu.:0.001  
# Median : 88.428    Median : 63.35   Median : 3.000   Median : 6.914   Median :0.003     Median :0.980   Median :0.001  
# Mean   :108.901    Mean   :100.28   Mean   : 4.599   Mean   : 7.099   Mean   :0.003     Mean   :0.977   Mean   :0.002  
# 3rd Qu.: 91.663    3rd Qu.: 68.06   3rd Qu.: 4.000   3rd Qu.: 8.220   3rd Qu.:0.005     3rd Qu.:0.982   3rd Qu.:0.001  
# Max.   :662.860    Max.   :329.96   Max.   :28.000   Max.   :28.432   Max.   :0.225     Max.   :0.986   Max.   :0.480  
# NA's   :9088       NA's   :7608                      NA's   :9088     NA's   :9088      NA's   :9088    NA's   :9088  

#### Numerical Summaries - Classroom Measurements
summary(subset(classroom_measurements,select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

# downloadThroughput uploadThroughput     minRTT           avgRTT       packetRetransRate NetworkLimRatio ClientLimRatio 
# Min.   :  0.328    Min.   :  1.36   Min.   : 1.000   Min.   : 2.753   Min.   :0.000     Min.   :0.502   Min.   :0.000
# 1st Qu.: 77.364    1st Qu.: 62.99   1st Qu.: 3.000   1st Qu.: 5.272   1st Qu.:0.002     1st Qu.:0.976   1st Qu.:0.001
# Median : 87.496    Median : 63.29   Median : 3.000   Median : 7.170   Median :0.003     Median :0.980   Median :0.001 
# Mean   : 89.827    Mean   : 76.75   Mean   : 4.702   Mean   : 7.456   Mean   :0.004     Mean   :0.977   Mean   :0.002
# 3rd Qu.: 90.852    3rd Qu.: 63.49   3rd Qu.: 5.000   3rd Qu.: 9.634   3rd Qu.:0.005     3rd Qu.:0.982   3rd Qu.:0.001
# Max.   :596.606    Max.   :328.29   Max.   :28.000   Max.   :28.432   Max.   :0.225     Max.   :0.986   Max.   :0.480  
# NA's   :7947       NA's   :6663                      NA's   :7947     NA's   :7947      NA's   :7947    NA's   :7947  

#### Numerical Summaries - Elementary Classroom Measurements
summary(subset(elem_classroom_measurements,select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

# downloadThroughput uploadThroughput     minRTT           avgRTT       packetRetransRate NetworkLimRatio ClientLimRatio 
# Min.   : 0.328     Min.   : 1.36    Min.   : 1.000   Min.   : 3.799   Min.   :0.000     Min.   :0.512   Min.   :0.000  
# 1st Qu.:78.710     1st Qu.:62.92    1st Qu.: 3.000   1st Qu.: 6.662   1st Qu.:0.002     1st Qu.:0.976   1st Qu.:0.001  
# Median :88.247     Median :63.20    Median : 4.000   Median : 7.475   Median :0.004     Median :0.980   Median :0.001  
# Mean   :81.252     Mean   :63.20    Mean   : 4.843   Mean   : 8.073   Mean   :0.004     Mean   :0.977   Mean   :0.002  
# 3rd Qu.:91.030     3rd Qu.:63.38    3rd Qu.: 8.000   3rd Qu.:10.221   3rd Qu.:0.005     3rd Qu.:0.982   3rd Qu.:0.001  
# Max.   :92.446     Max.   :68.55    Max.   :28.000   Max.   :28.432   Max.   :0.225     Max.   :0.986   Max.   :0.471  
# NA's   :5686       NA's   :4717                      NA's   :5686     NA's   :5686      NA's   :5686    NA's   :5686   

#### Numerical Summaries - High School Classroom Measurements
summary(subset(highschool_classroom_measurements,select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

# downloadThroughput uploadThroughput     minRTT           avgRTT       packetRetransRate NetworkLimRatio  ClientLimRatio  
# Min.   :  5.166    Min.   : 49.03   Min.   : 1.000   Min.   : 2.753   Min.   :0.0001    Min.   :0.5024   Min.   :0.0000  
# 1st Qu.: 74.827    1st Qu.: 63.33   1st Qu.: 3.000   1st Qu.: 4.635   1st Qu.:0.0016    1st Qu.:0.9748   1st Qu.:0.0006  
# Median : 85.446    Median : 67.94   Median : 3.000   Median : 5.146   Median :0.0022    Median :0.9797   Median :0.0009  
# Mean   :110.613    Mean   :110.82   Mean   : 4.353   Mean   : 5.963   Mean   :0.0026    Mean   :0.9756   Mean   :0.0026  
# 3rd Qu.: 89.558    3rd Qu.: 68.52   3rd Qu.: 4.000   3rd Qu.: 5.830   3rd Qu.:0.0030    3rd Qu.:0.9820   3rd Qu.:0.0012  
# Max.   :596.606    Max.   :328.29   Max.   :12.000   Max.   :12.587   Max.   :0.0421    Max.   :0.9856   Max.   :0.4798  
# NA's   :2261       NA's   :1946                      NA's   :2261     NA's   :2261      NA's   :2261     NA's   :2261    

#### Numerical Summaries - Non-Classroom Measurements
summary(subset(non_classroom_measurements,select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

# downloadThroughput uploadThroughput     minRTT           avgRTT       packetRetransRate NetworkLimRatio  ClientLimRatio  
# Min.   : 14.17     Min.   :135.4    Min.   : 2.000   Min.   : 2.703   Min.   :0.0000    Min.   :0.6357   Min.   :0.0000  
# 1st Qu.:185.47     1st Qu.:219.6    1st Qu.: 3.000   1st Qu.: 3.466   1st Qu.:0.0009    1st Qu.:0.9738   1st Qu.:0.0003  
# Median :234.43     Median :283.3    Median : 3.000   Median : 3.750   Median :0.0014    Median :0.9795   Median :0.0006  
# Mean   :243.38     Mean   :264.2    Mean   : 3.877   Mean   : 4.580   Mean   :0.0016    Mean   :0.9743   Mean   :0.0012  
# 3rd Qu.:286.91     3rd Qu.:294.0    3rd Qu.: 4.000   3rd Qu.: 4.348   3rd Qu.:0.0021    3rd Qu.:0.9818   3rd Qu.:0.0010  
# Max.   :662.86     Max.   :330.0    Max.   :13.000   Max.   :11.885   Max.   :0.0194    Max.   :0.9850   Max.   :0.0260  
# NA's   :1141       NA's   :945                       NA's   :1141     NA's   :1141      NA's   :1141     NA's   :1141   




### Fig. 18 - Distribution of All Download Speed Measurements by Hour of the Day
print(qplot(all_cleaned_data$hour,all_cleaned_data$downloadThroughput,main="Distribution of All Download Speed Meaurements by Hour of the Day",xlab="Hour of the Day",ylab="Download Speed (Mbps)"))

### Fig. 19 - Distribution of Download Speeds by Hour of the Day, Faceted by School Type
ggplot(all_cleaned_data,aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(grade_level~.)+ggtitle("Download Speed Measurements by Grade Level, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")


### Fig. 21 - Distribution of Download Speeds by Hour of the Day, High School, Faceted by Room
ggplot(filter(all_cleaned_data, grade_level != 'elementary'),aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements by Hour - High School")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Fig. 22 - Download Speed Measurements, High School IT Help Desk, by Hour
ggplot(filter(all_cleaned_data, room == 'help desk'),aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements, High School IT Help Desk, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")

### Fig. 23 - Comparison of Elementary and High School Classrooms - Download Speeds by Hour
ggplot(filter(all_cleaned_data, room != 'help desk'),aes( x=hour,y=downloadThroughput))+geom_smooth(se=F)+geom_point()+facet_grid(room~.)+ggtitle("Download Speed Measurements by Classroom, by Hour")+labs(x="Hour of the Day",y="Download Throughput (Mbps)")


### Fig. 25 - Distribution of All Upload Speed Measurements by Hour of the Day
ggplot(all_cleaned_data,aes( x=hour,y=uploadThroughput))+geom_point()+ggtitle("Upload Speed Measurements by Hour")+labs(x="Hour of the Day",y="Upload Throughput (Mbps)")



### Fig. 28 - Distribution of Upload Speed Measurements by Hour of the Day, Help Desk
ggplot(filter(all_cleaned_data, room == 'help desk'),aes( x=hour,y=uploadThroughput))+geom_point()+ggtitle("Upload Speed Measurements, Help Desk, by Hour")+labs(x="Hour of the Day",y="Upload Throughput (Mbps)")


ggplot(filter(classroom_measurements, downloadThroughput <= 100),aes(x=grade_level, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + ggtitle("Distribution of Classroom Download \nMeasurements by Grade Level <= 100 Mbps") + labs(x="Grade Level",y="Download Throughput (Mbps)")

### Box & Whisker Plot - All Download Measurements
ggplot(all_cleaned_data, aes(x=room, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + coord_flip() +ggtitle("Distribution of Download Measurements, All Data")

### Box & Whisker Plot - All Classroom Download Measurements
ggplot(classroom_measurements, aes(x=room, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + coord_flip()

### Box & Whisker Plot - All Elementary Classroom Download Measurements
ggplot(elem_classroom_measurements, aes(x=room, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + coord_flip()

### Box & Whisker Plot - All High School Classroom Download Measurements
ggplot(highschool_classroom_measurements, aes(x=downloadThroughput, y=datetime_EST)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + coord_flip()

### Box & Whisker Plot - All High School Classroom Download Measurements below 100 Mbps
ggplot(filter(highschool_classroom_measurements,downloadThroughput <= 100), aes(x=room, y=downloadThroughput)) + geom_point(color="lightblue", alpha=0.1, position="jitter") + geom_boxplot(outlier.size=0, alpha=0.2) + coord_flip()


##-- scratch pad below 

## Comparing Numeric Summaries of Throttled vs Non-Throttled Measurements

# Numeric Summary - Non-Throttled Measurements - High School Help Desk
summary(subset(filter(all_cleaned_data, room == 'help desk'),select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate,NetworkLimRatio,ClientLimRatio)))

# downloadThroughput uploadThroughput     minRTT           avgRTT       packetRetransRate NetworkLimRatio  ClientLimRatio  
# Min.   : 14.17     Min.   :135.4    Min.   : 2.000   Min.   : 2.703   Min.   :0.0000    Min.   :0.6357   Min.   :0.0000  
# 1st Qu.:185.47     1st Qu.:219.6    1st Qu.: 3.000   1st Qu.: 3.466   1st Qu.:0.0009    1st Qu.:0.9738   1st Qu.:0.0003  
# Median :234.43     Median :283.3    Median : 3.000   Median : 3.750   Median :0.0014    Median :0.9795   Median :0.0006  
# Mean   :243.38     Mean   :264.2    Mean   : 3.877   Mean   : 4.580   Mean   :0.0016    Mean   :0.9743   Mean   :0.0012  
# 3rd Qu.:286.91     3rd Qu.:294.0    3rd Qu.: 4.000   3rd Qu.: 4.348   3rd Qu.:0.0021    3rd Qu.:0.9818   3rd Qu.:0.0010  
# Max.   :662.86     Max.   :330.0    Max.   :13.000   Max.   :11.885   Max.   :0.0194    Max.   :0.9850   Max.   :0.0260  
# NA's   :1141       NA's   :945                       NA's   :1141     NA's   :1141      NA's   :1141     NA's   :1141    

# Numeric Summary - Throttled Measurements
summary(subset(filter(all_cleaned_data, room != 'help desk' & room != 'B323'),select = c(downloadThroughput,uploadThroughput,minRTT,avgRTT,packetRetransRate)))

# downloadThroughput uploadThroughput     minRTT           avgRTT       packetRetransRate
# Min.   : 0.328     Min.   : 1.36    Min.   : 1.000   Min.   : 3.799   Min.   :0.000    
# 1st Qu.:78.710     1st Qu.:62.92    1st Qu.: 3.000   1st Qu.: 6.662   1st Qu.:0.002    
# Median :88.247     Median :63.20    Median : 4.000   Median : 7.475   Median :0.004    
# Mean   :81.252     Mean   :63.20    Mean   : 4.843   Mean   : 8.073   Mean   :0.004    
# 3rd Qu.:91.030     3rd Qu.:63.38    3rd Qu.: 8.000   3rd Qu.:10.221   3rd Qu.:0.005    
# Max.   :92.446     Max.   :68.55    Max.   :28.000   Max.   :28.432   Max.   :0.225    
# NA's   :5686       NA's   :4717                      NA's   :5686     NA's   :5686     







# Latency / throughput
qplot(downloadThroughput,minRTT,data=filter(all_cleaned_data,minRTT <=20 & downloadThroughput >= 100))+geom_smooth(method="lm")
qplot(downloadThroughput,avgRTT,data=filter(all_cleaned_data,avgRTT <=20 & downloadThroughput >= 100))+geom_smooth(method="lm")
qplot(uploadThroughput,minRTT,data=filter(all_cleaned_data,minRTT <=20 & uploadThroughput >= 100))+geom_smooth(method="lm")



## Download Speeds / MinRTT by classroom - Level 3 Upstream transit only
qplot(downloadThroughput,minRTT,data=filter(all_cleaned_data,minRTT <=20 & downloadThroughput <= 100 & server_ip=="4.35.238.203"))+facet_wrap(~room)+ggtitle("Download Speeds and MinRTT, Transit through Level 3\n Faceted by Room")






### Elementary and High School Classrooms only, by Weekday
ggplot(filter(DownloadThroughput, room != 'help desk'), aes(y=downloadThroughput, x=weekday)) + geom_bar(stat="identity")+facet_wrap(~ grade_level)






