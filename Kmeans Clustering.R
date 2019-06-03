##clustering the uber demand into boroughs of NYC
# Load the .csv files
##these files are from web
apr14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-apr14.csv")
may14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-may14.csv")
jun14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jun14.csv")
jul14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv")
aug14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-aug14.csv")
sep14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-sep14.csv")


library(dplyr)
data14 <- bind_rows(apr14, may14, jun14, jul14, aug14, sep14)
head(data14,10)

summary(data14)
write.csv(data14,"dataclus.csv")

##  Date.Time : the date and time of the Uber pickup;
##Lat: the latitude of the Uber pickup;
##Lon: the longitude of the Uber pickup;
##Base: the TLC base company code affiliated with the Uber pickup. 
  
library(lubridate)

# Separate or mutate the Date/Time columns
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))
getwd()

head(data14)

##now performuing the k mean clusters
set.seed(20)
##we just need lat and long to cluster the data into 5 clusters
clusters <- kmeans(data14[,2:3], 5)

summary(clusters)

# Save the cluster number in the dataset as column 'Borough'
data14$Borough <- as.factor(clusters$cluster)
head(data14)


# Inspect 'clusters'
str(clusters)


##now we can get the summary of those additioanl data points along with the clusters
head(data14)
library(sqldf)
sqldf("select month, borough, count(*) from data14 group by 1,2")



##now studying the clusters bby visualizing them 
##facing errors resolving this part of the code
library(ggmap)
register_google(key = "YOUR_API_KEY")
has_google_key()

NYCMap <- get_map("New York")
ggmap(NYCMap) + geom_point(aes(x = Lon[], y = Lat[], colour = as.factor(Borough)),data = data14) +
  ggtitle("NYC Boroughs using KMean")

