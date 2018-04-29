shifts1 = read.csv("shifts1.csv", fileEncoding = "UTF-8-BOM")
shifts2 = read.csv("shifts2.csv", fileEncoding = "UTF-8-BOM")
customers = read.csv("customers.csv",fileEncoding = "UTF-8-BOM")
caregivers = read.csv("caregivers.csv",fileEncoding = "UTF-8-BOM")
locations = read.csv("location.csv",fileEncoding = "UTF-8-BOM")

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggmap)

# combining two shift files into a single file
shifts = rbind(shifts1, shifts2)

# data cleaning
    #converting revenue to numerical field

shifts$Revenue = as.numeric(as.character(shifts$Revenue))
shifts$Labor = as.numeric(as.character(shifts$Labor))

    #removing records with null bill amounts

shifts = shifts %>%
  filter(Revenue > 0)

customers$Birthday = mdy(customers$Birthday)
customers$FirstCarelogDate = mdy(customers$FirstCarelogDate)
customers$LastCarelogDate = mdy(customers$LastCarelogDate)
customers$CustomerDeceaseDate = mdy(customers$CustomerDeceaseDate)
customers$ClientDeactivatedDate = mdy(customers$ClientDeactivatedDate)

customers = customers %>%
  unite(col = "fulladdress", c(CustomerStreetAddr1,CustomerCityName, 
                               CustomerStateCode, CustomerPostalCode), 
        sep=", ",remove = F)

#removing clients without address data   

customers = customers %>%   
  filter(!CustomerStreetAddr1=="Unknown" | !CustomerCityName=="Unknown"
         | CustomerPostalCode>10000)

# combining location data to customer and caregiver tables

names(customers)[5] = paste("LocationKey")
names(caregivers)[4] = paste("LocationKey")
customers = customers %>%
  full_join(x = customers,y=locations, by="LocationKey")
caregivers = caregivers %>%
  full_join(x=caregivers, y=locations, by = "LocationKey")

# acquire geocode data for Santa Clara clients

SCgeolocation = customers[1:10,] %>%
  filter(LocationName=="Torrance")%>% 
  mutate(geolat = geocode(fulladdress)[,1],geolong = geocode(fulladdress)[,2])%>%
  select(CustomerKey,geolat,geolong)
  


