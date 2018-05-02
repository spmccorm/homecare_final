library(dplyr)
library(tidyverse)
library(lubridate)
library(ggmap)
#devtools::install_github("dkahle/ggmap")
#https://developers.google.com/maps/documentation/geocoding/get-api-key
#https://stackoverflow.com/questions/36175529/getting-over-query-limit-after-one-request-with-geocode?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
#register_google(key = "AIzaSyArX9t9nSUl4E9es8qRald_HNlcc8VeLJo")

# loading the data

shifts1 = read.csv("shifts1.csv", fileEncoding = "UTF-8-BOM")
shifts2 = read.csv("shifts2.csv", fileEncoding = "UTF-8-BOM")
customers = read.csv("customers.csv",fileEncoding = "UTF-8-BOM")
caregivers = read.csv("caregivers.csv",fileEncoding = "UTF-8-BOM")
locations = read.csv("location.csv",fileEncoding = "UTF-8-BOM")

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
  unite(col = "customerfulladdress", c(CustomerStreetAddr1,CustomerCityName, 
                                       CustomerStateCode, CustomerPostalCode), 
        sep=", ",remove = F)

#removing clients without address data

customers = customers %>%
  filter(!CustomerStreetAddr1=="Unknown" | !CustomerCityName=="Unknown" | 
           !is.na(CustomerStreetAddr1) | !is.na(CustomerCityName)
         | CustomerPostalCode>10000, Birthday>ymd("1900,1,1"), 
         CustomerGender == "M" | CustomerGender =="F" )

# combining location data to shift table

names(shifts)[21] = paste("LocationKey")
shifts = shifts %>%
  full_join(x=shifts, y=locations, by = "LocationKey")

# Connecting customer information to shift table
shifts = shifts %>%
  filter(!is.na(LocationName))
shifts = shifts %>%
  full_join(x=shifts, y=customers, by="CustomerKey")

# removing shifts that do not have a valid customer

shifts = shifts %>%
  filter(Birthday>mdy("1,1,1900")) %>%
  filter(!is.na(CustomerKey))

# connecting caregivers to shift table

names(shifts)[4] = paste("EmployeeKey")

shifts = shifts %>%
  full_join(x=shifts, y=caregivers, by="EmployeeKey") %>%
  filter(EmployeeKey>10000)

shifts$EmployeeCityName = as.character(shifts$EmployeeCityName)
shifts$EmployeeStreetAddr1 = as.character(shifts$EmployeeStreetAddr1)
shifts$EmployeeStateCode = as.character(shifts$EmployeeStateCode)
shifts$EmployeePostalCode = as.character(shifts$EmployeePostalCode)

shifts = shifts %>%
  filter(EmployeeStreetAddr1 !="Unknown") %>%
  filter(EmployeeCityName != "Unknown") %>%
  filter(EmployeeStateCode!="Unknown") %>%
  filter(EmployeePostalCode!="Unknown")

# concatenating caregiver address information

shifts = shifts %>%
  unite(col = "cgfulladdress", c(CustomerStreetAddr1,CustomerCityName, 
                                 CustomerStateCode, CustomerPostalCode), 
        sep=", ",remove = F)

# extracting 2017 shift data

shifts$DateKeyService = ymd(shifts$DateKeyService)

shifts = shifts %>%
  filter(year(DateKeyService) >= "2017")

# Extracting list of caregivers and clients in order to find geolocation

caregiverlocations = shifts %>%
  select(EmployeeKey, cgfulladdress)

customerlocations = shifts %>% 
  select(CustomerKey, customerfulladdress)

caregiverlocations = unique(caregiverlocations)
customerlocations = unique(customerlocations)

# running geocode function

# Cosimo

caregiverlocationscosimo = caregiverlocations[1:6000,] %>%
  mutate(geolon = geocode(cgfulladdress)[,1],
         geolat = geocode(cgfulladdress)[,2])

# Sean

caregiverlocationssean = caregiverlocations[6001:7000,] %>% #6001:12000 total
  mutate(geolon = geocode(cgfulladdress)[,1],
         geolat = geocode(cgfulladdress)[,2])

# Sean 2

caregiverlocationssean = caregiverlocationssean %>%
  mutate(geolon1 = ifelse(is.na(geolon), geocode(cgfulladdress)[,1],geolon),
         geolat1 = ifelse(is.na(geolat), geocode(cgfulladdress)[,2], geolat))
write.csv(caregiverlocationssean, file = "caregiverlocationssean.csv")

# Bruce

caregiverlocationsbruce = caregiverlocations[12001:18000,] %>%
  mutate(geolon = geocode(cgfulladdress)[,1],
         geolat = geocode(cgfulladdress)[,2])

# Lisi

caregiverlocationslisi = caregiverlocations[18001:24000,] %>%
  mutate(geolon = geocode(cgfulladdress)[,1],
         geolat = geocode(cgfulladdress)[,2])

# Della

caregiverlocationsdella = caregiverlocations[24001:24964,] %>%
  mutate(geolon = geocode(cgfulladdress)[,1],
         geolat = geocode(cgfulladdress)[,2])

customerlocations = customerlocations %>%
  mutate(geolon = geocode(customerfulladdress)[,1],
         geolat = geocode(customerfulladdress)[,2])