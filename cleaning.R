library(dplyr)
library(tidyverse)
library(lubridate)
library(ggmap)
library(geosphere)

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

shifts$EmployeeCityName = as.character(shifts$EmployeeCityName)
shifts$EmployeeStreetAddr1 = as.character(shifts$EmployeeStreetAddr1)
shifts$EmployeeStateCode = as.character(shifts$EmployeeStateCode)
shifts$EmployeePostalCode = as.character(shifts$EmployeePostalCode)


caregivers = caregivers %>%
  unite(col = "cgfulladdress", c(EmployeeStreetAddr1,
                                 EmployeeCityName,
                                 EmployeeStateCode,
                                EmployeePostalCode),sep = ", ",
        remove=F)


#removing clients without address data

customers = customers %>%
  filter(!CustomerStreetAddr1=="Unknown" | !CustomerCityName=="Unknown" | 
           !is.na(CustomerStreetAddr1) | !is.na(CustomerCityName)
         | CustomerPostalCode>10000, Birthday>ymd("1900,1,1"), 
         CustomerGender == "M" | CustomerGender =="F" )



# combining location data to shift table

names(shifts)[21] = paste("LocationKey")
shifts = shifts %>%
  left_join(x=shifts, y=locations, by = "LocationKey")
shifts$LocationName = as.character(shifts$LocationName)

# Connecting customer information to shift table
shifts = shifts %>%
  filter(!is.na(LocationName))
shifts = shifts %>%
  left_join(x=shifts, y=customers, by="CustomerKey")

# removing shifts that do not have a valid customer

shifts = shifts %>%
  filter(Birthday>mdy("1,1,1900")) %>%
  filter(!is.na(CustomerKey))

# connecting caregivers to shift table

names(shifts)[4] = paste("EmployeeKey")

shifts = shifts %>%
  left_join(x=shifts, y=caregivers, by="EmployeeKey") %>%
  filter(EmployeeKey>10000)

# removes shifts that do not have valid CG addresses

shifts = shifts %>%
  filter(EmployeeStreetAddr1 !="Unknown") %>%
  filter(EmployeeCityName != "Unknown") %>%
  filter(EmployeeStateCode!="Unknown") %>%
  filter(EmployeePostalCode!="Unknown")


# extracting 2017 shift data

shifts$DateKeyService = ymd(shifts$DateKeyService)

shifts = shifts %>%
  filter(year(DateKeyService) >= "2017")

# Extracting list of caregivers and clients in order to find geolocation

caregiverlocations = shifts %>%
  select(EmployeeKey, EmployeeStreetAddr1,
         EmployeeCityName, EmployeeStateCode,
         EmployeePostalCode)

customerlocations = shifts %>% 
  select(CustomerKey, customerfulladdress)

caregiverlocations = unique(caregiverlocations)
customerlocations = unique(customerlocations)

# running geocode function
  # as the following functions proved to be unsuccessful when run
  # in large batches, we extracted the customer and caregiver address data
  # (after extensive cleaning) and used an online resource to derive 
  # geo coordinates

    # caregivers

"caregiverlocations = caregiverlocations %>%
  mutate(geolon = geocode(cgfulladdress)[,1],
         geolat = geocode(cgfulladdress)[,2])"

    # customers

"customerlocations = customerlocations %>%
  mutate(geolon = geocode(customerfulladdress)[,1],
         geolat = geocode(customerfulladdress)[,2])"

  # writing address data to csvs so we can get geocodes 

#write.csv(customerlocations, file = "customerlocations.csv")
#write.csv(caregiverlocations, file = "caregiverlocations.csv")

# importing collected geocodes

customerlocations = read.csv("customergeocodes.csv", fileEncoding = "UTF-8-BOM")
caregiverlocations = read.csv("caregivergeocodes.csv", fileEncoding = "UTF-8-BOM")

# Connecting geocode data to shifts table

shifts = shifts %>%
  left_join(x=shifts,y=caregiverlocations, by="EmployeeKey")

shifts = shifts %>%
  left_join(x=shifts, y=customerlocations,by="CustomerKey")

# Removing customers & caregivers that did not have "clean" 
    # addresses (no geocodes found) 

shifts = shifts %>%
  filter(!is.na(customerlat)) %>%
  filter(!is.na(employeelon)) %>%
  filter(employeelat != "") %>%
  arrange(employeelat)

# Calculate distances from geocode data using the "Haversine Formula"

# This webside explains the calculation of the haversine distance
    #https://andrew.hedges.name/experiments/haversine/
"Earth_R=3961
dlon = shifts$customerlon - shifts$employeelon 
dlat = shifts$customerlat - shifts$employeelat 
a = (sin(dlat/2))^2 + cos(shifts$employeelat) * cos(shifts$customerlat) * (sin(dlon/2))^2 
c = 2 * atan2( sqrt(a), sqrt(1-a) ) 
d = Earth_R * c #where Earth_R is the radius of the Earth"

shifts = shifts %>% 
  mutate(Distance = distHaversine(p1 = cbind(shifts$employeelon, shifts$employeelat), 
                       p2=cbind(shifts$customerlon,shifts$customerlat))*0.000621371) %>%
  filter(Distance < 50)

# calculating average distance from cg to their clients

cgdistance = shifts %>%
  group_by(EmployeeKey) %>%
  summarise(avgdist = mean(Distance))

shifts = shifts %>%
  left_join(x=shifts, y=cgdistance, by= "EmployeeKey")

# calculating Lifetime Value of Customers and Caregivers

cgLTV=shifts %>%
  group_by(EmployeeKey) %>%
  summarise(cgLTV=sum(Revenue))

shifts = shifts %>%
  left_join(x=shifts, y=cgLTV, by="EmployeeKey")

customersLTV= shifts %>%
  group_by(CustomerKey) %>%
  summarise(customerLTV=sum(Revenue))

shifts = shifts %>%
  left_join(x=shifts, y=customersLTV, by="CustomerKey")

#calculating length of stay in order to derive average monthly revenue

lengthofstaycustomers = shifts %>%
  group_by(CustomerKey) %>%
  summarise(startdate = min(DateKeyService), enddate = max(DateKeyService)) %>%
  mutate(lengthofstaycustomer = as.numeric(enddate - startdate +1)) %>%
  select(CustomerKey, lengthofstaycustomer)

shifts = shifts %>%
  left_join(x=shifts, y=lengthofstaycustomers, by = "CustomerKey")

lengthofstaycaregivers =shifts %>%
  group_by(EmployeeKey)%>%
  summarise(startdate = min(DateKeyService), enddate = max(DateKeyService)) %>%
  mutate(lengthofstaycg = as.numeric(enddate - startdate +1)) %>%
  select(EmployeeKey,lengthofstaycg)

shifts = shifts %>%
  left_join(x=shifts, y=lengthofstaycaregivers, by ="EmployeeKey")

#calculating average monthly revenue per customer and cg

shifts = shifts %>%
  mutate(avgmonthlyrevcust = (customerLTV/lengthofstaycustomer)*30)

shifts = shifts %>%
  mutate(avgmonthlyrevcg = (cgLTV/lengthofstaycg)*30)

shifts = shifts %>%
  mutate(Location = ifelse(LocationName=="Walnut Creek","WalnutCreek",
                           ifelse(LocationName=="Santa Clara", "SantaClara",
                                  ifelse(LocationName=="San Diego", "SanDiego",
                                         ifelse(LocationName=="Culver City", "CulverCity",
                                         LocationName)))))

shifts = shifts %>%
  select(-LocationName)

names(shifts)[59] = paste("LocationName")

santaclarashifts = shifts %>%
  filter(LocationName=="SantaClara")

# Export shift data for use in applications and analysis script

write.csv(shifts, "shifts.csv")


