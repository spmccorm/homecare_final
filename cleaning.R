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

#converting date values using lubridate

customers$Birthday = mdy(customers$Birthday)
customers$FirstCarelogDate = mdy(customers$FirstCarelogDate)
customers$LastCarelogDate = mdy(customers$LastCarelogDate)
customers$CustomerDeceaseDate = mdy(customers$CustomerDeceaseDate)
customers$ClientDeactivatedDate = mdy(customers$ClientDeactivatedDate)
shifts$DateKeyService = ymd(shifts$DateKeyService)

#concatenating customer address information into a single field

customers = customers %>%
  unite(col = "customerfulladdress", c(CustomerStreetAddr1,CustomerCityName, 
                                       CustomerStateCode, CustomerPostalCode), 
        sep=", ",remove = F)

#converting employee address data from factor to character

shifts$EmployeeCityName = as.character(shifts$EmployeeCityName)
shifts$EmployeeStreetAddr1 = as.character(shifts$EmployeeStreetAddr1)
shifts$EmployeeStateCode = as.character(shifts$EmployeeStateCode)
shifts$EmployeePostalCode = as.character(shifts$EmployeePostalCode)

#concatenating employee address information into a single field

caregivers = caregivers %>%
  unite(col = "cgfulladdress", c(EmployeeStreetAddr1,
                                 EmployeeCityName,
                                 EmployeeStateCode,
                                EmployeePostalCode),sep = ", ",remove=F)


#removing clients without address data

customers = customers %>%
  filter(!CustomerStreetAddr1=="Unknown" | !CustomerCityName=="Unknown" | 
           !is.na(CustomerStreetAddr1) | !is.na(CustomerCityName)
         | CustomerPostalCode>10000, Birthday>ymd("1900,1,1"), 
         CustomerGender == "M" | CustomerGender =="F" )

#combining location data to shift table

names(shifts)[21] = paste("LocationKey")
shifts = shifts %>%
  full_join(x=shifts, y=locations, by = "LocationKey")

#connecting customer information to shift table

shifts = shifts %>%
  filter(!is.na(LocationName))
shifts = shifts %>%
  full_join(x=shifts, y=customers, by="CustomerKey")

#removing shifts that do not have a valid customer

shifts = shifts %>%
  filter(Birthday>mdy("1,1,1900")) %>%
  filter(!is.na(CustomerKey))

#connecting caregivers to shift table

names(shifts)[4] = paste("EmployeeKey")

shifts = shifts %>%
  full_join(x=shifts, y=caregivers, by="EmployeeKey") %>%
  filter(EmployeeKey>10000)

#removes shifts that do not have valid CG addresses

shifts = shifts %>%
  filter(EmployeeStreetAddr1 !="Unknown") %>%
  filter(EmployeeCityName != "Unknown") %>%
  filter(EmployeeStateCode!="Unknown") %>%
  filter(EmployeePostalCode!="Unknown")

#extracting 2017 shift data

shifts$DateKeyService = ymd(shifts$DateKeyService)

shifts = shifts %>%
  filter(year(DateKeyService) >= "2017")

#extracting list of caregivers and clients in order to find geolocation

caregiverlocations = shifts %>%
  select(EmployeeKey, EmployeeStreetAddr1,
         EmployeeCityName, EmployeeStateCode,
         EmployeePostalCode)

customerlocations = shifts %>% 
  select(CustomerKey, customerfulladdress)

caregiverlocations = unique(caregiverlocations)
customerlocations = unique(customerlocations)

#running geocode function
  # as the following functions proved to be unsuccessful when run
  # in large batches, we extracted the customer and caregiver address data
  # (after extensive cleaning) and used an online resource to derive 
  # geo coordinates

    #caregiver geocode formula that we attempted in R

      "caregiverlocations = caregiverlocations %>%
        mutate(geolon = geocode(cgfulladdress)[,1],
               geolat = geocode(cgfulladdress)[,2])"

    #customer geocode formula that we attempted in R

      "customerlocations = customerlocations %>%
        mutate(geolon = geocode(customerfulladdress)[,1],
               geolat = geocode(customerfulladdress)[,2])"
#writing address data to csvs so we can get geocodes externally

    #write.csv(customerlocations, file = "customerlocations.csv"")
    #write.csv(caregiverlocations, file = "caregiverlocations.csv")

#importing collected geocodes

customerlocations = read.csv("customergeocodes.csv", fileEncoding = "UTF-8-BOM")
caregiverlocations = read.csv("caregivergeocodes.csv", fileEncoding = "UTF-8-BOM")

#connecting geocode data to shifts table

shifts = shifts %>%
  left_join(x=shifts,y=caregiverlocations, by="EmployeeKey")

shifts = shifts %>%
  left_join(x=shifts, y=customerlocations,by="CustomerKey")

#removing customers & caregivers that did not have "clean" 
    # addresses (no geocodes found) 

shifts = shifts %>%
  filter(!is.na(customerlat)) %>%
  filter(!is.na(employeelon)) %>%
  filter(employeelat != "") %>%
  arrange(employeelat)

#calculate distances from geocode data using the "Haversine Formula"

shifts = shifts %>% 
  mutate(Distance = distHaversine(p1 = cbind(shifts$employeelon,shifts$employeelat),                                  p2=cbind(shifts$customerlon,shifts$customerlat))*0.000621371) %>%
  filter(Distance < 50)

cgdistance = shifts %>%
  group_by(EmployeeKey) %>%
  summarise(avgdist = mean(Distance))

shifts = shifts %>%
  left_join(x=shifts, y=cgdistance, by= "EmployeeKey")

#calculating customer lifetime value

customerLTV= shifts %>%
  group_by(CustomerKey) %>%
  summarise(customerLTV=sum(Revenue))

shifts = shifts %>%
  left_join(x=shifts, y=customerLTV, by="CustomerKey")

# calculating length of stay

lengthofstay = shifts %>%
  group_by(CustomerKey) %>%
  summarise(startdate = min(DateKeyService),
            enddate = max(DateKeyService)) %>%
  mutate(lengthofstay = (enddate - startdate)+1) %>%
  filter(lengthofstay >= 0 ) %>%
  select(CustomerKey, lengthofstay)

shifts = shifts %>%
  left_join(x=shifts, y=lengthofstay, by="CustomerKey")

shifts$lengthofstay = as.numeric(shifts$lengthofstay)

#calculating average monthly revenue using length of stay

shifts = shifts %>%
  mutate(avgmonthlyrev = customerLTV/lengthofstay*30)

#export shift data for use in application and analysis script

write.csv(shifts, "shifts.csv")

