gdpdata<-read.csv("economy-and-growth-raw-2021.csv")
countrydata<-read.csv("countrymetadata.csv")
incomedata<-read.csv("income.csv")
library(tidyverse)

## econ data
data1<- gdpdata %>% 
  filter(Indicator.Code %in% c("NY.GDP.PCAP.CD",
                               "NV.SRV.TOTL.ZS", 
                               "NV.IND.TOTL.ZS",
                               "NV.AGR.TOTL.ZS",
                               "NE.TRD.GNFS.ZS")) %>% 
  select(-"X2020") %>% 
  drop_na() %>% 
  pivot_longer(cols=starts_with("X"), 
               values_to = "Indicator.Value", names_to = "Year") %>% 
  mutate(Year = as.numeric(str_sub(Year, 2, -1))) %>% 
  select(Country.Name, Country.Code, Indicator.Code, Year, Indicator.Value) %>% 
  filter(!Country.Name %in% c("Africa Eastern and Southern", "Africa Western and Central",
                               "Caribbean small states", "East Asia & Pacific (excluding high income)",
                               "Early-demographic dividend", "East Asia & Pacific",
                               "Euro area", "Fragile and conflict affected situations",
                               "High income", "Heavily indebted poor countries (HIPC)",
                               "IBRD only", "IDA & IBRD total", "IDA total", 
                               "IDA blend", "IDA only", "Latin America & Caribbean (excluding high income)",
                               "Latin America & Caribbean", "Lower middle income",                                 
                               "Low & middle income", "Late-demographic dividend",
                               "Middle income", "North America", "OECD members",
                               "Pre-demographic dividend", "Post-demographic dividend",
                               "Sub-Saharan Africa (excluding high income)",          
                               "Sub-Saharan Africa", "East Asia & Pacific (IDA & IBRD countries)",
                               "Latin America & the Caribbean (IDA & IBRD countries)",
                               "South Asia (IDA & IBRD)",                             
                               "Sub-Saharan Africa (IDA & IBRD countries)",
                               "Upper middle income", "World",
                              "South Asia"
                               )) %>% 
  left_join(countrydata %>% select(Country.Code, Region), by=join_by(Country.Code)) 
  
  
## region and income class data
data2<-gdpdata %>% 
  filter(Indicator.Code == "NY.GDP.PCAP.CD") %>% 
  pivot_longer(cols=starts_with("X"), 
               values_to = "countrygni", names_to = "Year") %>% 
  mutate(Year = as.numeric(str_sub(Year, 2, -1))) %>% 
  left_join(incomedata, by=join_by("Year")) %>% 
  drop_na() %>% 
  mutate(
    GNIPerCap = as.numeric(GNIPerCap),
    incomeclass = ifelse(countrygni > GNIPerCap, "High Income", "Low Income")
  ) %>% 
  select(Country.Code, Year, incomeclass)

combineddata<-left_join(data1,data2, by=c("Country.Code","Year"))

fulldatacountries<-as.vector(combineddata %>% 
  group_by(Country.Code) %>% 
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  distinct(Country.Code))$Country.Code

finaldata<- combineddata %>% 
  filter(Country.Code %in% fulldatacountries)



write.csv(finaldata, "dsproject3data.csv")
