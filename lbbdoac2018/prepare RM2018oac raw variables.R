## prepare raw data from residents matrix and cost and debt master for output area clustering

rm2018person <- fread("file:///C:/R_projects/archetypes/27122018/RM2018CCPerson.csv")
rm2018person <- rm2018person[rm2018person$oa != ""]
rm2018person <- rm2018person %>% rename(OA11CD = oa)
rm2018HH <- fread("file:///C:/R_projects/archetypes/27122018/RM2018CCHousehold.csv")
rm2018HH <- rm2018HH[rm2018HH$OA11CD != ""]

names(rm2018person)
names(rm2018HH)

## get variable lookup table

RM2018OAC_Input_Lookup <- read.csv("file:///C:/R_projects/OAC/lbbdoac2018/2018_OAC_Raw_Variables_Lookup.csv", sep=",", stringsAsFactors = F)
names(RM2018OAC_Input_Lookup)
## create Ram Variables

Total_Population <- rm2018person %>% 
  group_by(OA11CD) %>% 
  summarise(count = n())

Total_Households <- rm2018HH %>% 
  group_by(OA11CD) %>% 
  summarise(count = n())

Total_Population_16_and_over <- rm2018person %>% 
  filter(age20180331 > 15) %>% 
  group_by(OA11CD) %>%
  summarise(count = n())

Total_Population_3_and_over <- rm2018person %>% 
  filter( age20180331 > 2) %>% 
  group_by(OA11CD) %>%
  summarise(count = n())

Total_Population_5_to_16  <- rm2018person %>% 
  filter(age20180331 > 4) %>%
  filter(age20180331 < 17) %>% 
  group_by(OA11CD) %>%
  summarise(count = n())


