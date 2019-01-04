## prepare raw data from residents matrix and cost and debt master for output area clustering
library(dplyr)
library(tidyr)
library(data.table)

rm(list = ls())

setwd("C:\\R_projects\\OAC\\lbbdoac\\lbbdoac2018")



rm2018person <- fread("file:///C:/R_projects/archetypes/27122018/RM2018CCPerson.csv")
rm2018person <- rm2018person[rm2018person$oa != ""]
rm2018person <- rm2018person %>% dplyr:: rename(OA11CD = oa)

rm2018HH <- fread("file:///C:/R_projects/archetypes/27122018/RM2018CCHousehold.csv")
rm2018HH <- rm2018HH[rm2018HH$OA11CD != ""]
rm2018HH <- rm2018HH %>% 
  mutate(POSTCODE = toupper(gsub(" ", "", POSTCODE))) %>% 
  filter(!is.na(POSTCODE)) %>%
  filter(POSTCODE != "")

names(rm2018person)
names(rm2018HH)

## join UPRN to RM2018 Person and remove UPRN nas
rm2018person <- rm2018person %>% 
  left_join(rm2018HH %>% dplyr::select("UPRN", "POSTCODE"), by = "UPRN") %>% 
  filter(!is.na(POSTCODE)) %>%
  filter(POSTCODE != "")

rm2018person <- rm2018person %>%
  arrange(UPRN) %>% 
  filter(UPRN != "") %>%
  filter(!is.na(UPRN)) %>% 
  mutate(UPRN = as.factor(UPRN)) #change to factor to allow corrcet grouping for null values

rm2018HH <- rm2018HH %>% 
  arrange(UPRN) %>% 
  filter(UPRN != "") %>%
  filter(!is.na(UPRN)) %>% 
  mutate(UPRN = as.factor(UPRN))


# ##get census variables required for LBBD
# 
# #Data inputs
# OAC_Input_Lookup <- read.csv("file:///C:/R_projects/OAC/downloaded data/2011 OAC 60 Variables/2011_OAC_Raw_kVariables_Lookup.csv",sep=",", stringsAsFactors = F)
# OAC_Input <- read.csv("file:///C:/R_projects/OAC/downloaded data/2011 OAC 60 Variables/2011_OAC_Raw_kVariables.csv",sep=",", stringsAsFactors = F)
# 
# ### get oa la lookup 
# 
# OA_LAD_RegionLUP <- fread("file:///C:/R_projects/OAC/downloaded data/2011 OA Population and Lookup.csv") %>% 
#   filter(LOCAL_AUTHORITY_NAME == "Barking and Dagenham") %>% 
#   select(OA) %>% 
#   left_join(OAC_Input)
# 
# # consolidate data for LBBD
# 
# census_OAC_Input <- OA_LAD_RegionLUP

## get variable lookup table

RM2018Household_Input_Lookup <- read.csv("file:///C:/R_projects/OAC/lbbdoac/lbbdoac2018/2018_Postcode_Raw_Variables_Lookup.csv", sep=",", stringsAsFactors = F)


## create Raw Variables

Total_Population <- rm2018person %>% 
  group_by(UPRN) %>% 
  summarise(Total_Population = n()) %>% 
  complete(UPRN, fill = list(Total_Population = 0))

Total_Households <- rm2018HH %>% 
  group_by(UPRN) %>% 
  summarise(Total_Households = n()) %>% 
  complete(UPRN, fill = list(count = 0)) %>%
  select(Total_Households)

Total_Population_16_and_over <- rm2018person %>% 
  filter(age20180331 >= 16) %>% 
  group_by(UPRN) %>%
  summarise(Total_Population_16_and_over = n()) %>% 
  complete(UPRN, fill = list(Total_Population_16_and_over = 0)) %>% 
  select(Total_Population_16_and_over)

Total_Population_16_to_74 <- rm2018person %>% 
  filter(age20180331 >= 16 & age20180331 <= 74) %>% 
  group_by(UPRN) %>%
  summarise(Total_Population_16_to_74 = n()) %>% 
  complete(UPRN, fill = list(Total_Population_16_to_74 = 0)) %>% 
  select(Total_Population_16_to_74)

Total_Population_3_and_over <- rm2018person %>% 
  filter( age20180331 >= 3) %>% 
  group_by(UPRN) %>%
  summarise(Total_Population_3_and_over = n()) %>% 
  complete(UPRN, fill = list(Total_Population_3_and_over = 0)) %>% 
  select(Total_Population_3_and_over)

Total_Population_school_census  <- rm2018person %>% 
  filter(id_sch != "") %>% # include only records where we have a UPN
  group_by(UPRN) %>%
  summarise(Total_Population_school_census = n()) %>% 
  complete(UPRN, fill = list(Total_Population_school_census = 0)) %>% 
  select(Total_Population_school_census)


RM2018_01 <- rm2018person %>% # Persons aged 0 to 4
  filter(age20180331 <= 4) %>%
  group_by(UPRN) %>%
  summarise(RM2018_01 = n()) %>% 
  complete(UPRN, fill = list(RM2018_01 = 0)) %>% 
  select(RM2018_01)
   
RM2018_02 <- rm2018person %>% # Persons aged 5 to 14
  filter(age20180331 >= 5 & age20180331 <= 14) %>%
  group_by(UPRN) %>%
  summarise(RM2018_02 = n())  %>% 
  complete(UPRN, fill = list(RM2018_02 = 0)) %>% 
  select(RM2018_02)

RM2018_02a <- rm2018person %>% # Persons aged 25 to 44
  filter(age20180331 >= 15 & age20180331 <= 24) %>%
  group_by(UPRN) %>%
  summarise(RM2018_02a = n())  %>% 
  complete(UPRN, fill = list(RM2018_02a = 0)) %>% 
  select(RM2018_02a)

RM2018_03 <- rm2018person %>% # Persons aged 25 to 44
  filter(age20180331 >= 25 & age20180331 <= 44) %>%
  group_by(UPRN) %>%
  summarise(RM2018_03 = n())  %>% 
  complete(UPRN, fill = list(RM2018_03 = 0)) %>% 
  select(RM2018_03)

RM2018_04 <- rm2018person %>% # Persons aged 45 to 64
  filter(age20180331 >= 45 & age20180331 <= 54) %>%
  group_by(UPRN) %>%
  summarise(RM2018_04 = n())  %>% 
  complete(UPRN, fill = list(RM2018_04 = 0)) %>% 
  select(RM2018_04)

RM2018_05 <- rm2018person %>% # Persons aged 65 to 89
  filter(age20180331 >= 65 & age20180331 <= 89) %>%
  group_by(UPRN) %>%
  summarise(RM2018_05 = n())  %>% 
  complete(UPRN, fill = list(RM2018_05 = 0)) %>% 
  select(RM2018_05)

RM2018_06 <- rm2018person %>% # Persons aged 90 and over
  filter(age20180331 >= 90) %>%
  group_by(UPRN) %>%
  summarise(RM2018_06 = n())  %>% 
  complete(UPRN, fill = list(RM2018_06 = 0)) %>% 
  select(RM2018_06)


RM2018_08 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "White British_Irish") %>%
  group_by(UPRN) %>%
  summarise(RM2018_08 = n())  %>% 
  complete(UPRN, fill = list(RM2018_08 = 0)) %>% 
  select(RM2018_08)


RM2018_09 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "White: Other") %>%
  group_by(UPRN) %>%
  summarise(RM2018_09 = n())  %>% 
  complete(UPRN, fill = list(RM2018_09 = 0)) %>% 
  select(RM2018_09)


RM2018_10 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Asian: Indian") %>%
  group_by(UPRN) %>%
  summarise(RM2018_10 = n())  %>% 
  complete(UPRN, fill = list(RM2018_10 = 0)) %>% 
  select(RM2018_10)

RM2018_11 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Asian: Pakistani") %>%
  group_by(UPRN) %>%
  summarise(RM2018_11 = n())  %>% 
  complete(UPRN, fill = list(RM2018_11 = 0)) %>% 
  select(RM2018_11)

RM2018_12 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Asian: Bangladeshi") %>%
  group_by(UPRN) %>%
  summarise(RM2018_12 = n())  %>% 
  complete(UPRN, fill = list(RM2018_12 = 0)) %>% 
  select(RM2018_12)

RM2018_13 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Chinese" | ethnicgroup2018 == "Other" | ethnicgroup2018 == "Black: Other" ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_13 = n())  %>% 
  complete(UPRN, fill = list(RM2018_13 = 0)) %>% 
  select(RM2018_13)

RM2018_14 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Black: African" ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_14 = n())  %>% 
  complete(UPRN, fill = list(RM2018_14 = 0)) %>% 
  select(RM2018_14)

RM2018_15 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Black Caribbean"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_15 = n())  %>% 
  complete(UPRN, fill = list(RM2018_15 = 0)) %>% 
  select(RM2018_15)

RM2018_17 <- rm2018person %>%  #  Language other than English spoken at home based on annual school census
  filter(plasc_Lang != "") %>%
  filter(plasc_Lang != "ENG") %>%
  group_by(UPRN) %>%
  summarise(RM2018_17 = n())  %>% 
  complete(UPRN, fill = list(RM2018_17 = 0)) %>% 
  select(RM2018_17)

RM2018_18 <- rm2018HH %>% # Households with no children under 16
  filter(numunder16 == 0  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_18 = n())  %>% 
  complete(UPRN, fill = list(RM2018_18 = 0)) %>% 
  select(RM2018_18)

RM2018_21 <- rm2018HH %>% # 
  filter(householdtype == "cohabiting adult households no under 16's"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_21 = n())  %>% 
  complete(UPRN, fill = list(RM2018_21 = 0)) %>% 
  select(RM2018_21)

RM2018_22 <- rm2018HH %>% # 
  filter(householdtype == "Family households with dependent children"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_22 = n())  %>% 
  complete(UPRN, fill = list(RM2018_22 = 0)) %>% 
  select(RM2018_22)

RM2018_23 <- rm2018HH %>% # 
  filter(householdtype == "older cohabiting households"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_23 = n())  %>% 
  complete(UPRN, fill = list(RM2018_23 = 0)) %>% 
  select(RM2018_23)

RM2018_24 <- rm2018HH %>% # 
  filter(householdtype == "older person living alone"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_24 = n())  %>% 
  complete(UPRN, fill = list(RM2018_24 = 0)) %>% 
  select(RM2018_24)

RM2018_25 <- rm2018HH %>% # 
  filter(householdtype == "single adult households"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_25 = n())  %>% 
  complete(UPRN, fill = list(RM2018_25 = 0)) %>% 
  select(RM2018_25)

RM2018_26 <- rm2018HH %>% # 
  filter(householdtype == "single adult households with dependant children"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_26 = n())  %>% 
  complete(UPRN, fill = list(RM2018_26 = 0)) %>% 
  select(RM2018_26)

RM2018_27 <- rm2018HH %>% # 
  filter(householdtype == "three generational households"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_27 = n())  %>% 
  complete(UPRN, fill = list(RM2018_27 = 0)) %>% 
  select(RM2018_27)

RM2018_28 <- rm2018HH %>% # 
  filter(estimated_tenure2018 == "OOC"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_28 = n())  %>% 
  complete(UPRN, fill = list(RM2018_28 = 0)) %>% 
  select(RM2018_28)

RM2018_29 <- rm2018HH %>% # 
  filter(estimated_tenure2018 == "Social"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_29 = n())  %>% 
  complete(UPRN, fill = list(RM2018_29 = 0)) %>% 
  select(RM2018_29)

RM2018_30 <- rm2018HH %>% # 
  filter(estimated_tenure2018 == "PR"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_30 = n())  %>% 
  complete(UPRN, fill = list(RM2018_30 = 0)) %>% 
  select(RM2018_30)

RM2018_31 <- rm2018HH %>% # 
  filter(estimated_tenure2018 == "Reside"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_31 = n())  %>% 
  complete(UPRN, fill = list(RM2018_31 = 0)) %>% 
  select(RM2018_31)

RM2018_40 <- rm2018HH %>% # Households Receiving Housing Benefit
  filter(BenRecieved == "HB"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_40 = n())  %>% 
  complete(UPRN, fill = list(RM2018_40 = 0)) %>% 
  select(RM2018_40)

RM2018_41 <- rm2018HH %>% # Households Receiving Ctax Relief Benefit
  filter(BenRecieved == "CTB"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_41 = n())  %>% 
  complete(UPRN, fill = list(RM2018_41 = 0)) %>% 
  select(RM2018_41)

RM2018_42 <- rm2018HH %>% # Households Receiving Housing Benefit and Ctax relief
  filter(BenRecieved == "BOTH"  ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_42 = n())  %>% 
  complete(UPRN, fill = list(RM2018_42 = 0)) %>% 
  select(RM2018_42)

RM2018_43 <- rm2018person %>% # persons on school census recieving fsm
  filter(FSM == 1 ) %>%
  group_by(UPRN) %>%
  summarise(RM2018_43 = n())  %>% 
  complete(UPRN, fill = list(RM2018_43 = 0)) %>% 
  select(RM2018_43)




## create dataframe with input raw variable counts

RM2018_Household_input <- data.frame(Total_Population, Total_Households, Total_Population_16_and_over, Total_Population_16_to_74,
                               Total_Population_3_and_over, Total_Population_school_census,
                               RM2018_01, RM2018_02, RM2018_02a, RM2018_03, RM2018_04, RM2018_05, RM2018_06,
                               RM2018_08, RM2018_09, RM2018_10, RM2018_11, RM2018_12, RM2018_13, RM2018_14, RM2018_15,
                               RM2018_17, RM2018_18,
                               RM2018_21, RM2018_22, RM2018_23, RM2018_24, RM2018_25, RM2018_26, RM2018_27,
                               RM2018_28, RM2018_29, RM2018_30, RM2018_31,
                               RM2018_40, RM2018_41, RM2018_42, RM2018_43)

length(RM2018_Household_input)-1 == nrow(RM2018Household_Input_Lookup) ### check we have matching vars in both tables


# remove uneeded objects

rm(Total_Population, Total_Households, Total_Population_16_and_over, Total_Population_16_to_74,
   Total_Population_3_and_over, Total_Population_school_census,
   RM2018_01, RM2018_02, RM2018_03, RM2018_04, RM2018_05, RM2018_06,
   RM2018_08, RM2018_09, RM2018_10, RM2018_11, RM2018_12, RM2018_13, RM2018_14, RM2018_15,
   RM2018_17, RM2018_18,
   RM2018_21, RM2018_22, RM2018_23, RM2018_24, RM2018_25, RM2018_26, RM2018_27,
   RM2018_28, RM2018_29, RM2018_30, RM2018_31,
   RM2018_40, RM2018_41, RM2018_42, RM2018_43)

rm(rm2018HH)
rm(rm2018person)

## write chosen counts


fwrite(RM2018_Household_input, "2018_Household_Raw_Variables.csv")



