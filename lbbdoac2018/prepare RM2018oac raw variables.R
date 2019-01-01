## prepare raw data from residents matrix and cost and debt master for output area clustering
library(dplyr)
library(tidyr)
library(data.table)

rm(list = ls())

rm2018person <- fread("file:///C:/R_projects/archetypes/27122018/RM2018CCPerson.csv")
rm2018person <- rm2018person[rm2018person$oa != ""]
rm2018person <- rm2018person %>% rename(OA11CD = oa)
rm2018person$OA11CD <- as.factor(rm2018person$OA11CD)


rm2018HH <- fread("file:///C:/R_projects/archetypes/27122018/RM2018CCHousehold.csv")
rm2018HH <- rm2018HH[rm2018HH$OA11CD != ""]
rm2018HH$OA11CD <- as.factor(rm2018HH$OA11CD)

names(rm2018person)
names(rm2018HH)

##get census variables required for LBBD

#Data inputs
OAC_Input_Lookup <- read.csv("file:///C:/R_projects/OAC/downloaded data/2011 OAC 60 Variables/2011_OAC_Raw_kVariables_Lookup.csv",sep=",", stringsAsFactors = F)
OAC_Input <- read.csv("file:///C:/R_projects/OAC/downloaded data/2011 OAC 60 Variables/2011_OAC_Raw_kVariables.csv",sep=",", stringsAsFactors = F)

### get oa la lookup 

OA_LAD_RegionLUP <- fread("file:///C:/R_projects/OAC/downloaded data/2011 OA Population and Lookup.csv") %>% 
  filter(LOCAL_AUTHORITY_NAME == "Barking and Dagenham") %>% 
  select(OA) %>% 
  left_join(OAC_Input)

# consolidate data for LBBD

census_OAC_Input <- OA_LAD_RegionLUP

## get variable lookup table

RM2018OAC_Input_Lookup <- read.csv("file:///C:/R_projects/OAC/lbbdoac/lbbdoac2018/2018_OAC_Raw_Variables_Lookup.csv", sep=",", stringsAsFactors = F)
## create Raw Variables

Total_Population <- rm2018person %>% 
  group_by(OA11CD) %>% 
  summarise(Total_Population = n()) %>% 
  complete(OA11CD, fill = list(Total_Population = 0))

Total_Households <- rm2018HH %>% 
  group_by(OA11CD) %>% 
  summarise(Total_Households = n()) %>% 
  complete(OA11CD, fill = list(count = 0)) %>%
  select(Total_Households)

Total_Population_16_and_over <- rm2018person %>% 
  filter(age20180331 >= 16) %>% 
  group_by(OA11CD) %>%
  summarise(Total_Population_16_and_over = n()) %>% 
  complete(OA11CD, fill = list(Total_Population_16_and_over = 0)) %>% 
  select(Total_Population_16_and_over)

Total_Population_16_to_74 <- rm2018person %>% 
  filter(age20180331 >= 16 & age20180331 <= 74) %>% 
  group_by(OA11CD) %>%
  summarise(Total_Population_16_and_over = n()) %>% 
  complete(OA11CD, fill = list(Total_Population_16_and_over = 0)) %>% 
  select(Total_Population_16_and_over)


Total_Population_3_and_over <- rm2018person %>% 
  filter( age20180331 >= 3) %>% 
  group_by(OA11CD) %>%
  summarise(Total_Population_3_and_over = n()) %>% 
  complete(OA11CD, fill = list(Total_Population_3_and_over = 0)) %>% 
  select(Total_Population_3_and_over)

Total_Population_school_census  <- rm2018person %>% 
  filter(id_sch != "") %>% # include only records where we have a UPN
  group_by(OA11CD) %>%
  summarise(Total_Population_school_census = n()) %>% 
  complete(OA11CD, fill = list(Total_Population_school_census = 0)) %>% 
  select(Total_Population_school_census)

Total_Population_census <- census_OAC_Input %>%
  select(Total_Population_census=Total_Population)

Total_Households_census <- census_OAC_Input %>%
  select(Total_Households_census=Total_Households)

Total_Population_16_and_over_census <- census_OAC_Input %>%
  select(Total_Population_16_and_over)

Total_Population_16_to_74_census <- census_OAC_Input %>%
  select(Total_Population_16_and_over_census=Total_Population_16_to_74)

Total_Population_3_and_over_census <- census_OAC_Input %>%
  select(Total_Population_3_and_over_census=Total_Population_3_and_over)

RM2018_01 <- rm2018person %>% # Persons aged 0 to 4
  filter(age20180331 <= 4) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_01 = n()) %>% 
  complete(OA11CD, fill = list(RM2018_01 = 0)) %>% 
  select(RM2018_01)
   
RM2018_02 <- rm2018person %>% # Persons aged 5 to 14
  filter(age20180331 >= 5 & age20180331 <= 14) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_02 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_02 = 0)) %>% 
  select(RM2018_02)

RM2018_03 <- rm2018person %>% # Persons aged 25 to 44
  filter(age20180331 >= 24 & age20180331 <= 44) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_03 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_03 = 0)) %>% 
  select(RM2018_03)

RM2018_04 <- rm2018person %>% # Persons aged 45 to 64
  filter(age20180331 >= 45 & age20180331 <= 54) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_04 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_04 = 0)) %>% 
  select(RM2018_04)

RM2018_05 <- rm2018person %>% # Persons aged 65 to 89
  filter(age20180331 >= 65 & age20180331 <= 89) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_05 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_05 = 0)) %>% 
  select(RM2018_05)

RM2018_06 <- rm2018person %>% # Persons aged 90 and over
  filter(age20180331 >= 90) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_06 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_06 = 0)) %>% 
  select(RM2018_06)

RM2018_07 <- census_OAC_Input %>% # Persons living in communal esatblishments (Census 2011)
  select(OA,k008) %>% 
  arrange(OA) %>% 
  select(RM2018_07=k008)

RM2018_08 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "White British_Irish") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_08 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_08 = 0)) %>% 
  select(RM2018_08)

RM2018_09 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "White: Other") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_09 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_09 = 0)) %>% 
  select(RM2018_09)

RM2018_10 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Asian: Indian") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_10 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_10 = 0)) %>% 
  select(RM2018_10)

RM2018_11 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Asian: Pakistani") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_11 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_11 = 0)) %>% 
  select(RM2018_11)

RM2018_12 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Asian: Bangladeshi") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_12 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_12 = 0)) %>% 
  select(RM2018_12)

RM2018_13 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Chinese" | ethnicgroup2018 == "Other" | ethnicgroup2018 == "Black: Other" ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_13 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_13 = 0)) %>% 
  select(RM2018_13)

RM2018_14 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Black: African" ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_14 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_14 = 0)) %>% 
  select(RM2018_14)

RM2018_15 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "Black Caribbean"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_15 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_15 = 0)) %>% 
  select(RM2018_15)

RM2018_17 <- rm2018person %>%  #  Language other than English spoken at home based on annual school census
  filter(plasc_Lang != "") %>%
  filter(plasc_Lang != "ENG") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_17 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_17 = 0)) %>% 
  select(RM2018_17)

RM2018_18 <- rm2018HH %>% # Households with no children under 16
  filter(numunder16 == 0  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_18 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_18 = 0)) %>% 
  select(RM2018_18)

RM2018_21 <- rm2018HH %>% # 
  filter(householdtype == "cohabiting adult households no under 16's"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_21 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_21 = 0)) %>% 
  select(RM2018_21)

RM2018_22 <- rm2018HH %>% # 
  filter(householdtype == "Family households with dependent children"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_22 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_22 = 0)) %>% 
  select(RM2018_22)

RM2018_23 <- rm2018HH %>% # 
  filter(householdtype == "older cohabiting households"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_23 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_23 = 0)) %>% 
  select(RM2018_23)

RM2018_24 <- rm2018HH %>% # 
  filter(householdtype == "older person living alone"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_24 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_24 = 0)) %>% 
  select(RM2018_24)

RM2018_25 <- rm2018HH %>% # 
  filter(householdtype == "single adult households"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_25 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_25 = 0)) %>% 
  select(RM2018_25)

RM2018_26 <- rm2018HH %>% # 
  filter(householdtype == "single adult households with dependant children"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_26 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_26 = 0)) %>% 
  select(RM2018_26)

RM2018_27 <- rm2018HH %>% # 
  filter(householdtype == "three generational households"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_27 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_27 = 0)) %>% 
  select(RM2018_27)

RM2018_28 <- rm2018HH %>% # 
  filter(estimated_tenure2018 == "OOC"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_28 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_28 = 0)) %>% 
  select(RM2018_28)

RM2018_29 <- rm2018HH %>% # 
  filter(estimated_tenure2018 == "Social"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_29 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_29 = 0)) %>% 
  select(RM2018_29)

RM2018_30 <- rm2018HH %>% # 
  filter(estimated_tenure2018 == "PR"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_30 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_30 = 0)) %>% 
  select(RM2018_30)

RM2018_31 <- rm2018HH %>% # 
  filter(estimated_tenure2018 == "Reside"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_31 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_31 = 0)) %>% 
  select(RM2018_31)

RM2018_32 <- census_OAC_Input %>% # Individuals day-to-day activities limited a lot or a little (Standardised Illness Ratio) (Census 2011)
  select(OA,k035) %>% 
  arrange(OA) %>% 
  select(RM2018_32=k035)

RM2018_33 <- census_OAC_Input %>% # Persons providing unpaid care (Census 2011)
  select(OA,k036) %>% 
  arrange(OA) %>% 
  select(RM2018_33=k036)

RM2018_34 <- census_OAC_Input %>% # Persons aged over 16 whose highest level of qualification is Level 1, Level 2 or Apprenticeship (Census 2011)
  select(OA,k037) %>% 
  arrange(OA) %>% 
  select(RM2018_34=k037)

RM2018_35 <- census_OAC_Input %>% # Persons aged over 16 whose highest level of qualification is Level 3 qualifications
  select(OA,k038) %>% 
  arrange(OA) %>% 
  select(RM2018_35=k038)

RM2018_36 <- census_OAC_Input %>% # Persons aged over 16 whose highest level of qualification is Level 4 qualifications and above
  select(OA,k039) %>% 
  arrange(OA) %>% 
  select(RM2018_36=k039)

RM2018_40 <- rm2018HH %>% # Households Receiving Housing Benefit
  filter(BenRecieved == "HB"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_40 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_40 = 0)) %>% 
  select(RM2018_40)

RM2018_41 <- rm2018HH %>% # Households Receiving Ctax Relief Benefit
  filter(BenRecieved == "CTB"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_41 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_41 = 0)) %>% 
  select(RM2018_41)

RM2018_42 <- rm2018HH %>% # Households Receiving Housing Benefit and Ctax relief
  filter(BenRecieved == "BOTH"  ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_42 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_42 = 0)) %>% 
  select(RM2018_42)

RM2018_43 <- rm2018person %>% # persons on school census recieving fsm
  filter(FSM == 1 ) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_43 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_43 = 0)) %>% 
  select(RM2018_43)




## create dataframe with input raw variable counts

RM2018_OAC_input <- data.frame(Total_Population, Total_Households, Total_Population_16_and_over, Total_Population_16_to_74,
                               Total_Population_3_and_over, Total_Population_school_census,
                               Total_Population_census, Total_Households_census, Total_Population_16_and_over_census, Total_Population_16_to_74_census,
                               Total_Population_3_and_over_census,
                               RM2018_01, RM2018_02, RM2018_03, RM2018_04, RM2018_05, RM2018_06, RM2018_07,
                               RM2018_08, RM2018_09, RM2018_10, RM2018_11, RM2018_12, RM2018_13, RM2018_14, RM2018_15,
                               RM2018_17, RM2018_18,
                               RM2018_21, RM2018_22, RM2018_23, RM2018_24, RM2018_25, RM2018_26, RM2018_27,
                               RM2018_28, RM2018_29, RM2018_30, RM2018_31,
                               RM2018_32, RM2018_33,
                               RM2018_34, RM2018_35, RM2018_36,
                               RM2018_40, RM2018_41, RM2018_42, RM2018_43)

length(RM2018_OAC_input)-1 == nrow(RM2018OAC_Input_Lookup) ### check we have matching vars in both tables

# remove uneeded objects

rm(Total_Population, Total_Households, Total_Population_16_and_over, Total_Population_16_to_74,
   Total_Population_3_and_over, Total_Population_school_census,
   Total_Population_census, Total_Households_census, Total_Population_16_and_over_census, Total_Population_16_to_74_census,
   Total_Population_3_and_over_census,
   RM2018_01, RM2018_02, RM2018_03, RM2018_04, RM2018_05, RM2018_06, RM2018_07,
   RM2018_08, RM2018_09, RM2018_10, RM2018_11, RM2018_12, RM2018_13, RM2018_14, RM2018_15,
   RM2018_17, RM2018_18,
   RM2018_21, RM2018_22, RM2018_23, RM2018_24, RM2018_25, RM2018_26, RM2018_27,
   RM2018_28, RM2018_29, RM2018_30, RM2018_31,
   RM2018_32, RM2018_33,
   RM2018_34, RM2018_35, RM2018_36,
   RM2018_40, RM2018_41, RM2018_42, RM2018_43)


#############################
### prepare for clustering ##
#############################

# count denominators to use by first getting relavant number of columns

count_denominators <- length(RM2018_OAC_input %>% select(starts_with("Total")))

#rows which contain denominators: this will be used to get relevant rows from the lookup
D_Col <- 1:count_denominators 

#Which Transformation technique would you like to use - Inverse Hyperbolic Sine (IHS) or Box-Cox?
#Enter "IHS" or "BOXCOX"

RQTRANSFORMATION <- "IHS"

#Save Percentage, IHS/Box-Cox and Range CSV Files with Clustered R.Data file?
#Enter "YES" or "NO"
RQOUTPUT <- "YES"


#Lambda Hat loops (The default is 20)
HatLoops <-20

#Number of Clusters you would like
CN <- 8

# This section sets the number of times to run k means - is stochastic, so varies probabilistically - we need a decent
# averaged mean of means so need a decent number of runs to avoid "chance clustering"

#Number of k-means loops to perform (The recommended minimum is 1000)

# for LBBD data on Phils home laptop
# 10000 takes 17.94 mins
# 1000 takes 1.8 mins
# 100 takes 11.7 secs

### however each version has produced same clusters!!!  

KM <- 1000

####################################################################################################
# Calculate Percentages ############################################################################
####################################################################################################

####################################################################################################
# Calculate Percentages ############################################################################
####################################################################################################

#Get numerator denominator list
K_Col <- (max(D_Col)+1):nrow(RM2018OAC_Input_Lookup)
K_Var <- RM2018OAC_Input_Lookup[K_Col,c(1,4)]
D_List <- RM2018OAC_Input_Lookup[D_Col,c(1,4)]

K_Var <- merge(K_Var,D_List, by="VariableStatisticalUnit",all.x=TRUE)
colnames(K_Var) <- c("VariableStatisticalUnit","VariableCode", "Denominator")
K_Var <- K_Var[order(K_Var$VariableCode),] 

RM2018_OAC_Input_PCT_RATIO <- as.matrix(RM2018_OAC_input[,1])
colnames(RM2018_OAC_Input_PCT_RATIO) <- "OA11CD"

#n <- 9
# Calculate loop
for (n in 1:nrow(K_Var)){
  
  if (!is.na(K_Var[n,3])) {
    
    #Uses grep to get the column id matching the current numerator
    numerator <- RM2018_OAC_input[,grep(paste("^",K_Var[n,2],"$",sep=""), colnames(RM2018_OAC_input))]
    
    #Uses grep to get the column id matching the current denominator
    denominator <- RM2018_OAC_input[,grep(paste("^",K_Var[n,3],"$",sep=""), colnames(RM2018_OAC_input))]
    
    assign(paste(K_Var[n,2],"_PCT",sep=""),as.data.frame(numerator/denominator*100))# Calculate %
    assign(paste(K_Var[n,2],"_PCT",sep=""),setNames(get(paste(K_Var[n,2],"_PCT",sep="")),paste(K_Var[n,2],"_PCT",sep="")))#Change column name
    
    #Create output files
    RM2018_OAC_Input_PCT_RATIO <- cbind(RM2018_OAC_Input_PCT_RATIO,get(paste(K_Var[n,2],"_PCT",sep="")))
    
    remove(list=paste(K_Var[n,2],"_PCT",sep=""))
    remove(list=c("numerator","denominator"))
    
  } else {#This mirrors the PCT loop, but modified for the ratio data
    
    ratio <- as.data.frame(RM2018_OAC_input[,grep(paste("^",K_Var[n,2],"$",sep=""), colnames(RM2018_OAC_input))])
    assign(paste(K_Var[n,2],"_RATIO",sep=""),ratio)# Calculate %
    assign(paste(K_Var[n,2],"_RATIO",sep=""),setNames(get(paste(K_Var[n,2],"_RATIO",sep="")),paste(K_Var[n,2],"_RATIO",sep="")))#Change column name
    RM2018_OAC_Input_PCT_RATIO <- cbind(RM2018_OAC_Input_PCT_RATIO,get(paste(K_Var[n,2],"_RATIO",sep="")))
    remove(list=paste(paste(K_Var[n,2],"_RATIO",sep="")))
    remove(ratio)
  }
}

summary(RM2018_OAC_Input_PCT_RATIO)






