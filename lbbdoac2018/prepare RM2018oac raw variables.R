## prepare raw data from residents matrix and cost and debt master for output area clustering
library(dplyr)
library(tidyr)
library(data.table)

rm(list = ls())

setwd("C:\\R_projects\\OAC\\lbbdoac\\lbbdoac2018")



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
  summarise(Total_Population_16_to_74 = n()) %>% 
  complete(OA11CD, fill = list(Total_Population_16_to_74 = 0)) %>% 
  select(Total_Population_16_to_74)

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
  select(Total_Population_16_and_over_census = Total_Population_16_and_over)

Total_Population_16_to_74_census <- census_OAC_Input %>%
  select(Total_Population_16_to_74_census = Total_Population_16_to_74)

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

sum(RM2018_08)

RM2018_09 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "White: Other") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_09 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_09 = 0)) %>% 
  select(RM2018_09)

sum(RM2018_09)

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

rm(rm2018HH)
rm(rm2018person)
rm(OAC_Input)

## write chosen counts

fwrite(RM2018_OAC_input, "2018_OAC_Raw_Variables.csv")


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

summary(RM2018_OAC_Input_PCT_RATIO) ## checks on percentage and ratio calcs

#On the basis of the percentage calculations, any OA where 0/0 resulted in an error, ammend as 0%
RM2018_OAC_Input_PCT_RATIO <- replace(RM2018_OAC_Input_PCT_RATIO, is.na(RM2018_OAC_Input_PCT_RATIO), 0)

if(RQOUTPUT=="YES")
{
  dir.create("Pre-Cluster Data", showWarnings = FALSE)
  write.table(RM2018_OAC_Input_PCT_RATIO, paste("Pre-Cluster Data/01_RM2018_OAC_Percentages.csv", sep = ""), sep = ",", row.names= FALSE, col.names = TRUE, qmethod = "double")
}

####################################################################################################
# Calculate inverse hyperbolic sine ################################################################
####################################################################################################

if(RQTRANSFORMATION =="IHS")
{
  RM2018_OAC_Input_PCT_RATIO_IHS <- log(RM2018_OAC_Input_PCT_RATIO[,2:ncol(RM2018_OAC_Input_PCT_RATIO)]+sqrt(RM2018_OAC_Input_PCT_RATIO[,2:ncol(RM2018_OAC_Input_PCT_RATIO)]^2+1))
  
  if(RQOUTPUT=="YES")
  {
    dir.create("Pre-Cluster Data", showWarnings = FALSE)
    write.table(RM2018_OAC_Input_PCT_RATIO_IHS, paste("Pre-Cluster Data/02_RM2018_OAC_Percentages_IHS_Transformed.csv", sep = ""), sep = ",", row.names= FALSE, col.names = TRUE, qmethod = "double")
  }
  
}

####################################################################################################
# Calculate Box-Cox ################################################################################
####################################################################################################
if(RQTRANSFORMATION =="BOXCOX")
{
  
  RM2018_OAC_Input_PCT_RATIO_BC<-RM2018_OAC_Input_PCT_RATIO[1]
  PCT_Data<-RM2018_OAC_Input_PCT_RATIO[, grep('PCT', names(RM2018_OAC_Input_PCT_RATIO))] 
  Ratio_Data<-RM2018_OAC_Input_PCT_RATIO[, grep('RATIO', names(RM2018_OAC_Input_PCT_RATIO))] 
  if(ncol(PCT_Data)>0){if (min(PCT_Data)<1) {RM2018_OAC_Input_PCT_RATIO_BC <- cbind(RM2018_OAC_Input_PCT_RATIO_BC,(PCT_Data+1))} else {RM2018_OAC_Input_PCT_RATIO_BC <- cbind(RM2018_OAC_Input_PCT_RATIO_BC,(PCT_Data))}}
  if(ncol(Ratio_Data)>0){if (min(Ratio_Data)<1) {RM2018_OAC_Input_PCT_RATIO_BC <- cbind(RM2018_OAC_Input_PCT_RATIO_BC,(Ratio_Data+1))} else {RM2018_OAC_Input_PCT_RATIO_BC <- cbind(RM2018_OAC_Input_PCT_RATIO_BC,(Ratio_Data))}}
  RM2018_OAC_Input_PCT_RATIO_BC<-RM2018_OAC_Input_PCT_RATIO_BC[2:ncol(RM2018_OAC_Input_PCT_RATIO_BC)]
  RM2018_OAC_Input_PCT_RATIO_BC<-RM2018_OAC_Input_PCT_RATIO_BC[,order(names(RM2018_OAC_Input_PCT_RATIO_BC))]
  RM2018_OAC_Input_PCT_RATIO_BC_Matrix<-as.matrix(RM2018_OAC_Input_PCT_RATIO_BC)
  VarNum<-ncol(RM2018_OAC_Input_PCT_RATIO_BC)
  RM2018_OAC_PCT_RATIO_BC<-RM2018_OAC_Input_PCT_RATIO[1]
  RM2018_OAC_PCT_RATIO_BC_Lambda<-NULL
  
  if(nrow(RM2018_OAC_Input_PCT_RATIO)>4999){LambdaSample=5000} else {LambdaSample=nrow(RM2018_OAC_Input_PCT_RATIO)}
  
  find_lambda <- function (data, lam = seq(-5, 5, 0.01), sample = LambdaSample) 
  {
    data <- sample(data,sample)
    sw <- NULL
    for (i in 1:length(lam)) {
      if (round(lam[i], 2) != 0) 
        sw <- rbind(sw, c(lam[i], shapiro.test((data^(lam[i]) - 1)/(lam[i]))$statistic))
      if (round(lam[i], 2) == 0) 
        sw <- rbind(sw, c(lam[i], shapiro.test(log(data))$statistic))
    }
    swlam <- sw[which.max(sw[, 2]), 1]
    return(swlam)
  }
  optim_lamda <- function (data, lam = seq(-5, 5, 0.01), sample = LambdaSample, runs=100) 
  {
    lmd_dist <- numeric()
    for (i in 1:runs){
      tmp_lmd <- find_lambda(data)
      lmd_dist <- c(tmp_lmd,lmd_dist)     
    }
    return(lmd_dist)
  }
  
  pb1 <- txtProgressBar(min = 0, max = VarNum, style = 3) 
  for (bx in 1:VarNum)
  {
    
    tmp<-RM2018_OAC_Input_PCT_RATIO_BC_Matrix[,bx]
    tmpNZ<-as.matrix(tmp)
    tmpNZ<-tmpNZ[which(rowSums(tmpNZ) > 1),]
    RM2018_OAC_PCT_RATIO_BCPCT<-data.frame(RM2018_OAC_Input_PCT_RATIO_BC_Matrix[,bx])
    colnames(RM2018_OAC_PCT_RATIO_BCPCT)<-colnames(RM2018_OAC_Input_PCT_RATIO_BC_Matrix)[bx]
    RM2018_OAC_PCT_RATIO_BC_Colname<-colnames(RM2018_OAC_Input_PCT_RATIO_BC_Matrix)[bx]
    
    Single_Lambda<-713	
    Multi_Lambda<-714
    count=0
    add=0
    repeat{
      count<-count+1
      try(Single_Lambda <- find_lambda(tmp),silent=TRUE)
      if(Single_Lambda!=713 | count==1000) break()
    }
    repeat{
      add<-add+1
      try(Multi_Lambda <- optim_lamda(tmp,runs=HatLoops),silent=TRUE)
      try(Multi_Lambda_Sum<-sum(Multi_Lambda),silent=TRUE)
      if(sum(Multi_Lambda_Sum!=714) | add==1000) break()
    }
    if(count==1000){Single_Lambda<-0}
    if(add==1000){Multi_Lambda<-Single_Lambda}
    
    Lambda_Hat <-median(Multi_Lambda)
    
    RM2018_OAC_PCT_RATIO_BC_Var<-as.data.frame(bcPower(tmp,Lambda_Hat))
    colnames(RM2018_OAC_PCT_RATIO_BC_Var)<-RM2018_OAC_PCT_RATIO_BC_Colname
    RM2018_OAC_PCT_RATIO_BC<-cbind(RM2018_OAC_PCT_RATIO_BC, RM2018_OAC_PCT_RATIO_BC_Var)
    RM2018_OAC_PCT_RATIO_BC_LambdaVar<-cbind(RM2018_OAC_PCT_RATIO_BC_Colname,Lambda_Hat)
    RM2018_OAC_PCT_RATIO_BC_Lambda<-rbind(RM2018_OAC_PCT_RATIO_BC_Lambda,RM2018_OAC_PCT_RATIO_BC_LambdaVar)
    setTxtProgressBar(pb1, bx)
  }
  
  RM2018_OAC_PCT_RATIO_BC[1]<-NULL
  RM2018_OAC_PCT_RATIO_BC_Lambda<-data.frame(RM2018_OAC_PCT_RATIO_BC_Lambda)
  RM2018_OAC_PCT_RATIO_BC_Lambda[,1]<- data.frame(K_Var[,2])
  colnames(RM2018_OAC_PCT_RATIO_BC_Lambda)<-c("Variable", "Lambda Value")
  dir.create("Transformation Data", showWarnings = FALSE)
  write.table(RM2018_OAC_PCT_RATIO_BC_Lambda, paste("lbbdoac/lbbdoac2018/Transformation Data/Percentages Box-Cox Lambda Values.csv", sep = ""), sep = ",", row.names= FALSE, col.names = TRUE, qmethod = "double")
  
  if(RQOUTPUT=="YES")
  {
    dir.create("lbbdoac/lbbdoac2018/Pre-Cluster Data", showWarnings = FALSE)
    write.table(RM2018_OAC_PCT_RATIO_BC, paste("Pre-Cluster Data/02_RM2018_OAC_Percentages_Box_Cox_Transformed.csv", sep = ""), sep = ",", row.names= FALSE, col.names = TRUE, qmethod = "double")
  }
  
}


####################################################################################################
# Calculate Range ##################################################################################
####################################################################################################

if (RQTRANSFORMATION == "IHS") {RM2018_OAC_Input_PCT_RATIO_TRANSFORM <- RM2018_OAC_Input_PCT_RATIO_IHS}
if (RQTRANSFORMATION == "BOXCOX") {RM2018_OAC_Input_PCT_RATIO_TRANSFORM <- OAC_PCT_RATIO_BC}

Range_01 <- function(x){(x-min(x))/(max(x)-min(x))}
RM2018_OAC_Input_PCT_RATIO_TRANSFORM_RANGE <- apply(RM2018_OAC_Input_PCT_RATIO_TRANSFORM, 2, Range_01)
rownames(RM2018_OAC_Input_PCT_RATIO_TRANSFORM_RANGE) <- RM2018_OAC_Input_PCT_RATIO$OA

colnames(RM2018_OAC_Input_PCT_RATIO_TRANSFORM_RANGE)<- K_Var$VariableCode

RM2018_OAC_Converted_Transformed_Range <-cbind(data.frame(RM2018_OAC_Input_PCT_RATIO$OA),RM2018_OAC_Input_PCT_RATIO_TRANSFORM_RANGE)
row.names(RM2018_OAC_Converted_Transformed_Range) <- NULL
colnames(RM2018_OAC_Converted_Transformed_Range)[1]<-"OA"

if(RQOUTPUT=="YES")
{
  dir.create("Pre-Cluster Data", showWarnings = FALSE)
  write.table(RM2018_OAC_Converted_Transformed_Range, paste("Pre-Cluster Data/03_RM2018_OAC_Percentages_Transformed_Range.csv", sep = ""), sep = ",", row.names= FALSE, col.names = TRUE, qmethod = "double")
}


####################################################################################################
# Pre K-Means ######################################################################################
####################################################################################################

ClusterStart <- Sys.time()

RM2018_OAC_Converted_Transformed_Range_Raw_Input <- RM2018_OAC_Converted_Transformed_Range

#CSV Input
#RM2018_OAC_Converted_Transformed_Range_Raw_Input <- read.csv(paste(getwd(),"/Input/","2011_OAC_Raw_kVariables.csv",sep=""))

OA<-RM2018_OAC_Converted_Transformed_Range_Raw_Input[1]

OA_Num <-nrow(OA)

RM2018_OAC_Converted_Transformed_Range_Raw_Input_Clusters <- RM2018_OAC_Converted_Transformed_Range_Raw_Input[grep("Cluster",names(RM2018_OAC_Converted_Transformed_Range_Raw_Input))]

RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Num <-ncol(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Clusters)
if(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Num>0)
{
  RM2018_OAC_Converted_Transformed_Range_Input <- RM2018_OAC_Converted_Transformed_Range_Raw_Input[-grep("Cluster",names(RM2018_OAC_Converted_Transformed_Range_Raw_Input))]
} else
{
  RM2018_OAC_Converted_Transformed_Range_Input <- RM2018_OAC_Converted_Transformed_Range_Raw_Input
}

RM2018_OAC_Converted_Transformed_Range_Input <- data.frame(RM2018_OAC_Converted_Transformed_Range_Input, row.names=1)
RM2018_OAC_Converted_Transformed_Range_Raw_Input_Best_Clusters <- RM2018_OAC_Converted_Transformed_Range_Raw_Input_Clusters[grep("Cluster",names(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Clusters))]

Best_Cluster_Input <-RM2018_OAC_Converted_Transformed_Range_Raw_Input_Best_Clusters[1,1]

RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Num <-ncol(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Clusters)
if(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Num==0){RQCLUS<- "SUPERGROUP"}
if(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Num>0)
{
  RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Char<-as.vector(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Clusters[1,1])
  RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Char_Num <- nchar(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Char)
  if(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Char_Num<1){RQCLUS<- "SUPERGROUP"}
  if(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Char_Num==1){RQCLUS<- "GROUP"}
  if(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Char_Num==2){RQCLUS<- "SUBGROUP"}
  if(RM2018_OAC_Converted_Transformed_Range_Raw_Input_Type_Char_Num>2){RQCLUS<- "SUPERGROUP"}
}


####################################################################################################
# K-Means ##########################################################################################
####################################################################################################

RM2018_OAC_Converted_Transformed_Range<- as.matrix(RM2018_OAC_Converted_Transformed_Range_Input,rownames.force=TRUE)
RM2018_OAC_Converted_Transformed_Range_Best_WSS_A <-c(1:5)
RM2018_OAC_Converted_Transformed_Range_Best_WSS_N <-1000000000000
RM2018_OAC_Converted_Transformed_Range_Best_WSS_A <-c(1:5)

pb1 <- txtProgressBar(min = 0, max = KM, style = 3) 	

for (i in 1:KM)
{
  Cluster_RM2018_OAC_Converted_Transformed_Range<- kmeans(RM2018_OAC_Converted_Transformed_Range, CN)
  RM2018_OAC_Converted_Transformed_Range_Best_WSS_B <- mean(Cluster_RM2018_OAC_Converted_Transformed_Range$withinss)
  RM2018_OAC_Converted_Transformed_Range_Best_WSS_C<- data.frame (i, RM2018_OAC_Converted_Transformed_Range_Best_WSS_B, Cluster_RM2018_OAC_Converted_Transformed_Range$tot.withinss, Cluster_RM2018_OAC_Converted_Transformed_Range$betweenss, Cluster_RM2018_OAC_Converted_Transformed_Range$totss)
  RM2018_OAC_Converted_Transformed_Range_Best_WSS_A <- rbind(RM2018_OAC_Converted_Transformed_Range_Best_WSS_A, RM2018_OAC_Converted_Transformed_Range_Best_WSS_C)
  
  if (RM2018_OAC_Converted_Transformed_Range_Best_WSS_B < RM2018_OAC_Converted_Transformed_Range_Best_WSS_N)	
  {
    BestWSS <- Cluster_RM2018_OAC_Converted_Transformed_Range
    RM2018_OAC_Converted_Transformed_Range_Best_WSS_N <- RM2018_OAC_Converted_Transformed_Range_Best_WSS_B
  }
  
  Sys.sleep(0.1)
  setTxtProgressBar(pb1, i)
  #Progress Bar will appear below. (n.b. It may take some time to appear for larger calculations)
}

if(RQOUTPUT=="YES")
{
  save.image("RM2018_OAC_Converted_Transformed_Range_Raw_Cluster_Data.RData")
}

RM2018_OAC_Converted_Transformed_Range_Best_WSS_A <- RM2018_OAC_Converted_Transformed_Range_Best_WSS_A[2:nrow(RM2018_OAC_Converted_Transformed_Range_Best_WSS_A),]
names(RM2018_OAC_Converted_Transformed_Range_Best_WSS_A)<- c("Run_Number", "Mean_Within_Sum_of_Squares", "Total_Within_Sum_of_Squares", "Total_Between_Cluster_Sum_of_Squares", "Total_Within_&_Between_Sum_of_Squares")

BestWSS_Centers<-as.data.frame(BestWSS$centers)

Cluster <-BestWSS$cluster

ClusterPlots <- Cluster

if(RQCLUS=="GROUP")
{
  RM2018_OAC_Converted_Transformed_Range_Best_Cluster_Ltr<-Cluster
  for (rcn in 1:CN)
  {
    RM2018_OAC_Converted_Transformed_Range_Best_Cluster_Ltr[RM2018_OAC_Converted_Transformed_Range_Best_Cluster_Ltr == rcn] <- letters[rcn]
  }
  Cluster <- sprintf("%s%s", Best_Cluster_Input, RM2018_OAC_Converted_Transformed_Range_Best_Cluster_Ltr)
}

if(RQCLUS=="SUBGROUP")
{
  RM2018_OAC_Converted_Transformed_Range_Best_Cluster_LtrNum<-Cluster
  for (rcn in 1:CN)
  {
    RM2018_OAC_Converted_Transformed_Range_Best_Cluster_LtrNum[RM2018_OAC_Converted_Transformed_Range_Best_Cluster_LtrNum == rcn] <- rcn
  }
  Cluster <- sprintf("%s%s", Best_Cluster_Input, RM2018_OAC_Converted_Transformed_Range_Best_Cluster_LtrNum)
}

RM2018_OAC_Converted_Transformed_Range_Data <- data.frame(RM2018_OAC_Converted_Transformed_Range)

RM2018_OAC_Converted_Transformed_Range<- data.frame(RM2018_OAC_Converted_Transformed_Range, Cluster)

Within_Cluster_Sum_of_Squares <-BestWSS$withinss

Points_Within_Cluster <-BestWSS$size

RowNumber <- data.frame(Cluster)

RowNumber <- nrow(RowNumber)

ColNumber <- ncol(RM2018_OAC_Converted_Transformed_Range_Input)

RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata <-data.frame(Within_Cluster_Sum_of_Squares, Points_Within_Cluster, BestWSS$centers)
colnames(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata)[1:2]<-c("Within_Cluster_Sum_of_Squares","Number_Assigned")
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata$Mean_Cluster_Within_Sum_of_Squares<-RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata[,1]/RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata[,2]
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata<-rbind(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata, sapply(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata, mean))
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_Cluster<-mixedsort(unique(RM2018_OAC_Converted_Transformed_Range$Cluster))
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_Cluster<-as.matrix(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_Cluster)
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata$Cluster<-rbind(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_Cluster,"Mean")
RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataCol<-ncol(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata)
RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataMean<-RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataCol-1
RM2018_OAC_Converted_Transformed_Range_Clusters_LastVar<-RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataCol-2
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata<-RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata[,c(RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataCol,1:2, RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataMean, 3:RM2018_OAC_Converted_Transformed_Range_Clusters_LastVar)] 

RM2018_OAC_Converted_Transformed_Range_CSV_Output<-cbind(OA, RM2018_OAC_Converted_Transformed_Range)

dir.create("Cluster Data", showWarnings = T)

write.table(RM2018_OAC_Converted_Transformed_Range_CSV_Output, paste("Cluster Data/RM2018_OAC_Converted_Transformed_Range_", i, "_KMeans_Runs.csv", sep = ""), sep = ",", row.names= FALSE, col.names = TRUE, qmethod = "double")

write.table(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata, paste("Cluster Data/RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_", i, "_Runs.csv", sep = ""), sep = ",", row.names=FALSE, col.names = TRUE, qmethod = "double")

ClusterEnd <- Sys.time()

save.image("RM2018_OAC_Converted_Transformed_Range_Clustered.RData")

ClusterEnd-ClusterStart

####################################################################################################
# End ##############################################################################################
####################################################################################################




