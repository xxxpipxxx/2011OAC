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
names(RM2018OAC_Input_Lookup)
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

Total_Population_5_to_16  <- rm2018person %>% 
  filter(age20180331 >= 5 & age20180331 <= 16) %>%
  group_by(OA11CD) %>%
  summarise(Total_Population_5_to_16 = n()) %>% 
  complete(OA11CD, fill = list(Total_Population_5_to_16 = 0)) %>% 
  select(Total_Population_5_to_16)

RM2018_1 <- rm2018person %>% # Persons aged 0 to 4
  filter(age20180331 <= 4) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_1 = n()) %>% 
  complete(OA11CD, fill = list(RM2018_1 = 0)) %>% 
  select(RM2018_1)
  

RM2018_2 <- rm2018person %>% # Persons aged 5 to 14
  filter(age20180331 >= 5 & age20180331 <= 14) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_2 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_2 = 0)) %>% 
  select(RM2018_2)

RM2018_3 <- rm2018person %>% # Persons aged 25 to 44
  filter(age20180331 >= 24 & age20180331 <= 44) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_3 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_3 = 0)) %>% 
  select(RM2018_3)

RM2018_4 <- rm2018person %>% # Persons aged 45 to 64
  filter(age20180331 >= 45 & age20180331 <= 54) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_4 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_4 = 0)) %>% 
  select(RM2018_4)

RM2018_5 <- rm2018person %>% # Persons aged 65 to 89
  filter(age20180331 >= 65 & age20180331 <= 89) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_5 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_5 = 0)) %>% 
  select(RM2018_5)

RM2018_6 <- rm2018person %>% # Persons aged 90 and over
  filter(age20180331 >= 90) %>%
  group_by(OA11CD) %>%
  summarise(RM2018_6 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_6 = 0)) %>% 
  select(RM2018_6)

Census_7 <- census_OAC_Input %>% # Persons living in communal esatblishments (Census 2011)
  select(OA,k008) %>% 
  arrange(OA) %>% 
  select(Census_7=k008)

table(rm2018person$ethnicgroup2018)

RM2018_8 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "White British_Irish") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_8 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_8 = 0)) %>% 
  select(RM2018_8)

RM2018_9 <- rm2018person %>% # 
  filter(ethnicgroup2018 == "White: Other") %>%
  group_by(OA11CD) %>%
  summarise(RM2018_9 = n())  %>% 
  complete(OA11CD, fill = list(RM2018_9 = 0)) %>% 
  select(RM2018_9)

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


RM2018_OAC_input <- data.frame(Total_Population, Total_Households, Total_Population_16_and_over, Total_Population_16_to_74, Total_Population_3_and_over, Total_Population_5_to_16,
                               RM2018_1, RM2018_2, RM2018_3, RM2018_4, RM2018_5, RM2018_6, Census_7,
                               RM2018_8, RM2018_9, RM2018_10, RM2018_11, RM2018_12, RM2018_13, RM2018_14, RM2018_15)




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

OAC_Input_PCT_RATIO <- as.matrix(RM2018_OAC_input[,1])
colnames(OAC_Input_PCT_RATIO) <- "OA"


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
    OAC_Input_PCT_RATIO <- cbind(OAC_Input_PCT_RATIO,get(paste(K_Var[n,2],"_PCT",sep="")))
    
    remove(list=paste(K_Var[n,2],"_PCT",sep=""))
    remove(list=c("numerator","denominator"))
    
  } else {#This mirrors the PCT loop, but modified for the ratio data
    
    ratio <- as.data.frame(RM2018_OAC_input[,grep(paste("^",K_Var[n,2],"$",sep=""), colnames(RM2018_OAC_input))])
    assign(paste(K_Var[n,2],"_RATIO",sep=""),ratio)# Calculate %
    assign(paste(K_Var[n,2],"_RATIO",sep=""),setNames(get(paste(K_Var[n,2],"_RATIO",sep="")),paste(K_Var[n,2],"_RATIO",sep="")))#Change column name
    OAC_Input_PCT_RATIO <- cbind(OAC_Input_PCT_RATIO,get(paste(K_Var[n,2],"_RATIO",sep="")))
    remove(list=paste(paste(K_Var[n,2],"_RATIO",sep="")))
    remove(ratio)
  }
}
