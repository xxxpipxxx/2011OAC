## test suitability of clusters 

library(dplyr)
library(tidyr)
library(data.table)

rm(list = ls())

setwd("C:\\R_projects\\OAC\\lbbdoac\\lbbdoac2018")


## get pre cluster percents and counts

preclusterV1PER  <- fread("Pre-Cluster Data/01_RM2018_OAC_Percentages.csv")
preclusterV1COUNT <- fread("2018_OAC_Raw_Variables.csv")


## get transformed and standardised data 

clusterV1  <- fread("Cluster Data/RM2018_OAC_Converted_Transformed_Range_1000_KMeans_Runs.csv")
clusterV1summary <- fread("Cluster Data/RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_1000_Runs.csv")

## get variable lookup table

RM2018OAC_Input_Lookup <- read.csv("2018_OAC_Raw_Variables_Lookup.csv", sep=",", stringsAsFactors = F)


compare_clusters <- preclusterV1COUNT %>% 
  left_join(clusterV1 %>% select(OA11CD = OA, Cluster))

compare_clusters <- compare_clusters[-1] %>% 
  group_by(Cluster) %>% 
  summarise_all(sum)

colsums <- colSums(compare_clusters)

compare_clusters <- bind_rows(compare_clusters, colsums)
compare_clusters$Cluster <- as.character(compare_clusters$Cluster)
compare_clusters$Cluster[9] <- "sum"


names(compare_clusters)

####################################################################################################
# Calculate Percentages on clustered groups ########################################################
####################################################################################################

# do we want to save the clustered summaries?
RQOUTPUT <- "YES"

# count denominators to use by first getting relavant number of columns

count_denominators <- length(compare_clusters %>% select(starts_with("Total")))

#rows which contain denominators: this will be used to get relevant rows from the lookup
D_Col <- 1:count_denominators 

#Get numerator denominator list
K_Col <- (max(D_Col)+1):nrow(RM2018OAC_Input_Lookup)
K_Var <- RM2018OAC_Input_Lookup[K_Col,c(1,4)]
D_List <- RM2018OAC_Input_Lookup[D_Col,c(1,4)]

K_Var <- merge(K_Var,D_List, by="VariableStatisticalUnit",all.x=TRUE)
colnames(K_Var) <- c("VariableStatisticalUnit","VariableCode", "Denominator")
K_Var <- K_Var[order(K_Var$VariableCode),] 

RM2018_OAC_Input_PCT_RATIO <- as.matrix(compare_clusters[,1])
colnames(RM2018_OAC_Input_PCT_RATIO) <- "OA11CD"

#n <- 9
# Calculate loop
for (n in 1:nrow(K_Var)){
  
  if (!is.na(K_Var[n,3])) {
    
    #Uses grep to get the column id matching the current numerator
    numerator <- compare_clusters[,grep(paste("^",K_Var[n,2],"$",sep=""), colnames(compare_clusters))]
    
    #Uses grep to get the column id matching the current denominator
    denominator <- compare_clusters[,grep(paste("^",K_Var[n,3],"$",sep=""), colnames(compare_clusters))]
    
    assign(paste(K_Var[n,2],"_PCT",sep=""),as.data.frame(numerator/denominator*100))# Calculate %
    assign(paste(K_Var[n,2],"_PCT",sep=""),setNames(get(paste(K_Var[n,2],"_PCT",sep="")),paste(K_Var[n,2],"_PCT",sep="")))#Change column name
    
    #Create output files
    RM2018_OAC_Input_PCT_RATIO <- cbind(RM2018_OAC_Input_PCT_RATIO,get(paste(K_Var[n,2],"_PCT",sep="")))
    
    remove(list=paste(K_Var[n,2],"_PCT",sep=""))
    remove(list=c("numerator","denominator"))
    
  } else {#This mirrors the PCT loop, but modified for the ratio data
    
    ratio <- as.data.frame(compare_clusters[,grep(paste("^",K_Var[n,2],"$",sep=""), colnames(compare_clusters))])
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
  dir.create("Clustergroups", showWarnings = FALSE)
  write.table(RM2018_OAC_Input_PCT_RATIO, paste("Clustergroups/01_RM2018_OACclusters_Percentages.csv", sep = ""), sep = ",", row.names= FALSE, col.names = TRUE, qmethod = "double")
}

cluster_groups <- fread("Clustergroups/01_RM2018_OACclusters_Percentages.csv")


barplot(cluster_groups$RM2018_06[1:8], names.arg= c(1:8), col = "blue" )#  Persons aged 90 plus
abline(h = cluster_groups$RM2018_06[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_07[1:8], names.arg= c(1:8), col = "blue" )#  Persons living in communal establishment
abline(h = cluster_groups$RM2018_07[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_08[1:8], names.arg= c(1:8), col = "blue" )#  White British
abline(h = cluster_groups$RM2018_08[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_09[1:8], names.arg=  c(1:8), col = "blue" ) # White Other
abline(h = cluster_groups$RM2018_09[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_10[1:8], names.arg=  c(1:8), col = "blue" ) # Indian
abline(h = cluster_groups$RM2018_10[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_11[1:8], names.arg=  c(1:8), col = "blue" ) # Pakistani
abline(h = cluster_groups$RM2018_11[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_12[1:8], names.arg=  c(1:8), col = "blue" ) # Bangladeshi
abline(h = cluster_groups$RM2018_12[9], col = "red" , lwd=4) 

barplot(cluster_groups$RM2018_13[1:8], names.arg=  c(1:8), col = "blue" ) # Other
abline(h = cluster_groups$RM2018_13[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_14[1:8], names.arg=  c(1:8), col = "blue" ) # Black African
abline(h = cluster_groups$RM2018_14[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_15[1:8], names.arg=  c(1:8), col = "blue" ) # Black Carribean
abline(h = cluster_groups$RM2018_15[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_17[1:8], names.arg=  c(1:8), col = "blue" ) # Proficiency in English
abline(h = cluster_groups$RM2018_17[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_18[1:8], names.arg=  c(1:8), col = "blue" ) # Households with no children under 16
abline(h = cluster_groups$RM2018_18[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_21[1:8], names.arg=  c(1:8), col = "blue" ) # cohabiting adult households no under 16's
abline(h = cluster_groups$RM2018_21[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_22[1:8], names.arg=  c(1:8), col = "blue" ) # Family households with dependent children
abline(h = cluster_groups$RM2018_22[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_23[1:8], names.arg=  c(1:8), col = "blue" ) # older cohabiting households
abline(h = cluster_groups$RM2018_23[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_24[1:8], names.arg=  c(1:8), col = "blue" ) # older person living alone
abline(h = cluster_groups$RM2018_24[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_25[1:8], names.arg=  c(1:8), col = "blue" ) # single adult households
abline(h = cluster_groups$RM2018_25[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_26[1:8], names.arg=  c(1:8), col = "blue" ) # single adult households with dependant children
abline(h = cluster_groups$RM2018_26[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_27[1:8], names.arg=  c(1:8), col = "blue" ) # three generational households
abline(h = cluster_groups$RM2018_27[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_28[1:8], names.arg=  c(1:8), col = "blue" ) # Households who own or have shared ownership of property
abline(h = cluster_groups$RM2018_28[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_29[1:8], names.arg=  c(1:8), col = "blue" ) # Households who are Social Renting 
abline(h = cluster_groups$RM2018_29[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_30[1:8], names.arg=  c(1:8), col = "blue" ) # Households who are Private Renting 
abline(h = cluster_groups$RM2018_30[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_31[1:8], names.arg=  c(1:8), col = "blue" ) # Households who are Reside Renting 
abline(h = cluster_groups$RM2018_31[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_32[1:8], names.arg=  c(1:8), col = "blue" ) # Individuals day-to-day activities limited a lot or a little (Standardised Illness Ratio)
abline(h = cluster_groups$RM2018_32[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_33[1:8], names.arg=  c(1:8), col = "blue" ) # Persons providing unpaid care
abline(h = cluster_groups$RM2018_33[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_34[1:8], names.arg=  c(1:8), col = "blue" ) # Persons aged over 16 whose highest level of qualification is Level 1, Level 2 or Apprenticeship
abline(h = cluster_groups$RM2018_34[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_35[1:8], names.arg=  c(1:8), col = "blue" ) # Persons aged over 16 whose highest level of qualification is Level 3 qualifications
abline(h = cluster_groups$RM2018_35[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_36[1:8], names.arg=  c(1:8), col = "blue" ) # Persons aged over 16 whose highest level of qualification is Level 4 qualifications and above
abline(h = cluster_groups$RM2018_36[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_40[1:8], names.arg=  c(1:8), col = "blue" ) # Households Receiving Housing Benefit
abline(h = cluster_groups$RM2018_40[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_41[1:8], names.arg=  c(1:8), col = "blue" ) # Households Receiving Council Tax Relief
abline(h = cluster_groups$RM2018_41[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_42[1:8], names.arg=  c(1:8), col = "blue" ) # Households Receiving Council Tax Relief & Housing Benefit
abline(h = cluster_groups$RM2018_42[9], col = "red" , lwd=4)

barplot(cluster_groups$RM2018_43[1:8], names.arg=  c(1:8), col = "blue" ) # Persons recieving free school meals
abline(h = cluster_groups$RM2018_43[9], col = "red" , lwd=4)



