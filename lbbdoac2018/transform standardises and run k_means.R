## get reured pre prepared files, create percentages, transform and standardise then run k_means

library(dplyr)
library(tidyr)
library(data.table)
library(car)
library(gtools)

rm(list = ls())
setwd("C:\\R_projects\\OAC\\2011OAC\\lbbdoac2018")

## get preprepared varisbles created from R script "prepare RM2018oac raw variabes.R

RM2018_OAC_input <- read.csv("2018_OAC_Raw_Variables.csv", stringsAsFactors = T)
#check for numeric ( as imports as integer)
RM2018_OAC_input[-1] <- RM2018_OAC_input[-1] %>% mutate_all(as.numeric)
str(RM2018_OAC_input)


## get variable lookup table

RM2018OAC_Input_Lookup <- read.csv("2018_OAC_Raw_Variables_Lookup.csv", sep=",", stringsAsFactors = F)
## create Raw Variables


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
rownames(RM2018_OAC_Input_PCT_RATIO_TRANSFORM_RANGE) <- RM2018_OAC_Input_PCT_RATIO$OA11CD

colnames(RM2018_OAC_Input_PCT_RATIO_TRANSFORM_RANGE)<- K_Var$VariableCode

RM2018_OAC_Converted_Transformed_Range <-cbind(data.frame(RM2018_OAC_Input_PCT_RATIO$OA11CD),RM2018_OAC_Input_PCT_RATIO_TRANSFORM_RANGE)
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
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata$Mean_Cluster_Within_Sum_of_Squares<-
  RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata[,1]/RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata[,2]
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata<-rbind(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata, sapply(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata, mean))
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_Cluster<-mixedsort(unique(RM2018_OAC_Converted_Transformed_Range$Cluster))
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_Cluster<-as.matrix(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_Cluster)
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata$Cluster<-rbind(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_Cluster,"Mean")
RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataCol<-ncol(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata)
RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataMean<-RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataCol-1
RM2018_OAC_Converted_Transformed_Range_Clusters_LastVar<-RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataCol-2
RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata<-
  RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata[,c(RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataCol,1:2, RM2018_OAC_Converted_Transformed_Range_Cluster_MetadataMean, 3:RM2018_OAC_Converted_Transformed_Range_Clusters_LastVar)] 

RM2018_OAC_Converted_Transformed_Range_CSV_Output<-cbind(OA, RM2018_OAC_Converted_Transformed_Range)

dir.create("Cluster Data", showWarnings = F)

write.table(RM2018_OAC_Converted_Transformed_Range_CSV_Output, paste("Cluster Data/RM2018_OAC_Converted_Transformed_Range_", i, "_KMeans_Runs.csv", sep = ""), sep = ",", row.names= FALSE, col.names = TRUE, qmethod = "double")

write.table(RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata, paste("Cluster Data/RM2018_OAC_Converted_Transformed_Range_Cluster_Metadata_", i, "_Runs.csv", sep = ""), sep = ",", row.names=FALSE, col.names = TRUE, qmethod = "double")

ClusterEnd <- Sys.time()

save.image("RM2018_OAC_Converted_Transformed_Range_Clustered.RData")

ClusterEnd-ClusterStart

####################################################################################################
# End ##############################################################################################
####################################################################################################

