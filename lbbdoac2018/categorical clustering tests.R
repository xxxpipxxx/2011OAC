## prepare raw data from residents matrix and cost and debt master for output area clustering
library(dplyr)
library(tidyr)
library(data.table)
library(clustMixType) ## for plots
library(klaR) # for k modes

rm(list = ls())

setwd("C:\\R_projects\\OAC\\lbbdoac\\lbbdoac2018")


RM2018individual_raw_input <- read.csv("file:///C:/R_projects/OAC/lbbdoac/lbbdoac2018/2018_Individual_Raw_Variables.csv")



df1 <- RM2018individual_raw_input[-1]

#df1 <- sample_n(RM2018individual_raw_input, size =10000)

df1 <- df1 %>% mutate_if(is.numeric,as.factor)
df1 <- df1 %>% mutate_if(is.integer,as.factor)

str(df1)

# apply k-prototyps
cluster_individual <- kmodes(df1, 8, iter.max = 15, weighted = F)
clprofiles(cluster_individual, df1) # plots

## attach cluster group to data 

cluster <- cluster_individual$cluster

RM2018individual_raw_input$Cluster <- cluster

## svae rawdata
write.table(RM2018individual_raw_input, "Cluster Data/RM2018individual_raw_input_clustered.csv")

