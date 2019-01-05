library(plotrix)
library(data.table)
library(dplyr)

rm(list = ls())
df <- fread("file:///C:/R_projects/OAC/2011OAC/lbbdoac2018/Clustergroups/01_RM2018_OACclusters_Percentages.csv")

num <-df[-9,-1]
denom<-df[9,-1]


num <- as.data.frame(num)
denom <- as.data.frame(lapply(denom, rep, 8))

scaled.df <- num-denom

#scaled.df <- scale(df[,-1])

cluster1names <- colnames(scaled.df)

cluster1names <- gsub("RM2018_", "",cluster1names)
cluster1names <- fread("2018_OAC_Raw_Variables_Lookup.csv")
cluster1names <- cluster1names[12:47,9]
cluster1names <- cluster1names$variable_short

limits <- c(-30,30) #scales between
#limits <- NULL # defauly auto scale


radial.plot(scaled.df[1,1:ncol(scaled.df)], labels=cluster1names, start=0,clockwise=T, rp.type="p", line.col="firebrick1",
            lwd=3, show.grid=T, grid.col="blue", point.symbols=3,point.col="black",show.centroid=F, radlab = F, radial.lim =limits)

radial.plot(scaled.df[2,1:ncol(scaled.df)], labels=cluster1names, start=0,clockwise=T, rp.type="p", line.col="firebrick1",
            lwd=3, show.grid=T, grid.col="blue", point.symbols=3,point.col="black",show.centroid=F, radlab = F, radial.lim =limits)

radial.plot(scaled.df[3,1:ncol(scaled.df)], labels=cluster1names, start=0,clockwise=T, rp.type="p", line.col="firebrick1",
            lwd=3, show.grid=T, grid.col="blue", point.symbols=3,point.col="black",show.centroid=F, radlab = F, radial.lim =limits)

radial.plot(scaled.df[4,1:ncol(scaled.df)], labels=cluster1names, start=0,clockwise=T, rp.type="p", line.col="firebrick1",
            lwd=3, show.grid=T, grid.col="blue", point.symbols=3,point.col="black",show.centroid=F, radlab = F, radial.lim =limits)

radial.plot(scaled.df[5,1:ncol(scaled.df)], labels=cluster1names, start=0,clockwise=T, rp.type="p", line.col="firebrick1",
            lwd=3, show.grid=T, grid.col="blue", point.symbols=3,point.col="black",show.centroid=F, radlab = F, radial.lim =limits)

radial.plot(scaled.df[6,1:ncol(scaled.df)], labels=cluster1names, start=0,clockwise=T, rp.type="p", line.col="firebrick1",
            lwd=3, show.grid=T, grid.col="blue", point.symbols=3,point.col="black",show.centroid=F, radlab = F, radial.lim =limits)

radial.plot(scaled.df[7,1:ncol(scaled.df)], labels=cluster1names, start=0,clockwise=T, rp.type="p", line.col="firebrick1",
            lwd=3, show.grid=T, grid.col="blue", point.symbols=3,point.col="black",show.centroid=F, radlab = F, radial.lim =limits)

radial.plot(scaled.df[8,1:ncol(scaled.df)], labels=cluster1names, start=0,clockwise=T, rp.type="p", line.col="firebrick1",
            lwd=3, show.grid=T, grid.col="blue", point.symbols=3,point.col="black",show.centroid=F, radlab = F, radial.lim =limits)

