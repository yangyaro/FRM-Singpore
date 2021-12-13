

rm(list = ls())
library(data.table)
library(igraph,warn.conflicts=F)
require(timeDate)
library(stringr)
library(graphics)

setwd("~/Desktop/Master thesis data and code")

FRM = read.csv("FRM_index_20191201_20201231.csv",header = TRUE)
date=gsub("-", "", FRM$Date)

for(t in c(1:270)){
  print(t)
  print(date[t])
  png(file=paste("Matrix_png/AME_",date[t],".png",sep = ''), width=750,height=750,bg="transparent")
  adj0 <- read.csv(file=paste("Matrix/FRM_Matix_SG_",date[t],".csv",sep = ''), header=TRUE,sep=",", row.names=1)
  adj0=as.matrix(adj0)[c(1:20),c(1:20)]
  adj0=apply(adj0, 2,as.numeric)
  netw1=graph_from_adjacency_matrix(adj0,mode="directed",weighted=T)
  V(netw1)$color <- ifelse(V(netw1)$name == "Visa.Inc.x", "orange", "lightgrey")
  colors = rep("Gray", alpha.f = .8, length(E(netw1)))
  colors = ifelse(head_of(netw1, E(netw1))$name == "Visa.Inc.x", 'blue', colors) #inflow
  colors = ifelse(tail_of(netw1, E(netw1))$name == "Visa.Inc.x", 'orange', colors) #outflow
  plot(netw1,
       layout=layout_in_circle,
       vertex.label=colnames(adj0),
       edge.width=0.8,#ifelse(colors == "white", 0, 0.8),
       edge.color=colors,#adjustcolor("Gray", alpha.f = .8),
       edge.arrow.size=0.9,
       edge.arrow.width=1,
  )
  title(xlab= date[t], 
        sub = paste0("FRM: ",round(FRM$FRM[t],5)),
        cex.lab = 1.15,   
        font.lab= 2, 
        cex.sub = 1.15, 
        font.sub = 2,
  )
  dev.off()
}





