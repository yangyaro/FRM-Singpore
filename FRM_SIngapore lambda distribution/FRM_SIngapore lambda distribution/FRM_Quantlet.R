

## 0. Preparation

rm(list = ls(all = TRUE))
graphics.off()

# adjust your working directory at your convenience:


setwd("~/Desktop/Master thesis data and code")

source("R_FRM_Statistics_Algorithm.R")

## 1. Data Preprocess

#Preprocess the data
Mktcap_new=read.csv( "cap-of-coporation20190603-20201231.csv", header = TRUE )
Stock_Prices_new = read.csv("cap-of-coporation20190603-20201231.csv", header = TRUE)
Macro_new = read.csv("Sing-economic-variable20190603-20201231.csv", header = TRUE)
Mktcap_new$ticker=as.Date(Mktcap_new$ticker)
Stock_Prices_new$ticker=as.Date(Stock_Prices_new$ticker)
Macro_new$ticker=as.Date(Macro_new$ticker)
##Important!
colnames(Mktcap_new)==colnames(Stock_Prices_new)
#Mktcap$ticker==Stock_Prices$ticker

##Same Date 
All_prices= merge(Stock_Prices_new, Macro_new,by = "ticker")
All_data= merge(All_prices,Mktcap_new,by = "ticker")
Stock_Prices=All_data[,1:ncol(Stock_Prices_new )]
Macro=All_data[,c(1,(ncol(Stock_Prices_new)+1):(ncol(Stock_Prices_new)+6))]
Mktcap=All_data[,c(1,(ncol(Stock_Prices_new)+7):ncol(All_data))]

# Load the market capitalization data matrix
Mktcap = Mktcap[2 : nrow(Mktcap), 2 : ncol(Mktcap)]
Mktcap[is.na(Mktcap)] = 0

# Load the stock prices and macro-prudential data matrix
All_prices= merge(Stock_Prices, Macro, by = "ticker", all.x = TRUE)

# Calculate the daily return and differences matrix of all selected financial companies and macro-prudential variables; use exponential function for selected macro-prudential variables that are expressed in first order differences
All_prices$U10YR2YR= exp(All_prices$U10YR2YR)
All_prices$U2YINDEX= exp(All_prices$U2YINDEX)

All_return= diff(log(cbind(as.numeric(gsub("-", "", All_prices$ticker)), as.matrix(All_prices[, (2 : ncol(All_prices))]))))
All_return[, 1] = as.numeric(gsub("-", "", All_prices$ticker[2 : nrow(All_prices)]))
All_return[is.na(All_return)] = 0
#write.table(All_return[,1:399], "Data_America.csv", row.names = F, col.names = T)

# Sorting the Market Capitalization Data
FRM_Sort = function(Data){Data[is.na(Data)] = 0; sort(Data, decreasing = TRUE, index.return = TRUE)}

# Determininig the index number of each company according to decreasing market capitalization
Mktcap_Index = matrix(0, nrow(Mktcap), ncol(Mktcap))
# Determininig the market capitalization value of each company according to decreasing market capitalization
Mktcap_Value = matrix(0, nrow(Mktcap), ncol(Mktcap))
Time_Start = Sys.time()
for (t in seq(1, nrow(Mktcap), 1)){
  Mktcap_Index[t, ] = t(apply(Mktcap, 1, FRM_Sort))[1, t][[1]] $ix
  Mktcap_Value[t, ] = data.matrix(t(apply(Mktcap, 1, FRM_Sort))[1, t][[1]]$x)
}

Mktcap_Index = cbind(All_return[, 1], Mktcap_Index)

Mktcap_Value = cbind(All_return[, 1], Mktcap_Value)
Time_End = Sys.time()

## 2. Save Results

Time_End - Time_Start



## 3. Data input for the model


All_return[is.na(All_return)] = 0
# Stock_Returns column=the numbers of Americas_Market_Capitalizations_Index
All_prices = All_return[, 1 : ncol(Mktcap_Index)]
#Macrovariables 
Macro_return = All_return[, (ncol(Mktcap_Index) + 1) : ncol(All_return)] 


## 4. Estimation

J = 50    # Number of largest financial companies
s = 30       # Estimation Window Size, s = 63
tau = 0.05    # Tail Risk Level, tau = 0.05
I = 20         # Number of Iterations, I = 20

Date_Start = 20191201
Date_End = 20201231
# calculate how many days for FRM series
r = sum(((All_return[, 1] >= Date_Start) & (All_return[, 1] <= Date_End)) * matrix(1, nrow(All_return), 1))
#calculte the total days in the document  
Index_End = max(((All_return[, 1] >= Date_Start) & (All_return[, 1] <= Date_End)) * matrix(1 : nrow(All_return), nrow(All_return), 1))
FRM_SG_Statistics_Estimation_Matrix = matrix(0, r, ((ncol(All_return) - 1) * (ncol(All_return) - 1 + 2) + 2))
dim(FRM_SG_Statistics_Estimation_Matrix)
FRM_SG_Statistics_Estimation_Matrix[, dim(FRM_SG_Statistics_Estimation_Matrix)[2]] = All_return[(Index_End - r + 1) : (Index_End), 1]

FRM_matix=matrix(0,J,J+6)
FRM_series=matrix(0,r,J+1)
FRM_series[,1]= All_return[(Index_End - r + 1) : (Index_End), 1]
FRM_series_final=matrix(0,r,2)
FRM_series_final[,1]=All_return[(Index_End - r + 1) : (Index_End), 1]

#Make companies constant Select the biggest companies 

Data0 = cbind(All_return[(Index_End - r + 1 - s + 1) : (Index_End - r + 1), (as.matrix(as.numeric(Mktcap_Index[Index_End - r + 1, 2 : ncol(Mktcap_Index)][1 : J])+1))], Macro_return[(Index_End - r + 1 - s) : (Index_End - r), ])

for (t in ((Index_End - r + 1) : (Index_End))){ 
  for (j in 1 : J){ 
    ##change comapnis
    #Data = cbind(All_return[(t - s + 1) : t, (as.matrix(Mktcap_Index[t, 2 : ncol(Mktcap_Index)][1 : J] + 1))], Macro_return[(t - s + 1) : t, ])
    ##same companies
    
  Data = cbind(All_return[(t - s + 1) : t, colnames(Data0)[1:J]], Macro_return[(t - s) : (t-1), ])
    Data_Market_Index = cbind(Mktcap_Index[t, 1 : (J + 1)], t(as.matrix(c(ncol(Mktcap_Index) : (ncol(All_return) - 1)))) %x% matrix(1, 1, 1))
    #FRM_Quantile_Regression
    Est = FRM_Quantile_Regression(as.matrix(Data), j, tau, I)
    Est_hat = t(as.matrix(Est$beta[which(Est$Cgacv == min(Est$Cgacv)), ]))
    Est_lambda_hat = abs(data.matrix(Est$lambda[which(Est$Cgacv == min(Est$Cgacv))]))
    Est_FRM_Condition_hat = Est$FRM_Condition
    Vector_Data_Market_Index = Data_Market_Index[, c(-1, -(j + 1))] #Data_Market_Index[, c(-1, -101))] -1，-101 delete the column 105 columns
    FRM_series[r-(Index_End-t),j+1]=Est_lambda_hat                                                                                      
    
    #R_save adjacency matrix
    if (j==1){
      FRM_matix[j,c(2:(J+6))]=Est_hat
    } else {
      FRM_matix[j,c(1:(j-1),(j+1):(J+6))]=Est_hat
    }
  }
  FRM_SG_matix=FRM_matix[,c(1:J)]
  colnames(FRM_SG_matix)=colnames(Data)[1:J]
  rownames(FRM_SG_matix)=colnames(Data)[1:J]
  write.csv(FRM_SG_matix, paste0("Matrix/FRM_Matix_SG_",All_return[(Index_End - r + 1) : (Index_End), 1][t-(Index_End - r )], ".csv"),  quote = FALSE) 
}

FRM_series_final[,2]=rowSums(FRM_series[,c(2:(J+1))])/J

## 5. Updated FRM index
dt=FRM_series_final[,1]
dFRM=round(FRM_series_final[,2],9)
dt_format=as.Date(as.character(FRM_series_final[,1]),  "%Y%m%d")
dt_format = as.Date(dt_format, format = "%Y-%m-%d")
dt_save=as.character(dt_format)
Final_FRM = cbind(dt_save, dFRM)
colnames(Final_FRM )=c('Date','FRM')
write.csv(Final_FRM, paste0("FRM_index_", Date_Start, "_", Date_End, ".csv"),row.names = FALSE,  quote = FALSE) 

## 6. Lambda_for_each_company
colnames(FRM_series)=append(as.character("Date"),colnames(Data0)[1:50])
write.csv(FRM_series, paste0("FRM_Lambda_for_each_company_", Date_Start, "_", Date_End, ".csv"),row.names = FALSE,  quote = FALSE) 


## 7. Top twenty risky companies for last day
Risk_com_top20=cbind(colnames(Data)[order(FRM_series[r,c(2:51)],decreasing = TRUE)[1:20]],sort(FRM_series[r,c(2:51)],decreasing = TRUE)[1:20])
colnames(Risk_com_top20)=c('Company','Risk')
write.csv(Risk_com_top20, paste0("SG_R_FRM_Top20_Risky_Companies_",Date_End, ".csv"),row.names = FALSE,  quote = FALSE) 



##  8.plot
library(ggplot2)
#library(cowplot)
FRM_series=read.csv( "FRM_Lambda_for_each_company_20191201_20201231.csv", header = TRUE)
x <- rep(FRM_series[,1], 50)
x=as.factor(x)
y <- FRM_series[,2] 
for (i in (3:51)){
  y=append(y,FRM_series[,i])
}

df <- data.frame(x=x, y=y)

p = ggplot(df, aes(x=x, y=y)) +  
  geom_boxplot() + 
  stat_summary(fun=max, geom="line", aes(group=1),colour = "red")  + 
  stat_summary(fun=max, geom="point", aes(group=1),colour = "red") +
  stat_summary(fun=mean, geom="line", aes(group=1),colour = "blue") +
  stat_summary(fun=mean, geom="point",colour = "blue") +
  theme( panel.grid=element_blank(), 
         axis.title = element_blank(),
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA),
         legend.box.background = element_rect(fill = "transparent"),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(angle = 90, hjust = 1,size =12 ),
         axis.text.y = element_text(size =14 )
  )
png('AME.png',width=900,height=600,bg="transparent")
p
dev.off()
