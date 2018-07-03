cc_data_clean <- read.csv("C:\\Users\\Ashish Arora\\Desktop\\Case Studies\\CC GENERAL.csv", header = TRUE)


#KPI's defined by me
cc_data_clean$MONTHLY_AVG_PUR <- cc_data_clean$PURCHASES/cc_data_clean$TENURE

cc_data_clean$MONTHLY_AVG_INSTALL_PUR <- cc_data_clean$ONEOFF_PURCHASES/cc_data_clean$TENURE

cc_data_clean$MONTHLY_AVG_ONEOFF_PUR <- cc_data_clean$INSTALLMENTS_PURCHASES/cc_data_clean$TENURE

cc_data_clean$MONTHLY_AVG_CASH_PUR <- cc_data_clean$CASH_ADVANCE/cc_data_clean$TENURE

cc_data_clean$PAY_RATIO <- cc_data_clean$PAYMENTS/cc_data_clean$MINIMUM_PAYMENTS

cc_data_clean$LIMIT_USAGE <- cc_data_clean$BALANCE/cc_data_clean$CREDIT_LIMIT

#missing value treatment
#We can also do mean value imputation but since the number of missing values is less I have imputated the values by 0.
cc_data_clean[is.na(cc_data_clean)]<-0


## FACTOR ANALYSIS 
corrm<- cor(cc_data_clean[,2:24])                                 ### CORRELATION MATRIX

require(psych)
require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

write.csv(eigen_values, "C:\\Users\\Ashish Arora\\Desktop\\Case Studies\\EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY


FA<-fa(r=corrm, 7, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
loadings<-data.frame(FA_SORT$loadings[1:23,]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

write.csv(loadings, "C:\\Users\\Ashish Arora\\Desktop\\Case Studies\\loadings.csv") ### SAVING THE FILE


mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

vars <- c("ONEOFF_PURCHASES",
          "MONTHLY_AVG_INSTALL_PUR",
          "PURCHASES",
          "MONTHLY_AVG_PUR",
          "CASH_ADVANCE",
          "MONTHLY_AVG_CASH_PUR",
          "CREDIT_LIMIT",
          "MONTHLY_AVG_ONEOFF_PUR",
          #"PURCHASES_INSTALLMENTS_FREQUENCY",
          #"PURCHASES_FREQUENCY",
          "LIMIT_USAGE",
          "BALANCE",
          #"PRC_FULL_PAYMENT",
          "PAY_RATIO",
          #"ONEOFF_PURCHASES_FREQUENCY",
          "TENURE"
)
##LOG TRANScc_data_clean_with_log_transformation <- apply(cc_data_clean[vars],2, signedlog10)

signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

cc_data_clean_with_log_transformation <- data.frame(apply(cc_data_clean[vars],2, signedlog10))

descriptive_stats<-t(data.frame(apply(cc_data_clean_with_log_transformation[,vars,drop=F], 2, mystats)))
write.csv(descriptive_stats, "C:\\Users\\Ashish Arora\\Desktop\\Case Studies\\descriptive_stats.csv")

#Outliers
boxplot(cc_data_clean_with_log_transformation[,vars]) #we can observe now that the number of outliers have been significantly decreased with log transformation

#Capping outliers at 99% confidence
cc_data_clean_with_log_transformation$ONEOFF_PURCHASES[cc_data_clean_with_log_transformation$ONEOFF_PURCHASES >3.82541888] <-3.82541888
cc_data_clean_with_log_transformation$MONTHLY_AVG_INSTALL_PUR[cc_data_clean_with_log_transformation$MONTHLY_AVG_INSTALL_PUR >2.75417494] <-2.75417494
cc_data_clean_with_log_transformation$PURCHASES[cc_data_clean_with_log_transformation$PURCHASES >3.95314267] <-3.95314267
cc_data_clean_with_log_transformation$MONTHLY_AVG_PUR[cc_data_clean_with_log_transformation$MONTHLY_AVG_PUR >2.88004277] <-2.88004277
cc_data_clean_with_log_transformation$CASH_ADVANCE[cc_data_clean_with_log_transformation$CASH_ADVANCE >3.98173526] <-3.98173526
cc_data_clean_with_log_transformation$MONTHLY_AVG_CASH_PUR[cc_data_clean_with_log_transformation$MONTHLY_AVG_CASH_PUR >2.95242049] <-2.95242049
cc_data_clean_with_log_transformation$CREDIT_LIMIT[cc_data_clean_with_log_transformation$CREDIT_LIMIT >4.23044892] <-4.23044892
cc_data_clean_with_log_transformation$BALANCE[cc_data_clean_with_log_transformation$BALANCE >3.97029127] <-3.97029127
cc_data_clean_with_log_transformation$PAY_RATIO[cc_data_clean_with_log_transformation$PAY_RATIO >1.69335218] <-1.69335218
cc_data_clean_with_log_transformation$LIMIT_USAGE[cc_data_clean_with_log_transformation$LIMIT_USAGE >0.02409915] <-0.02409915
cc_data_clean_with_log_transformation$TENURE[cc_data_clean_with_log_transformation$TENURE >1.07918125] <-1.07918125
cc_data_clean_with_log_transformation$MONTHLY_AVG_ONEOFF_PUR[cc_data_clean_with_log_transformation$MONTHLY_AVG_ONEOFF_PUR >2.51446821] <-2.51446821


#Prepare final Data
#standardizing the data
cc_data_clean_with_log_transformation <-data.frame(cc_data_clean_with_log_transformation)
inputdata_final = scale(cc_data_clean_with_log_transformation)
inputdata_final<-data.frame(inputdata_final)
inputdata_final[is.na(inputdata_final)] <- 0

#View(inputdata_final)
#building clusters using k-means clustering 

cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)



cc_data_new<-cbind(cc_data_clean,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )

#Graph based on k-means - Optional
require(cluster)

clusplot(inputdata_final, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

###Profiling

#Converting into factors
cc_data_new$km_clust_3=factor(cc_data_new$km_clust_3)
cc_data_new$km_clust_4=factor(cc_data_new$km_clust_4)
cc_data_new$km_clust_5=factor(cc_data_new$km_clust_5)
cc_data_new$km_clust_6=factor(cc_data_new$km_clust_6)

require(tables)
profile<-tabular(1+BALANCE+MONTHLY_AVG_PUR+PAYMENTS+
                   PURCHASES_TRX+MONTHLY_AVG_CASH_PUR+
                   CASH_ADVANCE_TRX+CASH_ADVANCE_FREQUENCY+
                   MONTHLY_AVG_ONEOFF_PUR+INSTALLMENTS_PURCHASES+
                   PURCHASES_INSTALLMENTS_FREQUENCY+PURCHASES_FREQUENCY+
                   LIMIT_USAGE+BALANCE+PRC_FULL_PAYMENT+
                   BALANCE_FREQUENCY+MINIMUM_PAYMENTS+ONEOFF_PURCHASES_FREQUENCY
                 +ONEOFF_PURCHASES+CASH_ADVANCE+CREDIT_LIMIT+TENURE+MONTHLY_AVG_INSTALL_PUR+PAY_RATIO+
                   MONTHLY_AVG_ONEOFF_PUR+MONTHLY_AVG_CASH_PUR+MONTHLY_AVG_PUR ~mean+(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6),
                 data=cc_data_new)
profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
View(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=cc_data_new)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,"C:\\Users\\Ashish Arora\\Desktop\\Case Studies\\profile1.csv",row.names = F)
write.csv(profile2,"C:\\Users\\Ashish Arora\\Desktop\\Case Studies\\profile2.csv",row.names = F)
#############################END OF k-Means Segmentation############################
