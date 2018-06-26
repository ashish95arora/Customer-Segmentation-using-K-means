libname segment '/folders/myfolders/Segmentation Case Study';

proc import datafile='/folders/myfolders/Segmentation Case Study/CC GENERAL.csv' out=segment.data dbms=csv replace; 
guessingrows=100; 
run; 

proc contents data=segment.data;
run;

/*KPI's defined by me*/
data segment.data;
set segment.data;
MONTHLY_AVG_PUR=PURCHASES/TENURE;
MONTHLY_AVG_INSTALL_PUR=ONEOFF_PURCHASES/TENURE;
MONTHLY_AVG_ONEOFF_PUR=INSTALLMENTS_PURCHASES/TENURE;
MONTHLY_AVG_CASH_PUR=CASH_ADVANCE/TENURE;
PAY_RATIO=PAYMENTS/MINIMUM_PAYMENTS;
LIMIT_USAGE=BALANCE/CREDIT_LIMIT;
run;

data segment.data;
   set segment.data;
   array change _numeric_;
        do over change;
            if change=. then change=0;
        end;
 run;


/*Factor Analysis*/
/*In R we used the METHOD as MAXIMUM LIKELIHOOD but after using this method in SAS we see that
 Communality is greater than 1.0 so this FACTOR ANALYSIS is an ultraheywood case.
 Though we can force the factor analysis to run but this will not be fully optimum.
So in SAS I am using PRINCIPAL METHOD and will observe the difference.
*/

ods html file='/folders/myfolders/Segmentation Case Study/factor_analysis1.xls';
PROC FACTOR DATA= segment.data
METHOD = Principal SCREE MINEIGEN=0 NFACTOR = 7
ROTATE = VARIMAX REORDER OUT= Factor ultraheywood;
var 
BALANCE	BALANCE_FREQUENCY	PURCHASES	ONEOFF_PURCHASES	INSTALLMENTS_PURCHASES	CASH_ADVANCE	PURCHASES_FREQUENCY	ONEOFF_PURCHASES_FREQUENCY	PURCHASES_INSTALLMENTS_FREQUENCY	CASH_ADVANCE_FREQUENCY	CASH_ADVANCE_TRX	PURCHASES_TRX	CREDIT_LIMIT	PAYMENTS	MINIMUM_PAYMENTS	PRC_FULL_PAYMENT	TENURE 
MONTHLY_AVG_PUR
MONTHLY_AVG_INSTALL_PUR
MONTHLY_AVG_ONEOFF_PUR
MONTHLY_AVG_CASH_PUR
PAY_RATIO
LIMIT_USAGE
;
run;
ods html close;
/*creating copy of the ORIGINAL VARIABLES befor segmentation is applied*/
data segment.data;
set segment.data;
z_BALANCE	=BALANCE;
z_BALANCE_FREQUENCY=BALANCE_FREQUENCY;
z_PURCHASES=PURCHASES;
z_ONEOFF_PURCHASES=ONEOFF_PURCHASES;
z_INSTALLMENTS_PURCHASES=	INSTALLMENTS_PURCHASES;
z_CASH_ADVANCE=CASH_ADVANCE;
z_PURCHASES_FREQUENCY	=PURCHASES_FREQUENCY;
z_ONEOFF_PURCHASES_FREQUENCY=	ONEOFF_PURCHASES_FREQUENCY;
z_PURCHASES_INSTALL_FREQUENCY=	PURCHASES_INSTALLMENTS_FREQUENCY;
z_CASH_ADVANCE_FREQUENCY	=CASH_ADVANCE_FREQUENCY;
z_CASH_ADVANCE_TRX=CASH_ADVANCE_TRX;
z_PURCHASES_TRX=PURCHASES_TRX;
z_CREDIT_LIMIT=CREDIT_LIMIT;	
z_PAYMENTS=	PAYMENTS;
z_MINIMUM_PAYMENTS=	MINIMUM_PAYMENTS;
z_PRC_FULL_PAYMENT=	PRC_FULL_PAYMENT;
z_TENURE =TENURE;
z_MONTHLY_AVG_PUR=MONTHLY_AVG_PUR;
z_MONTHLY_AVG_INSTALL_PUR=MONTHLY_AVG_INSTALL_PUR;
z_MONTHLY_AVG_ONEOFF_PUR=MONTHLY_AVG_ONEOFF_PUR;
z_MONTHLY_AVG_CASH_PUR=MONTHLY_AVG_CASH_PUR;
z_PAY_RATIO=PAY_RATIO;
z_LIMIT_USAGE=LIMIT_USAGE;
  run;


data segment.data_transformed;
set segment.data;
BALANCE	=log(BALANCE+1);
BALANCE_FREQUENCY=log(BALANCE_FREQUENCY+1);
PURCHASES=log(PURCHASES+1);
ONEOFF_PURCHASES=log(ONEOFF_PURCHASES+1);
INSTALLMENTS_PURCHASES=	log(INSTALLMENTS_PURCHASES+1);
CASH_ADVANCE=log(CASH_ADVANCE+1);
PURCHASES_FREQUENCY	=log(PURCHASES_FREQUENCY+1);
ONEOFF_PURCHASES_FREQUENCY=	log(ONEOFF_PURCHASES_FREQUENCY+1);
PURCHASES_INSTALL_FREQUENCY=	log(PURCHASES_INSTALLMENTS_FREQUENCY+1);
CASH_ADVANCE_FREQUENCY	=log(CASH_ADVANCE_FREQUENCY+1);
CASH_ADVANCE_TRX=log(CASH_ADVANCE_TRX+1);
PURCHASES_TRX=log(PURCHASES_TRX+1);
CREDIT_LIMIT=log(CREDIT_LIMIT	+1);
PAYMENTS=	log(PAYMENTS+1);
MINIMUM_PAYMENTS=	log(MINIMUM_PAYMENTS+1);
PRC_FULL_PAYMENT=	log(PRC_FULL_PAYMENT+1);
TENURE =log(TENURE+1);
MONTHLY_AVG_PUR=log(MONTHLY_AVG_PUR+1);
MONTHLY_AVG_INSTALL_PUR=log(MONTHLY_AVG_INSTALL_PUR+1);
MONTHLY_AVG_ONEOFF_PUR=log(MONTHLY_AVG_ONEOFF_PUR+1);
MONTHLY_AVG_CASH_PUR=log(MONTHLY_AVG_CASH_PUR+1);
PAY_RATIO=log(PAY_RATIO+1);
LIMIT_USAGE=log(LIMIT_USAGE+1);
  run;
  
proc means data=segment.data_transformed min max mean std var P95;
var 
BALANCE	BALANCE_FREQUENCY	PURCHASES	ONEOFF_PURCHASES	INSTALLMENTS_PURCHASES	CASH_ADVANCE	PURCHASES_FREQUENCY	ONEOFF_PURCHASES_FREQUENCY	PURCHASES_INSTALLMENTS_FREQUENCY	CASH_ADVANCE_FREQUENCY	CASH_ADVANCE_TRX	PURCHASES_TRX	CREDIT_LIMIT	PAYMENTS	MINIMUM_PAYMENTS	PRC_FULL_PAYMENT	TENURE 
MONTHLY_AVG_PUR
MONTHLY_AVG_INSTALL_PUR
MONTHLY_AVG_ONEOFF_PUR
MONTHLY_AVG_CASH_PUR
PAY_RATIO
LIMIT_USAGE
;
run;

/*Setting the cut off limit at 95% confidence level*/
data temp;
set segment.data_transformed;
 if BALANCE>8.6848251 then BALANCE=8.6848251;
 if PURCHASES>8.2942796 then PURCHASES=8.2942796;
 if ONEOFF_PURCHASES>7.8920784 then ONEOFF_PURCHASES=7.8920784;
 if INSTALLMENTS_PURCHASES>1753.08 then INSTALLMENTS_PURCHASES=1753.08;
 if CASH_ADVANCE>8.4456313 then CASH_ADVANCE=8.4456313;
 if CASH_ADVANCE_FREQUENCY>0.5833330 then CASH_ADVANCE_FREQUENCY=0.5833330;
 if CASH_ADVANCE_TRX>15.0000000 then CASH_ADVANCE_TRX=15.0000000;
 if PURCHASES_TRX>57.0000000 then PURCHASES_TRX=57.0000000;
  if CREDIT_LIMIT>9.3927453 then CREDIT_LIMIT=9.3927453;
   if PAYMENTS>6083.43 then PAYMENTS=6083.43;
    if MINIMUM_PAYMENTS>2722.22 then MINIMUM_PAYMENTS=2722.22;
    if TENURE>2.5649494 then TENURE=2.5649494;
    if MONTHLY_AVG_PUR >5.8307504 then MONTHLY_AVG_PUR=5.8307504;
    if MONTHLY_AVG_INSTALL_PUR >5.4405834 then MONTHLY_AVG_INSTALL_PUR=5.4405834;
    if MONTHLY_AVG_ONEOFF_PUR >5.0275470 then MONTHLY_AVG_ONEOFF_PUR =5.0275470;
    if MONTHLY_AVG_CASH_PUR >6.0559385 then MONTHLY_AVG_CASH_PUR =6.0559385;
    if PAY_RATIO >3.0860336 then PAY_RATIO =3.0860336;
 	if LIMIT_USAGE>0.6763878 then LIMIT_USAGE=0.6763878;

run;



/*Standarzing*/
proc standard data=segment.data_transformed mean=0 std=1 out=temp;
var BALANCE	
BALANCE_FREQUENCY	
PURCHASES	
ONEOFF_PURCHASES	
INSTALLMENTS_PURCHASES	
CASH_ADVANCE	
PURCHASES_FREQUENCY	
ONEOFF_PURCHASES_FREQUENCY	
PURCHASES_INSTALLMENTS_FREQUENCY	
CASH_ADVANCE_FREQUENCY	
CASH_ADVANCE_TRX	
PURCHASES_TRX	
CREDIT_LIMIT	
PAYMENTS	
MINIMUM_PAYMENTS	
PRC_FULL_PAYMENT	
TENURE 
MONTHLY_AVG_PUR
MONTHLY_AVG_INSTALL_PUR
MONTHLY_AVG_ONEOFF_PUR
MONTHLY_AVG_CASH_PUR
PAY_RATIO
LIMIT_USAGE;
run;

/*Cluster Analysis- segmenting in 3,4,5 and 6 clusters*/
proc fastclus data=temp out=temp maxclusters=3 cluster=clus_3 maxiter=100 ;
var BALANCE	
BALANCE_FREQUENCY	
PURCHASES	
ONEOFF_PURCHASES	
INSTALLMENTS_PURCHASES	
CASH_ADVANCE	
PURCHASES_FREQUENCY	
ONEOFF_PURCHASES_FREQUENCY	
PURCHASES_INSTALLMENTS_FREQUENCY	
CASH_ADVANCE_FREQUENCY	
CASH_ADVANCE_TRX	
PURCHASES_TRX	
CREDIT_LIMIT	
PAYMENTS	
MINIMUM_PAYMENTS	
PRC_FULL_PAYMENT	
TENURE 
MONTHLY_AVG_PUR
MONTHLY_AVG_INSTALL_PUR
MONTHLY_AVG_ONEOFF_PUR
MONTHLY_AVG_CASH_PUR
PAY_RATIO
LIMIT_USAGE;
run;


proc fastclus data=temp out=temp maxclusters=4 cluster=clus_4 maxiter=100 ;
var BALANCE	
BALANCE_FREQUENCY	
PURCHASES	
ONEOFF_PURCHASES	
INSTALLMENTS_PURCHASES	
CASH_ADVANCE	
PURCHASES_FREQUENCY	
ONEOFF_PURCHASES_FREQUENCY	
PURCHASES_INSTALLMENTS_FREQUENCY	
CASH_ADVANCE_FREQUENCY	
CASH_ADVANCE_TRX	
PURCHASES_TRX	
CREDIT_LIMIT	
PAYMENTS	
MINIMUM_PAYMENTS	
PRC_FULL_PAYMENT	
TENURE 
MONTHLY_AVG_PUR
MONTHLY_AVG_INSTALL_PUR
MONTHLY_AVG_ONEOFF_PUR
MONTHLY_AVG_CASH_PUR
PAY_RATIO
LIMIT_USAGE;
run;

proc fastclus data=temp out=temp maxclusters=5 cluster=clus_5 maxiter=100 ;
var BALANCE	
BALANCE_FREQUENCY	
PURCHASES	
ONEOFF_PURCHASES	
INSTALLMENTS_PURCHASES	
CASH_ADVANCE	
PURCHASES_FREQUENCY	
ONEOFF_PURCHASES_FREQUENCY	
PURCHASES_INSTALLMENTS_FREQUENCY	
CASH_ADVANCE_FREQUENCY	
CASH_ADVANCE_TRX	
PURCHASES_TRX	
CREDIT_LIMIT	
PAYMENTS	
MINIMUM_PAYMENTS	
PRC_FULL_PAYMENT	
TENURE 
MONTHLY_AVG_PUR
MONTHLY_AVG_INSTALL_PUR
MONTHLY_AVG_ONEOFF_PUR
MONTHLY_AVG_CASH_PUR
PAY_RATIO
LIMIT_USAGE;
run;


proc fastclus data=temp out=temp maxclusters=6 cluster=clus_6 maxiter=100 ;
var BALANCE	
BALANCE_FREQUENCY	
PURCHASES	
ONEOFF_PURCHASES	
INSTALLMENTS_PURCHASES	
CASH_ADVANCE	
PURCHASES_FREQUENCY	
ONEOFF_PURCHASES_FREQUENCY	
PURCHASES_INSTALLMENTS_FREQUENCY	
CASH_ADVANCE_FREQUENCY	
CASH_ADVANCE_TRX	
PURCHASES_TRX	
CREDIT_LIMIT	
PAYMENTS	
MINIMUM_PAYMENTS	
PRC_FULL_PAYMENT	
TENURE 
MONTHLY_AVG_PUR
MONTHLY_AVG_INSTALL_PUR
MONTHLY_AVG_ONEOFF_PUR
MONTHLY_AVG_CASH_PUR
PAY_RATIO
LIMIT_USAGE;
run;

/*Checking segment size*/
proc freq data=temp;
table clus_3 clus_4 clus_5 clus_6;
run;
/*After looking at the frequency distribution among the clusters. It is evident that cluster 4 is most appropriately scattered*/


/*Applying the segmentation on original variables*/
ods html file='/folders/myfolders/Segmentation Case Study/profiling.xls';
proc tabulate data=temp;
var z_BALANCE	
z_BALANCE_FREQUENCY	
z_PURCHASES	
z_ONEOFF_PURCHASES	
z_INSTALLMENTS_PURCHASES	
z_CASH_ADVANCE	
z_PURCHASES_FREQUENCY	
z_ONEOFF_PURCHASES_FREQUENCY	
z_PURCHASES_INSTALL_FREQUENCY	
z_CASH_ADVANCE_FREQUENCY	
z_CASH_ADVANCE_TRX	
z_PURCHASES_TRX	
z_CREDIT_LIMIT	
z_PAYMENTS	
z_MINIMUM_PAYMENTS	
z_PRC_FULL_PAYMENT	
z_TENURE 
z_MONTHLY_AVG_PUR
z_MONTHLY_AVG_INSTALL_PUR
z_MONTHLY_AVG_ONEOFF_PUR
z_MONTHLY_AVG_CASH_PUR
z_PAY_RATIO
z_LIMIT_USAGE;
class  clus_3 clus_4 clus_5 clus_6;
table (z_BALANCE	
z_BALANCE_FREQUENCY	
z_PURCHASES	
z_ONEOFF_PURCHASES	
z_INSTALLMENTS_PURCHASES	
z_CASH_ADVANCE	
z_PURCHASES_FREQUENCY	
z_ONEOFF_PURCHASES_FREQUENCY	
z_PURCHASES_INSTALL_FREQUENCY	
z_CASH_ADVANCE_FREQUENCY	
z_CASH_ADVANCE_TRX	
z_PURCHASES_TRX	
z_CREDIT_LIMIT	
z_PAYMENTS	
z_MINIMUM_PAYMENTS	
z_PRC_FULL_PAYMENT	
z_TENURE 
z_MONTHLY_AVG_PUR
z_MONTHLY_AVG_INSTALL_PUR
z_MONTHLY_AVG_ONEOFF_PUR
z_MONTHLY_AVG_CASH_PUR
z_PAY_RATIO
z_LIMIT_USAGE)*mean, clus_3 clus_4 clus_5 clus_6 All;
run;
ods html close;
