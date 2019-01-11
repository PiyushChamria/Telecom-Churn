getwd()

setwd("C:\\Users\\Documents\\R Projects")

library(data.table)
library(lubridate)
library(dplyr)
options(scipen = 999)

user_transaction<-fread("User_Transaction_History.csv",sep=",",header = TRUE)
str(user_transaction)

lot_size<-fread("Lot_Size.csv",sep=",",header = TRUE)
str(lot_size)

lot_size$`Contract Size`<-gsub(",","",lot_size$`Contract Size`)

lot_size$`Contract Size` <- as.integer(lot_size$`Contract Size`)
str(lot_size)

summary(user_transaction)

user_transaction<-user_transaction[,-c(1,2,3,4,5,7,14,15,16,19,21,24,25,26,27,33,38,39,40,41,42,43,46,48,49,54,57,58,60)]

user_transaction$open_datetime<-as.POSIXct(user_transaction$OpenTime, origin="1970-01-01")
user_transaction$close_datetime<-as.POSIXct(user_transaction$CloseTime, origin="1970-01-01")

user_transaction$open_date<-ymd_hms(user_transaction$open_datetime)
user_transaction$open_date<-as.Date(user_transaction$open_date)

user_transaction$close_date<-ymd_hms(user_transaction$close_datetime)
user_transaction$close_date<-as.Date(user_transaction$close_date)

ut_user<-user_transaction

class(user_transaction$open_date)
class(user_transaction$close_date)

class(user_transaction$Cmd)

unique(user_transaction$Cmd)

length(unique(user_transaction$Cmd))

length(unique(ut$Login))
length(unique(ut$Order))
length(unique(ut$Symbol))

ut_user<-ut_user %>% arrange(Login,close_datetime)
ut_user_clust<-ut_user[,-c(5,6,7,8,9,10,11,13,14,16,17,20,23,24,25,27,28,29,30)]

ut_user_clust_final <- ut_user_clust %>% arrange(Login,close_datetime) %>% group_by(Login,Order) %>% 
  summarise(value = sum(Profit)) %>%  mutate(Equity = cumsum(value))

ut_user_clust$Equity<-ut_user_clust_final$Equity

ut_user_clust<-left_join(ut_user_clust,lot_size,by=c("Symbol"="Security"))

ut_user_clust$units<-ut_user_clust$Volume * ut_user_clust$`Contract Size`

str(ut_user_clust)

library(dplyr)
library(caret)
library(irr)
library(VIF)
library(gains)

telecom_final<-read.csv("telecomfinal.csv")
dim(telecom_final)
colSums(is.na(telecom_final))

#let's delete the column which has more than 25 % missing values with respect to number of rows

tele_col_name<-names(telecom_final)
length(tele_col_name)
item_removed<-vector(length = 81)
length(item_removed)
item_removed

for (i in 1:length(item_removed)) {
  if(sum(is.na(telecom_final[,tele_col_name[i]]))>18000){
    item_removed[i]<-i
  }
}
item_removed
telecom_final[,-item_removed]->telecom_final_Analyse
View(telecom_final_Analyse)
colSums(is.na(telecom_final_Analyse))
str(telecom_final_Analyse)

#variable profiling for continuous variable

col_name_final<-colnames(telecom_final_Analyse)
col_name_final
dim(telecom_final_Analyse)
Variable_profile<-vector("list",68)
names(Variable_profile)<-col_name_final
telecom_final_Analyse$income<-as.factor(telecom_final_Analyse$income)

#need to delete thoswe rows with minimum number of missing values

colSums(is.na(telecom_final_Analyse))

missing_MOU<-which(is.na(telecom_final_Analyse$mou_Mean))
telecom_final_Analyse<-telecom_final_Analyse[-missing_MOU,]
for (j in 1:68) {
  if(class(telecom_final_Analyse[,col_name_final[j]])=="numeric" || class(telecom_final_Analyse[,col_name_final[j]])=="integer"){
    telecom_final_Analyse%>%mutate(dec=ntile(telecom_final_Analyse[,col_name_final[j]],10))->telecom_final_Analyse
    
    z<-as.name(col_name_final[j])
    telecom_final_Analyse%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N,greaterthan=min(z,na.rm=TRUE),lessthan=max(z,na.rm=TRUE))->Variable_profile[[col_name_final[j]]]
    
  }
  else{
    x<-as.name(col_name_final[j])
    telecom_final_Analyse%>%group_by_(x)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N)->Variable_profile[[col_name_final[j]]]
    
  }
}

View(telecom_final_Analyse)

#let's deal with all the categorical variable 
factor_variable<-vector(length = 11)
i<-1


for (j in 1:68) {
  if(class(telecom_final_Analyse[,col_name_final[j]])=="factor"){
    
    factor_variable[i]<-col_name_final[j]
    i<-i+1
  }
}

factor_variable
str(telecom_final_Analyse)

#let's analyse one by one 
#check with income

Variable_profile[["income"]] #you can clearly see level 7 has same churn rate as the missing values we can replace them with 7

missing_income<-which(is.na(telecom_final_Analyse$income))
telecom_final_Analyse$income[missing_income]<-7

#lets check with "crclscod" 
sum(is.na(telecom_final_Analyse$crclscod))
#it does not have any missing values

#lets check with  "asl_flag"  
sum(is.na(telecom_final_Analyse$asl_flag))
#it does not have any missing values

#let's check with "prizm_social_one"
sum(is.na(telecom_final_Analyse$prizm_social_one))
#check with its profile
Variable_profile[["prizm_social_one"]]
#replcae level C,S and U with C
index_prizm<-which(telecom_final_Analyse$prizm_social_one=="S" |
                     telecom_final_Analyse$prizm_social_one=="U")

telecom_final_Analyse$prizm_social_one[index_prizm]<-"C"
#we can substitu al the na values with "T"
missing_prizm<-which(is.na(telecom_final_Analyse$prizm_social_one))
telecom_final_Analyse$prizm_social_one[missing_prizm]<-"T"


#let's check with "area"
sum(is.na(telecom_final_Analyse$area))
#check with variable profile
Variable_profile[["area"]]
#we can replace all the missing values with OHIO 
missing_area<-which(is.na(telecom_final_Analyse$area))
telecom_final_Analyse$area[missing_area]<-"OHIO AREA"

#let's check with "refurb_new"
sum(is.na(telecom_final_Analyse$refurb_new))
Variable_profile[["refurb_new"]]
#we can omit this value
missing_refurb_new<-which(is.na(telecom_final_Analyse$refurb_new))
telecom_final_Analyse[-missing_refurb_new,]->telecom_final_Analyse


#lets check with "hnd_webcap"
sum(is.na(telecom_final_Analyse$hnd_webcap))
#let;s check with the varibale profile
Variable_profile[["hnd_webcap"]]
#we can replcae missing values with WC
missing_hnd_webcap<-which(is.na(telecom_final_Analyse$hnd_webcap))
telecom_final_Analyse$hnd_webcap[missing_hnd_webcap]<-"WC"


#let's check with "marital" 
sum(is.na(telecom_final_Analyse$marital))
#let's check with  variable profie
Variable_profile[["marital"]]
#we can replace all the missing values with "S"
missing_marital<-which(is.na(telecom_final_Analyse$marital))
telecom_final_Analyse$marital[missing_marital]<-"S"


#let's check with  "ethnic" 
sum(is.na(telecom_final_Analyse$ethnic))
#let's check with variable profile
Variable_profile[[ "ethnic" ]]
#we can replcae all the missing values with level "M"
missing_ethinic<-which(is.na(telecom_final_Analyse$ethnic))
telecom_final_Analyse$ethnic[missing_ethinic]<-"M"


#lets check with "car_buy" 
sum(is.na(telecom_final_Analyse$car_buy))
#let's check with variabe profile 
Variable_profile[["car_buy" ]]
#we can omit this variable as we have infromation on 27000 rows only 
telecom_final_Analyse[,-54]->telecom_final_Analyse

#let's check with "csa"  
sum(is.na(telecom_final_Analyse$csa))
#check with variable profile 
Variable_profile[["csa"]][which(Variable_profile[["csa"]]$churn_percentage>=0.219 & 
                                  Variable_profile[["csa"]]$churn_percentage<=0.23),]
missing_CSA<-which(is.na(telecom_final_Analyse$csa))
telecom_final_Analyse$csa[missing_CSA]<-"AIRORA803"

#we are done with factor variable 
k<-0
profile_continuous<-vector(length = 9)
col_name_final<-col_name_final[-54]
dim(telecom_final_Analyse)
n<-1
for ( j in 1:67) {
  if(class(telecom_final_Analyse[,col_name_final[j]])=="numeric" || class(telecom_final_Analyse[,col_name_final[j]])=="integer"){
    if(sum(is.na(telecom_final_Analyse[,col_name_final[j]])) > 0){
      profile_continuous[n]<-col_name_final[j]
      n<-n+1
    }
  }
}
colSums()
colSums(is.na(telecom_final_Analyse[,profile_continuous[1:9]]))
#let's check with continuous variable 
profile_continuous[7:9]
#"forgntvl" "mtrcycle" "truck" these three are found out to be categorical 
#convert them into categorical 
as.factor(telecom_final_Analyse[,profile_continuous[7]])->telecom_final_Analyse[,profile_continuous[7]]
as.factor(telecom_final_Analyse[,profile_continuous[8]])->telecom_final_Analyse[,profile_continuous[8]]
as.factor(telecom_final_Analyse[,profile_continuous[9]])->telecom_final_Analyse[,profile_continuous[9]]
for (j in 7:9) {
  x<-as.name(profile_continuous[j])
  telecom_final_Analyse%>%group_by_(x)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N)->Variable_profile[[profile_continuous[j]]]
}
#for  forgntvl 
Variable_profile[[profile_continuous[7]]]
#we can substitute all the missing values with 1
missing_forgntvl<-which(is.na(telecom_final_Analyse$forgntvl))
demo$forgntvl[missing_forgntvl]<-1
telecom_final_Analyse$forgntvl[missing_forgntvl]<-1
#for truck and car
Variable_profile[[profile_continuous[8]]]
Variable_profile[[profile_continuous[9]]]
#we can drop these variable as there are no such differecne in churn percentage
#in both the levels
str(telecom_final_Analyse)
telecom_final_Analyse[,-47]->telecom_final_Analyse
telecom_final_Analyse[,-48]->telecom_final_Analyse
col_name_final[-47]->col_name_final
col_name_final[-48]->col_name_final

#let's deal with continuous variables
profile_continuous[1]
Variable_profile[[profile_continuous[1]]]
#as all the na values all together has 56%of churn rate we can not replace it 
#so we can delete these rows 
missing_change_mou<-which(is.na(telecom_final_Analyse$change_mou))
telecom_final_Analyse[-missing_change_mou,]->telecom_final_Analyse


profile_continuous[2]
#let's deal with "avg6mou"
Variable_profile[["avg6mou"]]
plot(Variable_profile[["avg6mou"]]$dec,Variable_profile[["avg6mou"]]$churn_percentage)
#let's substitute the missing values with average of 10th quantile data points
telecom_final_Analyse%>%mutate(dec=ntile(telecom_final_Analyse[,"avg6mou"],10))->telecom_final_Analyse
telecom_final_Analyse%>%filter(dec==10)%>%summarise(avg=sum(avg6mou)/n())
#we can substitute this value instead of missing values
missing_avg6mou<-which(is.na(telecom_final_Analyse$avg6mou))
telecom_final_Analyse$avg6mou[missing_avg6mou]<-1861.491


profile_continuous[3]
#let's deal with avg6qty
Variable_profile[[ "avg6qty"]]
#let's substitute the missing values with average of 10th quantile data points
telecom_final_Analyse%>%mutate(dec=ntile(telecom_final_Analyse[,"avg6qty"],10))->telecom_final_Analyse
telecom_final_Analyse%>%filter(dec==10)%>%summarise(avg=sum(avg6qty)/n())
#we can substitute this value instead of missing values
missing_avg6qty<-which(is.na(telecom_final_Analyse$avg6qty))
telecom_final_Analyse$avg6qty[missing_avg6qty]<-614.2922

profile_continuous[4]
#let's deal with "age1"
Variable_profile[["age1"]]
#let's divide it into quartile 
telecom_final_Analyse%>%mutate(dec=ntile(telecom_final_Analyse[,"age1"],4))->telecom_final_Analyse
telecom_final_Analyse%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N,greaterthan=min(age1,na.rm=TRUE),lessthan=max(age1,na.rm=TRUE))
#let's substitute al the missing values with the average of 3rd quartile
telecom_final_Analyse%>%filter(dec==3)%>%summarise(avg=sum(age1)/n())
missing_age1<-which(is.na(telecom_final_Analyse$age1))
telecom_final_Analyse$age1[missing_age1]<-43


profile_continuous[5]
#let's deal with "age2"
Variable_profile[["age2"]]
#let's divide it into tertitle
telecom_final_Analyse%>%mutate(dec=ntile(telecom_final_Analyse[,"age2"],3))->telecom_final_Analyse
telecom_final_Analyse%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N,greaterthan=min(age2,na.rm=TRUE),lessthan=max(age2,na.rm=TRUE))
#let's substitute al the missing values with the average of 3rd quartile
telecom_final_Analyse%>%filter(dec==3)%>%summarise(avg=sum(age2)/n())
missing_age2<-which(is.na(telecom_final_Analyse$age2))
telecom_final_Analyse$age2[missing_age2]<-51


profile_continuous[6]
#lets deal with "hnd_price"
Variable_profile[["hnd_price"]]
#let's didvide them in octile
telecom_final_Analyse%>%mutate(dec=ntile(telecom_final_Analyse[,"hnd_price"],8))->telecom_final_Analyse
telecom_final_Analyse%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N,greaterthan=min(hnd_price,na.rm=TRUE),lessthan=max(hnd_price,na.rm=TRUE))

#let's substitute all the missing values with the average of 8th dec
telecom_final_Analyse%>%filter(dec==8)%>%summarise(avg=sum(hnd_price)/n())
missing_hnd_price<-which(is.na(telecom_final_Analyse$hnd_price))
telecom_final_Analyse$hnd_price[missing_hnd_price]<-208.3288



#let's check with missing values in the data sets
dim(telecom_final_Analyse)
colSums(is.na(telecom_final_Analyse))
telecom_final_Analyse[,-66]->telecom_final_Analyse
summarise(telecom_final_Analyse)
summary(telecom_final_Analyse)


#derive variable for "Network issues are leading to churn"
#Completion_Percentage=Completed Voice Calls/Total Placed Calls
telecom_final_Analyse$Completion_Percentage<-telecom_final_Analyse$comp_vce_Mean/telecom_final_Analyse$plcd_vce_Mean
sum(is.na(telecom_final_Analyse$Completion_Percentage))
range(telecom_final_Analyse$Completion_Percentage,na.rm = T)
missing_compeletion_percentage<-which(is.na(telecom_final_Analyse$Completion_Percentage))
#we can delete these records as it has numberplaced calls 0
telecom_final_Analyse[-missing_compeletion_percentage,]->telecom_final_Analyse
range(telecom_final_Analyse$Completion_Percentage)

#let's create another column to check the ratio of Overage revenue and total revenue 
#i.e OVRREV_MEAN/TOTREV
telecom_final_Analyse$optimum<-telecom_final_Analyse$ovrrev_Mean/telecom_final_Analyse$totrev
summary(telecom_final_Analyse$optimum)
#let's check all number of missing values before performing logistic regression
#I am choosing a cut off of 0.3 to define optimal plan or non optimal 


colSums(is.na(telecom_final_Analyse))
telecom_final_Analyse[,-47]->telecom_final_Analyse
summary(telecom_final_Analyse)

factor_variable
Variable_profile[[factor_variable[1]]]
unique(telecom_final_Analyse$crclscod)
unique(telecom_final_Analyse$asl_flag)
unique(telecom_final_Analyse$prizm_social_one)
unique(telecom_final_Analyse$area)
unique(telecom_final_Analyse$refurb_new)
#let's divide the data into test and training sets
set.seed(2000)
training_index<- sample(nrow(telecom_final_Analyse),nrow(telecom_final_Analyse)*0.70,replace = F)
training<-telecom_final_Analyse[training_index,]
test<-telecom_final_Analyse[-training_index,]
#let's build the model with training  data ,taking churn as the dependent variable 
View(training)



col_name_final<-colnames(telecom_final_Analyse)
col_name_final
dim(telecom_final_Analyse)
Variable_profile<-vector("list",66)
names(Variable_profile)<-col_name_final

for (j in 1:66) {
  if(class(telecom_final_Analyse[,col_name_final[j]])=="numeric" || class(telecom_final_Analyse[,col_name_final[j]])=="integer"){
    telecom_final_Analyse%>%mutate(dec=ntile(telecom_final_Analyse[,col_name_final[j]],10))->telecom_final_Analyse
    
    z<-as.name(col_name_final[j])
    telecom_final_Analyse%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N,greaterthan=min(z,na.rm=TRUE),lessthan=max(z,na.rm=TRUE))->Variable_profile[[col_name_final[j]]]
    
  }
  else{
    x<-as.name(col_name_final[j])
    telecom_final_Analyse%>%group_by_(x)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N)->Variable_profile[[col_name_final[j]]]
    
  }
}

factor_variable
Variable_profile[["income"]]
Variable_profile[["crclscod"]]->crc
View(crc)


col_name_final
grep("Range",col_name_final)->index_range
#we can delete all the columns that has range as we have the mean values for them 
telecom_final_Analyse[, -index_range]->telecom_final_Analyse
dim(telecom_final_Analyse)

telecom_final_Analyse[,-58]->telecom_final_Analyse
col_name_final<-colnames(telecom_final_Analyse)
col_name_final
dim(telecom_final_Analyse)
Variable_profile<-vector("list",57)
names(Variable_profile)<-col_name_final

for (j in 1:57) {
  if(class(telecom_final_Analyse[,col_name_final[j]])=="numeric" || class(telecom_final_Analyse[,col_name_final[j]])=="integer"){
    telecom_final_Analyse%>%mutate(dec=ntile(telecom_final_Analyse[,col_name_final[j]],10))->telecom_final_Analyse
    
    z<-as.name(col_name_final[j])
    telecom_final_Analyse%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N,greaterthan=min(z,na.rm=TRUE),lessthan=max(z,na.rm=TRUE))->Variable_profile[[col_name_final[j]]]
    
  }
  else{
    x<-as.name(col_name_final[j])
    telecom_final_Analyse%>%group_by_(x)%>%summarise(n=sum(churn),N=n(),churn_percentage=n/N)->Variable_profile[[col_name_final[j]]]
    
  }
}
telecom_final_Analyse[,-58]->telecom_final_Analyse
model_builder<-telecom_final_Analyse[,-c(24,27)]
par(mfrow=c(3,2))
Variable_profile[["crclscod"]][,1]
factor_variable 
length(col_name_final)
i<-1
for (j in 1:57) {
  if(class(telecom_final_Analyse[,col_name_final[j]])=="factor"){
    
    factor_variable[i]<-col_name_final[j]
    i<-i+1
  }
}
factor_variable
for (i in 6:11) {
  x=Variable_profile[[factor_variable[i]]][,1]
  y=Variable_profile[[factor_variable[i]]]$churn_percentage
  plot(x,y,title(main = col_name_final[i]))
}

col_name_final
summary(telecom_final_Analyse)
set.seed(2000)
training_index<- sample(nrow(telecom_final_Analyse),nrow(telecom_final_Analyse)*0.70,replace = F)
training<-telecom_final_Analyse[training_index,]
test<-telecom_final_Analyse[-training_index,]
#let's build the model with training  data ,taking churn as the dependent variable 
View(training)

model<-glm(churn~+mou_Mean+totmrc_Mean+change_mou
           +drop_blk_Mean+months+totcalls+income
           +eqpdays+custcare_Mean+callwait_Mean+iwylis_vce_Mean
           +adjqty+ovrrev_Mean+rev_Mean+ovrmou_Mean+comp_vce_Mean
           +plcd_vce_Mean+avg3mou+avgmou+avg3qty+avgqty
           +avg6mou+avg6qty+asl_flag+prizm_social_one+refurb_new+hnd_webcap
           +marital+ethnic+age1+age2+models+hnd_price+actvsubs
           +uniqsubs+forgntvl+opk_dat_Mean+recv_sms_Mean
           +blck_dat_Mean+mou_pead_Mean,family = binomial(link = "logit"),data = training[,-c(24,44,27)])
col_name_final

mod<-glm(churn~.,family = binomial(link = "logit"), data = training[,-c(24,44,27)])
summary(mod)
step(mod,direction = "both")
mod<-glm(formula = churn ~ mou_Mean + totmrc_Mean + change_mou + months + 
           totcalls + eqpdays + iwylis_vce_Mean + adjqty + ovrrev_Mean + 
           rev_Mean + avg3mou + avgmou + avg6mou + avg6qty + asl_flag + 
           prizm_social_one + refurb_new + ethnic + age1 + age2 + models + 
           hnd_price + actvsubs + uniqsubs + da_Mean + datovr_Mean + 
           drop_vce_Mean + adjmou + adjrev + Customer_ID + Completion_Percentage, 
         family = binomial(link = "logit"), data = training[, -c(24, 
                                                                 44, 27)])
summary(mod)
mod$coefficients
head(sort(mod$coefficients,decreasing = T),6)


dim(telecom_final_Analyse)
unique(telecom_final_Analyse$ethnic)
#creating dummy variables for levels C,G,N,P,S,U,Z
telecom_final_Analyse$ethnic_C<-ifelse(telecom_final_Analyse$ethnic=="C",1,0)
telecom_final_Analyse$ethnic_G<-ifelse(telecom_final_Analyse$ethnic=="G",1,0)
telecom_final_Analyse$ethnic_N<-ifelse(telecom_final_Analyse$ethnic=="N",1,0)
telecom_final_Analyse$ethnic_P<-ifelse(telecom_final_Analyse$ethnic=="P",1,0)
telecom_final_Analyse$ethnic_S<-ifelse(telecom_final_Analyse$ethnic=="S",1,0)
telecom_final_Analyse$ethnic_U<-ifelse(telecom_final_Analyse$ethnic=="U",1,0)
telecom_final_Analyse$ethnic_Z<-ifelse(telecom_final_Analyse$ethnic=="Z",1,0)

#again divide it in training and test
set.seed(2000)
training_index<- sample(nrow(telecom_final_Analyse),nrow(telecom_final_Analyse)*0.70,replace = F)
training<-telecom_final_Analyse[training_index,]
test<-telecom_final_Analyse[-training_index,]

mod<-glm(formula = churn ~ mou_Mean + totmrc_Mean + change_mou + months + 
           totcalls + eqpdays + iwylis_vce_Mean + adjqty + ovrrev_Mean + 
           rev_Mean + avg3mou + avgmou + avg6mou + avg6qty + asl_flag + 
           prizm_social_one + refurb_new+ethnic_C+ethnic_G+ethnic_N+ethnic_P+ethnic_S+ethnic_U+
           ethnic_Z+ age1 + age2 + models + 
           hnd_price + actvsubs + uniqsubs + da_Mean + datovr_Mean + 
           drop_vce_Mean + adjmou + adjrev+ Completion_Percentage+optimum, 
         family = binomial(link = "logit"), data = training[, -c(24, 
                                                                 44, 27)])
summary(mod)
#we can drop mou_mean,da_mean and datovr_mean from the model
mod<-glm(formula = churn ~  totmrc_Mean + change_mou + months + 
           totcalls + eqpdays + iwylis_vce_Mean + adjqty + ovrrev_Mean + 
           rev_Mean + avg3mou + avgmou + avg6mou + avg6qty + asl_flag + 
           prizm_social_one + refurb_new+ethnic_C+ethnic_G+ethnic_N+ethnic_P+ethnic_S+ethnic_U+
           ethnic_Z+ age1 + age2 + models + 
           hnd_price + actvsubs + uniqsubs + 
           drop_vce_Mean + adjmou + adjrev+ Completion_Percentage, 
         family = binomial(link = "logit"), data = training[, -c(24, 
                                                                 44, 27)])
summary(mod)
abs(-5)
#model validation
test$pred<-predict(mod,type = "response",newdata = test)
head(pred)
table(training$churn)/nrow(training)
#choosing cutoff value according to kappa value
s<-seq(0.25,0.5,0.01)
n<-1 
a<-as.vector(length(s))
for (i in s ) {
  
  print(i)
  test$result<-ifelse(test$pred>i,1,0)
  a[n]<-confusionMatrix(test$result,test$churn,positive = "1")$overall[2]
  
  print(n)
  n=n+1
}
max(a)
#As maximum kappa is related to cutoff 0.28 we would go with this cutoff value
test$result<-ifelse(test$pred>=0.28,1,0)
confusionMatrix(test$result,test$churn,positive ="1")

predicted<-mod$fitted.values
pred<-prediction(predicted,training$churn)
pref<-performance(pred,"tpr","fpr")
plot(pref)
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

#auc is coming to be 0.6249715
#which is quite good according to the solution guide

head(sort(abs(mod$coefficients),decreasing = T),10)
summary(mod)
telecom_final_Analyse$pred<-predict(mod,type = "response",newdata = telecom_final_Analyse)
telecom_final_Analyse$target<-ifelse(telecom_final_Analyse$pred>=0.28,1,0)
#high churn
confusionMatrix(telecom_final_Analyse$target,telecom_final_Analyse$churn,positive = "1")
summary(mod)
unique(telecom_final_Analyse$uniqsubs)
# What are the top five factors driving likelihood of churn at Mobicom? 
head(sort(abs(mod$coefficients),decreasing = T),10)
#optimum
#ethinic
#completion percentage
#asl_flag
#refurb_new
#2.  Validation of survey findings. a) Whether "cost and billing" and "network and service quality" are 
#important factors influencing churn behaviour.  b) Are data usage connectivity issues turning out to be 
#costly? In other words, is it leading to churn?  
#a)Yes
#b)No
#3.Would you recommend rate plan migration as a proactive retention strategy? 
#yes
#4. What would be your recommendation on how to use this churn model for prioritisation of customers for a 
#proactive retention campaigns in the future? 
#based on Usage based promotions to increase minutes of usage (MOU) for both voice and 
#data. It is an accepted fact that low usage and high churn go hand in hand
telecom_final_Analyse%>%arrange(desc(pred))->telecom_final_Analyse
View(telecom_final_Analyse)
Variable_profile[["adjmou"]]
#we can say <4033 rupees spending per month is low usage ,as it comes under 40percentile
index_mou<-which(telecom_final_Analyse$pred>=0.28 & telecom_final_Analyse$adjmou<=4033)
customer_for_mou<-telecom_final_Analyse$Customer_ID[index_mou]
#rate plan migration as a proactive retention strategy
#for this we can those customer for which optimum is more than 30% that is my overage revnue contributes 
#more than 30% to the total revnue
index_optimum<-which(telecom_final_Analyse$optimum>=0.30)
customer_optimum<-telecom_final_Analyse$Customer_ID[index_optimum]
#family bundle
#let's check the variable profile for uniqsubs
Variable_profile[["uniqsubs"]]
#for this we can target those families which have more than 5 uniqsubs and high chur rate
telecom_final_Analyse%>%arrange(desc(uniqsubs))->telecom_final_Analyse
index_family_bundle<-which(telecom_final_Analyse$uniqsubs>=5 & telecom_final_Analyse$pred>=0.28)
customer_family_bundle<-telecom_final_Analyse$Customer_ID[index_family_bundle]

#5. What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a 
#concern and therefore, Mobicom would like to save their high revenue customers besides managing 
#churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should 
#prioritized if "revenue saves" is also a priority besides controlling churn. In other words, controlling churn 
#is the primary objective and revenue saves is the secondary objective. 
length(telecom_final_Analyse$Customer_ID)
#so we can afford 0.20*61069 customers to select for this bundling strategy
0.20*61609
#nearly 12322
#let's check with the variable profile for adjrev
Variable_profile[["adjrev"]]
#we can include those customers which have adjrev morethan 753 and high churn rate 
index_churn_rev<-which(telecom_final_Analyse$pred>=0.28 & telecom_final_Analyse$adjrev>=753 )
customer_churn_rev<-telecom_final_Analyse$Customer_ID[index_churn_rev]
