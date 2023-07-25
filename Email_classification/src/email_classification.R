setwd("path")
data=read.csv("Classification.csv",header=TRUE)

######## Splitting data ########
s<-c(sample(1:13,3, replace=FALSE),sample(14:26,3,replace=FALSE),sample(27:59,7,replace= FALSE),sample(60:67,1,replace=FALSE),sample(68:74,1,replace=FALSE),sample(75:80,1,replace=FALSE))
test<-data[s,]
train<-data[-s,]
data_new<-rind(train, test)
dim(data_new)

#############Creating Dictionary of words#############

##Account Administration Dictionary##

data_new1 = train[train$SERVICE == "Account Administration",]
data_new12=as.character(data_new1[,7])
data_new13 <- str_replace_all(data_new12,"[[:punct:]]"," ")
z=str_replace_all(data_new13, "[^[:alnum:]]", " ")
AA_new = gsub("\\d", "", z)## Deleting numbers from string
AA_new1=gsub("   ", " ", AA_new, fixed = TRUE) ## Delete double space from string
AA_new1=unlist(strsplit(AA_new1, " ")) ## complete dictionary one list is words of one e-mails.

data1=as.data.frame(cbind("Account Administration",AA_new1))
colnames(data1)=c("SERVICE","TERMS")

##Cash Dictionary##

data_new2 = train[train$SERVICE == "Cash",]
data_new22=as.character(data_new2[,7])
data_new23 <- str_replace_all(data_new22,"[[:punct:]]"," ")
z=str_replace_all(data_new23, "[^[:alnum:]]", " ")
Ca_new = gsub("\\d", "", z)## Deleting numbers from string
Ca_new1=gsub("   ", " ", Ca_new, fixed = TRUE) ## Delete double space from string
Ca_new1=unlist(strsplit(Ca_new1, " ")) ## complete dictionary one list is words of one e-mails.

data2=as.data.frame(cbind("Cash",Ca_new1))
colnames(data2)=c("SERVICE","TERMS")

##Client Services Dictionary##

data_new3 = train[train$SERVICE == "Client Services",]
data_new32=as.character(data_new3[,7])
data_new32 <- str_replace_all(data_new32,"[[:punct:]]"," ")
z=str_replace_all(data_new32, "[^[:alnum:]]", " ")
Cs_new = gsub("\\d", "", z)## Deleting numbers from string
Cs_new1=gsub("   ", " ", Cs_new, fixed = TRUE) ## Delete double space from string
Cs_new1=unlist(strsplit(Cs_new1, " ")) ## complete dictionary one list is words of one e-mails.

data3=as.data.frame(cbind("Client Services",Cs_new1))
colnames(data3)=c("SERVICE","TERMS")

##Corporate Actions Dictionary##

data_new4 = train[train$SERVICE == "Corporate Actions",]
data_new41=as.character(data_new4[,7])
data_new42 <- str_replace_all(data_new41,"[[:punct:]]"," ")
z=str_replace_all(data_new42, "[^[:alnum:]]", " ")
Ca_new = gsub("\\d", "", z)## Deleting numbers from string
Ca_new1=gsub("   ", " ", Ca_new, fixed = TRUE) ## Delete double space from string
Ca_new1=unlist(strsplit(Ca_new1, " ")) ## complete dictionary one list is words of one e-mails.

data4=as.data.frame(cbind("Corporate Actions",Ca_new1))
colnames(data4)=c("SERVICE","TERMS")

###Income Dictionary###

data_new5 = train[train$SERVICE == "Income",]
data_new51=as.character(data_new5[,7])
data_new52 <- str_replace_all(data_new51,"[[:punct:]]"," ")
z=str_replace_all(data_new52, "[^[:alnum:]]", " ")
In_new = gsub("\\d", "", z)## Deleting numbers from string
In_new1=gsub("   ", " ", In_new, fixed = TRUE) ## Delete double space from string
In_new1=unlist(strsplit(In_new1, " ")) ## complete dictionary one list is words of one e-mails.

data5=as.data.frame(cbind("Income",In_new1))
colnames(data5)=c("SERVICE","TERMS")

##Securities Settlements Dictionary##
data_new6 = train[train$SERVICE == "Securities Settlements",]
data_new61=as.character(data_new6[,7])
data_new62 <- str_replace_all(data_new61,"[[:punct:]]"," ")
z=str_replace_all(data_new62, "[^[:alnum:]]", " ")
Ss_new = gsub("\\d", "", z)## Deleting numbers from string
Ss_new1=gsub("   ", " ", Ss_new, fixed = TRUE) ## Delete double space from string
Ss_new1=unlist(strsplit(Ss_new1, " ")) ## complete dictionary one list is words of one e-mails.

data6=as.data.frame(cbind("Securities Settlements",Ss_new1))
colnames(data6)=c("SERVICE","TERMS")

## Combine all dictionary in one ##

train_dataset<-rbind(data1,data2,data3,data4,data5,data6)

## Prepare test dataset:
test<-data[s,]
test<-data[66,]
#test<-test[test$PYID=="CI-AAAAB057376",]

data_test=as.character(test[,7])
data_test <- str_replace_all(data_test,"[[:punct:]]"," ")
data_test=str_replace_all(data_test, "[^[:alnum:]]", " ")
data_test = gsub("\\d", "", data_test)## Deleting numbers from string
data_test=gsub("   ", " ", data_test, fixed = TRUE) ## Delete double space from string
data_test=unlist(strsplit(data_test, " ")) ## complete dictionary one list is words of one e-mails.


## For new words in test-mail:

for(i in 1:length(data_test)){
  if(!data_test[i] %in% train_dataset$TERMS)
    train_dataset <- rbind(train_dataset, data.frame(SERVICE = "NA", TERMS = data_test[i]))
}

#Counting the freq of words#
word_freq<-table(train_dataset)
#word_freq
word_freq1=word_freq+1
#word_freq1

#Defining prior of each class
prior<-apply(word_freq,1,sum)/sum(word_freq)

#Calculating p(Terms|SERVICE)
likelihood<-prop.table(word_freq1,margin=1)
#likelihood

#predict for words in each e-mails:P(TERMS|SERVICE)
predict_mail<-function(data_test){
  
  P_account_karnel<-likelihood[SERVICE="Account Administration",TERMS=data_test]*prior["Account Administration"]
  P_account_karnel
  P_cash_karnel<-likelihood[SERVICE="Cash",TERMS=unlist(test_data)]*prior["Cash"]
  P_client_karnel<-likelihood[SERVICE="Client Services",TERMS=unlist(test_data)]*prior["Client Services"]
  P_corporate_karnel<-likelihood[SERVICE="Corporate Actions",TERMS=unlist(test_data)]*prior["Corporate Actions"]
  P_income_karnel<-likelihood[SERVICE="Income",TERMS=unlist(test_data)]*prior["Income"]
  P_security_karnel<-likelihood[SERVICE="Securities Settlements",TERMS=unlist(test_data)]*prior["Securities Settlements"]
  
  
  P_karnel<-P_account_karnel+P_cash_karnel+P_client_karnel+P_corporate_karnel+P_income_karnel+P_security_karnel
  
  #a = melt(likelihood,id="SERVICE")
  #b = merge(test_data,a,by="TERMS")
  
  ###
  P_account_karnel<-likelihood[Class="spam",Words=test_mail1]*prior["Account Administration"]
  
  #Calculating Bayes probability P(SERVICE|Terms)
  
  Account_administration<-P_account_karnel/P_karnel
  Cash<-P_cash_karnel/P_karnel
  Client_Service<-P_client_karnel/P_karnel
  Corporate_Actions<-P_corporate_karnel/P_karnel
  Income<-P_income_karnel/P_karnel
  Securities_Settlements<-P_security_karnel/P_karnel
  
  p<-rbind(Account_administration,Cash,Client_Service,Corporate_Actions,Income,Securities_Settlements)
  
  word_class<-rep(NA,length(test_data))
  names(word_class)<-test_data
  for(j in 1:ncol(p)){
    for(i in 1:nrow(p)){
      if(p[i,j]==max(p[,j]))word_class[j]<-rownames(p)[i]
    }
  }
  
  cl<-names(sort(table(word_class)))
  SERVICE<-cl[length(cl)]
  
  return(SERVICE)
}
