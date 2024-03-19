#Packages for the code
library(readxl)
library(dbplyr)
library(tidyverse)
library(ggplot2) #plots
library(ggpubr) #4-part plot
library(Metrics) #rmse
library(forecast) #MA moving average

#Reading the first dataset
italy=read_excel("owid-covid-data.xlsx")
notitaly=as.data.frame(italy)
#Filtering to keep only Italy
italy=filter(italy,location=="Italy")


#Reading the second dataset
WD=read.csv("right_world_daily.csv")
#Filtering to keep only Italt
Italy_Daily=filter(WD,CountryName=="Italy")

#Slicing the first dataset in order to have the same dates
italy1<-italy %>% slice(21:1018)
#We need the columns date, total cases and total deaths
italy1=subset(italy1,select=c("date","total_cases","total_deaths"))

#Creating a new dataset with the italy1 dataset and some of the columns in the second dataset 
CurrentlyPositive=Italy_Daily$CurrentlyPositive
Hospitalized=Italy_Daily$Hospitalized
ICU=Italy_Daily$IntensiveCare
new_italy=cbind(italy1,CurrentlyPositive,Hospitalized,ICU)
new_italy[is.na(new_italy)]=0
#We need the column with the Recovered cases, which we create by substracting from the total cases the deaths and currently positive
new_italy$Recovered=new_italy$total_cases-new_italy$total_deaths-new_italy$CurrentlyPositive
#converting the date variable from character to date
new_italy$date=as.Date(new_italy$date,"%Y-%m-%d")

#Creating our main dataset from 2022/06/08 to 2022/09/21
data=new_italy[840:945,]

#In this model we choose for the creation of N(t) the new cases from 7 days before and we also smoothe with a moving average
#the new cases time series.
#We create the vector N which is the cases that are admitted in the hospital which is 
#N(t)=rate*newcases
new_cases=c()
for(i in 1:105){
  new_cases[i]=data$total_cases[i+1]-data$total_cases[i]
}
set.seed(123)
rate=runif(15,min=0.1,max=1)
rate
rate=round(rate,digits=3)

#Smoothing the new cases time series
new_cases1=head(new_cases,-6)
data1=tail(data,-7)
data1$new_cases=new_cases1


MA_new_cases=forecast::ma(new_cases1,order=8,centre = TRUE)
MA_new_cases=as.numeric(MA_new_cases)
MA_new_cases1=tail(MA_new_cases,-4)
MA_new_cases1=head(MA_new_cases1,-4)

data1=tail(data1,-4)
data1=head(data1,-4)
data1$MA_new_cases=MA_new_cases1

rmse(data1$new_cases,data1$MA_new_cases)


plot(data1$new_cases)
lines(data1$new_cases,col="red",type="l")
lines(data1$MA_new_cases,col="blue",type="l")

#We keep MA(8) because its the least rmse with the best
#result

#We also add a new term in the recovered state, the cases
#that recovered outside the hospitals

N=c()
recovered_non_severe=c()
mi=0.01
ro=0.05
rmseH <- array(0, dim=c(20,20,20))
rmseC <- array(0, dim=c(20,20,20))
rmseR <- array(0, dim=c(20,20,20))
rmseD <- array(0, dim=c(20,20,20))
sum_rmse<- array(0, dim=c(20,20,20))

rmseHmin=c()
rmseCmin=c()
rmseRmin=c()
rmseDmin=c()
sum_rmse_min=c()

min_indexesH=list()
min_indexesC=list()
min_indexesR=list()
min_indexesD=list()
min_indexes_sum=list()

L=seq(0,0.19,by=0.01)
K=seq(0.61,0.8,by=0.01)
T=seq(0.6,0.79,by=0.01)

H=c()
H[1]=data1$Hospitalized[1]
C=c()
C[1]=data1$ICU[1]
R=c()
R[1]=data1$Recovered[1]
D=c()
D[1]=data1$total_deaths[1]


for(i in 1:15){
  for(j in 1:20){
    for(f in 1:20){
      for(g in 1:20){
        N=rate[i]*MA_new_cases1
        recovered_non_severe=(1-rate[i])*MA_new_cases1
        
        for(t in 1:90){
          
          H[t+1]=N[t]+(1-L[j]-K[f]-mi)*H[t]
          C[t+1]=L[j]*H[t]+(1-T[g]-ro)*C[t]
          R[t+1]=K[f]*H[t]+T[g]*C[t]+R[t]+recovered_non_severe[t]
          D[t+1]=mi*H[t]+ro*C[t]+D[t]
        }
        rmseH[j,f,g]=rmse(data1$Hospitalized,H)
        rmseC[j,f,g]=rmse(data1$ICU,C)
        rmseR[j,f,g]=rmse(data1$Recovered,R)
        rmseD[j,f,g]=rmse(data1$total_deaths,D)
        sum_rmse[j,f,g]=sum(rmseH[j,f,g],rmseC[j,f,g],rmseR[j,f,g],rmseD[j,f,g])
      }
    }
  }
  
  rmseHmin[i]=min(rmseH)
  rmseCmin[i]=min(rmseC)
  rmseRmin[i]=min(rmseR)
  rmseDmin[i]=min(rmseD)
  sum_rmse_min[i]=min(sum_rmse)
  
  min_indexesH[[i]]=which(rmseH==rmseHmin[i],arr.ind = TRUE)
  min_indexesC[[i]]=which(rmseC==rmseCmin[i],arr.ind = TRUE)
  min_indexesR[[i]]=which(rmseR==rmseRmin[i],arr.ind = TRUE)
  min_indexes_sum[[i]]=which(sum_rmse==sum_rmse_min[i],arr.ind = TRUE)
  
}




d=which.min(sum_rmse_min)
d
lamda=L[min_indexes_sum[[d]][1]]

kappa=K[min_indexes_sum[[d]][2]]

taf=T[min_indexes_sum[[d]][3]]

c(rate[d],lamda,kappa,taf)
#den einai accurate
N=rate[d]*MA_new_cases1
recovered_non_severe=(1-rate[d])*MA_new_cases1

H=c()
H[1]=data1$Hospitalized[1]
C=c()
C[1]=data1$ICU[1]
R=c()
R[1]=data1$Recovered[1]
D=c()
D[1]=data1$total_deaths[1]


rmseH2=matrix(0,nrow=20,ncol=20)
rmseC2=matrix(0,nrow=20,ncol=20)
rmseR2=matrix(0,nrow=20,ncol=20)
rmseD2=matrix(0,nrow=20,ncol=20)
rmse_sum2=matrix(0,nrow=20,ncol=20)


M=seq(0,0.19,by=0.01)
Re=seq(0,0.19,by=0.01)


for(h in 1:20){
  for(l in 1:20){
    for(t in 1:90){
      H[t+1]=N[t]+(1-lamda-kappa-M[h])*H[t]
      C[t+1]=lamda*H[t]+(1-taf-Re[l])*C[t]
      R[t+1]=kappa*H[t]+taf*C[t]+R[t]+recovered_non_severe[t]
      D[t+1]=M[h]*H[t]+Re[l]*C[t]+D[t]
    }
    rmseH2[h,l]=rmse(data1$Hospitalized,H)
    rmseC2[h,l]=rmse(data1$ICU,C)
    rmseR2[h,l]=rmse(data1$Recovered,R)
    rmseD2[h,l]=rmse(data1$total_deaths,D)
    rmse_sum2[h,l]=sum(rmseH2[h,l],rmseC2[h,l],rmseR2[h,l],rmseD2[h,l])
  }
}
a=min(rmse_sum2)
a
indexed_min=which(rmse_sum2==min(rmse_sum2),arr.ind = TRUE)
indexed_min
mi=M[indexed_min[1,1]]
ro=Re[indexed_min[1,2]]

c(mi,ro)



#Model
for(t in 1:90){
  H[t+1]=N[t]+(1-lamda-kappa-mi)*H[t]
  C[t+1]=lamda*H[t]+(1-taf-ro)*C[t]
  R[t+1]=kappa*H[t]+taf*C[t]+R[t]+recovered_non_severe[t]
  D[t+1]=mi*H[t]+ro*C[t]+D[t]
}
data_estimate=cbind(H,C,R,D)
rmse_final_H=rmse(data1$Hospitalized,H)
rmse_final_C=rmse(data1$ICU,C)
rmse_final_R=rmse(data1$Recovered,R)
rmse_final_D=rmse(data1$total_deaths,D)
rmse_final_SUM=sum(rmse_final_H,rmse_final_C,rmse_final_R,rmse_final_D)

c(rmse_final_SUM,rmse_final_H,rmse_final_C,rmse_final_R,rmse_final_D)

#visualising
data_estimate=cbind(data1,H,C,R,D)

plot(data_estimate$date,data_estimate$new_cases,type="l",col="red")

lines(data_estimate$date,data_estimate$MA_new_cases,col="green")


plot(data_estimate$Hospitalized,type="l",col="red",ylim=c(0,30000))
lines(data_estimate$H,col="green")

plot(data_estimate$date,data_estimate$ICU,type="l",col="red",ylim=c(0,1000))
lines(data_estimate$date,data_estimate$C,col="green")

plot(data_estimate$date,data_estimate$Recovered,type="l",col="red")
lines(data_estimate$date,data_estimate$R,col="green")

plot(data_estimate$date,data_estimate$total_deaths,type="l",col="red")
lines(data_estimate$date,data_estimate$D,col="green")

##We employ the Kalman Filter
Qseq=seq(-0.4,1.6,by=0.1)
Rseq=seq(0,2,by=0.1)


rmseHpost=matrix(0,nrow=21,ncol=21)
rmseCpost=matrix(0,nrow=21,ncol=21)
rmseRpost=matrix(0,nrow=21,ncol=21)
rmseDpost=matrix(0,nrow=21,ncol=21)
rmse_sum_post=matrix(0,nrow=21,ncol=21)
data_kalman=head(data1,-2)
K_Hpost=c()
K_Cpost=c()
K_Rpost=c()
K_Dpost=c()




for(k in 1:21){
  for(l in 1:21){
    
    A=matrix(c(0.27,0,0,0,0.02,0.37,0,0,0.7,0.62,1,0,0.01,0.01,0,1),nrow=4,ncol=4,byrow=TRUE)
    DN=list()
    DN[[1]]=matrix(c(N[1],0,recovered_non_severe[1],0),nrow=4,ncol=1)
    xpr=list()
    Ppr=list()
    Hk=diag(4)
    Y=list()
    xpost=list()
    Ppost=list()
    KG=list()
    x0=matrix(c(data1$Hospitalized[1],data1$ICU[1],data1$Recovered[1],data1$total_deaths[1]),nrow=4,ncol=1)
    P0=diag(4)
    xpr[[1]]=x0
    Ppr[[1]]=P0
    df=cbind(data1$Hospitalized,data1$ICU,data1$Recovered,data1$total_deaths)
    xpost[[1]]=x0
    Ppost[[1]]=P0
    
    for(i in 1:89){
      
      Q=diag(c(1+Qseq[k],1+Qseq[k],0.5+Qseq[k],1+Qseq[k]),nrow=4,ncol=4)
      Rk=diag(c(0.5+Rseq[l],0.05+Rseq[l],1+Rseq[l],1.3+Rseq[l]),nrow=4,ncol=4)
      
      #a priori estimate
      DN[[i+1]]=matrix(c(N[i+1],0,recovered_non_severe[i+1],0),nrow=4,ncol=1)
      
      
      xpr[[i+1]]=A%*%x0+DN[[i+1]]
      
      #a priori covariance matrix
      
      Ppr[[i+1]]=A%*%P0%*%t(A)+Q
      
      
      #Kalman Gain
      
      A1=Ppr[[i+1]]%*%t(Hk)
      A2=Hk%*%Ppr[[i+1]]%*%t(Hk)+Rk
      
      KG[[i]]=A1%*%solve(A2)
      
      #A posteriori estimate
      
      Y[[i]]=matrix(df[i+1,],nrow=4,ncol=1)
      Y[[i]]=as.numeric(Y[[i]])
      
      xpost[[i+1]]=xpr[[i+1]]+KG[[i]]%*%(Y[[i]]-xpr[[i+1]])
      
      
      #A posteriori covariance matrix
      
      Ppost[[i+1]]=(diag(4)-KG[[i]]%*%Hk)%*%Ppr[[i+1]]  
      
      ##Turning the new estimates into old ones
      x0=xpost[[i+1]]
      P0=Ppost[[i+1]]
      
      
      K_Hpost[i]=xpost[[i]][1]
      K_Cpost[i]=xpost[[i]][2]
      K_Rpost[i]=xpost[[i]][3]
      K_Dpost[i]=xpost[[i]][4]
    }
    
    rmseHpost[k,l]=rmse(data_kalman$Hospitalized,K_Hpost)
    rmseCpost[k,l]=rmse(data_kalman$ICU,K_Cpost)
    rmseRpost[k,l]=rmse(data_kalman$Recovered,K_Rpost)
    rmseDpost[k,l]=rmse(data_kalman$total_deaths,K_Dpost)
    rmse_sum_post[k,l]=sum(rmseHpost[k,l],rmseCpost[k,l],rmseRpost[k,l],rmseDpost[k,l])
  }
}

a=min(rmse_sum_post)
a
indexed_min=which(rmse_sum_post==min(rmse_sum_post),arr.ind = TRUE)
indexed_min
k=Qseq[indexed_min[1,1]]
l=Rseq[indexed_min[1,2]]
c(k,l)

Q=diag(c(1+k,1+k,0.5+k,1+k),nrow=4,ncol=4)
Rk=diag(c(0.5+l,0.05+l,1+l,1.3+l),nrow=4,ncol=4)

#So for these Q, R we have the smaller rmse


A=matrix(c(0.27,0,0,0,0.02,0.37,0,0,0.7,0.62,1,0,0.01,0.01,0,1),nrow=4,ncol=4,byrow=TRUE)
DN=list()
DN[[1]]=matrix(c(N[1],0,recovered_non_severe[1],0),nrow=4,ncol=1)
xpr=list()
Ppr=list()
Hk=diag(4)
Y=list()
xpost=list()
Ppost=list()
KG=list()

x0=matrix(c(data1$Hospitalized[1],data1$ICU[1],data1$Recovered[1],data1$total_deaths[1]),nrow=4,ncol=1)
P0=diag(4)
xpr[[1]]=x0
Ppr[[1]]=P0
df=cbind(data1$Hospitalized,data1$ICU,data1$Recovered,data1$total_deaths)

xpost[[1]]=x0
Ppost[[1]]=P0

for(i in 1:90){
  #a priori estimate
  DN[[i+1]]=matrix(c(N[i+1],0,recovered_non_severe[1],0),nrow=4,ncol=1)
  
  
  xpr[[i+1]]=A%*%x0+DN[[i+1]]
  
  #a priori covariance matrix
  
  Ppr[[i+1]]=A%*%P0%*%t(A)+Q
  
  
  #Kalman Gain
  
  A1=Ppr[[i+1]]%*%t(Hk)
  A2=Hk%*%Ppr[[i+1]]%*%t(Hk)+Rk
  
  KG[[i]]=A1%*%solve(A2)
  
  #A posteriori estimate
  
  Y[[i]]=matrix(df[i+1,],nrow=4,ncol=1)
  Y[[i]]=as.numeric(Y[[i]])
  
  xpost[[i+1]]=xpr[[i+1]]+KG[[i]]%*%(Y[[i]]-xpr[[i+1]])
  
  
  #A posteriori covariance matrix
  
  Ppost[[i+1]]=(diag(4)-KG[[i]]%*%Hk)%*%Ppr[[i+1]]  
  
  ##Turning the new estimates into old ones
  x0=xpost[[i+1]]
  P0=Ppost[[i+1]]
}



#Creating the H,C,R,D vectors#
K_Hapr=c()
K_Capr=c()
K_Rapr=c()
K_Dapr=c()

for (i in 1:89) {
  K_Hapr[i]=xpr[[i]][1]
}
for (i in 1:89) {
  K_Capr[i]=xpr[[i]][2]
}
for (i in 1:89) {
  K_Rapr[i]=xpr[[i]][3]
}
for (i in 1:89) {
  K_Dapr[i]=xpr[[i]][4]
}



K_Hpost=c()
K_Cpost=c()
K_Rpost=c()
K_Dpost=c()

for (i in 1:89) {
  K_Hpost[i]=xpost[[i]][1]
}
for (i in 1:89) {
  K_Cpost[i]=xpost[[i]][2]
}
for (i in 1:89) {
  K_Rpost[i]=xpost[[i]][3]
}
for (i in 1:89) {
  K_Dpost[i]=xpost[[i]][4]
}



##Visualize the results##
data_estimate2=head(data_estimate,-2)
data_estimate2=cbind(data_estimate2,K_Hapr,K_Capr,K_Rapr,K_Dapr,K_Hpost,K_Cpost,K_Rpost,K_Dpost)

plot(data_estimate2$date,data_estimate2$Hospitalized,type="l",col="red",ylim=c(2000,30000))
lines(data_estimate2$date,data_estimate2$H,col="green")
lines(data_estimate2$date,data_estimate2$K_Hapr,col="blue")
lines(data_estimate2$date,data_estimate2$K_Hpost,col="black")


plot(data_estimate2$date,data_estimate2$ICU,type="l",col="red",ylim=c(0,700))
lines(data_estimate2$date,data_estimate2$C,col="green")
lines(data_estimate2$date,data_estimate2$K_Capr,col="blue")
lines(data_estimate2$date,data_estimate2$K_Cpost,col="black")

plot(data_estimate2$date,data_estimate2$Recovered,type="l",col="red")
lines(data_estimate2$date,data_estimate2$R,col="green")
lines(data_estimate2$date,data_estimate2$K_Rapr,col="blue")
lines(data_estimate2$date,data_estimate2$K_Rpost,col="black")

plot(data_estimate2$date,data_estimate2$total_deaths,type="l",col="red")
lines(data_estimate2$date,data_estimate2$D,col="green")
lines(data_estimate2$date,data_estimate2$K_Dapr,col="blue")
lines(data_estimate2$date,data_estimate2$K_Dpost,col="black")


rmse_Kalman_Hapr=rmse(data_estimate2$Hospitalized,data_estimate2$K_Hapr)
rmse_Kalman_Capr=rmse(data_estimate2$ICU,data_estimate2$K_Capr)
rmse_Kalman_Rapr=rmse(data_estimate2$Recovered,data_estimate2$K_Rapr)
rmse_Kalman_Dapr=rmse(data_estimate2$total_deaths,data_estimate2$K_Dapr)
rmse_Kalman_SUMapr=sum(rmse_Kalman_Hapr,rmse_Kalman_Capr,rmse_Kalman_Rapr,rmse_Kalman_Dapr)

rmse_Kalman_Hpost=rmse(data_estimate2$Hospitalized,data_estimate2$K_Hpost)
rmse_Kalman_Cpost=rmse(data_estimate2$ICU,data_estimate2$K_Cpost)
rmse_Kalman_Rpost=rmse(data_estimate2$Recovered,data_estimate2$K_Rpost)
rmse_Kalman_Dpost=rmse(data_estimate2$total_deaths,data_estimate2$K_Dpost)
rmse_Kalman_SUMpost=sum(rmse_Kalman_Hpost,rmse_Kalman_Cpost,rmse_Kalman_Rpost,rmse_Kalman_Dpost)

c(rmse_final_SUM,rmse_final_H,rmse_final_C,rmse_final_R,rmse_final_D)
c(rmse_Kalman_SUMapr,rmse_Kalman_Hapr,rmse_Kalman_Capr,rmse_Kalman_Rapr,rmse_Kalman_Dapr)
c(rmse_Kalman_SUMpost,rmse_Kalman_Hpost,rmse_Kalman_Cpost,rmse_Kalman_Rpost,rmse_Kalman_Dpost)




