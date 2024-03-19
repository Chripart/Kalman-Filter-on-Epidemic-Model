#Packages for the code
library(readxl)
library(dbplyr)
library(tidyverse)
library(ggplot2) #plots
library(ggpubr) #4-part plot
library(Metrics) #rmse


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

#Following, a scatterplot of the observations of all 4 states 
theme_set(theme_pubr())
GH=ggplot(data=data,aes(x=date,y=Hospitalized))+
  geom_point()
GC=ggplot(data=data,aes(x=date,y=ICU))+
  geom_point()
GD=ggplot(data=data,aes(x=date,y=total_deaths))+
  geom_point()
GR=ggplot(data=data,aes(x=date,y=Recovered))+
  geom_point()
figure=ggarrange(GH,GC,GR,GD, labels=c("Hospitalized","ICU","Recovered","Deaths"),ncol=2,nrow=2)
figure

#We create the vector N which is the cases that are admitted in the hospital which is 
#N(t)=rate*newcases
new_cases=c()
for(i in 1:105){
  new_cases[i]=data$total_cases[i+1]-data$total_cases[i]
}
set.seed(123)
rate=runif(15,min=0.001,max=0.05)
rate
rate=round(rate,digits=3)


#We set the mu and rho constant and we run different combinations of the other 4 parameters.
#We choose rate from the different values we created
#We choose the lambda, kappa and tau from different intervals, which we change based on the rmse
#After that we take the parameters and we set it as constant. Then we estimate the 
#best mu and rho for the rmse 

N=c()
mi=0.05
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
K=seq(0.81,1, by=0.01)
T=seq(0.81,1,by=0.01)

H=c()
H[1]=data$Hospitalized[1]
C=c()
C[1]=data$ICU[1]
R=c()
R[1]=data$Recovered[1]
D=c()
D[1]=data$total_deaths[1]



for(i in 1:15){
  for(j in 1:20){
    for(f in 1:20){
      for(g in 1:20){
        N=rate[i]*new_cases
        
        for(t in 1:105){
          
          H[t+1]=N[t]+(1-L[j]-K[f]-mi)*H[t]
          C[t+1]=L[j]*H[t]+(1-T[g]-ro)*C[t]
          R[t+1]=K[f]*H[t]+T[g]*C[t]+R[t]
          D[t+1]=mi*H[t]+ro*C[t]+D[t]
        }
        rmseH[j,f,g]=rmse(data$Hospitalized,H)
        rmseC[j,f,g]=rmse(data$ICU,C)
        rmseR[j,f,g]=rmse(data$Recovered,R)
        rmseD[j,f,g]=rmse(data$total_deaths,D)
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
lambda=L[min_indexes_sum[[d]][1]]
kappa=K[min_indexes_sum[[d]][2]]
taf=T[min_indexes_sum[[d]][3]]
c(rate[d],lambda,kappa,taf)

d=11
lambda=0.02
kappa=0.48
taf=0.5

N=rate[d]*new_cases
rmseH2=matrix(0,nrow=20,ncol=20)
rmseC2=matrix(0,nrow=20,ncol=20)
rmseR2=matrix(0,nrow=20,ncol=20)
rmseD2=matrix(0,nrow=20,ncol=20)
rmse_sum2=matrix(0,nrow=20,ncol=20)
M=seq(0.01,0.2,by=0.01)
Re=seq(0.01,0.2,by=0.01)
for(h in 1:20){
  for(l in 1:20){
    for(t in 1:105){
      H[t+1]=N[t]+(1-lambda-kappa-M[h])*H[t]
      C[t+1]=lambda*H[t]+(1-taf-Re[l])*C[t]
      R[t+1]=kappa*H[t]+taf*C[t]+R[t]
      D[t+1]=M[h]*H[t]+Re[l]*C[t]+D[t]
    }
    rmseH2[h,l]=rmse(data$Hospitalized,H)
    rmseC2[h,l]=rmse(data$ICU,C)
    rmseR2[h,l]=rmse(data$Recovered,R)
    rmseD2[h,l]=rmse(data$total_deaths,D)
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


N=0.048*new_cases
lambda=0.02
kappa=0.48
taf=0.44
mi=0
ro=0.01

#After we find the best combination we create the model to produce the rmse values

for(t in 1:105){
  H[t+1]=N[t]+(1-lambda-kappa-mi)*H[t]
  C[t+1]=lambda*H[t]+(1-taf-ro)*C[t]
  R[t+1]=kappa*H[t]+taf*C[t]+R[t]
  D[t+1]=mi*H[t]+ro*C[t]+D[t]
}
data_estimate=cbind(H,C,R,D)
rmse_final_H=rmse(data$Hospitalized,H)
rmse_final_C=rmse(data$ICU,C)
rmse_final_R=rmse(data$Recovered,R)
rmse_final_D=rmse(data$total_deaths,D)
rmse_final_SUM=sum(rmse_final_H,rmse_final_C,rmse_final_R,rmse_final_D)

c(rmse_final_SUM,rmse_final_H,rmse_final_C,rmse_final_R,rmse_final_D)

#visualising our results
data_estimate=cbind(data,H,C,R,D)
plot(data_estimate$Hospitalized,type="l",col="red",ylim=c(0,30000))
lines(data_estimate$H,col="green")
plot(data_estimate$date,data_estimate$ICU,type="l",col="red",ylim=c(0,1000))
lines(data_estimate$date,data_estimate$C,col="green")
plot(data_estimate$date,data_estimate$Recovered,type="l",col="red")
lines(data_estimate$date,data_estimate$R,col="green")
plot(data_estimate$date,data_estimate$total_deaths,type="l",col="red")
lines(data_estimate$date,data_estimate$D,col="green")


#We employ the Kalman Filter


Qseq=seq(-0.9,1.1,by=0.1)
Rseq=seq(0,2,by=0.1)


rmseHpost=matrix(0,nrow=21,ncol=21)
rmseCpost=matrix(0,nrow=21,ncol=21)
rmseRpost=matrix(0,nrow=21,ncol=21)
rmseDpost=matrix(0,nrow=21,ncol=21)
rmse_sum_post=matrix(0,nrow=21,ncol=21)
data_kalman=head(data,-2)
K_Hpost=c()
K_Cpost=c()
K_Rpost=c()
K_Dpost=c()




for(k in 1:21){
  for(l in 1:21){
    
    A=matrix(c(0.5,0,0,0,0.02,0.55,0,0,0.48,0.44,1,0,0,0.01,0,1),nrow=4,ncol=4,byrow=TRUE)
    DN=list()
    DN[[1]]=matrix(c(N[1],0,0,0),nrow=4,ncol=1)
    xpr=list()
    Ppr=list()
    Hk=diag(4)
    Y=list()
    xpost=list()
    Ppost=list()
    KG=list()
    x0=matrix(c(data$Hospitalized[1],data$ICU[1],data$Recovered[1],data$total_deaths[1]),nrow=4,ncol=1)
    P0=diag(4)
    xpr[[1]]=x0
    Ppr[[1]]=P0
    df=cbind(data$Hospitalized,data$ICU,data$Recovered,data$total_deaths)
    xpost[[1]]=x0
    Ppost[[1]]=P0
    
    for(i in 1:104){
      
      Q=diag(c(1+Qseq[k],1+Qseq[k],1+Qseq[k],1+Qseq[k]),nrow=4,ncol=4)
      Rk=diag(c(0.3+Rseq[l],0.05+Rseq[l],10+Rseq[l],10+Rseq[l]),nrow=4,ncol=4)
      
      #a priori estimate
      DN[[i+1]]=matrix(c(N[i+1],0,0,0),nrow=4,ncol=1)
      
      
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

Q=diag(c(1+k,1+k,1+k,1+k),nrow=4,ncol=4)
Rk=diag(c(0.3+l,0.05+l,10+l,10+l),nrow=4,ncol=4)

#So for these Q, R we have the smaller rmse


A=matrix(c(0.5,0,0,0,0.02,0.55,0,0,0.48,0.44,1,0,0,0.01,0,1),nrow=4,ncol=4,byrow=TRUE)
DN=list()
DN[[1]]=matrix(c(N[1],0,0,0),nrow=4,ncol=1)
xpr=list()
Ppr=list()
Hk=diag(4)
Y=list()
xpost=list()
Ppost=list()
KG=list()

x0=matrix(c(data$Hospitalized[1],data$ICU[1],data$Recovered[1],data$total_deaths[1]),nrow=4,ncol=1)
P0=diag(4)
xpr[[1]]=x0
Ppr[[1]]=P0
df=cbind(data$Hospitalized,data$ICU,data$Recovered,data$total_deaths)

xpost[[1]]=x0
Ppost[[1]]=P0

for(i in 1:104){
  #a priori estimate
  DN[[i+1]]=matrix(c(N[i+1],0,0,0),nrow=4,ncol=1)
  
  
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

for (i in 1:104) {
  K_Hapr[i]=xpr[[i]][1]
}
for (i in 1:104) {
  K_Capr[i]=xpr[[i]][2]
}
for (i in 1:104) {
  K_Rapr[i]=xpr[[i]][3]
}
for (i in 1:104) {
  K_Dapr[i]=xpr[[i]][4]
}



K_Hpost=c()
K_Cpost=c()
K_Rpost=c()
K_Dpost=c()

for (i in 1:104) {
  K_Hpost[i]=xpost[[i]][1]
}
for (i in 1:104) {
  K_Cpost[i]=xpost[[i]][2]
}
for (i in 1:104) {
  K_Rpost[i]=xpost[[i]][3]
}
for (i in 1:104) {
  K_Dpost[i]=xpost[[i]][4]
}



##Visualize the results##
data_estimate2=head(data_estimate,-2)
data_estimate2=cbind(data_estimate2,K_Hapr,K_Capr,K_Rapr,K_Dapr,K_Hpost,K_Cpost,K_Rpost,K_Dpost)

plot(data_estimate2$date,data_estimate2$Hospitalized,type="l",col="red",ylim=c(0,15000))
lines(data_estimate2$date,data_estimate2$H,col="green")
lines(data_estimate2$date,data_estimate2$K_Hapr,col="blue")
lines(data_estimate2$date,data_estimate2$K_Hpost,col="black")


plot(data_estimate2$date,data_estimate2$ICU,type="l",col="red",ylim=c(0,600))
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
