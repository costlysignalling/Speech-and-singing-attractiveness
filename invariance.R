
start<-Sys.time()

data<-read.delim("raw.data.ratings6.txt")

data$age[data$age==99]<-NA

summary(data[1:30])

#function enabling us to transform frequencies into musical distances (in halftones) from A4 (440 Hz)
halftones<-function(freq){
return(12*log2(freq/440))
}

#function that shuffles data
shuffle<-function(data){
  for(i in 1:ncol(data)){
    take<-data[,i]
    take<-sample(take)
    data[,i]<-take
  }
  return(data)
}


library(psych)
library(lavaan)
library(semTools)
library(semPlot)

data<-data[data$Hetero_Homo<4,]
attach(data)


data$mean_speech_ATTR_BRraters<-(INTRO_ATTR_average_BRraters+TA_ATTR_average_BRraters)/2
data$mean_speech_ATTR_CZraters<-(INTRO_ATTR_average_CZraters+TA_ATTR_average_CZraters)/2

data$mean_singing_ATTR_BRraters<-(HB_ATTR_average_BRraters+SA_ATTR_average_BRraters)/2
data$mean_singing_ATTR_CZraters<-(HB_ATTR_average_CZraters+SA_ATTR_average_CZraters)/2



data$HB_min<-halftones(data$HB_min)
data$HB_max<-halftones(data$HB_max)

data$INTRO_min<-halftones(data$INTRO_min)
data$INTRO_max<-halftones(data$INTRO_max)

data$SA_min<-halftones(data$SA_min)
data$SA_max<-halftones(data$SA_max)

data$TA_min<-halftones(data$TA_min)
data$TA_max<-halftones(data$TA_max)


data$HB_pitch_range<-data$HB_max-data$HB_min
data$INTRO_pitch_range<-data$INTRO_max-data$INTRO_min
data$SA_pitch_range<-data$SA_max-data$SA_min
data$TA_pitch_range<-data$TA_max-data$TA_min

data$HB_F0<-halftones(data$HB_F0)
data$INTRO_F0<-halftones(data$INTRO_F0)
data$SA_F0<-halftones(data$SA_F0)
data$TA_F0<-halftones(data$TA_F0)


summary(c(data$HB_pitch_range,data$SA_pitch_range))


data$sing_F0<-(data$HB_F0+data$SA_F0)/2
data$sing_range<-(data$HB_pitch_range+data$SA_pitch_range)/2

data$talk_F0<-(data$INTRO_F0+data$TA_F0)/2
data$talk_range<-(data$INTRO_pitch_range+data$TA_pitch_range)/2

data$sing_ATTR<-(data$mean_singing_ATTR_BRraters+data$mean_singing_ATTR_CZraters)/2
data$talk_ATTR<-(data$mean_speech_ATTR_BRraters+data$mean_speech_ATTR_CZraters)/2


##model specification
model<- '
WHR_WSR ~~ height
WHR_WSR ~~ weight
WHR_WSR ~~ age

height ~~ weight
height ~~ age

weight ~~ age

sing_F0 ~ WHR_WSR + height + weight + age
sing_range ~ WHR_WSR + height + weight + age

talk_F0 ~ WHR_WSR + height + weight + age
talk_range ~ WHR_WSR + height + weight + age

sing_ATTR ~ sing_F0 + sing_range + WHR_WSR + height + weight + age
talk_ATTR ~ talk_F0 + talk_range + WHR_WSR + height + weight + age

SOI_TOTAL ~ sing_ATTR + sing_F0 + sing_range + talk_ATTR + talk_F0 + talk_range + WHR_WSR + height + weight + age

        '

model2<- '
WHR_WSR ~~ c(v1,v1)*height
WHR_WSR ~~ c(v2,v2)*weight
WHR_WSR ~~ c(v3,v3)*age

height ~~ c(v4,v4)*weight
height ~~ c(v5,v5)*age

weight ~~ c(v6,v6)*age

sing_F0 ~ c(v7,v7)*WHR_WSR + c(v8,v8)*height + c(v9,v9)*weight + c(v10,v10)*age
sing_range ~ c(v11,v11)*WHR_WSR + c(v12,v12)*height + c(v13,v13)*weight + c(v14,v14)*age

talk_F0 ~ c(v15,v15)*WHR_WSR + c(v16,v16)*height + c(v17,v17)*weight + c(v18,v18)*age
talk_range ~ c(v19,v19)*WHR_WSR + c(v20,v20)*height + c(v21,v21)*weight + c(v22,v22)*age

sing_ATTR ~ c(v23,v23)*sing_F0 + c(v24,v24)*sing_range + c(v25,v25)*WHR_WSR + c(v26,v26)*height + c(v27,v27)*weight + c(v28,v28)*age
talk_ATTR ~ c(v29,v29)*talk_F0 + c(v30,v30)*talk_range + c(v31,v31)*WHR_WSR + c(v32,v32)*height + c(v33,v33)*weight + c(v34,v34)*age

SOI_TOTAL ~ c(v35,v35)*sing_ATTR + c(v36,v36)*sing_F0 + c(v37,v37)*sing_range + c(v38,v38)*talk_ATTR + c(v39,v39)*talk_F0 + c(v40,v40)*talk_range + c(v41,v41)*WHR_WSR + c(v42,v42)*height + c(v43,v43)*weight + c(v44,v44)*age

        '


#Specify coordinates for plotting

arrange<-c(
WHR_WSR=c(0,6),
height=c(0,5),
weight=c(0,4),
age=c(0,3),
sing_F0=c(2,8),
sing_range=c(2,7),
talk_F0=c(2,2),
talk_range=c(2,1),
sing_ATTR=c(3.5,7.5),
talk_ATTR=c(3.5,1.5),
SOI_TOTAL=c(4,4.5)
)

arrange<-matrix(arrange[c(9:22,1:8)],ncol=2,byrow=T)

#Dividing dataset to males and females

dataF<-data[data$sex==1,]
dataM<-data[data$sex==2,]

names(dataF)[1:20]

dataF<-dataF[,c(1,2,3,4,23,27,28,32,883:888)]
dataM<-dataM[,c(1,2,3,4,23,27,28,32,883:888)]

dataF<-dataF[rowSums(is.na(dataF))==0,]
dataM<-dataM[rowSums(is.na(dataM))==0,]

nrow(dataF)
nrow(dataM)

FBR<-dataF[dataF$target_country==1,]
FCZ<-dataF[dataF$target_country==2,]

MBR<-dataM[dataM$target_country==1,]
MCZ<-dataM[dataM$target_country==2,]

nrow(MBR)
mean(MBR$age)
sd(MBR$age)

nrow(FBR)
summary(FBR$age)
mean(FBR$age)
sd(FBR$age)


nrow(MCZ)
summary(MCZ$age)
mean(MCZ$age)
sd(MCZ$age)

nrow(FCZ)
summary(FCZ$age)
mean(FCZ$age)
sd(FCZ$age)


halftones<-function(freq){
return(12*log2(freq/440))
}


nrow(data)


data2<-rbind(dataF,dataM)
data2$Z_Target_ID
data$Z_Target_ID


data5<-read.delim("raw.data.ratings5.txt")
names(data5)[1:30]

datanew<-data5[match(data2$Z_Target_ID,data5$Z_Target_ID),]
ncol(datanew)
nrow(datanew)

newF<-datanew[datanew$sex==1,]
newM<-datanew[datanew$sex==2,]

summary(c(newF$INTRO_F0,newF$TA_F0))
summary(c(newM$INTRO_F0,newM$TA_F0))


summary(c(newF$HB_F0,newF$SA_F0))
summary(c(newM$HB_F0,newM$SA_F0))



summary(halftones(c(newF$INTRO_F0,newF$TA_F0)))

new<-datanew

F0<-c(new$INTRO_F0,new$TA_F0,new$HB_F0,new$SA_F0)
range<-c(new$INTRO_max,new$TA_max,new$HB_max,new$SA_max)-c(new$INTRO_min,new$TA_min,new$HB_min,new$SA_min)
cor(F0,range)

F0<-halftones(c(new$INTRO_F0,new$TA_F0,new$HB_F0,new$SA_F0))
range<-halftones(c(new$INTRO_max,new$TA_max,new$HB_max,new$SA_max))-halftones(c(new$INTRO_min,new$TA_min,new$HB_min,new$SA_min))



attach(datanew)

datanew$mean_speech_ATTR_BRraters<-(INTRO_ATTR_average_BRraters+TA_ATTR_average_BRraters)/2
datanew$mean_speech_ATTR_CZraters<-(INTRO_ATTR_average_CZraters+TA_ATTR_average_CZraters)/2

datanew$mean_singing_ATTR_BRraters<-(HB_ATTR_average_BRraters+SA_ATTR_average_BRraters)/2
datanew$mean_singing_ATTR_CZraters<-(HB_ATTR_average_CZraters+SA_ATTR_average_CZraters)/2


datanew$HB_pitch_range<-datanew$HB_max-datanew$HB_min
datanew$INTRO_pitch_range<-datanew$INTRO_max-datanew$INTRO_min
datanew$SA_pitch_range<-datanew$SA_max-datanew$SA_min
datanew$TA_pitch_range<-datanew$TA_max-datanew$TA_min



datanew$sing_F0<-(datanew$HB_F0+datanew$SA_F0)/2
datanew$sing_range<-(datanew$HB_pitch_range+datanew$SA_pitch_range)/2

datanew$talk_F0<-(datanew$INTRO_F0+datanew$TA_F0)/2
datanew$talk_range<-(datanew$INTRO_pitch_range+datanew$TA_pitch_range)/2

datanew$sing_ATTR<-(datanew$mean_singing_ATTR_BRraters+datanew$mean_singing_ATTR_CZraters)/2
datanew$talk_ATTR<-(datanew$mean_speech_ATTR_BRraters+datanew$mean_speech_ATTR_CZraters)/2

datanew$sing_range

nrow(datanew)


#I had to make new, cleaned, data without NAs
write.table(datanew,"raw.data.ratings6.txt",sep="\t",row.names=F)

data<-rbind(dataF,dataM)

nrow(data)

data$sex<-as.factor(data$sex)
data$target_country<-as.factor(data$target_country)

levels(data$sex)<-c("F","M")
levels(data$target_country)<-c("BR","CZ")


fit<-sem(model, data=data,group="sex")
summary(fit, standardized=T,fit.measures=T)
chi1<-fitMeasures(fit)[3]
df1<-fitMeasures(fit)[4]

fit<-sem(model2, data=data,group="sex")
summary(fit, standardized=T,fit.measures=T)
chi2<-fitMeasures(fit)[3]
df2<-fitMeasures(fit)[4]

chi1
df1

chi2
df2

chi2-chi1
df2-df1

pchisq(chi2-chi1,df2-df1,lower.tail=F)

dataM<-data[data$sex=="M",]
dataF<-data[data$sex=="F",]

#countries path invariance
#Males

fit<-sem(model, data=dataM,group="target_country")
summary(fit, standardized=T,fit.measures=T)
chi1<-fitMeasures(fit)[3]
df1<-fitMeasures(fit)[4]

fit<-sem(model2, data=dataM,group="target_country")
summary(fit, standardized=T,fit.measures=T)
chi2<-fitMeasures(fit)[3]
df2<-fitMeasures(fit)[4]

chi1
df1

chi2
df2

chi2-chi1
df2-df1

pchisq(chi2-chi1,df2-df1,lower.tail=F)

#females only

fit<-sem(model, data=dataF,group="target_country")
summary(fit, standardized=T,fit.measures=T)
chi1<-fitMeasures(fit)[3]
df1<-fitMeasures(fit)[4]

fit<-sem(model2, data=dataF,group="target_country")
summary(fit, standardized=T,fit.measures=T)
chi2<-fitMeasures(fit)[3]
df2<-fitMeasures(fit)[4]

chi1
df1

chi2
df2

chi2-chi1
df2-df1

pchisq(chi2-chi1,df2-df1,lower.tail=F)



#This is a bit different, should work for CFA
measurementInvariance(model=model, 
                      data = data, 
                      group = "sex")



nrow(dataF)
nrow(dataM)

