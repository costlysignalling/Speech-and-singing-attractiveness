data<-read.delim("raw.data.ratings6.txt")

halftones<-function(freq){
return(12*log2(freq/440))
}

nrow(data)
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



data$sing_F0<-(data$HB_F0+data$SA_F0)/2
data$sing_range<-(data$HB_pitch_range+data$SA_pitch_range)/2

data$talk_F0<-(data$INTRO_F0+data$TA_F0)/2
data$talk_range<-(data$INTRO_pitch_range+data$TA_pitch_range)/2

data$sing_ATTR<-(data$mean_singing_ATTR_BRraters+data$mean_singing_ATTR_CZraters)/2
data$talk_ATTR<-(data$mean_speech_ATTR_BRraters+data$mean_speech_ATTR_CZraters)/2

dataM<-data[data$sex==2,]
dataF<-data[data$sex==1,]

nrow(dataM)
nrow(dataF)

cor.test(dataM$sing_F0,dataM$talk_F0)
cor.test(dataF$sing_F0,dataF$talk_F0)


cor.test(dataM$sing_range,dataM$talk_range)
cor.test(dataF$sing_range,dataF$talk_range)


cor.test(dataM$sing_ATTR,dataM$talk_ATTR)
cor.test(dataF$sing_ATTR,dataF$talk_ATTR)

summary(dataM$sing_ATTR)
summary(dataM$talk_ATTR)

mean(dataM$sing_ATTR-dataM$talk_ATTR)

t.test(dataM$sing_ATTR,dataM$talk_ATTR,paired=T)

summary(dataF$sing_ATTR)
summary(dataF$talk_ATTR)

mean(dataF$sing_ATTR-dataF$talk_ATTR)
t.test(dataF$sing_ATTR,dataF$talk_ATTR,paired=T)


model<-lm(sing_ATTR~sex+target_country+age,data=data)
summary(model)
summary(aov(model))


model<-lm(sing_ATTR~as.factor(sex)+as.factor(target_country)+age,data=data)
summary(model)
summary(aov(model))

summary(aov(model))[[1]][,2]/sum(summary(aov(model))[[1]][,2])



model<-lm(talk_ATTR~as.factor(sex)+as.factor(target_country)+age,data=data)
summary(model)
summary(aov(model))

summary(aov(model))[[1]][,2]/sum(summary(aov(model))[[1]][,2])

tapply(data$talk_ATTR,data$sex,mean)
tapply(data$sing_ATTR,data$sex,mean)

tapply(data$sing_ATTR,data$target_country,mean)


model<-lm(cbind(talk_F0,talk_range,sing_F0,sing_range)~as.factor(sex)+as.factor(target_country)+age,data=data)
summary(model)
summary(aov(model))

summary(aov(model))[[1]][,2]/sum(summary(aov(model))[[1]][,2])
summary(aov(model))[[2]][,2]/sum(summary(aov(model))[[2]][,2])
summary(aov(model))[[3]][,2]/sum(summary(aov(model))[[3]][,2])
summary(aov(model))[[4]][,2]/sum(summary(aov(model))[[4]][,2])

BRM<-data[data$sex==2&data$target_country==1,]
CZM<-data[data$sex==2&data$target_country==2,]
AM<-data[data$sex==2,]
BRF<-data[data$sex==1&data$target_country==1,]
CZF<-data[data$sex==1&data$target_country==2,]
AF<-data[data$sex==1,]

mean(BRM$talk_F0)
sd(BRM$talk_F0)

mean(CZM$talk_F0)
sd(CZM$talk_F0)

mean(AM$talk_F0)
sd(AM$talk_F0)

mean(BRF$talk_F0)
sd(BRF$talk_F0)

mean(CZF$talk_F0)
sd(CZF$talk_F0)

mean(AF$talk_F0)
sd(AF$talk_F0)



mean(BRM$talk_F0)
sd(BRM$talk_F0)

mean(CZM$talk_F0)
sd(CZM$talk_F0)

mean(AM$talk_F0)
sd(AM$talk_F0)

mean(BRF$talk_F0)
sd(BRF$talk_F0)

mean(CZF$talk_F0)
sd(CZF$talk_F0)

mean(AF$talk_F0)
sd(AF$talk_F0)



mean(BRM$sing_F0)
sd(BRM$sing_F0)

mean(CZM$sing_F0)
sd(CZM$sing_F0)

mean(AM$sing_F0)
sd(AM$sing_F0)

mean(BRF$sing_F0)
sd(BRF$sing_F0)

mean(CZF$sing_F0)
sd(CZF$sing_F0)

mean(AF$sing_F0)
sd(AF$sing_F0)



mean(BRM$talk_range)
sd(BRM$talk_range)

mean(CZM$talk_range)
sd(CZM$talk_range)

mean(AM$talk_range)
sd(AM$talk_range)

mean(BRF$talk_range)
sd(BRF$talk_range)

mean(CZF$talk_range)
sd(CZF$talk_range)

mean(AF$talk_range)
sd(AF$talk_range)



mean(BRM$sing_range)
sd(BRM$sing_range)

mean(CZM$sing_range)
sd(CZM$sing_range)

mean(AM$sing_range)
sd(AM$sing_range)

mean(BRF$sing_range)
sd(BRF$sing_range)

mean(CZF$sing_range)
sd(CZF$sing_range)

mean(AF$sing_range)
sd(AF$sing_range)







