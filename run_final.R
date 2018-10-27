
start<-Sys.time()

data<-read.delim("raw.data.ratings5.txt")

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

dataF<-dataF[,c(4,23,27,28,32,883:888)]
dataM<-dataM[,c(4,23,27,28,32,883:888)]

dataF<-dataF[rowSums(is.na(dataF))==0,]
dataM<-dataM[rowSums(is.na(dataM))==0,]


#first function get.parameters

get.par<-function(substrat,runs=10000){
fit<-sem(model, data=substrat)

esti<-parameterEstimates(fit, standardized=T)
estiO<-esti

randE<-cbind(estiO[,4],estiO[,4])

for(run in 1:runs){
subs<-shuffle(substrat)
fit<-sem(model, data=subs)
esti<-parameterEstimates(fit, standardized=T)
randE<-cbind(randE,esti[,4])
print(run)
}

ncol(randE)
randE<-randE[,c(-1,-2)]

permp<-NA
for(i in 1:nrow(randE)){
p1<-sum(randE[i,]>estiO[,4][i])/runs
p2<-sum(randE[i,]<estiO[,4][i])/runs
permp[i]<-min(p1,p2)*2
}

estiO<-cbind(estiO[,1:7],permp,estiO[,8:ncol(estiO)])

return(estiO)
}

all.res<-function(set,bootjack=10000){

origest<-get.par(set)

#starts jackknife

estJ<-cbind(origest[,4],origest[,4])
pvalJ<-cbind(origest[,7],origest[,7])
ppermJ<-cbind(origest[,8],origest[,8])
stdJ<-cbind(origest[,12],origest[,12])

for(jack in 1:nrow(set)){
jacked<-set[-jack,]
framest<-get.par(jacked,bootjack)
estJ<-cbind(estJ,framest[,4])
pvalJ<-cbind(pvalJ,framest[,7])
ppermJ<-cbind(ppermJ,framest[,8])
stdJ<-cbind(stdJ,framest[,12])
print(jack)
}

estJ<-estJ[,c(-1,-2)]
pvalJ<-pvalJ[,c(-1,-2)]
ppermJ<-ppermJ[,c(-1,-2)]
stdJ<-stdJ[,c(-1,-2)]

jack.res<-cbind(
origest[,4],
apply(estJ,1,mean),
apply(estJ,1,sd),
apply(estJ,1,quantile,0.025),
apply(estJ,1,quantile,0.975),
apply(estJ,1,min),
apply(estJ,1,max),
origest[,7],
apply(pvalJ,1,mean),
apply(pvalJ,1,sd),
apply(pvalJ,1,quantile,0.025),
apply(pvalJ,1,quantile,0.975),
apply(pvalJ,1,min),
apply(pvalJ,1,max),
origest[,8],
apply(ppermJ,1,mean),
apply(ppermJ,1,sd),
apply(ppermJ,1,quantile,0.025),
apply(ppermJ,1,quantile,0.975),
apply(ppermJ,1,min),
apply(ppermJ,1,max),
origest[,12],
apply(stdJ,1,mean),
apply(stdJ,1,sd),
apply(stdJ,1,quantile,0.025),
apply(stdJ,1,quantile,0.975),
apply(stdJ,1,min),
apply(stdJ,1,max))

jack.res<-as.data.frame(jack.res)

names(jack.res)<-
paste(
rep(c("est","p","p2","std"),each=7),
rep(c("observed","JK mean","sd","2.5%","97.5%","min","max"),times=4))

jack.res<-cbind(origest[,1:3],jack.res)

jack.res.round<-jack.res
for(i in 4:ncol(jack.res.round)){
jack.res.round[,i]<-round(jack.res.round[,i],3)
}

origest.round<-origest
for(i in 4:ncol(origest.round)){
origest.round[,i]<-round(origest.round[,i],3)
}

criterion<-jack.res.round[,c(1,2,3,25,30,31,11,17,18,24)]

return(list(origest,origest.round,jack.res,jack.res.round,criterion))
}

resM<-all.res(dataM)
resF<-all.res(dataF)


write.table(resM[[1]],"resM_raw.txt",sep="\t",row.names=F)
write.table(resM[[2]],"resM_ready.txt",sep="\t",row.names=F)

write.table(resM[[3]],"jackM_raw.txt",sep="\t",row.names=F)
write.table(resM[[4]],"jackM_ready.txt",sep="\t",row.names=F)

criterion<-resM[[5]]
cleaned<-criterion[criterion[,7]<0.05,]

write.table(criterion,"criterionM_full.txt",sep="\t",row.names=F)
write.table(cleaned,"criterionM_clean.txt",sep="\t",row.names=F)



write.table(resF[[1]],"resF_raw.txt",sep="\t",row.names=F)
write.table(resF[[2]],"resF_ready.txt",sep="\t",row.names=F)

write.table(resF[[3]],"jackF_raw.txt",sep="\t",row.names=F)
write.table(resF[[4]],"jackF_ready.txt",sep="\t",row.names=F)

criterion<-resF[[5]]
cleaned<-criterion[criterion[,7]<0.05,]

write.table(criterion,"criterionF_full.txt",sep="\t",row.names=F)
write.table(cleaned,"criterionF_clean.txt",sep="\t",row.names=F)

end<-Sys.time()

end-start


