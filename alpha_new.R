data<-read.delim("raw.data.ratings6.txt")
names(data)

nrow(data)

data<-data[data$Hetero_Homo<4,]
library(psych)
library(lavaan)
library(semPlot)


#function enabling us to transform frequencies into musical distances (in halftones) from A4 (440 Hz)
halftones<-function(freq){
return(12*log2(freq/440))
}


versions<-paste(
rep(c("BR","CZ"),each=4),
rep(c("F","M"),each=2,times=2),
"rated_by",
rep(c("BR","CZ"),times=4),
sep="_"
)

versions2<-paste(rep(versions,each=4),rep(c("HB","IN","SA","TA"),times=length(versions)),sep="_")

versions2<-versions2[rep(rep(c(0,4),times=4)+rep(1:4,each=2),times=4)+rep(c(0,8,16,24),each=8)]

al1<-NA

for(i in 1:length(versions2)){
file<-data[,substr(names(data),1,nchar(versions2[i]))==versions2[i]]
al1[i]<-alpha(file)[[1]][1]
}

al1<-matrix(al1,nrow=4,byrow=T)

colnames(al1)<-paste(rep(c("HB","IN","SA","TA"),each=2),"rated by",rep(c("BR","CZ")))
rownames(al1)<-c("BR_F","BR_M","CZ_F","CZ_M")

View(file)


mode1<-paste(
rep(c("BR","CZ"),each=2),
rep(c("F","M"),each=1,times=2),sep="_"
)

mode2<-c("HB","IN","SA","TA")

al2<-NA
pocitadlo<-1

for(i in 1:length(mode2)){
for(j in 1:length(mode1)){
file<-data[,substr(names(data),18,19)==mode2[i]]
file<-file[,substr(names(file),1,4)==mode1[j]]
al2[pocitadlo]<-alpha(file)[[1]][1]
pocitadlo<-pocitadlo+1
}
}

al2

al2<-matrix(al2,nrow=4,byrow=F)

colnames(al2)<-paste(c("HB","IN","SA","TA"),"both")
rownames(al2)<-c("BR_F","BR_M","CZ_F","CZ_M")


al_both<-cbind(al1[,1:2],al2[,1],al1[,3:4],al2[,2],al1[,5:6],al2[,3],al1[,7:8],al2[,4])
colnames(al_both)[c(3,6,9,12)]<-colnames(al2)


write.table(al1,"alpha1.txt",col.names=NA,sep="\t")
write.table(al2,"alpha2.txt",col.names=NA,sep="\t")
write.table(al_both,"alpha_both.txt",col.names=NA,sep="\t")

mean(unlist(al1))
min(unlist(al1))

mean(unlist(al2))
min(unlist(al2))


attach(data)
names(data)

sing_F0
sing_range

names(data)
measures<-data[,867:874]
#measures<-data[,859:866]
names(measures)
cormat<-cor(measures,use="complete.obs")
colnames(cormat)<-c("HB_BR","HB_CZ","IN_BR","IN_CZ","SA_BR","SA_CZ","TA_BR","TA_CZ")
cormat

write.table(cormat,"cormat1.txt",col.names=NA,sep="\t")

names(data)[1:50]

vocals<-data[,c(5,6,9,10,13,14,17,18)]
#vocals<-data[,c(5:12)]
names(vocals)
cormat2<-cor(vocals,use="complete.obs")
cormat2

cor(data[,5],data[,6])
cor(data[,9],data[,10])
cor(data[,13],data[,14])
cor(data[,17],data[,18])

write.table(cormat2,"cormat2.txt",col.names=NA,sep="\t")


data$HB_min<-halftones(data$HB_min)
data$HB_max<-halftones(data$HB_max)

data$INTRO_min<-halftones(data$INTRO_min)
data$INTRO_max<-halftones(data$INTRO_max)

data$SA_min<-halftones(data$SA_min)
data$SA_max<-halftones(data$SA_max)

data$TA_min<-halftones(data$TA_min)
data$TA_max<-halftones(data$TA_max)


data$HB_range<-data$HB_max-data$HB_min
data$INTRO_range<-data$INTRO_max-data$INTRO_min
data$SA_range<-data$SA_max-data$SA_min
data$TA_range<-data$TA_max-data$TA_min

data$HB_F0<-halftones(data$HB_F0)
data$INTRO_F0<-halftones(data$INTRO_F0)
data$SA_F0<-halftones(data$SA_F0)
data$TA_F0<-halftones(data$TA_F0)


vocals<-data[,c(5,6,9,10,13,14,17,18)]
#vocals<-data[,c(5:12)]
cormat3<-cor(vocals,use="complete.obs")
cormat3

cor(data[,5],data[,6])
cor(data[,9],data[,10])
cor(data[,13],data[,14])
cor(data[,17],data[,18])

write.table(cormat3,"cormat3.txt",col.names=NA,sep="\t")


cormat2
cormat3


data$mean_speech_ATTR_BRraters<-(INTRO_ATTR_average_BRraters+TA_ATTR_average_BRraters)/2
data$mean_speech_ATTR_CZraters<-(INTRO_ATTR_average_CZraters+TA_ATTR_average_CZraters)/2

data$mean_singing_ATTR_BRraters<-(HB_ATTR_average_BRraters+SA_ATTR_average_BRraters)/2
data$mean_singing_ATTR_CZraters<-(HB_ATTR_average_CZraters+SA_ATTR_average_CZraters)/2


#Together bz category
cor(data$mean_speech_ATTR_BRraters,data$mean_singing_ATTR_BRraters,use="complete.obs")
cor(data$mean_speech_ATTR_CZraters,data$mean_singing_ATTR_CZraters,use="complete.obs")

cor.test(data$mean_speech_ATTR_CZraters,data$mean_speech_ATTR_BRraters,use="complete.obs")
cor.test(data$mean_singing_ATTR_CZraters,data$mean_singing_ATTR_BRraters,use="complete.obs")

#across
cor(data$mean_speech_ATTR_BRraters,data$mean_singing_ATTR_CZraters,use="complete.obs")
cor(data$mean_speech_ATTR_CZraters,data$mean_singing_ATTR_BRraters,use="complete.obs")


#calculate target variables

cor(data$HB_F0,data$SA_F0)
cor(data$HB_range,data$SA_range)

cor(data$INTRO_F0,data$TA_F0)
cor(data$INTRO_range,data$TA_range)

cormat
cormat2
cormat3

data$sing_F0<-(data$HB_F0+data$SA_F0)/2
data$sing_range<-(data$HB_range+data$SA_range)/2

data$talk_F0<-(data$INTRO_F0+data$TA_F0)/2
data$talk_range<-(data$INTRO_range+data$TA_range)/2

data$sing_ATTR<-(data$mean_singing_ATTR_BRraters+data$mean_singing_ATTR_CZraters)/2
data$talk_ATTR<-(data$mean_speech_ATTR_BRraters+data$mean_speech_ATTR_CZraters)/2


names(data)[1:30]


soi.dim<-data[,29:31]
cormat4<-cor(soi.dim,use="complete.obs")
cormat4

cormat2
cormat3

nrow(data)
View(data)
