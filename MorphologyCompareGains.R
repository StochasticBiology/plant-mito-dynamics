#Mitochondrial Morphology comparisons

#find any area data values in the directory, build a graph of them 
dataFilesName <- Sys.glob(file.path("*","*","*25AREA*.txt"))
#need to ensure our type name is in the correct position it is found within this search function, so see data frames. 

df1<-data.frame(grep("mtgfp620",dataFilesName),
           rep("mtgfp620",length(grep("mtgfp620",dataFilesName))))
colnames(df1)<-c("position","type")

df2<-data.frame(grep("mtgfp640",dataFilesName),
                rep("mtgfp640",length(grep("mtgfp640",dataFilesName))))
colnames(df2)<-c("position","type")

df3<-data.frame(grep("msh1",dataFilesName),
                rep("msh1",length(grep("msh1",dataFilesName))))
colnames(df3)<-c("position","type")

df<-rbind(df1,df2,df3)
dfordered<-df[order(df$position),]

dataFiles <- lapply(Sys.glob(file.path("*","*","*25AREA*.txt")), read.table)

df<-data.frame(dfordered$type,dataFilesName)


#looking for the data files that are of a type, so ask for those that have e.g.
#type="msh1" according to our positions list
areadf620<-c()
meangrey620<-c()
IntDen620<-c()
for(i in dfordered[dfordered$type=="mtgfp620",]$position){
    areadf620<-c(areadf620,dataFiles[[i]][1])
    meangrey620<-c(meangrey620,dataFiles[[i]][2])
    IntDen620<-c(IntDen620,dataFiles[[i]][6])
    boxplot(IntDen620)
}
boxplot(areadf620)
boxplot(meangrey620)
boxplot(IntDen620)

min(unlist(IntDen620)) 
max(unlist(IntDen620)) 
mean(unlist(IntDen620))

#looking for the data files that are of a type, so ask for those that have e.g.
#type="msh1" according to our positions list
areadf640<-c()
meangrey640<-c()
IntDen640<-c()
for(i in dfordered[dfordered$type=="mtgfp640",]$position){
  areadf640<-c(areadf640,dataFiles[[i]][1])
  meangrey640<-c(meangrey640,dataFiles[[i]][2])
  IntDen640<-c(IntDen640,dataFiles[[i]][6])
  boxplot(IntDen640)
}
boxplot(areadf640)
boxplot(meangrey640)
boxplot(IntDen640)

min(unlist(IntDen640)) 
max(unlist(IntDen640)) 
mean(unlist(IntDen640))

#looking for the data files that are of a type, so ask for those that have e.g.
#type="msh1" according to our positions list
areadfmsh<-c()
meangreymsh<-c()
IntDenmsh<-c()
for(i in dfordered[dfordered$type=="msh1",]$position){
  areadfmsh<-c(areadfmsh,dataFiles[[i]][1])
  meangreymsh<-c(meangreymsh,dataFiles[[i]][2])
  IntDenmsh<-c(IntDenmsh,dataFiles[[i]][6])
  boxplot(IntDenmsh)
}
boxplot(areadfmsh)
boxplot(meangreymsh)
boxplot(IntDenmsh)

min(unlist(IntDenmsh)) 
max(unlist(IntDenmsh)) 
mean(unlist(IntDenmsh))

p1<-hist(unlist(IntDen620),)
p2<-hist(unlist(IntDenmsh), main = "histogram of integrated density values")
p3<-hist(unlist(IntDen640), main = "histogram of integrated density values")

#msh1 vs gain 620
png("compareGains620vsmsh1.png")
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,60), main = "histogram of integrated density values")
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,60), add=T)
legend("topright", c("IntDen620", "IntDenmsh"), col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), lwd=10)
dev.off()

#msh1 vs gain 640
png("compareGains640vsmsh1.png")
plot( p2, col=rgb(0,0,1,1/4), xlim=c(0,60), main = "histogram of integrated density values")
plot( p3, col=rgb(1,0,0,1/4), xlim=c(0,60), add=T)
legend("topright", c("IntDen640","IntDenmsh"), col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), lwd=10)# first histogram
dev.off()

#How different, by percentage, are the grey values across the 620, 640 and msh1 mitochondria readings?

mean(unlist(meangreymsh))
mean(unlist(meangrey620))
mean(unlist(meangrey640))

#max grey is 255
onepercent=255/100
mean(unlist(meangreymsh))/onepercent
mean(unlist(meangrey620))/onepercent
mean(unlist(meangrey640))/onepercent

