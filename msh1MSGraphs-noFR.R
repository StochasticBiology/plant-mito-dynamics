#Summary Statistics Graphs for MSH1 mitochondrial dynamics manuscript

#required packages 
message("Loading libraries...")
library(ggplot2)
library(rstatix)
library(ggpubr)

#list all names of directories
dataFileNames<-c(rep(paste("mtgfp-rawtrajectories/mtGFP-",1:18,".xml",sep=""),1:18,18),rep(paste("msh1-rawtrajectories/MSH-",1:28,".xml",sep=""),1:28,28))

#list all names of videos
dataFrameNames<-c(rep(paste("GFP",1:18,sep=""),1:18,18),rep(paste("MSH",1:28,sep=""),1:28,28))

#list their scientific names
type<-c(rep("mtGFP",length(grep("GFP[0-9]",dataFrameNames))),rep("mtGFP-msh1",length(grep("MSH[0-9]",dataFrameNames)))) 

#use this names of videos to get each stats and speed data frames, store as a list, 
#so it is easily accessible as statsList[[i]] or speedsList[[i]] for each video, under name dataFrameNames[i]
statsList<-NULL
speedList<-NULL
coloctimeList<-NULL
for(i in 1:length(dataFrameNames)){
  statsList[i]<-list(read.csv(paste(dataFrameNames[i],"-stats.csv",sep="")))
  speedList[i]<-list(read.csv(paste(dataFrameNames[i],"-speeds.csv",sep="")))
  coloctimeList[i]<-list(read.csv(paste(dataFrameNames[i],"-colocal-time.csv",sep="")))
}

#Kruskal Wallis testing
#useful guides: https://rcompanion.org/handbook/F_08.html
#https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
#  http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
#Dunns test is the most popular post-hoc analysis, but not needed in this script as only comparing two samples

#set the colour palette for plots  
mypalette = c("#0073C2FF", "#00A36C", "#868686FF")

#=================================
#physical statistics
#=================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean speed
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
speedMeanlist<-NULL
for(i in 1:length(dataFrameNames)){
  #The first value in the data frame is the mean
  speedMeanlist<-c(speedMeanlist,speedList[[i]]$x[1])
}
speedm<-data.frame(type,speedMeanlist)
colnames(speedm)<-c("Type","speedMean")

speedm$Type <- factor(speedm$Type, levels = unique(speedm$Type))   #To be able to plot x axis in the order it appears in the data frame 
ggplot(speedm, aes(x=Type, y=speedMean)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames))))


  
  speedplot<-ggboxplot(speedm, x = "Type", y = "speedMean", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) + ylab("Overall mean speed (µm/frame)") + scale_y_continuous(limits = c(0,NA)) +
    stat_compare_means(label.y= max(speedm$speedMean)+(max(speedm$speedMean)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
  speedplot
  
  ggsave(paste("speedMeanPlot-noFR.png",sep=""),speedplot,width = 10, height=16, units = "cm")
  ggsave(paste("speedMeanPlot-noFR.pdf",sep=""),speedplot,width = 10, height=16, units = "cm")
  





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean colocalisation time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colocMeanlist<-NULL
for(i in 1:length(dataFrameNames)){
  #The first value in the data frame is the mean
  colocMeanlist<-c(colocMeanlist,coloctimeList[[i]]$x[1])
}
colocmean<-data.frame(type,colocMeanlist)
colnames(colocmean)<-c("Type","colocMean")

colocmean$Type <- factor(colocmean$Type, levels = unique(colocmean$Type))   #To be able to plot x axis in the order it appears in the data frame 
ggplot(colocmean, aes(x=Type, y=colocMean)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames))))

  
colocplot<-ggboxplot(colocmean, x = "Type", y = "colocMean", color = "Type", palette = mypalette,   outlier.shape = NA) +  
  geom_jitter(width = 0.2,aes(color = Type)) + ylab("Overall mean colocalisation time (frames)") + scale_y_continuous(limits = c(0,NA)) +
    stat_compare_means(label.y= max(colocmean$colocMean)+(max(colocmean$colocMean)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
colocplot
  
ggsave(paste("colocMeanPlot-noFR.png",sep=""),colocplot,width = 10, height=16, units = "cm")
ggsave(paste("colocMeanPlot-noFR.pdf",sep=""),colocplot,width = 10, height=16, units = "cm")
  





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean Intermitochondrial distances at frames 10,50,100,110,115,118,119,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(f in c(10,50,100,110,115,118,119,120)){
  minDistslist<-NULL
  for(i in 1:length(dataFrameNames)){
    #get the value at chosen frame for the mean minimum distances,for all the input video data names
    minDistslist<-c(minDistslist,statsList[[i]]$mean.min.dist[f])
  }
  dm<-data.frame(type,minDistslist)
  colnames(dm)<-c("Type","DistsMeans")
  
  dm$Type <- factor(dm$Type, levels = unique(dm$Type))   #To be able to plot x axis in the order it appears in the data frame 
  ggplot(dm, aes(x=Type, y=DistsMeans)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames))))

  distsplot<-ggboxplot(dm, x = "Type", y = "DistsMeans", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) + ylab("Mean intermitochondrial distance (µm)") +  scale_y_continuous(limits = c(0,NA)) +
      stat_compare_means(label.y= max(dm$DistsMeans)+(max(dm$DistsMeans)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
  distsplot
    
  ggsave(paste("distsMeanPlotFrame",f,"-noFR.png",sep=""),distsplot,width = 10, height=16, units = "cm")
  ggsave(paste("distsMeanPlotFrame",f,"-noFR.pdf",sep=""),distsplot,width = 10, height=16, units = "cm")
    

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Intermitochondrial distances Mean of Means
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
minDistslist<-NULL
for(i in 1:length(dataFrameNames)){
  #get the mean value for all mean minimum distances,  for all the input video data names
  minDistslist<-c(minDistslist,mean(statsList[[i]]$mean.min.dist))
}

dm<-data.frame(type,minDistslist)
colnames(dm)<-c("Type","DistsMeans")

dm$Type <- factor(dm$Type, levels = unique(dm$Type))   #To be able to plot x axis in the order it appears in the data frame 
ggplot(dm, aes(x=Type, y=DistsMeans)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames))))

overalldistsplot<-ggboxplot(dm, x = "Type", y = "DistsMeans", color = "Type", palette = mypalette,   outlier.shape = NA) +  
  geom_jitter(width = 0.2,aes(color = Type)) + ylab("Overall mean intermitochondrial distance (µm)") + scale_y_continuous(limits = c(0,NA)) +
    stat_compare_means(label.y= max(dm$DistsMeans)+(max(dm$DistsMeans)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
overalldistsplot
  
ggsave(paste("distsMeanofMeansPlot-noFR.png",sep=""),overalldistsplot,width = 10, height=16, units = "cm")
ggsave(paste("distsMeanofMeansPlot-noFR.pdf",sep=""),overalldistsplot,width = 10, height=16, units = "cm")
  
#--------------------
#Figure 2
#--------------------

Fig2<-ggarrange(overalldistsplot, speedplot, colocplot, labels = c("A","B","C"),
                  nrow = 1,
                  ncol = 3 ,font.label = list(size = 25) )
Fig2
ggsave(paste("Figure2.svg",sep=""), Fig2, width = 30, height=13, units = "cm")






#=================================
#social (network) statistics - No mean of means, as these network formations exhibit time-dependent behavior
#=================================

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean Degree at frames 10,50,100,110,115,118,119,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(f in c(10,50,100,110,115,118,119,120)){
  meanDegreelist<-NULL
  for(i in 1:length(dataFrameNames)){
    #get the value at chosen frame for the mean degree, for all the input video data names
    meanDegreelist<-c(meanDegreelist,statsList[[i]]$mean.degree[f])
  }
  degm<-data.frame(type,meanDegreelist)
  colnames(degm)<-c("Type","DegreeMeans")
  
  degm$Type <- factor(degm$Type, levels = unique(degm$Type))   #To be able to plot x axis in the order it appears in the data frame 
  ggplot(degm, aes(x=Type, y=DegreeMeans)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames))))
  
  degreeplot<-ggboxplot(degm, x = "Type", y = "DegreeMeans", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) + ylab("Mean degree") +  scale_y_continuous(limits = c(0,NA)) +
      stat_compare_means(label.y= max(degm$DegreeMeans)+(max(degm$DegreeMeans)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
  degreeplot
    
  ggsave(paste("degreeMeansPlotFrame",f,"-noFR.png",sep=""),degreeplot,width = 10, height=16, units = "cm")
  ggsave(paste("degreeMeansPlotFrame",f,"-noFR.pdf",sep=""),degreeplot,width = 10, height=16, units = "cm")
    
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean network efficiency at frames 10,50,100,110,115,118,119,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(f in c(10,50,100,110,115,118,119,120)){
  netEfflist<-NULL
  for(i in 1:length(dataFrameNames)){
    #get the value at chosen frame for the mean network efficiency,  for all the input video data names
    netEfflist<-c(netEfflist,statsList[[i]]$efficiency[f])
  }
  effm<-data.frame(type,netEfflist)
  colnames(effm)<-c("Type","EfficiencyMeans")
  
  effm$Type <- factor(effm$Type, levels = unique(effm$Type))   #To be able to plot x axis in the order it appears in the data frame 
  ggplot(effm, aes(x=Type, y=EfficiencyMeans)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames))))
  

  efficiencyplot<-ggboxplot(effm, x = "Type", y = "EfficiencyMeans", color = "Type", palette = mypalette,   outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) + ylab("Mean network efficiency")  + scale_y_continuous(limits = c(0,NA)) +
      stat_compare_means(label.y= max(effm$EfficiencyMeans)+(max(effm$EfficiencyMeans)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
  efficiencyplot
    
  ggsave(paste("efficiencyMeansPlotFrame",f,"-noFR.png",sep=""),efficiencyplot,width = 10, height=16, units = "cm")
  ggsave(paste("efficiencyMeansPlotFrame",f,"-noFR.pdf",sep=""),efficiencyplot,width = 10, height=16, units = "cm")
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Network diameter at frames 10,50,100,110,115,118,119,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(f in c(10,50,100,110,115,118,119,120)){
  diamlist<-NULL
  for(i in 1:length(dataFrameNames)){
    #get the value at chosen frame for the mean network diameter, for all the input video data names
    diamlist<-c(diamlist,statsList[[i]]$diameter[f])
  }
  diam<-data.frame(type,diamlist)
  colnames(diam)<-c("Type","NetworkDiameter")
  
  diam$Type <- factor(diam$Type, levels = unique(diam$Type))   #To be able to plot x axis in the order it appears in the data frame 
  ggplot(diam, aes(x=Type, y=NetworkDiameter)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames))))

  diameterplot<-ggboxplot(diam, x = "Type", y = "NetworkDiameter", color = "Type", palette = mypalette,   outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) + ylab("Network diameter")  + scale_y_continuous(limits = c(0,NA)) +
      stat_compare_means(label.y= max(diam$NetworkDiameter)+(max(diam$NetworkDiameter)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
  diameterplot
    
  ggsave(paste("diameterPlotFrame",f,"-noFR.png",sep=""),diameterplot,width = 10, height=16, units = "cm")
  ggsave(paste("diameterPlotFrame",f,"-noFR.pdf",sep=""),diameterplot,width = 10, height=16, units = "cm")
    

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Network betweeness at frames 10,50,100,110,115,118,119,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(f in c(10,50,100,110,115,118,119,120)){
  bclist<-NULL
  for(i in 1:length(dataFrameNames)){
    #get the value at chosen frame for the mean network diameter, for all the input video data names
    bclist<-c(bclist,statsList[[i]]$betweenness[f])
  }
  BC<-data.frame(type,bclist)
  colnames(BC)<-c("Type","meanBC")
  
  BC$Type <- factor(BC$Type, levels = unique(BC$Type))   #To be able to plot x axis in the order it appears in the data frame 
  ggplot(BC, aes(x=Type, y=meanBC)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
  

    bcplot<-ggboxplot(BC, x = "Type", y = "meanBC", color = "Type", palette = mypalette,   outlier.shape = NA) +  
      geom_jitter(width = 0.2,aes(color = Type)) + ylab("Mean node betweeness")  + scale_y_continuous(limits = c(0,NA)) +
      stat_compare_means(label.y= max(BC$meanBC)+(max(BC$meanBC)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
    bcplot
    
    ggsave(paste("bcPlotFrame",f,"-noFR.png",sep=""),bcplot,width = 10, height=16, units = "cm")
    ggsave(paste("bcPlotFrame",f,"-noFR.pdf",sep=""),bcplot,width = 10, height=16, units = "cm")
    
  
}

#______________
#Figure 3
#______________

#These are sensitive plots as the social statistics plots rely on the final plot to come out of the above functions always being frame 120. 
#If you change the frame value the function loops over, you need to be aware it changes this frame plotted below. 

Fig3<-ggarrange(degreeplot, efficiencyplot, diameterplot ,bcplot, labels = c("A","B","C","D"),
                     nrow = 1,
                     ncol = 4 ,font.label = list(size = 25)  )
Fig3
ggsave(paste("Figure3.svg",sep=""), Fig3, width = 35, height=13, units = "cm")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Connected components at frames 10,50,100,110,115,118,119,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(f in c(10,50,100,110,115,118,119,120)){
  ccmlist<-NULL
  for(i in 1:length(dataFrameNames)){
    #get the value at chosen frame for the mean network diameter, for all the input video data names
    ccmlist<-c(ccmlist,statsList[[i]]$cc.mean.size[f])
  }
  CCM<-data.frame(type,ccmlist)
  colnames(CCM)<-c("Type","meannumberCC")
  
  CCM$Type <- factor(CCM$Type, levels = unique(CCM$Type))   #To be able to plot x axis in the order it appears in the data frame 
  ggplot(CCM, aes(x=Type, y=meannumberCC)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
  
  
  ccmplot<-ggboxplot(CCM, x = "Type", y = "meannumberCC", color = "Type", palette = mypalette,   outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) + ylab("Mean number connected components")  + scale_y_continuous(limits = c(0,NA)) +
    stat_compare_means(label.y= max(CCM$meannumberCC)+(max(CCM$meannumberCC)/2.5))    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
  ccmplot
  
  ggsave(paste("ccmPlotFrame",f,"-noFR.png",sep=""),ccmplot,width = 10, height=16, units = "cm")
  ggsave(paste("ccmPlotFrame",f,"-noFR.pdf",sep=""),ccmplot,width = 10, height=16, units = "cm")
  
  
}





