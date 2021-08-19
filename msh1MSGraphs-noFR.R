#Summary Statistics Graphs for MSH1 mitochondrial dynamics manuscript

#required packages 
message("Loading libraries...")
library(ggplot2)
library(rstatix)
library(ggpubr)

workingDirectory<-"~/Documents/PIPELINEnew/"
setwd(workingDirectory)
#Read in data names
GFP1<-"mtGFP/bigcellmanymitos-2-bonuscell-ADJUSTEDCROPPED_Tracks-1-1_5-4-5-2-F2_57.xml"
GFP10<-"mtGFP/120notasstrongPI_use-ADJUSTEDCROPPED_Tracks-1-2_5-4-5-2-F5_08.xml"
GFP11<-"mtGFP/C2-justmtGFPniceADJUSTED-3DDriftCorrectCROPPED-cell3_Tracks-1-5-4-5-2.xml"
GFP12<-"mtGFP/C2-mtgfp-okADJUSTED-2x3DDriftCorrect-CROPPED-cell1_Tracks-1-4-4-5-2.xml"
GFP13<-"mtGFP/C2-justmtGFPniceADJUSTED-3DDriftCorrectCROPPED-cell2_Tracks1-4-4-5-2.xml"
GFP14<-"mtGFP/C2-mtgfp-bitdrifty-3DDriftCorrect-ADJUSTEDCROPPED_Tracks-1-6-4-5-2.xml"
GFP15<-"mtGFP/C2-mtgfp-okADJUSTED-2x3DDriftCorrect-CROPPED-cell2_Tracks-1-5-4-5-2.xml"
GFP16<-"mtGFP/C2-justmtGFPniceADJUSTED-3DDriftCorrectCROPPED-cell1_Tracks-1-5-4-5-2.xml"
GFP17<-"mtGFP/C2-justmtGFPbitjumpy-3DDriftCorrect-ADJUSTEDCROPPED_Tracks-1-5-4-5-2.xml"
GFP18<-"mtGFP/C2-mtgfp-niceADJUSTED-3DDriftCorrectCROPPED_Tracks-1-5-4-5-2.xml"
GFP2<-"mtGFP/manycells120-2-bonuscell-ADJUSTEDCROPPED_Tracks-1-3-4-5-2-F6_86.xml"
GFP3<-"mtGFP/nice120ADJUSTEDCROPPED_Tracks-1-5-4-5-2-F0.xml"
GFP4<-"mtGFP/goodvid120ADJUSTEDCROPPED_Tracks-1-5-4-5-2-F5_77.xml"
GFP5<-"mtGFP/manycells120ADJUSTED-RE-CROPPED_Tracks-1-4-4-5-2-F8_29.xml"
GFP6<-"mtGFP/MiyoGFPjustover5min1-4zoomADJUSTEDCROPPED_Tracks-1-4-4-5-2-F7_66.xml"
GFP7<-"mtGFP/mitoGFPslightlystressed-ADJUSTEDCROPPED-2-TRIMMEDTO120FRAMES_Tracks-1-4-4-5-2-F8_04.xml"
GFP8<-"mtGFP/bigcellmanymitosADJUSTEDCROPPED-TOPMIS-CROPREMOVED_Tracks-1-4-4-5-2-F5_56.xml"
GFP9<-"mtGFP/singlecell120_useADJUSTEDCROPPED_Tracks-1-4-4-5-5-F5_5.xml"
MSH1<-"msh1/C2-msh1-smallcells-ADJUSTEDCROPPED-cell1_Tracks-0_8-6-4-5-2.xml"
MSH10<-"msh1/C2-line11-9-2-good2ADJUSTEDCROPPED_Tracks-1-3-4-5-2-F9.xml"
MSH11<-"msh1/C2-msh1-drifty-Correct3DDrift-ADJUSTEDCROPPED-cell1_Tracks-1-4-4-5-2.xml"
MSH12<-"msh1/C2-msh1-loadscells-3DDriftCorrectADJUSTEDCROPPED-cell1_Tracks-1-7-3-4-2-F8_43.xml"
MSH13<-"msh1/C2-msh1bitjumpy2ADJUSTEDCROPPED-cell2_Tracks_1_4_4_5_2.xml"
MSH14<-"msh1/C2-msh1-smallcells-ADJUSTEDCROPPED-cell2_Tracks-0_8-6-3-5-2.xml"
MSH15<-"msh1/C2-msh1-manycellsniceADJUSTEDCROPPED-cell2_Tracks-0_8-6-4-5-2-F6_9.xml"
MSH16<-"msh1/C2-msh1-alright-Correct3DDrift-ADJUSTEDCROPPED-cell2_Tracks-1-5-4-5-2.xml"
MSH17<-"msh1/C2-line11-9-2-good2ADJUSTEDCROPPED-cell3_Tracks-1-4-4-5-2.xml"
MSH18<-"msh1/C2-msh1-greatbutdrifty-Correct3DDriftTWICEADJUSTEDCROPPED_Tracks-1-4-4-5-2.xml"
MSH19<-"msh1/C2-msh1nicebutreallyrjumpyADJUSTEDCROPPED_Tracks-1-5-4-5-2.xml"
MSH2<-"msh1/C2-msh1bitjumpy2ADJUSTEDCROPPED-cell1_Tracks_1_4_4_5_2.xml"
MSH20<-"msh1/C2-msh1bitjumpyADJUSTEDCROPPED-cell2_Tracks_1_4_4_5_2.xml"
MSH21<-"msh1/C2-msh1-justoneADJUSTEDCROPPED_Tracks1-4-4-5-2.xml"
MSH22<-"msh1/C2-msh1-middle-Correct3DDrift-ADJUSTEDCROPPED_Tracks-1-4-4-5-2.xml"
MSH23<-"msh1/C2-line11-9-2-good2ADJUSTEDCROPPED-cell2_Tracks-1-4-4-5-2.xml"
MSH24<-"msh1/C2-msh1-loadscells-3DDriftCorrectADJUSTEDCROPPED-cell5_Tracks-1-6-3-4-2.xml"
MSH25<-"msh1/C2-msh1-loadscells-3DDriftCorrectADJUSTEDCROPPED-cell2_Tracks-0_8-8-4-5-2-F9_7.xml"
MSH26<-"msh1/C2-msh1-drifty-Correct3DDrift-ADJUSTEDCROPPED-cell2_Tracks-1-4-4-5-2.xml"
MSH27<-"msh1/C2-msh1-loadscells-3DDriftCorrectADJUSTEDCROPPED-cell3_Tracks-0_8-5-4-5-2.xml"
MSH28<-"msh1/C2-msh1-loadscells-3DDriftCorrectADJUSTEDCROPPED-cell4_Tracks-1-6-4-5-2.xml"
MSH3<-"msh1/C2-msh1-alright-Correct3DDrift-ADJUSTEDCROPPED-cell1_Tracks-0_8-4-4-5-2.xml"
MSH4<-"msh1/C2-msh1-manycellsniceADJUSTEDCROPPED-cell1_Tracks-1-5-4-5-2.xml"
MSH5<-"msh1/C2-msh1bitjumpyADJUSTEDCROPPED-cell1_Tracks-1_4_4_5_2.xml"
MSH6<-"msh1/C2-line11-9-2-goodbitjumpyADJUSTEDCROPPED_Tracks-1-4-4-5-2.xml"
MSH7<-"msh1/C2-msh1-niceADJUSTEDCROPPED_Tracks-1-6-4-5-3-F7_15.xml"
MSH8<-"msh1/C2-msh1-ratherjumpy-ADJUSTEDCROPPED_Tracks-1-4-4-5-2.xml"
MSH9<-"msh1/C2-line11-9-2-goodbitjumpy2ADJUSTEDCROPPED_Tracks-1-4-4-5-2.xml"

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
  statsList[i]<-list(read.csv(paste(mget(dataFrameNames[i]),"-stats.csv",sep="")))
  speedList[i]<-list(read.csv(paste(mget(dataFrameNames[i]),"-speeds.csv",sep="")))
  coloctimeList[i]<-list(read.csv(paste(mget(dataFrameNames[i]),"-colocal-time.csv",sep="")))
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
  
  #ggsave(paste(workingDirectory ,"speedMeanPlot-noFR.png",sep=""),speedplot,width = 10, height=16, units = "cm")
  #ggsave(paste(workingDirectory ,"speedMeanPlot-noFR.pdf",sep=""),speedplot,width = 10, height=16, units = "cm")
  





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
  
#ggsave(paste(workingDirectory ,"colocMeanPlot-noFR.png",sep=""),colocplot,width = 10, height=16, units = "cm")
#ggsave(paste(workingDirectory ,"colocMeanPlot-noFR.pdf",sep=""),colocplot,width = 10, height=16, units = "cm")
  





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
    
  #ggsave(paste(workingDirectory ,"distsMeanPlotFrame",f,"-noFR.png",sep=""),distsplot,width = 10, height=16, units = "cm")
  #ggsave(paste(workingDirectory ,"distsMeanPlotFrame",f,"-noFR.pdf",sep=""),distsplot,width = 10, height=16, units = "cm")
    

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
  
#ggsave(paste(workingDirectory ,"distsMeanofMeansPlot-noFR.png",sep=""),overalldistsplot,width = 10, height=16, units = "cm")
#ggsave(paste(workingDirectory ,"distsMeanofMeansPlot-noFR.pdf",sep=""),overalldistsplot,width = 10, height=16, units = "cm")
  
#--------------------
#Figure 2
#--------------------

Fig2<-ggarrange(overalldistsplot, speedplot, colocplot, labels = c("A","B","C"),
                  nrow = 1,
                  ncol = 3 ,font.label = list(size = 25) )
Fig2
ggsave(paste(workingDirectory ,"Figure2.svg",sep=""), Fig2, width = 30, height=13, units = "cm")






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
    
  #ggsave(paste(workingDirectory ,"degreeMeansPlotFrame",f,"-noFR.png",sep=""),degreeplot,width = 10, height=16, units = "cm")
  #ggsave(paste(workingDirectory ,"degreeMeansPlotFrame",f,"-noFR.pdf",sep=""),degreeplot,width = 10, height=16, units = "cm")
    
  
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
    
  #ggsave(paste(workingDirectory ,"efficiencyMeansPlotFrame",f,"-noFR.png",sep=""),efficiencyplot,width = 10, height=16, units = "cm")
  #ggsave(paste(workingDirectory ,"efficiencyMeansPlotFrame",f,"-noFR.pdf",sep=""),efficiencyplot,width = 10, height=16, units = "cm")
  
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
    
  #ggsave(paste(workingDirectory ,"diameterPlotFrame",f,"-noFR.png",sep=""),diameterplot,width = 10, height=16, units = "cm")
  #ggsave(paste(workingDirectory ,"diameterPlotFrame",f,"-noFR.pdf",sep=""),diameterplot,width = 10, height=16, units = "cm")
    

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
    
    #ggsave(paste(workingDirectory ,"bcPlotFrame",f,"-noFR.png",sep=""),bcplot,width = 10, height=16, units = "cm")
    #ggsave(paste(workingDirectory ,"bcPlotFrame",f,"-noFR.pdf",sep=""),bcplot,width = 10, height=16, units = "cm")
    
  
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
ggsave(paste(workingDirectory ,"Figure3.svg",sep=""), Fig3, width = 35, height=13, units = "cm")



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
  
  #ggsave(paste(workingDirectory ,"ccmPlotFrame",f,"-noFR.png",sep=""),ccmplot,width = 10, height=16, units = "cm")
  #ggsave(paste(workingDirectory ,"ccmPlotFrame",f,"-noFR.pdf",sep=""),ccmplot,width = 10, height=16, units = "cm")
  
  
}





