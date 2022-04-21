#Summary Statistics Graphs for MSH1 mitochondrial dynamics manuscript

#required packages 
message("Loading libraries...")
library(ggplot2)
library(rstatix)
library(ggpubr)

#list all names of directories
dataFileNames<-c(rep(paste("mtgfp-rawtrajectories/mtGFP-",1:18,".xml",sep=""),1:18,18),rep(paste("msh1-rawtrajectories/MSH-",1:28,".xml",sep=""),1:28,28),rep(paste("friendly-rawtrajectories/Friendly-",1:19,".xml",sep=""),1:19,19))

#list all names of videos
dataFrameNames<-c(rep(paste("mtGFP",1:18,sep=""),1:18,18),rep(paste("MSH",1:28,sep=""),1:28,28),rep(paste("F",1:19,sep=""),1:19,19))

#list their scientific names
type<-c(rep("mtGFP",length(grep("GFP[0-9]",dataFrameNames))),rep("mtGFP-msh1",length(grep("MSH[0-9]",dataFrameNames))),rep("friendly",length(grep("F[0-9]",dataFrameNames)))) 

#use this names of videos to get each stats and speed data frames, store as a list, 
#so it is easily accessible as statsList[[i]] or speedsList[[i]] for each video, under name dataFrameNames[i]
statsList<-NULL
speedList<-NULL
coloctimeList<-NULL
for(i in 1:length(dataFrameNames)){
  statsList[i]<-list(read.csv(paste(dataFileNames[i],"-stats.csv",sep="")))
  speedList[i]<-list(read.csv(paste(dataFileNames[i],"-speeds.csv",sep="")))
  coloctimeList[i]<-list(read.csv(paste(dataFileNames[i],"-colocal-time.csv",sep="")))
}

#Kruskal Wallis testing
#useful guides: https://rcompanion.org/handbook/F_08.html
#https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
#  http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
#Dunns test is the most popular post-hoc analysis


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
ggplot(speedm, aes(x=Type, y=speedMean)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames)))) 

kt<- kruskal.test(speedMean ~ Type, data = speedm)
if(kt$p.value < 0.05){
  #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
  
  #false discovery rate
  speedm %>% dunn_test(speedMean ~ Type, p.adjust.method = "fdr") 
  #pwc = pairwise comparison
  pwc<- speedm %>% dunn_test(speedMean ~ Type, p.adjust.method = "fdr") 
  #position over appropriate banners
  pwc <- pwc %>% add_xy_position(x = "Type")
  #round it
  pwc$p.adj<-round(pwc$p.adj, digits = 3)
  
  speedplot<-ggboxplot(speedm, x = "Type", y = "speedMean", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Overall mean speed (µm/frame)")+ scale_y_continuous(limits = c(0,NA)) +
    stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
    stat_compare_means(label.y= max(speedm$speedMean)+(max(speedm$speedMean)/2.5)) +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
    theme(text = element_text(size = 10))
  speedplot
  
  ggsave(paste("speedMeanPlot.png",sep=""),speedplot,width = 10, height=16, units = "cm")
  ggsave(paste("speedMeanPlot.pdf",sep=""),speedplot,width = 10, height=16, units = "cm")
  
} else {
  #do not include the post-hoc analysis
  
  speedplot<-ggboxplot(speedm, x = "Type", y = "speedMean", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Overall mean speed (µm/frame)") + scale_y_continuous(limits = c(0,NA)) +
    stat_compare_means(label.y= max(speedm$speedMean)+(max(speedm$speedMean)/2.5))   +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
    theme(text = element_text(size = 10))
  speedplot
  
  ggsave(paste("speedMeanPlot.png",sep=""),speedplot,width = 10, height=16, units = "cm")
  ggsave(paste("speedMeanPlot.pdf",sep=""),speedplot,width = 10, height=16, units = "cm")
  
}




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
ggplot(colocmean, aes(x=Type, y=colocMean)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))

kt<- kruskal.test(colocMean ~ Type, data = colocmean)
if(kt$p.value < 0.05){
  #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
  
  #false discovery rate
  colocmean %>% dunn_test(colocMean ~ Type, p.adjust.method = "fdr") 
  #pwc = pairwise comparison
  pwc<- colocmean %>% dunn_test(colocMean ~ Type, p.adjust.method = "fdr") 
  #position over appropriate banners
  pwc <- pwc %>% add_xy_position(x = "Type")
  #round it
  pwc$p.adj<-round(pwc$p.adj, digits = 3)
  
  colocplot<-ggboxplot(colocmean, x = "Type", y = "colocMean", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Overall mean colocalisation time (frames)")+ scale_y_continuous(limits = c(0,NA)) +
    stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
    stat_compare_means(label.y= max(colocmean$colocMean)+(max(colocmean$colocMean)/2.5))   +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
    theme(text = element_text(size = 10))
  colocplot
  
  ggsave(paste("colocMeanPlot.png",sep=""),colocplot,width = 10, height=16, units = "cm")
  ggsave(paste("colocMeanPlot.pdf",sep=""),colocplot,width = 10, height=16, units = "cm")
  
} else {
  #do not include the post-hoc analysis
  
  colocplot<-ggboxplot(colocmean, x = "Type", y = "colocMean", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Overall mean colocalisation time (frames)") + scale_y_continuous(limits = c(0,NA)) +
    stat_compare_means(label.y= max(colocmean$colocMean)+(max(colocmean$colocMean)/2.5))  +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
    theme(text = element_text(size = 10))
  colocplot
  
  ggsave(paste("colocMeanPlot.png",sep=""),colocplot,width = 10, height=16, units = "cm")
  ggsave(paste("colocMeanPlot.pdf",sep=""),colocplot,width = 10, height=16, units = "cm")
  
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean Intermitochondrial distances at frames 5,10,50,100,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(f in c(5,10,50,100,120)){
  minDistslist<-NULL
  for(i in 1:length(dataFrameNames)){
    #get the value at chosen frame for the mean minimum distances,for all the input video data names
    minDistslist<-c(minDistslist,statsList[[i]]$mean.min.dist[f])
  }
  dm<-data.frame(type,minDistslist)
  colnames(dm)<-c("Type","DistsMeans")
  
  dm$Type <- factor(dm$Type, levels = unique(dm$Type))   #To be able to plot x axis in the order it appears in the data frame 
  ggplot(dm, aes(x=Type, y=DistsMeans)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
  
  kt<- kruskal.test(DistsMeans ~ Type, data = dm)
  if(kt$p.value < 0.05){
  #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
    
    #false discovery rate
    dm %>% dunn_test(DistsMeans ~ Type, p.adjust.method = "fdr") 
    #pwc = pairwise comparison
    pwc<- dm %>% dunn_test(DistsMeans ~ Type, p.adjust.method = "fdr") 
    #position over appropriate banners
    pwc <- pwc %>% add_xy_position(x = "Type")
    #round it
    pwc$p.adj<-round(pwc$p.adj, digits = 3)
    
    distsplot<-ggboxplot(dm, x = "Type", y = "DistsMeans", color = "Type", palette = mypalette,  outlier.shape = NA) +  
      geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Mean intermitochondrial distance (µm)") + scale_y_continuous(limits = c(0,NA)) +
      stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
      stat_compare_means(label.y= max(dm$DistsMeans)+(max(dm$DistsMeans)/2.5))   +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
      theme(text = element_text(size = 10))
    distsplot
    
    ggsave(paste("distsMeanPlotFrame",f,".png",sep=""),distsplot,width = 10, height=16, units = "cm")
    ggsave(paste("distsMeanPlotFrame",f,".pdf",sep=""),distsplot,width = 10, height=16, units = "cm")

  } else {
    #do not include the post-hoc analysis
    
    distsplot<-ggboxplot(dm, x = "Type", y = "DistsMeans", color = "Type", palette = mypalette,  outlier.shape = NA) +  
      geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Mean intermitochondrial distance (µm)") +  scale_y_continuous(limits = c(0,NA)) +
      stat_compare_means(label.y= max(dm$DistsMeans)+(max(dm$DistsMeans)/2.5))   +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
      theme(text = element_text(size = 10))
    distsplot
    
    ggsave(paste("distsMeanPlotFrame",f,".png",sep=""),distsplot,width = 10, height=16, units = "cm")
    ggsave(paste("distsMeanPlotFrame",f,".pdf",sep=""),distsplot,width = 10, height=16, units = "cm")
    
  }
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
ggplot(dm, aes(x=Type, y=DistsMeans)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))

kt<- kruskal.test(DistsMeans ~ Type, data = dm)
if(kt$p.value < 0.05){
  #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
  
  #false discovery rate
  dm %>% dunn_test(DistsMeans ~ Type, p.adjust.method = "fdr") 
  #pwc = pairwise comparison
  pwc<- dm %>% dunn_test(DistsMeans ~ Type, p.adjust.method = "fdr") 
  #position over appropriate banners
  pwc <- pwc %>% add_xy_position(x = "Type")
  #round it
  pwc$p.adj<-round(pwc$p.adj, digits = 3)
  
  overalldistsplot<-ggboxplot(dm, x = "Type", y = "DistsMeans", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Overall mean intermitochondrial distance (µm)")+ scale_y_continuous(limits = c(0,NA)) +
    stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
    stat_compare_means(label.y= max(dm$DistsMeans)+(max(dm$DistsMeans)/2.5))  +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
    theme(text = element_text(size = 10))
  overalldistsplot
  
  ggsave(paste("distsMeanofMeansPlot.png",sep=""),overalldistsplot,width = 10, height=16, units = "cm")
  ggsave(paste("distsMeanofMeansPlot.pdf",sep=""),overalldistsplot,width = 10, height=16, units = "cm")
  
} else {
  #do not include the post-hoc analysis
  
  overalldistsplot<-ggboxplot(dm, x = "Type", y = "DistsMeans", color = "Type", palette = mypalette,  outlier.shape = NA) +  
    geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Overall mean intermitochondrial distance (µm)") + scale_y_continuous(limits = c(0,NA)) +
    stat_compare_means(label.y= max(dm$DistsMeans)+(max(dm$DistsMeans)/2.5))    +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
    theme(text = element_text(size = 10))
  overalldistsplot
  
  ggsave(paste("distsMeanofMeansPlot.png",sep=""),overalldistsplot,width = 10, height=16, units = "cm")
  ggsave(paste("distsMeanofMeansPlot.pdf",sep=""),overalldistsplot,width = 10, height=16, units = "cm")
  
}




#=================================
#social (network) statistics - No mean of means, as these network formations exhibit time-dependent behavior
#=================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean Degree at chosen frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
degplots<-function(frames){
  listdeg<-c()
 for(f in frames){
    meanDegreelist<-NULL
    for(i in 1:length(dataFrameNames)){
      #get the value at chosen frame for the mean degree, for all the input video data names
      meanDegreelist<-c(meanDegreelist,statsList[[i]]$mean.degree[f])
    }
    degm<-data.frame(type,meanDegreelist)
    colnames(degm)<-c("Type","DegreeMeans")
    
    degm$Type <- factor(degm$Type, levels = unique(degm$Type))   #To be able to plot x axis in the order it appears in the data frame 
    ggplot(degm, aes(x=Type, y=DegreeMeans)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
    
    kt<- kruskal.test(DegreeMeans ~ Type, data = degm)
    if(kt$p.value < 0.05){
      #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
      
      #false discovery rate
      degm %>% dunn_test(DegreeMeans ~ Type, p.adjust.method = "fdr") 
      #pwc = pairwise comparison
      pwc<- degm %>% dunn_test(DegreeMeans ~ Type, p.adjust.method = "fdr") 
      #position over appropriate banners
      pwc <- pwc %>% add_xy_position(x = "Type")
      #round it
      pwc$p.adj<-round(pwc$p.adj, digits = 3)
      
      degreeplot<-ggboxplot(degm, x = "Type", y = "DegreeMeans", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Mean degree")  +  scale_y_continuous(limits = c(0,NA)) +
        stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
        stat_compare_means(label.y= max(degm$DegreeMeans)+(max(degm$DegreeMeans)/2.5))   +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      degreeplot
      
      ggsave(paste("degreeMeansPlotFrame",f,".png",sep=""),degreeplot,width = 10, height=16, units = "cm")
      ggsave(paste("degreeMeansPlotFrame",f,".pdf",sep=""),degreeplot,width = 10, height=16, units = "cm")
      
      listdeg <- c(listdeg, list(degreeplot))
      
    } else {
      #do not include the post-hoc analysis
      
      degreeplot<-ggboxplot(degm, x = "Type", y = "DegreeMeans", color = "Type", palette = mypalette, outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Mean degree") +  scale_y_continuous(limits = c(0,NA)) +
        stat_compare_means(label.y= max(degm$DegreeMeans)+(max(degm$DegreeMeans)/2.5))  +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      degreeplot
      
      ggsave(paste("degreeMeansPlotFrame",f,".png",sep=""),degreeplot,width = 10, height=16, units = "cm")
      ggsave(paste("degreeMeansPlotFrame",f,".pdf",sep=""),degreeplot,width = 10, height=16, units = "cm")
      
      listdeg <- c(listdeg, list(degreeplot))
      
    }
  }
  return(listdeg)
}

#only want frames 10,50,100, so just enter those. 
deg <-ggarrange(plotlist=degplots(c(10,50,100)), labels = c("A.i","A.ii","A.iii"),
                nrow = 3,
                ncol = 1 ,font.label = list(size = 18))
deg
ggsave(paste("FigureS8PartA.png",sep=""),deg,width = 10, height=45, units = "cm")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean network efficiency at chosen frames 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

effplots<-function(frames){
  listeff<-c()
  for(f in frames){
    netEfflist<-NULL
    for(i in 1:length(dataFrameNames)){
      #get the value at chosen frame for the mean network efficiency,  for all the input video data names
      netEfflist<-c(netEfflist,statsList[[i]]$efficiency[f])
    }
    effm<-data.frame(type,netEfflist)
    colnames(effm)<-c("Type","EfficiencyMeans")
    
    effm$Type <- factor(effm$Type, levels = unique(effm$Type))   #To be able to plot x axis in the order it appears in the data frame 
    ggplot(effm, aes(x=Type, y=EfficiencyMeans)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
    
    kt<- kruskal.test(EfficiencyMeans ~ Type, data = effm)
    if(kt$p.value < 0.05){
      #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
      
      #false discovery rate
      effm %>% dunn_test(EfficiencyMeans ~ Type, p.adjust.method = "fdr") 
      #pwc = pairwise comparison
      pwc<- effm %>% dunn_test(EfficiencyMeans ~ Type, p.adjust.method = "fdr") 
      #position over appropriate banners
      pwc <- pwc %>% add_xy_position(x = "Type")
      #round it
      pwc$p.adj<-round(pwc$p.adj, digits = 3)
      
  
      efficiencyplot<-ggboxplot(effm, x = "Type", y = "EfficiencyMeans", color = "Type", palette = mypalette, outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Mean network efficiency") +  scale_y_continuous(limits = c(0,NA)) +
        stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
        stat_compare_means(label.y= max(effm$EfficiencyMeans)+(max(effm$EfficiencyMeans)/2.5))  +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      efficiencyplot
      
      ggsave(paste("efficiencyMeansPlotFrame",f,".png",sep=""),efficiencyplot,width = 10, height=16, units = "cm")
      ggsave(paste("efficiencyMeansPlotFrame",f,".pdf",sep=""),efficiencyplot,width = 10, height=16, units = "cm")
      
      listeff <- c(listeff, list(efficiencyplot))
      
    } else {
      #do not include the post-hoc analysis
      
      efficiencyplot<-ggboxplot(effm, x = "Type", y = "EfficiencyMeans", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) + ylab("Mean network efficiency")  + scale_y_continuous(limits = c(0,NA)) +
        stat_compare_means(label.y= max(effm$EfficiencyMeans)+(max(effm$EfficiencyMeans)/2.5))  +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      efficiencyplot
      
      ggsave(paste("efficiencyMeansPlotFrame",f,".png",sep=""),efficiencyplot,width = 10, height=16, units = "cm")
      ggsave(paste("efficiencyMeansPlotFrame",f,".pdf",sep=""),efficiencyplot,width = 10, height=16, units = "cm")
      
      listeff <- c(listeff, list(efficiencyplot))
      
    }
  }
 return(listeff)
}


#only want frames 10,50,100, so just enter those. 
eff <-ggarrange(plotlist=effplots(c(10,50,100)), labels = c("B.i","B.ii","B.iii"),
                nrow = 3,
                ncol = 1 ,font.label = list(size = 18))
eff
ggsave(paste("FigureS8PartB.png",sep=""),eff,width = 10, height=45, units = "cm")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Network diameter at chosen frames 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

diaplots<-function(frames){
  listdia<-c()
  for(f in frames){
    diamlist<-NULL
    for(i in 1:length(dataFrameNames)){
      #get the value at chosen frame for the mean network diameter, for all the input video data names
      diamlist<-c(diamlist,statsList[[i]]$diameter[f])
    }
    diam<-data.frame(type,diamlist)
    colnames(diam)<-c("Type","NetworkDiameter")
    
    diam$Type <- factor(diam$Type, levels = unique(diam$Type))   #To be able to plot x axis in the order it appears in the data frame 
    ggplot(diam, aes(x=Type, y=NetworkDiameter)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
    
    kt<- kruskal.test(NetworkDiameter ~ Type, data = diam)
    if(kt$p.value < 0.05){
      #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
      
      #false discovery rate
      diam %>% dunn_test(NetworkDiameter ~ Type, p.adjust.method = "fdr") 
      #pwc = pairwise comparison
      pwc<- diam %>% dunn_test(NetworkDiameter ~ Type, p.adjust.method = "fdr") 
      #position over appropriate banners
      pwc <- pwc %>% add_xy_position(x = "Type")
      #round it
      pwc$p.adj<-round(pwc$p.adj, digits = 3)
      
      
      diameterplot<-ggboxplot(diam, x = "Type", y = "NetworkDiameter", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Network diameter") +  scale_y_continuous(limits = c(0,NA)) +
        stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
        stat_compare_means(label.y= max(diam$NetworkDiameter)+(max(diam$NetworkDiameter)/2.5)) +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      diameterplot
      
      ggsave(paste("diameterPlotFrame",f,".png",sep=""),diameterplot,width = 10, height=16, units = "cm")
      ggsave(paste("diameterPlotFrame",f,".pdf",sep=""),diameterplot,width = 10, height=16, units = "cm")
      
      listdia <- c(listdia, list(diameterplot))
      
    } else {
      #do not include the post-hoc analysis
      
      diameterplot<-ggboxplot(diam, x = "Type", y = "NetworkDiameter", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Network diameter")  + scale_y_continuous(limits = c(0,NA)) +
        stat_compare_means(label.y= max(diam$NetworkDiameter)+(max(diam$NetworkDiameter)/2.5)) +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      diameterplot
      
      ggsave(paste("diameterPlotFrame",f,".png",sep=""),diameterplot,width = 10, height=16, units = "cm")
      ggsave(paste("diameterPlotFrame",f,".pdf",sep=""),diameterplot,width = 10, height=16, units = "cm")
      
      listdia <- c(listdia, list(diameterplot))
      
    }
  }
 return(listdia)
}


#only want frames 10,50,100, so just enter those. 
dia <-ggarrange(plotlist=diaplots(c(10,50,100)), labels = c("C.i","C.ii","C.iii"),
               nrow = 3,
               ncol = 1 ,font.label = list(size = 18))
dia
ggsave(paste("FigureS8PartC.png",sep=""),dia,width = 10, height=45, units = "cm")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Network betweeness at chosen frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bcplots<-function(frames){
  listbc<-c()
  for(f in frames){
    bclist<-NULL
    for(i in 1:length(dataFrameNames)){
      #get the value at chosen frame for the mean network diameter, for all the input video data names
      bclist<-c(bclist,statsList[[i]]$betweenness[f])
    }
    BC<-data.frame(type,bclist)
    colnames(BC)<-c("Type","meanBC")
    
    BC$Type <- factor(BC$Type, levels = unique(BC$Type))   #To be able to plot x axis in the order it appears in the data frame 
    ggplot(BC, aes(x=Type, y=meanBC)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
    
    kt<- kruskal.test(meanBC ~ Type, data = BC)
    if(kt$p.value < 0.05){
      #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
      
      #false discovery rate
      BC %>% dunn_test(meanBC ~ Type, p.adjust.method = "fdr") 
      #pwc = pairwise comparison
      pwc<- BC %>% dunn_test(meanBC ~ Type, p.adjust.method = "fdr") 
      #position over appropriate banners
      pwc <- pwc %>% add_xy_position(x = "Type")
      #round it
      pwc$p.adj<-round(pwc$p.adj, digits = 3)
      
      
      bcplot<-ggboxplot(BC, x = "Type", y = "meanBC", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Mean node betweeness") +  scale_y_continuous(limits = c(0,NA)) +
        stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
        stat_compare_means(label.y= max(BC$meanBC)+(max(BC$meanBC)/2.5))  +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      bcplot
      
      ggsave(paste("bcPlotFrame",f,".png",sep=""),bcplot,width = 10, height=16, units = "cm")
      ggsave(paste("bcPlotFrame",f,".pdf",sep=""),bcplot,width = 10, height=16, units = "cm")
      
      listbc <- c(listbc, list(bcplot))
      
    } else {
      #do not include the post-hoc analysis
      
      bcplot<-ggboxplot(BC, x = "Type", y = "meanBC", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Mean node betweeness")  + scale_y_continuous(limits = c(0,NA)) +
        stat_compare_means(label.y= max(BC$meanBC)+(max(BC$meanBC)/2.5)) +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      bcplot
      
      ggsave(paste("bcPlotFrame",f,".png",sep=""),bcplot,width = 10, height=16, units = "cm")
      ggsave(paste("bcPlotFrame",f,".pdf",sep=""),bcplot,width = 10, height=16, units = "cm")
      
      listbc <- c(listbc, list(bcplot))
      
    }
  }
  return(listbc)
}

#only want frames 10,50,100, so just enter those. 
bc <-ggarrange(plotlist=bcplots(c(10,50,100)), labels = c("D.i","D.ii","D.iii"),
              nrow = 3,
              ncol = 1 ,font.label = list(size = 18))
bc
ggsave(paste("FigureS8PartD.png",sep=""),bc,width = 10, height=45, units = "cm")



#~~~~~~~~~
#Figure 4
#~~~~~~~~~

toprow<-ggarrange(overalldistsplot, speedplot, colocplot, labels = c("A","B","C"),
              nrow = 1,
              ncol = 3 ,font.label = list(size = 25) )

#We just want frame 120 for these plots 
#Has to be a long-winded format to avoid "Cannot convert object of class list into a grob."

D<-ggarrange( plotlist=degplots(120))
E<-ggarrange( plotlist=effplots(120) )
FF<-ggarrange( plotlist=diaplots(120) )
G<-ggarrange( plotlist=bcplots(120)  )

bottomrow<-ggarrange(D,E,FF,G, labels = c("D","E","F","G"),
          nrow = 1,
          ncol = 4 ,font.label = list(size = 25) )

fig4<-ggarrange(toprow,bottomrow, nrow=2, ncol=1)
ggsave(paste("Figure4.png",sep=""), fig4, width = 35, height=30, units = "cm")







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Node and edge number plots over time for supplement 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Node number at frames 10,50,100,110,115,118,119,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nodeplots<-function(){
  a<-c()
  listn<-c()
  for(f in c(10,50,100,120)){
    nodelist<-NULL
    for(i in 1:length(dataFrameNames)){
      #get the value at chosen frame for the node number, for all the input video data names
      nodelist<-c(nodelist,statsList[[i]]$num.vertices[f])
      b<-data.frame(dataFrameNames[i],statsList[[i]]$num.vertices[f])
      a<-rbind(a,b)
    }
    nn<-data.frame(type,nodelist)
    colnames(nn)<-c("Type","NodeNumber")
    
    nn$Type <- factor(nn$Type, levels = unique(nn$Type))   #To be able to plot x axis in the order it appears in the data frame 
    ggplot(nn, aes(x=Type, y=NodeNumber)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
    
    kt<- kruskal.test(NodeNumber ~ Type, data = nn)
    if(kt$p.value < 0.05){
      #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
      
      #false discovery rate
      nn %>% dunn_test(NodeNumber ~ Type, p.adjust.method = "fdr") 
      #pwc = pairwise comparison
      pwc<- nn %>% dunn_test(NodeNumber ~ Type, p.adjust.method = "fdr") 
      #position over appropriate banners
      pwc <- pwc %>% add_xy_position(x = "Type")
      #round it
      pwc$p.adj<-round(pwc$p.adj, digits = 3)
      
      nodeplot<-ggboxplot(nn, x = "Type", y = "NodeNumber", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Node number")  +  scale_y_continuous(limits = c(0,NA)) +
        stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
        stat_compare_means(label.y= max(nn$NodeNumber)+(max(nn$NodeNumber)/2.5)) +  # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      
      nodeplot
      
      ggsave(paste("NodeNumberPlotFrame",f,".png",sep=""),nodeplot,width = 10, height=16, units = "cm")
      ggsave(paste("NodeNumberPlotFrame",f,".pdf",sep=""),nodeplot,width = 10, height=16, units = "cm")
      
      listn <- c(listn, list(nodeplot))
      
    } else {
      #do not include the post-hoc analysis
      
      nodeplot<-ggboxplot(nn, x = "Type", y = "NodeNumber", color = "Type", palette = mypalette, outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) + ylab("Node number") +  scale_y_continuous(limits = c(0,NA)) +
        stat_compare_means(label.y= max(nn$NodeNumber)+(max(nn$NodeNumber)/2.5)) +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      nodeplot
      
      ggsave(paste("NodeNumberPlotFrame",f,".png",sep=""),nodeplot,width = 10, height=16, units = "cm")
      ggsave(paste("NodeNumberPlotFrame",f,".pdf",sep=""),nodeplot,width = 10, height=16, units = "cm")
      
      listn <- c(listn, list(nodeplot))
    }
  }
  return(listn)
}


n <-ggarrange(plotlist=nodeplots(), labels = c("A.i","A.ii","A.iii","A.iv"),
              nrow = 1,
              ncol = 4 ,font.label = list(size = 18) , hjust=0.01 ,vjust=3)
n
ggsave(paste("NodeNumberPlotFramesTogether.png",sep=""),n,width = 35, height=15, units = "cm")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Edge number at frames 10,50,100,110,115,118,119,120
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

edgeplots<-function(){
  liste<-c()
  for(f in c(10,50,100,120)){
    edgelist<-NULL
    for(i in 1:length(dataFrameNames)){
      #get the value at chosen frame for the edge number, for all the input video data names
      edgelist<-c(edgelist,statsList[[i]]$num.edges[f])
    }
    en<-data.frame(type,edgelist)
    colnames(en)<-c("Type","EdgeNumber")
    
    en$Type <- factor(en$Type, levels = unique(en$Type))   #To be able to plot x axis in the order it appears in the data frame 
    ggplot(en, aes(x=Type, y=EdgeNumber)) + geom_dotplot(binaxis = "y", stackdir ="center") +  ggtitle(paste( "MitoGFP n=",length(grep("GFP[0-9]",dataFrameNames)), "      Msh1 n=", length(grep("MSH[0-9]",dataFrameNames)),"     friendly n=",length(grep("F[0-9]",dataFrameNames))))
    
    kt<- kruskal.test(EdgeNumber ~ Type, data = en)
    if(kt$p.value < 0.05){
      #if kruskal wallis test is significant to 0.05, perform and plot the pairwise post-hoc comparison 
      
      #false discovery rate
      en %>% dunn_test(EdgeNumber ~ Type, p.adjust.method = "fdr") 
      #pwc = pairwise comparison
      pwc<- en %>% dunn_test(EdgeNumber ~ Type, p.adjust.method = "fdr") 
      #position over appropriate banners
      pwc <- pwc %>% add_xy_position(x = "Type")
      #round it
      pwc$p.adj<-round(pwc$p.adj, digits = 3)
      
      edgeplot<-ggboxplot(en, x = "Type", y = "EdgeNumber", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Edge number")  +  scale_y_continuous(limits = c(0,NA)) +
        stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
        stat_compare_means(label.y= max(en$EdgeNumber)+(max(en$EdgeNumber)/2.5)) +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      edgeplot
      
      ggsave(paste("EdgeNumberPlotFrame",f,".png",sep=""),edgeplot,width = 10, height=16, units = "cm")
      ggsave(paste("EdgeNumberPlotFrame",f,".pdf",sep=""),edgeplot,width = 10, height=16, units = "cm")
      
      liste <- c(liste, list(edgeplot))
      
    } else {
      #do not include the post-hoc analysis
      
      edgeplot<-ggboxplot(en, x = "Type", y = "EdgeNumber", color = "Type", palette = mypalette,  outlier.shape = NA) +  
        geom_jitter(width = 0.2,aes(color = Type)) +  ylab("Edge number") +  scale_y_continuous(limits = c(0,NA)) +
        stat_compare_means(label.y= max(en$EdgeNumber)+(max(en$EdgeNumber)/2.5)) +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
        theme(text = element_text(size = 10))
      edgeplot
      
      ggsave(paste("EdgeNumberPlotFrame",f,".png",sep=""),edgeplot,width = 10, height=16, units = "cm")
      ggsave(paste("EdgeNumberPlotFrame",f,".pdf",sep=""),edgeplot,width = 10, height=16, units = "cm")
      
      liste <- c(liste, list(edgeplot))
    }
  }
  return(liste)
}

#Figure S6

e <-ggarrange(plotlist=edgeplots(), labels = c("B.i","B.ii","B.iii","B.iv"),
              nrow = 1,
              ncol = 4 ,font.label = list(size = 18) , hjust=0.01 ,vjust=3)
e
ggsave(paste("EdgeNumberPlotFramesTogether.png",sep=""),e,width = 35, height=15, units = "cm")

