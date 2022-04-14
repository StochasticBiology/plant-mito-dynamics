#!/usr/bin/env Rscript
library(ggplot2)
library(rstatix)
library(ggpubr)

#find any mean enrichment values in the directory, build a graph of them 
dataFilesName <- Sys.glob(file.path("*","meanEnrichment*.csv"))

type<-c(rep("GFP",length(grep("GFP[0-9]",dataFilesName))),rep("MSH",length(grep("MSH[0-9]",dataFilesName)))) 

dataFiles <- lapply(Sys.glob(file.path("*","meanEnrichment*.csv")), read.table)

df<-data.frame(unlist(dataFiles),type,dataFilesName)

g1<-ggboxplot(df, y="unlist.dataFiles.",x="type",outlier.shape = NA) + geom_jitter(width = 0.2,aes(color = type)) + ylab("Mean Enrichment")
 
#runs Mann Whitney on the two samples. 
wilcox.test(unlist(df[df$type=="GFP",][1]),unlist(df[df$type=="MSH",][1]))

g2<-ggboxplot(df, y="unlist.dataFiles.",x="type",outlier.shape = NA) + 
  geom_jitter(width = 0.2,size=2.5,aes(color = type)) + ylab("Mean Enrichment") + 
  scale_y_continuous(limits = c(0,1.1*max(df$unlist.dataFiles.))) +
  theme(text = element_text(size=25),
        axis.title.x = element_blank(),
        legend.position="none") +
  scale_x_discrete(labels= c("mtGFP",
                             expression(paste("mtGFP-", italic("msh1"), sep="")))) +
  stat_compare_means(size=7)    # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
g2

ggsave("mtgfpvsmshColocEnrichment.pdf",g2 )
ggsave("mtgfpvsmshColocEnrichment.png",g2 )
