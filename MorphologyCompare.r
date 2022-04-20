#Mitochondrial Morphology comparisons

#find any area data values in the directory, build a graph of them
dataFilesName <- Sys.glob(file.path("*","*25AREA*.txt"))

#need to ensure our type name is in the correct position it is found within this search function,
#so we grab their position, write a type, and grab their file name.

msh1files <- data.frame(grep("msh1", dataFilesName),
                        rep("msh1", length(grep(
                          "msh1", dataFilesName
                        ))),
                        dataFilesName[grep("msh1", dataFilesName)])
colnames(msh1files) <- c("position", "type", "name")

mtgfpfiles <- data.frame(grep("mtgfp", dataFilesName),
                         rep("mtgfp", length(grep(
                           "mtgfp", dataFilesName
                         ))),
                         dataFilesName[grep("mtgfp", dataFilesName)])
colnames(mtgfpfiles) <- c("position", "type", "name")

#then bind them and re-order to ensure correct positioning.
df <- rbind(msh1files, mtgfpfiles)
dfordered <- df[order(df$position), ]

#we then want to create plotting groups for each biological and technical rep.
plant <- rep("x", nrow(dfordered))
plant[grep("plant1", dfordered$name)] <- 1
plant[grep("plant2", dfordered$name)] <- 2
plant[grep("plant3", dfordered$name)] <- 3

cell <- rep("x", nrow(dfordered))
cell[grep("snap1", dfordered$name)] <- 1
cell[grep("snap2", dfordered$name)] <- 2
cell[grep("snap3", dfordered$name)] <- 3

df2 <- data.frame(dfordered, plant, cell)

#This grabs the actual data from the data files.
dataFiles <- lapply(Sys.glob(file.path("*","*25AREA*.txt")), read.table)

#now let's gather all area values
areasList <- NULL
for (i in 1:length(dataFiles)) {
  #The first value in the data frame is the mean
  areasList <- rbind(areasList, dataFiles[[i]][["Area"]])
}

#and put them into a clear table with their type, seedling number and cell number.

extendedData<-NULL
for (i in 1:nrow(areasList)) {
  extendedData<-rbind(extendedData,
                      data.frame(areasList[i, ], 
                                 rep(df2$type[i], length(areasList[i, ])),
                                 rep(df2$plant[i], length(areasList[i, ])),
                                 rep(df2$cell[i], length(areasList[i, ]))
                                 )
                      )
}

colnames(extendedData)<-c("area","type","plant","cell")

#Then we generate the mean of 25x mito area measurements
areaMean<-c()
areas<-c()
for(i in 1:length(dataFiles)){
  areaMean<-c(areaMean,mean(dataFiles[[i]][["Area"]]))
  areas<-rbind(areas,dataFiles[[i]][["Area"]])
}

df<-data.frame(df2,areaMean)

library(ggplot2)
library(dplyr)
library(rstatix)
library(ggpubr)

#set the colour palette for plots  
mypalette =  c("#43CD80","#63B8FF","#FFA500")
            #green      #blue       #orange
#plot just the area means
pdf("mitoMorphologyMeanAreas.pdf")
  meansplot<-ggboxplot(df, x = "type", y = "areaMean", palette=mypalette, outlier.shape = NA) +  
    geom_jitter(width = 0.2,size=5,alpha=0.5,aes(color = plant,shape=cell)) +  ylab("Mean Mitochondrial area (µm\u00B2)")+ scale_y_continuous(limits = c(0,NA)) +
    #stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
    stat_compare_means() +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
    theme(text = element_text(size = 16)) + 
    scale_x_discrete(labels = c(expression(paste("mtGFP-", italic("msh1"), sep = "" )),"mtGFP"))
  meansplot
dev.off()

#Plot all the area values for both genotypes
pdf("mitoMorphologyAreas.pdf")
areasplot<-ggboxplot(extendedData, x = "type", y = "area", palette=mypalette, outlier.shape = NA) +  
  geom_jitter(width = 0.4,size=3,alpha=0.3,aes(color = plant,shape=cell)) +  ylab("Mitochondrial area (µm\u00B2)")+ scale_y_continuous(limits = c(0,NA)) +
  #stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
  stat_compare_means(size=4.5) +   # Add global p-value. Function does this automatically. label.y specification is positioning relative to axes
  theme(text = element_text(size = 16)) + 
  scale_x_discrete(labels = c(expression(paste("mtGFP-", italic("msh1"), sep = "" )),"mtGFP"))
areasplot
dev.off()

