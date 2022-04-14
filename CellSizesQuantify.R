#Quantifying cell types in order to make claims about differences in spread between msh1 and mtGFP
#I took the length/width and area of the CROPPEDADJUSTED cells for all msh1/mtGFP/fr cells
#length and width by drawing and measuring a line across the cell and area by tracing around the outside (ImageJ), and using Measure function (reads out in microns)

library(ggplot2)
library(rstatix)
library(ggpubr)

cellSizes<-read.csv("/Users/d1795494/Documents/GIT-plant-mito-dynamics/cellSizes/CellSizesQuantify.csv")
cellSizesTable<-data.frame(cellSizes$Type,cellSizes$Y..long.,as.numeric(cellSizes$X..short.),cellSizes$Area.All.sizes.taken.from.ADJUSTEDCROPPED.videos)
colnames(cellSizesTable)<-c("type","length","width","area" )

#area
ggplot(cellSizesTable, aes(x=type, y=area)) + geom_dotplot(binaxis = "y", stackdir ="center") + ylab("Area (µm\u00B2)") +
  ggtitle(paste( "MitoGFP n=",length(which(cellSizesTable$type %in% "mtGFP")), "Msh1 n=", length(which(cellSizesTable$type %in% "msh1")),"friendly n=",length(which(cellSizesTable$type %in% "friendly"))))

#length
ggplot(cellSizesTable, aes(x=type, y=length)) + geom_dotplot(binaxis = "y", stackdir ="center") + ylab("Length (µm)") +
  ggtitle(paste( "MitoGFP n=",length(which(cellSizesTable$type %in% "mtGFP")), "Msh1 n=", length(which(cellSizesTable$type %in% "msh1")),"friendly n=",length(which(cellSizesTable$type %in% "friendly"))))

#width
ggplot(cellSizesTable, aes(x=type, y=width)) + geom_dotplot(binaxis = "y", stackdir ="center") + ylab("Width (µm)") +
  ggtitle(paste( "MitoGFP n=",length(which(cellSizesTable$type %in% "mtGFP")), "Msh1 n=", length(which(cellSizesTable$type %in% "msh1")),"friendly n=",length(which(cellSizesTable$type %in% "friendly"))))

#test area
head(cellSizesTable)
kruskal.test(area ~ type, data = cellSizesTable)
cellSizesTable %>% dunn_test(area ~ type, p.adjust.method = "fdr") 

#test length
head(cellSizesTable)
kruskal.test(length ~ type, data = cellSizesTable)
cellSizesTable %>% dunn_test(length ~ type, p.adjust.method = "fdr") 

#test area
head(cellSizesTable)
kruskal.test(width ~ type, data = cellSizesTable)
cellSizesTable %>% dunn_test(width ~ type, p.adjust.method = "fdr") 


#set the colour palette for plots  
mypalette = c("#0073C2FF", "#00A36C", "#868686FF")

# Plot pairwise comparisons
#width
compare_means(width ~ type,  data = cellSizesTable, method = "kruskal.test")
pwc<-cellSizesTable %>% dunn_test(width ~ type, p.adjust.method = "fdr") 

pwc <- pwc %>% add_xy_position(x = "type")
pwc$p.adj<-round(pwc$p.adj, digits = 3)

widthplot<-ggboxplot(cellSizesTable, x = "type", y = "width",
          color = "type", palette = mypalette, outlier.shape = NA) +  
  geom_jitter(width = 0.2,aes(color = type)) + ylab("Cell width (µm)") + 
  #stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.05) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
  stat_compare_means(label.y= max(cellSizesTable$width)+(max(cellSizesTable$width)/5))    # Add global p-value
widthplot
ggsave("CellWidthPlot.png",widthplot)

#length

compare_means(length ~ type,  data = cellSizesTable, method = "kruskal.test")
pwc<-cellSizesTable %>% dunn_test(length ~ type, p.adjust.method = "fdr") 

pwc <- pwc %>% add_xy_position(x = "type")
pwc$p.adj<-round(pwc$p.adj, digits = 3)

lengthplot<-ggboxplot(cellSizesTable, x = "type", y = "length",
                     color = "type", palette = mypalette,outlier.shape = NA) +  
  geom_jitter(width = 0.2,aes(color = type)) + ylab("Cell length (µm)") + 
 # stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.05) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
  stat_compare_means(label.y= max(cellSizesTable$length)+(max(cellSizesTable$length)/5))    # Add global p-value
lengthplot
ggsave("CelllengthPlot.png",lengthplot)

#area

x$name <- factor(x$name, levels = x$name[order(x$val)])
x$name  # notice the changed order of factor levels

compare_means(area ~ type,  data = cellSizesTable, method = "kruskal.test")
pwc<-cellSizesTable %>% dunn_test(area ~ type, p.adjust.method = "fdr") 

pwc <- pwc %>% add_xy_position(x = "type")
pwc$p.adj<-round(pwc$p.adj, digits = 3)

areaplot<-ggboxplot(cellSizesTable, x = "type", y = "area",
                      color = "type", palette = mypalette, outlier.shape = NA) +  
  geom_jitter(width = 0.2,aes(color = type)) + ylab("Cell area (µm\u00B2)") + 
 # stat_pvalue_manual(pwc, label= "p = {p.adj}",step.increase = 0.09) + # Add pairwise comparisons p-value change height of label by vjust = -0.2
  stat_compare_means(label.y= max(cellSizesTable$area)+(max(cellSizesTable$area)/5))    # Add global p-value
areaplot
ggsave("CellareaPlot.png",areaplot,width = 10, height=16, units = "cm")

#______________________________________
#^ This plot is Supplementary Figure 5
#______________________________________

