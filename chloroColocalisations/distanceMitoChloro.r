#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#Mitochondrial trajectories
mitoTrajsName<-args[1]
#Chlroplast Trajectories
chloroTrajsName<-args[2]
#Cell Area input
A<-as.numeric(args[3])

# we need two input XML files, one for chloroplast trajectories, the other for mitochondrial trajectories.
if (length(args) < 3) {
  stop("Please provide two input files, the first one for mitochondrial trajectories, the second for chloroplast trajectories, as well as a cell area estimate, in microns^2 ", call.=FALSE)
}

#Need to parse the xml files to make workable so we borrow the start of the script from the new pipeline, that Iain wrote:

message("Loading libraries...")
library(XML)

#for this chloroplast analysis, chloro trajs that has 3x 120 frame trajs weren;t running through, so 
#I addes list() to the start of each tmp1 command, to flatten what otherwise are made into data frames for the 3x120 columns vs 2x120 and a 49 for example.
processXML<-function(filename){
  # read XML to list
  message("Reading and parsing XML...")
  traj.list = xmlToList(filename)
  
  # initialise dataframe for storing trajectories
  df = data.frame(traj=NULL, t=NULL, x=NULL, y=NULL)
  
  # loop through list, extracting coordinates and times and binding to growing dataframe
  message("Extracting trajectories...")
  # horrible code to extract trajectories from XML output
  # issues here include that XML output is character format, has the same label for every row ($particle$detection) and comes in a very nested list form
  # we use one nested sapply to grab the coordinate of interest from each entry, chop off the extraneous metadata, then another sapply to convert to numeric values
  tmp = sapply(traj.list, function(v) sapply(v, function(w) w[1]))
  tmp = tmp[-length(tmp)]
  tmp1 = list(sapply(tmp, function(v) as.numeric(v[-length(v)])))
  ts = unlist(tmp1)
  
  tmp = sapply(traj.list, function(v) sapply(v, function(w) w[2]))
  tmp = tmp[-length(tmp)]
  tmp1 = list(sapply(tmp, function(v) as.numeric(v[-length(v)])))
  xs = unlist(tmp1)
  
  tmp = sapply(traj.list, function(v) sapply(v, function(w) w[3]))
  tmp = tmp[-length(tmp)]
  tmp1 = list(sapply(tmp, function(v) as.numeric(v[-length(v)])))
  ys = unlist(tmp1)
  
  lengths = sapply(traj.list, function(v) length(v)-1)
  lengths = lengths[-length(lengths)]
  labels = rep(1:length(lengths), times = lengths)
  
  df = data.frame(traj=labels, t=ts+1, x=xs, y=ys)
  
  # output the trajectories in simple format
  output.filename = paste(filename, "-trajs.csv", sep="")
  write.csv(df, output.filename, row.names=FALSE)
  return(df)
}

mitoTrajs<-processXML(mitoTrajsName)
chloroTrajs<-processXML(chloroTrajsName)

#test env.
#setwd("~/Documents/ChloroplastColocalisations/msh1manuscriptColocalisationExperiments")
#mitoTrajs<-read.csv("goodvid120ADJUSTEDCROPPED_Tracks-1-5-4-5-2-F5_77.xml-trajs.csv")
#chloroTrajs<-read.csv("C1-goodvid120ADJUSTEDCROPPED_Tracks-Chloro_2-5_2_3_3_2.xml-trajs.csv")
#mitoTrajsName<-"goodvid120ADJUSTEDCROPPED_Tracks-1-5-4-5-2-F5_77.xml-trajs.csv"
#A<-1202.689



#typical chloroplast radius in microns
typicalChloroplastRadius<-1.5
#Estimate of available area of cell A_c within distance d of the centre of a chloroplast
#(note, if some chloroplasts are up against the edge of the cell, this will be less that just a disc around the chloroplast -- but we can probably just use that disc at first)
d<-2*typicalChloroplastRadius
A_c<-pi*d^2
#for plotting
coordMax<-ceiling(max(max(mitoTrajs$x),max(mitoTrajs$y)))


calculateN_c <-  function(t) {
  N_c <- 0
  tallylist <- c()
  store <- c()
  #print(c)
  keepcoord <- c()
  tally <- 0
  if (t %in% chloroTrajs$t) {
    minidf <- chloroTrajs[chloroTrajs$t == t,]
    findsperframe <- 0
    #go through these chloroplasts
    for (c in 1:nrow(minidf)) {
      #print(minidf[c,])
      #find its coordinate at that frame
      C_X <- minidf[c, 3]
      C_Y <- minidf[c, 4]
      #print(c(C_X, C_Y))
      #find the mitos in that frame
      mitoListAtFramet <-
        mitoTrajs[mitoTrajs$t == t, ]
      
      for (m in 1:nrow(mitoListAtFramet)) {
        #find mito coordinates at that frame
        M_X <- mitoListAtFramet[m, 3]
        M_Y <- mitoListAtFramet[m, 4]
        
        #Calculate euclidean distance
        D <- sqrt((C_X - M_X) ^ 2 + (C_Y - M_Y) ^ 2)
        
        #is this distance less that our threshold?
        if (D <= d) {
          N_c <- N_c + 1
          #print(c(C_X,C_Y))
          keepcoord <- rbind(keepcoord, c(M_X, M_Y))
          store <- c(store, D)
          tally <- tally + 1
          findsperframe <- findsperframe + 1
        }
      }
    }
    #~~~~
    #uncomment if you want proof of cololcalisation distance by plotting points on a simple graph.
    #png(paste("", mitoTrajsName, "frame", t, ".png", sep = ""))
    plot(
      mitoTrajs[mitoTrajs$t == t,][, 3],
      mitoTrajs[mitoTrajs$t == t,][, 4],
      xlim = c(0, coordMax),
      ylim = c(0, coordMax),
      col = "green",
      pch = 3,
      main = paste("found coords n=", findsperframe, ",frame=", t, sep =
                     "")
    )
    points(chloroTrajs[chloroTrajs$t == t, ][3:4], col = "red")
    points(keepcoord, col = "blue")
    #dev.off()
    #~~~~
    
    tallylist <- c(tallylist, tally)
  } else {
    #This catch is put in just incase there is a gap (in t) in tracking the chloroplast
    return(NA)
  }
  N_c <- tallylist
  return(N_c)
}


N_ctotal <- 0
Elist <- c()
for (t in 1:max(mitoTrajs$t)) {
#for (t in 105) {
  #print(t)
  #N = number of mitos in each frame
  N <- nrow(mitoTrajs[mitoTrajs$t == t,])
  #Go through all chloros in their specific frames (or else it won't be colocalisation to an actual chloro), find any mitos within distance d of them.
  N_c <- calculateN_c(t)
  #N_ctotal should == calculateN_cOld() outcome
  N_ctotal <- N_ctotal + N_c
  #Enrichment calculation (the ratio of mitochondrial density in chloroplast-adjacent regions to the density in the non-adjacent regions.)
  E = (N_c / A_c) / ((N - N_c) / (A - A_c))
  Elist <- c(Elist, E)
}

png(paste("meanEnrichment", mitoTrajsName,".png", sep = ""))
plot(c(1:max(mitoTrajs$t)),
     Elist,
     main = paste("Mean Enrichment value =",
                  round(mean(na.omit(Elist)), 3),
                  "SD=",
                  round(sd(na.omit(Elist)), 3)))
dev.off()

write(round(mean(na.omit(Elist)), 3),paste("meanEnrichment", mitoTrajsName,".csv",sep=""))

