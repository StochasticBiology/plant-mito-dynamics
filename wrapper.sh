
#This script will look within all subdirectories for any .xml files, and run them through the trajectory-analysis.R script.
find . -name "*.xml" -print0 | while read -d $'\0' file
do  
    Rscript trajectory-analysis.R "$file" 1.6 10 50 100
done

#Once you have all output files from all trajectory files, run the r scripts for statistics and graphs

Rscript msh1MSGraphs.R

#If you'd like some of the graphs without friendly plotted, use 

Rscript msh1MSGraphs-noFR.R

#If you'd like the cell size (length/width/area) plots and statistics, use

Rscript cellSizesQuantify.R