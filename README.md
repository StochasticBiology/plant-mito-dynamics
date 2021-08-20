# Analysis of collective mitochondrial dynamics in plants


Retrive a local copy with

```sh
git clone https://github.com/StochasticBiology/plant-mito-dynamics.git
```

## Requirements

Before running, ensure you have access to:
- R (Version 4.0.1 or later)
  - Required packages:
    - XML
    - igraph
    - brainGraph
    - ggplot2
    - rstatix
    - ggpubr

## Run all trajectory data through analysis script, generate graphs and gather statistics using

```sh
./wrapper.sh
```
n.b. uses threshold distance of 1.6µm

or go through file by file (below)

## Analysis of experimental data

You will find all raw trajectory files from TrackMate export in the subdirectories
`mtgfp-rawtrajectories`
`msh1-rawtrajectories`
`friendly-rawtrajectories`

The script to process trajectories, and export adjacency matrices, trajectories, summary statistics, mitochondrial colocalisation time and mitochondrial speeds is `trajectory-analysis.R`

To run, use:

Rscript `trajectory-analysis.R` [input XML file] [threshold distance in µm] [any frames for which to output graph plots]

For example 

```sh
Rscript trajectory-analysis.R test.xml 1.6 1 50 100
```

Threshold here means the encounter threshold. We used <=1.6µm to count as an encounter between two mitochondria. 

## Graph Plotting

Graph plotting and statistical analysis was done using scripts
 - `msh1MSGraphs.R` For Figure 4, S6, S7
 - `msh1MSGraphs-noFR.R` For Figure 2, 3
 - `CellSizesQuantify.R` For Figure S5, uses `CellSizesQuantify.csv` as input

## Video Data

All video data will be made available in subdirectories 
- `mtgfp-videos`
- `msh1-videos`
- `friendly-videos`
