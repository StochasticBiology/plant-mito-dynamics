# Analysis of collective mitochondrial dynamics in plants

Note: This code base is a little complicated and contains some Mathematica code. For a streamlined pipeline for the extraction and analysis of these networks without proprietary software, consider https://github.com/StochasticBiology/mito-network-sharing .

## Download

Retrieve a local copy with

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

## General analysis

The workhorse code takes XML output of tracked mitochondrial trajectories from TrackMate [1], and computes and outputs physical and network statistics of collective mitochondrial motion. This code is `trajectory-analysis.R`. For a detailed description of the analysis approach, see Chustecki et al. [2].

To run, use:

Rscript `trajectory-analysis.R` [input XML file] [threshold distance in µm] [any specific frame numbers for which to output graph plots]

For example 

```sh
Rscript trajectory-analysis.R test.xml 1.6 1 50 100
```

would analyse `test.xml` using a 1.6µm distance as a threshold for encounters, and output specific plots for frames 1, 50, and 100.

## Wrapper script for msh1 project

Run all trajectory data through analysis script, generate graphs and gather statistics using the bash script

```sh
./wrapper.sh
```
(n.b. uses threshold distance of 1.6µm)

or go through file by file (below)

## Experimental data for msh1 project

You will find all raw trajectory files from TrackMate export in the subdirectories
`mtgfp-rawtrajectories`
`msh1-rawtrajectories`
`friendly-rawtrajectories`

## Graph plotting for msh1 project

Graph plotting and statistical analysis was done using scripts
 - `msh1MSGraphs.R` For Figure 4, S6, S7
 - `msh1MSGraphs-noFR.R` For Figure 2, 3
 - `CellSizesQuantify.R` For Figure S5, uses `cellSizes/CellSizesQuantify.csv` as input

## Video data for msh1 project

All video data is available in subdirectories 
- `mtgfp-videos`
- `msh1-videos`
- `friendly-videos`

## References

[1] https://imagej.net/plugins/trackmate/
[2] https://www.sciencedirect.com/science/article/pii/S2405471221001332
