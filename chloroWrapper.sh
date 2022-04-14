#!/bin/bash

# Each cell used has its own area (µm^2) that the distanceMitoChloro.R uses as input. These are taken from the CellSizesQuantify.csv file but are written out here for ease of use. If using own experimental data, insert mitochondrial and chloroplast trajectory files separately and an estimate of cell area, run as: distanceMitoChloro.R [mitotraj.example] [chlorotrajs.example] [cellareainmicrons]

cd chloroColocalisations

./distanceMitoChloro.R GFP1-mitos.xml GFP1-chloros.xml 546.017
./distanceMitoChloro.R GFP2-mitos.xml GFP2-chloros.xml 737.903
./distanceMitoChloro.R GFP3-mitos.xml GFP3-chloros.xml 3224.905
./distanceMitoChloro.R GFP6-mitos.xml GFP6-chloros.xml 1125.252
./distanceMitoChloro.R GFP10-mitos.xml GFP10-chloros.xml 1504.609
./distanceMitoChloro.R GFP11-mitos.xml GFP11-chloros.xml 615.274
./distanceMitoChloro.R GFP12-mitos.xml GFP12-chloros.xml 1113.034
./distanceMitoChloro.R GFP4-mitos.xml GFP4-chloros.xml 1202.689
./distanceMitoChloro.R GFP5-mitos.xml GFP5-chloros.xml 1590.431
./distanceMitoChloro.R GFP8-mitos.xml GFP8-chloros.xml 2697.122

./distanceMitoChloro.R MSH6-mitos.xml MSH6-chloros.xml 1789.144
./distanceMitoChloro.R MSH17-mitos.xml MSH17-chloros.xml 1008.383
./distanceMitoChloro.R MSH19-mitos.xml MSH19-chloros.xml 1364.212
./distanceMitoChloro.R MSH20-mitos.xml MSH20-chloros.xml 807.691
./distanceMitoChloro.R MSH22-mitos.xml MSH22-chloros.xml 1250.798
./distanceMitoChloro.R MSH26-mitos.xml MSH26-chloros.xml 851.658
./distanceMitoChloro.R MSH7-mitos.xml MSH7-chloros.xml 1651.684
./distanceMitoChloro.R MSH13-mitos.xml MSH13-chloros.xml 885.533
./distanceMitoChloro.R MSH25-mitos.xml MSH25-chloros.xml 982.454
./distanceMitoChloro.R MSH10-mitos.xml MSH10-chloros.xml 2983.425

cd ..

./collateEvalues.R


