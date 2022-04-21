#!/bin/bash

# Each cell used has its own area (µm^2) that the distanceMitoChloro.R uses as input. These are taken from the CellSizesQuantify.csv file but are written out here for ease of use. If using own experimental data, insert mitochondrial and chloroplast trajectory files separately and an estimate of cell area, run as: distanceMitoChloro.R [mitotraj.example] [chlorotrajs.example] [cellareainmicrons]

cd chloroColocalisations

./distanceMitoChloro.r GFP1-mitos.xml GFP1-chloros.xml 546.017
./distanceMitoChloro.r GFP2-mitos.xml GFP2-chloros.xml 737.903
./distanceMitoChloro.r GFP3-mitos.xml GFP3-chloros.xml 3224.905
./distanceMitoChloro.r GFP6-mitos.xml GFP6-chloros.xml 1125.252
./distanceMitoChloro.r GFP10-mitos.xml GFP10-chloros.xml 1504.609
./distanceMitoChloro.r GFP11-mitos.xml GFP11-chloros.xml 615.274
./distanceMitoChloro.r GFP12-mitos.xml GFP12-chloros.xml 1113.034
./distanceMitoChloro.r GFP4-mitos.xml GFP4-chloros.xml 1202.689
./distanceMitoChloro.r GFP5-mitos.xml GFP5-chloros.xml 1590.431
./distanceMitoChloro.r GFP8-mitos.xml GFP8-chloros.xml 2697.122

./distanceMitoChloro.r MSH6-mitos.xml MSH6-chloros.xml 1789.144
./distanceMitoChloro.r MSH17-mitos.xml MSH17-chloros.xml 1008.383
./distanceMitoChloro.r MSH19-mitos.xml MSH19-chloros.xml 1364.212
./distanceMitoChloro.r MSH20-mitos.xml MSH20-chloros.xml 807.691
./distanceMitoChloro.r MSH22-mitos.xml MSH22-chloros.xml 1250.798
./distanceMitoChloro.r MSH26-mitos.xml MSH26-chloros.xml 851.658
./distanceMitoChloro.r MSH7-mitos.xml MSH7-chloros.xml 1651.684
./distanceMitoChloro.r MSH13-mitos.xml MSH13-chloros.xml 885.533
./distanceMitoChloro.r MSH25-mitos.xml MSH25-chloros.xml 982.454
./distanceMitoChloro.r MSH10-mitos.xml MSH10-chloros.xml 2983.425

cd ..

./collateEvalues.R


