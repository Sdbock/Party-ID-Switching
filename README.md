# Visualizing-Partisan-Identification-Switching-in-the-General-Social-Survey-Panel-2016-2020
This is the replication package to the article "Visualizing Partisan Identification Switching in the General Social Survey Panel, 2016-2020" published in Socius (2021). It includes all data files and R scripts needed to recreate the visualizations in the main text, as well as the appendix. 

# How to run files
The replication package is organized as an RStudio project. As such, be sure to keep all files within the same folder. After downloading the repo folder (either "pull the repo" or download as a ZIP file), first open the RStudio project file. From there, the "cleaning_data.R" script—within the "scripts" folder—should be run first. This loads, cleans, and organizes the data in order to analyze. Next, the "generating_figures.R" script loads the cleaned data generated from the "cleaning_data.R" script, and generates two figures that will be saved in the "figures" folder. 
