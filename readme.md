# Reinforced Designs: Multiple Instruments Plus Control Groups as Evidence Factors in an Observational Study of the Effectiveness of Catholic Schools

# Author Contributions Checklist Form

## Data

### Abstract

The paper uses publicly available data from the Wisconsin Longitudinal Study.


### Availability 

The data set is publicly available.  It can be downloaded from https://www.ssc.wisc.edu/wlsresearch/. Data used in the present paper are also included in the R package ‘blockingChallenge’ that accompany this paper.


### Description 

This paper uses public variables from the Wisconsin Longitudinal Study for the analysis of the question of interest.  Detailed description of the variables can be found from https://www.ssc.wisc.edu/wlsresearch/documentation/.  The PDF manual of the ‘blockingChallenge’ package, submitted with this paper, includes summary description of the variables used in the present study.


## Code

### Abstract

There are two sets of codes: (1) for the analysis of the Wisconsin Longitudinal Study data, and (2) for reproducing the simulation results.

### Description

We are submitting an R package named ‘blockingChallenge’.  The replication code for data analysis is given in the manual of this R package, pages 3–6 of the PDF manual.  The code is appropriately documented; it includes details of the additional R packages needed, runtime, and exact seed to replicate the results.  

(2)	For the simulation results we are submitting the required code separately in a zip file, simulationcode.zip.  Please see the ‘readme.txt’ file included in the zip for a guide on running these codes.

### Optional Information 

The codes are written in the statistical software R version 3.4.0 x86_64 (https://www.r-project.org/). All codes were run on a Windows 7 system (32bit) with 16GB RAM and Intel Code i7-3770 3.40GHz processor on a 1TB physical drive. The R package has been tested with R 3.5.1 and R devel.


## Instructions for Use

### Reproducibility 

Example code in the PDF manual of the R package ‘blockingChallenge’ includes code to reproduce Figure 1, Table 1, and Table 4. 

Table 2 and 3 are created using the codes in the zip file ‘simulationcode.zip’; simualtiontable_power.R creates Table 2, simualtiontable_ds.R creates Table 3.  Please see the ‘readme.txt’ file and the documentation included in the respective R codes for workflow detail.
