---
title: "README"
output: html_document
---

# floral_traits
Repository for data and scripts for the tongue and flower MS Reverte-Saiz, Roswell, et al. 

## scripts in archive *not working italicized*, others not tested

CV_simulation.R
*differences between null models.R*
*NOT WORKING null model site-round.R*
quantile regression.R

## scripts in use for MS or exploration
*`null model trait matching.R`* generates histograms of difference between
tongue and flower depth on a per-visit basis, comparing permutation null to
observed by superimposing

*`trait matching test filtered site round`* hard codes each site and round, and
then for each bee spp. determines which flowers it could have visited (I think).
Then it uses those flowers to generate null models for the difference between
tongue length and the corolla depth.

*fourth_corner.R* generates various versions of linear models that use trait
matching as a predictor and interaction frequency/occurence as a response.
Traits don't seem very important.


## Workflow
1. *`1 Data cleaning.R`* combines and cleans data for all analyses
1. *plots.R*  generates MS fig 1 showing entire dataset in terms of tongue
length and corolla depth.
1. *null model trait matching.R* generates a null model that permutes individual
bees within site-round and computes trait matching
1. *threshold test filtered site round but global result.R* takes null model
values and generates figures for proportion of forbidden links
1. *fourth_corner.R* fits various GLM(M)s to test whether tongue-depth^2 and
is.forbidden predict interaction occurence and frequency. #### currently no
outputs from this file!
1. *trait matching test filtered site round.R* uses null model to generate all
of Sara's plots. #### need to spend more time with this now!


## Data Files Produced
 data file | Folder     | Description  | Origin Code |
-----------|---------------|------------------------------------- | --------|
 | | | | 

## Code
File name  |Folder     |  Description   | Output
-----------|---------------|----------------------------- |------|
`traits.R`  | scripts |  assembles interactions and trait data| ##### SHOULD MAKE AN OUTPUT INSTEAD OF CALLING EACH TIME?
`trait matching test filtered site round.R`  | scripts |   |
`threshhold test filtered site round.R`|code/getData |  | 

## notes
20220103 Looks like we calculate all tongue lenghts. This seems fine, make sure
it is really clear in methods though. For IT measurements, should update. Do we
have the most up-to-date data? Can we use a published dataset?
