# floral_traits
#what you see below is an example of a nice readme put together by Dan Cariveau. I thought we could copy. 
---
title: "README"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# floral_traits
Repository for data and scripts for the tongue and flower MS Roswell, Reverte-Saiz, et al. 

#scripts in archive *not working italicized*, others not tested

CV_simulation.R
*differences between null models.R*
*NOT WORKING null model site-round.R*
quantile regression.R

#scripts in use for MS or exploration
*`null model trait matching.R`* generates histograms of difference between tongue and flower depth on a per-visit basis, comparing permutation null to observed by superimposing

*`trait matching test filtered site round`* hard codes each site and round, and then for each bee spp. determines which flowers it could have visited (I think). Then it uses those flowers to generate null models for the difference between tongue length and the corolla depth. 

*fourth_corner.R* generates various versions of linear models that use trait matching as a predictor and interaction frequency/occurence as a response. Traits don't seem very important. 


##Workflow
1. *traits.R* combines and cleans data for all analyses
2. *plots.R*  generates MS fig 1 showing entire dataset in terms of tongue length and corolla depth.
3. *null model trait matching.R* generates a null model that permutes individual bees within site-round and computes trait matching
4. *trait matching test filtered site round.R* uses null model to generate all of Sara's plots. #### need to spend more time with this now!
5. *fourth_corner.R* fits various GLM(M)s to test whether tongue-depth^2 and is.forbidden predict interaction occurence and frequency. #### currently no outputs from this file!


### Data Files Produced
 data file | Folder     | Description  | Origin Code |
-----------|---------------|------------------------------------- | --------
allbees.csv | data/fromR   | all bees from regional db joined into 1 table and filtered based on names, locations, phenology | getRare.R
bees_designated_cig.csv | data/fromR | list of ~158 CIG species and rarity designation | designate_Rare.R
 bees_designated_all.csv | data/fromR | list of ~380  species and rarity | designate_Rare.R
cig_analysis_quart.csv |  data/fromR  | final datasheet for analysis of quartiles | cig_aggregate.R
cig_analysis_medianrare.csv |  data/fromR  | final datasheet for median across categories | cig_aggregate.R
cig_analysis_l.csv |  data/fromR  | final datasheet for analysis of lecticity| cig_aggregate.R
traits.csv |  data/fromR| direct from Cariveau Lab SQL | getRare.R
specimens.csv | data/fromR | list of specimens in CIG. Needs to be upated. | getRare.R
specimens_designated.csv | data/fromR | each specimen with associated rarity deisgnation |designate_Rare.R
ranges.csv | data/fromR | range maps of bees from AMNH databases | range_estimates.R
exotic_bees.csv | data/other  | list of exotic species. Cannot remember where from | 
lecticity.csv | data/other  | species by lecticity. Partly from database, partly from Fowler. Need to update in traits | 




.

### Code
File name  |Folder     |  Description   | Output
-----------|---------------|----------------------------- |------|
traits.R    | scripts |  assembles interactions and trait data| ##### SHOULD MAKE AN OUTPUT INSTEAD OF CALLING EACH TIME?
`trait matching test filtered site round`  | main |  passwords for Cariveau and Winfree lab DB |
designate_rare.R |code/getData |  takes traits and allbees (from all database),  assigns rarity and merges with CIG dataset | bees_designated_cig.csv, bees_designated_all.csv, specimens_designated.csv
cig_aggregate | code/getData | aggregates cig data based on quartiles, median rarity and lecticity| cig_analysis_medianrare.csv, cig_analysis_quarts.csv, cig_analysis_lecticity.csv
range_estimates.R | getData | gets range maps from AMNH database from Nacho/Tina | ranges.csv
