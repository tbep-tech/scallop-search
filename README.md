# README

[![build](https://github.com/tbep-tech/scallop-search/workflows/build/badge.svg)](https://github.com/tbep-tech/scallop-search/actions) [![DOI](https://zenodo.org/badge/295835015.svg)](https://zenodo.org/badge/latestdoi/295835015)

Evaluation of Tampa Bay Watch, Great Bay Scallop Search, [link](https://tbep-tech.github.io/scallop-search/)

The data object at `R/cntdat.RData` includes compiled data for all available years. The columns are:

-   *id*: unique crew id to track which boat visited which site, hex in a given year. You make this up, just make sure the numbers are consistent within a year (i.e., it's okay to have one id shared across years because ids are only unique to the year)
-   *Site*: an identifier for transects within a hex for each crew/year combo. If it is unclear how many sites were visited at a hex for a given year, just put Site1 and the total scallop count for the crew/year row entry.
-   *hex*: the hex number where the transects were located
-   *Scallops found*: count of scallops found by a crew in a given year at a specified hex and Site. Enter zero if searched but none were found (there should be no "NA" or blank entries, i.e., a hex is not listed if it was not visited in a given year) year: survey year
