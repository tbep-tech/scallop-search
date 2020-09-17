library(tidyverse)
library(sf)
library(readxl)

prj <- 4326

# format excel data -------------------------------------------------------

rawdat <- read_excel('data-raw/GBSS_2020_Results.xlsx', na = '<Null>') %>%
  select(matches('^Site|^Scallop')) %>%
  rename(
    Site1 = `Site Number...2`,
    Site2 = `Site Number...4`,
    Site1_cnt1 = `Scallop Count...10`,
    Site1_cnt2 = `Scallop Count...21`,
    Site1_cnt3 = `Scallop Count...31`,
    Site2_cnt1 = `Scallop Count...41`,
    Site2_cnt2 = `Scallop Count...52`,
    Site2_cnt3 = `Scallop Count...63`
  ) %>%
  mutate(id = 1:nrow(.))

cnts <- rawdat %>%
  select(-Site1, -Site2) %>%
  gather('var', 'val', -id) %>%
  separate(var, c('Site', 'rep'), sep = '_') %>%
  group_by(id, Site) %>%
  summarise(`Scallops found` = sum(val, na.rm = T), .groups = 'drop')

cntdat <- rawdat %>%
  select(Site1, Site2, id) %>%
  gather('Site', 'hex', -id) %>%
  left_join(cnts, by = c('id', 'Site')) %>%
  arrange(id, Site)

save(cntdat, file = 'data/cntdat.RData', compress = 'xz')

# hex data ----------------------------------------------------------------

hex <- st_read('data-raw/HexagonSites_Edited200910_Final.shp') %>%
  st_transform(crs = prj) %>%
  select(Bay_Segment = Bay_Segmen, hex = Site_Numbe)

save(hex, file = 'data/hex.RData', compress = 'xz')
