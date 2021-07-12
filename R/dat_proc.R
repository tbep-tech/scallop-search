library(tidyverse)
library(sf)
library(readxl)
library(here)

prj <- 4326

# format excel data -------------------------------------------------------

## 2020 data --------------------------------------------------------------

rawdat <- read_excel(here('data-raw/GBSS_2020_Results.xlsx'), na = '<Null>') %>%
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

cntdat2020 <- rawdat %>%
  select(Site1, Site2, id) %>%
  gather('Site', 'hex', -id) %>%
  left_join(cnts, by = c('id', 'Site')) %>%
  mutate(
    yr = 2020
  ) %>%
  arrange(id, Site)

## all other years --------------------------------------------------------

fl <- here('data-raw/Scallop Seach Data.xlsx')
shts <- excel_sheets(fl)

cntdatother <- NULL

# note that 2012 has data but is incomplete, it's flagged as missing here
for(sht in shts){

  cat(sht, '\n')
  tmp <- read_excel(fl, sheet = sht) %>%
    mutate(
      yr = as.numeric(sht)
    )

  chk <- tmp %>%
    na.omit() %>%
    nrow %>%
    {. == 0}

  if(chk){
    cat('\t missing\n')
    next()
  }

  cntdatother <- bind_rows(cntdatother, tmp)

}

## combine all data -------------------------------------------------------

cntdat <- cntdatother %>%
  select(
    yr,
    id = Team,
    Site,
    hex = Hex,
    `Scallops found` = `Number of Scallops`
  ) %>%
  mutate(
    Site = paste0('Site', Site),
    hex = gsub('^[A-Z,a-z]', '', hex),
    hex = as.numeric(hex)
  ) %>%
  bind_rows(cntdat2020) %>%
  arrange(yr, id)

save(cntdat, file = 'data/cntdat.RData', compress = 'xz')

# hex data ----------------------------------------------------------------

hex <- st_read('data-raw/HexagonSites_Edited200910_Final.shp') %>%
  st_transform(crs = prj) %>%
  select(Bay_Segment = Bay_Segmen, hex = Site_Numbe)

save(hex, file = 'data/hex.RData', compress = 'xz')
