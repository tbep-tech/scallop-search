library(tidyverse)
library(sf)
library(here)
library(googlesheets4)
library(googledrive)

gs4_deauth()
drive_deauth()

prj <- 4326

# google drive path
gdrive_pth <- 'https://drive.google.com/drive/u/0/folders/1Z2DjoS0IJjudW5C8rpNCXvSIVVTvJdej'

# google sheets in the drive
fls <- drive_ls(gdrive_pth, type = 'spreadsheet')

# format google drive data --------------------------------------------------------------------

# 2022 data -----------------------------------------------------------------------------------

id <- fls[grep('Scallop_Search_2022', fls$name), 'id'] %>% pull(id)

rawdat <- read_sheet(id)

cntdat2022 <- rawdat %>%
  select(
    id = `Captain #`,
    Site = `Site #`,
    transect = `Transect #`,
    hex = `Hexagon Site Number`,
    `Scallops found` = `Scallop Count`
    ) %>%
  fill(id) %>%
  group_by(id, Site, hex) %>%
  summarise(`Scallops found` = sum(`Scallops found`), .groups = 'drop') %>%
  mutate(
    yr = 2022,
    Site = ifelse(!is.na(Site), paste0('Site', Site), Site)
    ) %>%
  select(yr, everything()) %>%
  arrange(id, Site)

## 2020 data --------------------------------------------------------------

# sheet id
id <- fls[grep('Scallop_Search_2020', fls$name), 'id'] %>% pull(id)

rawdat <- read_sheet(id, na = '<Null>') %>%
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

# data up to 2019 -----------------------------------------------------------------------------

# sheet id
id <- fls[grep('Scallop_Search_to_2019', fls$name), 'id'] %>% pull(id)

# tabs in sheet
shts <- sheet_properties(id) %>%
  pull(name)

cntdatother <- NULL

# note that 2012 has data but is incomplete, it's flagged as missing here
for(sht in shts){

  cat(sht, '\n')
  tmp <- read_sheet(id, sheet = sht) %>%
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
  bind_rows(cntdat2022) %>%
  arrange(yr, id)

save(cntdat, file = 'data/cntdat.RData', compress = 'xz')

# hex data ----------------------------------------------------------------

hex <- st_read('data-raw/HexagonSites_Edited200910_Final.shp') %>%
  st_transform(crs = prj) %>%
  select(Bay_Segment = Bay_Segmen, hex = Site_Numbe)

save(hex, file = 'data/hex.RData', compress = 'xz')
