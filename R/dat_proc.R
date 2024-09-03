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

# hex data ----------------------------------------------------------------

hex <- st_read(here('data-raw/2024_GBSS_Hex_Update.shp')) %>%
  st_transform(crs = prj) %>%
  select(Bay_Segment = Bay_Segmen, hex = Site_Numbe) %>%
  mutate(
    yr = case_when(
      hex < 301 ~ 'pre 2023',
      hex %in% c(301:340) ~ 'added 2023',
      hex %in% c(401:433) ~ 'added 2024'
    ),
    dups = ifelse(duplicated(hex) | duplicated(hex, fromLast = TRUE), T, F)
  )

save(hex, file = 'data/hex.RData', compress = 'xz')

# format google drive data --------------------------------------------------------------------

# 2024 data -----------------------------------------------------------------------------------

id <- fls[grep('Scallop_Search_2024', fls$name), 'id'] %>% pull(id)

rawdat <- read_sheet(id)

# hex as zero means corrected hex was outside of boundaries, entries retained for total boat count
cntdat2024 <- rawdat %>%
  filter(!is.na(`OBJECTID`)) %>%
  select(
    id = `Boat Captain`,
    hexorig = `Hexagon Number from Submitted Survey`,
    hex = `Updated Hex # Post Review`,
    Bay_Segment = BaySegment,
    `Scallops found` = `Scallops`
  ) %>%
  mutate(
    id = as.numeric(factor(id)),
    hex = case_when(
      hex == 0 & id == 21 ~ 83, # these were corrected hex numbers that were outside, but contained scalloped, used original hex
      hex == 0 & id == 11 ~ 56,
      T ~ hex
    )
  ) %>%
  select(-hexorig) %>%
  mutate(
    Site = 1:n(),
    .by = id
  ) %>%
  mutate(
    yr = 2024,
    Bay_Segment = as.character(factor(Bay_Segment,
      levels = c('Lower Tampa Bay', 'Boca Ciega Bay', 'Middle Tampa Bay'),
      labels = c('LTB', 'BCB', 'MTB')
    )),
    Site = paste0('Site', Site)
  ) %>%
  select(yr, id, Site, hex, Bay_Segment, `Scallops found`) %>%
  arrange(id, Site)

# 2023 data -----------------------------------------------------------------------------------

id <- fls[grep('Scallop_Search_2023', fls$name), 'id'] %>% pull(id)

rawdat <- read_sheet(id)

cntdat2023 <- rawdat %>%
  select(
    id = `Boat Captain`,
    Site = `Transect Number`,
    hex = `Hexagon Site Number`,
    `Bay Segment...11`,
    `Bay Segment...12`,
    `Scallops found` = `Scallop Count`,
    Notes
  ) %>%
  filter(!is.na(id)) %>%
  filter(!grepl('^Test', Notes)) %>%
  mutate(
    id = as.numeric(factor(id)),
    Site = case_when(
      Site == 'First' ~ 'Site1',
      Site == 'Second' ~ 'Site2',
      Site == 'Third - optional' ~ 'Site3'
    ),
    `Bay Segment...12` = ifelse(`Bay Segment...12` == 'Middle Tampa Bay', 'mtb', `Bay Segment...12`),
    Bay_Segment = ifelse(is.na(`Bay Segment...11`), `Bay Segment...12`, `Bay Segment...11`),
    Bay_Segment = as.character(factor(Bay_Segment,
      levels = c('ltb', 'bcb', 'mtb'),
      labels = c('LTB', 'BCB', 'MTB')
    )),
    acthex = gsub(".*Hex (\\d+$)", "\\1", Notes),
    acthex = as.numeric(gsub('[^0-9]', '', acthex)),
    hex = ifelse(is.na(acthex), hex, acthex)
  ) %>%
  group_by(id, Site, Bay_Segment, hex) %>%
  summarise(`Scallops found` = sum(`Scallops found`), .groups = 'drop') %>%
  mutate(
    yr = 2023
  ) %>%
  select(yr, everything()) %>%
  arrange(id, Site) %>%
  mutate(
    Site2 = 1,
    Site2 = cumsum(Site2), # there are some where Site is duplicated if hex corrected
    Site2 = paste0('Site', Site2),
    .by = c('id', 'hex')
  ) %>%
  select(yr, id, Site = Site2, hex, Bay_Segment, `Scallops found`)

# 2022 data -----------------------------------------------------------------------------------

id <- fls[grep('Scallop_Search_2022', fls$name), 'id'] %>% pull(id)

rawdat <- read_sheet(id)

cntdat2022 <- rawdat %>%
  select(
    id = `Captain #`,
    Site = `Site #`,
    transect = `Transect #`,
    hex = `Hexagon Site Number`,
    Bay_Segment = `Bay Segment`,
    `Scallops found` = `Scallop Count`
    ) %>%
  fill(id) %>%
  group_by(id, Site, hex, Bay_Segment) %>%
  summarise(`Scallops found` = sum(`Scallops found`), .groups = 'drop') %>%
  mutate(
    yr = 2022,
    Site = ifelse(!is.na(Site), paste0('Site', Site), Site),
    Bay_Segment = as.character(factor(Bay_Segment,
      levels = c('ltb', 'bcb', 'mtb'),
      labels = c('LTB', 'BCB', 'MTB')
    ))
    ) %>%
  select(yr, everything()) %>%
  arrange(id, Site)

## 2020 data --------------------------------------------------------------

# sheet id
id <- fls[grep('Scallop_Search_2020', fls$name), 'id'] %>% pull(id)

rawdat <- read_sheet(id, na = '<Null>') %>%
  select(matches('^Bay\\sSegment|^Site|^Scallop')) %>%
  rename(
    Bay_Segment1 = `Bay Segment (Site 1)`,
    Bay_Segment2 = `Bay Segment (Site 2)`,
    Site1 = `Site Number...2`,
    Site2 = `Site Number...4`,
    Site1_cnt1 = `Scallop Count...10`,
    Site1_cnt2 = `Scallop Count...21`,
    Site1_cnt3 = `Scallop Count...31`,
    Site2_cnt1 = `Scallop Count...41`,
    Site2_cnt2 = `Scallop Count...52`,
    Site2_cnt3 = `Scallop Count...63`
  ) %>%
  mutate(
    id = 1:nrow(.),
    Bay_Segment1 = as.character(factor(Bay_Segment1,
      levels = c('Lower Tampa Bay', 'Boca Ciega Bay', 'Middle Tampa Bay'),
      labels = c('LTB', 'BCB', 'MTB')
    )),
    Bay_Segment2 = as.character(factor(Bay_Segment2,
      levels = c('Lower Tampa Bay', 'Boca Ciega Bay', 'Middle Tampa Bay'),
      labels = c('LTB', 'BCB', 'MTB')
    ))
  ) %>%
  unite('Site1', Bay_Segment1, Site1) %>%
  unite('Site2', Bay_Segment2, Site2)

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
  separate(hex, c('Bay_Segment', 'hex'), sep = '_') %>%
  filter(!hex == 'NA') %>%
  mutate(
    hex = as.numeric(hex)
  ) %>%
  select(yr, id, Site, hex, Bay_Segment, `Scallops found`) %>%
  arrange(id, Site)

# data up to 2019 -----------------------------------------------------------------------------

# sheet id
id <- fls[grep('Scallop_Search_2019_and_prior', fls$name), 'id'] %>% pull(id)

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

cntdatother <- cntdatother %>%
  mutate(
    Site = paste0('Site', Site),
    Bay_Segment = gsub('[0-9]+$', '', Hex),
    Bay_Segment = as.character(factor(Bay_Segment,
                                      levels = c('L', 'B', 'M'),
                                      labels = c('LTB', 'BCB', 'MTB')
    )),
    Hex = gsub('^[A-Z,a-z]', '', Hex),
    Hex = as.numeric(Hex)
  ) %>%
  select(
    yr,
    id = Team,
    Site,
    hex = Hex,
    Bay_Segment,
    `Scallops found` = `Number of Scallops`
  )

## combine all data -------------------------------------------------------

load(file = here("data/hex.RData"))
duphex <- hex[hex$dup, ] %>%
  st_drop_geometry() %>%
  pull(hex) %>%
  unique()

cntdat <- cntdatother %>%
  bind_rows(cntdat2020) %>%
  bind_rows(cntdat2022) %>%
  bind_rows(cntdat2023) %>%
  bind_rows(cntdat2024) %>%
  mutate(
    dups = hex %in% duphex
  ) %>%
  arrange(yr, id)

save(cntdat, file = 'data/cntdat.RData', compress = 'xz')

# check correct dups --------------------------------------------------------------------------

# check if any bay segment, hex duplicates in hex are not found in cntdat
load(file = here('data/cntdat.RData'))
load(file = here('data/hex.RData'))

hexdup <- hex[hex$dup, ] %>%
  st_drop_geometry() %>%
  select(Bay_Segment, hex) %>%
  distinct() %>%
  unite('hex', Bay_Segment, hex) %>%
  pull(hex)

cntdatdup <- cntdat %>%
  filter(dups) %>%
  select(Bay_Segment, hex) %>%
  distinct() %>%
  unite('hex', Bay_Segment, hex) %>%
  pull(hex)

# this should be F, otherwise find the duplicated hex in cntdat that has the wrong bay segment
# plo_fun joins cntdat to hex by hex number, which is okay unless hex numbers are duplicated across bay segments
# if a hex is duplicated, cntdat is joined by hex using both hex and bay segment
# sometimes the bay segment name from the raw data is wrong
# so we have to make sure all the duplicated hex numbers have correct bay segments
any(!cntdatdup %in% hexdup)

