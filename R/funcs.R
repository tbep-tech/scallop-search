# map results by year
plo_fun <- function(cntdat, yr, colpal = NULL){

  tomap <- cntdat %>%
    filter(yr == !!yr) %>%
    dplyr::filter(hex %in% !!hex$hex) %>%
    group_by(hex) %>%
    summarise(
      `Scallops found` = sum(`Scallops found`, na.rm = T),
      `Boats searching` = length(unique(id)),
      .groups = 'drop'
    ) %>%
    left_join(hex, ., by = 'hex') %>%
    rename(Site = hex) %>%
    mutate(
      lab = case_when(
        is.na(`Scallops found`) ~ paste0('Site ', Site, ', not searched'),
        `Boats searching` == 1 ~ paste0('Site ', Site, ', ', `Scallops found`, ' scallops found, ', `Boats searching`, ' boat searching'),
        T ~ paste0('Site ', Site, ', ', `Scallops found`, ' scallops found, ', `Boats searching`, ' boats searching')
      )
    )

  if(is.null(colpal))
    colpal <- colorNumeric(palette = c('tomato1', 'lightgreen', '#00806E'), na.color = "#FFFFFF00", domain = range(tomap$`Scallops found`, na.rm = T), alpha = TRUE)

  out <- mapview(tomap, legend = F, fill = NA, homebutton = F) %>%
    .@map %>%
    clearShapes() %>%
    addPolygons(
      data = tomap,
      stroke = T,
      color = 'black',
      weight = 1,
      fillColor = ~colpal(`Scallops found`),
      fillOpacity = 0.4,
      opacity = 1,
      label = ~lab
    ) %>%
    addLegend("topright", pal = colpal, title = 'Scallops found', values = tomap$`Scallops found`, opacity = 0.6)

  return(out)

}

# scallop count value box
sclbox_fun <- function(cntdat, yr){

  cnts <- cntdat %>%
    filter(yr == !!yr) %>%
    pull(`Scallops found`) %>%
    sum(na.rm = T)

  out <- valueBox(cnts, 'Scallops found', color = col, icon = 'fa-search')

  return(out)

}

# boat count value box
btsbox_fun <- function(cntdat, yr){

  cnts <- cntdat %>%
    filter(yr == !!yr) %>%
    pull(id) %>%
    unique %>%
    length

  out <- valueBox(cnts, 'Boats searching', color = col, icon = 'fa-ship')

  return(out)

}

# locations searched value box
lcsbox_fun <- function(cntdat, yr){

  cnts <- cntdat %>%
    filter(yr == !!yr) %>%
    pull(hex) %>%
    unique %>%
    length

  out <- valueBox(cnts, 'Locations searched', color = col, icon = 'fa-map')

  return(out)

}
