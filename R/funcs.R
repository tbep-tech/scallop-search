# leaflet basemap for plotting function
bsmap_fun <- function(){

  esri <- rev(grep("^Esri", leaflet::providers, value = TRUE))
  esri <- esri[!grepl('DeLorme', esri)]

  m <- leaflet::leaflet() %>%
    leaflet::setView(-82.6914, 27.68572, zoom = 11)

  for (provider in esri) {
    m <- m %>% leaflet::addProviderTiles(provider, group = provider)
  }

  out <- m %>%
    leaflet::addLayersControl(baseGroups = names(esri),
                              options = leaflet::layersControlOptions(collapsed = T),
                              position = 'topleft')

  return(out)

}

# map results by year
plo_fun <- function(cntdat, yr, hexsf, colpal = NULL){

  if(yr < 2023)
    hexsf <- hexsf[hexsf$yr == 'pre 2023', ]

  if(yr == 2023)
    hexsf <- hexsf[hexsf$yr %in% c('pre 2023', 'added 2023'), ]

  if(yr == 2024) # all, but added for posterity
    hexsf <- hexsf[hexsf$yr %in% c('pre 2023', 'added 2023', 'added 2024'), ]

  # polygons
  tomap <- cntdat %>%
    filter(yr == !!yr) %>%
    dplyr::filter(hex %in% !!hexsf$hex) %>%
    group_by(hex) %>%
    summarise(
      `Scallops found` = sum(`Scallops found`, na.rm = T),
      `Boats searching` = length(unique(id)),
      .groups = 'drop'
    ) %>%
    left_join(hexsf, ., by = 'hex') %>%
    rename(Site = hex) %>%
    mutate(
      lab = case_when(
        is.na(`Scallops found`) ~ paste0('Site ', Site, ', not searched'),
        `Boats searching` == 1 ~ paste0('Site ', Site, ', ', `Scallops found`, ' scallops found, ', `Boats searching`, ' boat searching'),
        T ~ paste0('Site ', Site, ', ', `Scallops found`, ' scallops found, ', `Boats searching`, ' boats searching')
      )
    )

  # text labels
  totxt <- suppressWarnings({
    tomap %>%
      select(`Scallops found`) %>%
      filter(!is.na(`Scallops found`) & `Scallops found` > 0) %>%
      st_centroid()
  })

  if(is.null(colpal))
    colpal <- colorNumeric(palette = c('tomato1', 'lightgreen', '#00806E'), na.color = "#FFFFFF00", domain = range(tomap$`Scallops found`, na.rm = T), alpha = TRUE)

  bsmap <- bsmap_fun()

  out <- bsmap %>%
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
    addLabelOnlyMarkers(data = totxt, label = ~as.character(`Scallops found`),
                        labelOptions = leaflet::labelOptions(noHide = T, textOnly = T, direction = 'center')) %>%
    addLegend("topright", pal = colpal, title = 'Scallops found', values = tomap$`Scallops found`, opacity = 0.6)

  return(out)

}

# scallop count value box
sclbox_fun <- function(cntdat, yr){

  cnts <- cntdat %>%
    filter(yr == !!yr) %>%
    pull(`Scallops found`) %>%
    sum(na.rm = T)

  out <- valueBox(cnts, tags$p(paste('Scallops found in', yr), style = 'font-size: 150%'), color = '#427355', icon = 'fa-search')

  return(out)

}

# boat count value box
btsbox_fun <- function(cntdat, yr){

  cnts <- cntdat %>%
    filter(yr == !!yr) %>%
    pull(id) %>%
    unique %>%
    length

  out <- valueBox(cnts, tags$p(paste('Boats searching in', yr), style = 'font-size: 150%'),  color = '#004F7E', icon = 'fa-ship')

  return(out)

}

# scallops scaled value box
cntbox_fun <- function(cntdat, yr){

  cnts <- cntdat %>%
    filter(yr == !!yr) %>%
    summarise(
      tot = sum(`Scallops found`, na.rm = T),
      bts = length(unique(id)),
      .groups = 'drop'
    ) %>%
    mutate(
      totscl = round(tot / bts, 1)
    ) %>%
    pull(totscl)

  out <- valueBox(cnts, tags$p(paste('Scallops per boat in', yr), style = 'font-size: 150%'), color = '#958984', icon = 'fa-balance-scale')

  return(out)

}

# summary trends bar plot
sumplo_fun <- function(cntdat){

  toplo <- cntdat %>%
    group_by(yr) %>%
    summarise(
      tot = sum(`Scallops found`, na.rm = T),
      bts = length(unique(id)),
      .groups = 'drop'
    ) %>%
    mutate(
      yr = factor(yr),
      totscl = round(tot / bts, 1)
    ) %>%
    complete(yr)

  fntsz <- 15

  p1 <- plot_ly(toplo, x = ~yr, y = ~tot, type = 'bar', color = I('#427355'), text = ~tot,
                hoverinfo = 'y'
  ) %>%
    layout(
      yaxis = list(title = 'Total scallops found', rangemode = 'nonnegative', titlefont = list(size = fntsz), tickfont = list(size = fntsz)),
      xaxis = list(title = '', titlefont = list(size = fntsz), tickfont = list(size = fntsz)),
      showlegend = FALSE
    )

  p2 <- plot_ly(toplo, x = ~yr, y = ~bts, type = 'bar', color = I('#004F7E'), text = ~bts,
                hoverinfo = 'y'
  ) %>%
    layout(
      yaxis = list(title = 'Boats searching', rangemode = 'nonnegative', titlefont = list(size = fntsz), tickfont = list(size = fntsz)),
      xaxis = list(title = '', titlefont = list(size = fntsz), tickfont = list(size = fntsz)),
      showlegend = FALSE
    )

  p3 <- plot_ly(toplo, x = ~yr, y = ~totscl, type = 'bar', color = I('#958984'), text = ~totscl,
                hoverinfo = 'y'
  ) %>%
    layout(
      yaxis = list(title = 'Scallops per boat', rangemode = 'nonnegative', titlefont = list(size = fntsz), tickfont = list(size = fntsz)),
      xaxis = list(title = '', titlefont = list(size = fntsz), tickfont = list(size = fntsz)),
      showlegend = FALSE
    )

  out <- subplot(p1, p2, p3, shareX = TRUE, titleY = TRUE, nrows = 3)

  return(out)

}
