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
    addLegend("topright", pal = colpal, title = 'Scallops found', values = tomap$`Scallops found`, opacity = 0.6) %>%
    setView(-82.6914, 27.68572, zoom = 12)

  return(out)

}

# scallop count value box
sclbox_fun <- function(cntdat, yr){

  cnts <- cntdat %>%
    filter(yr == !!yr) %>%
    pull(`Scallops found`) %>%
    sum(na.rm = T)

  out <- valueBox(cnts, paste('Scallops found in', yr), color = '#427355', icon = 'fa-search')

  return(out)

}

# boat count value box
btsbox_fun <- function(cntdat, yr){

  cnts <- cntdat %>%
    filter(yr == !!yr) %>%
    pull(id) %>%
    unique %>%
    length

  out <- valueBox(cnts, paste('Boats searching in', yr), color = '#004F7E', icon = 'fa-ship')

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

  out <- valueBox(cnts, paste('Scallops per boat in', yr), color = '#958984', icon = 'fa-balance-scale')

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
      yr = factor(yr, levels = seq(min(yr), max(yr))),
      totscl = round(tot / bts, 1)
    ) %>%
    complete(yr)

  p1 <- plot_ly(toplo, x = ~yr, y = ~tot, type = 'bar', color = I('#427355'), text = ~tot,
                hoverinfo = 'y'
  ) %>%
    layout(
      yaxis = list(title = 'Total scallops found', rangemode = 'nonnegative'),
      xaxis = list(title = ''),
      showlegend = FALSE
    )

  p2 <- plot_ly(toplo, x = ~yr, y = ~bts, type = 'bar', color = I('#004F7E'), text = ~bts,
                hoverinfo = 'y'
  ) %>%
    layout(
      yaxis = list(title = 'Boats searching', rangemode = 'nonnegative'),
      xaxis = list(title = ''),
      showlegend = FALSE
    )

  p3 <- plot_ly(toplo, x = ~yr, y = ~totscl, type = 'bar', color = I('#958984'), text = ~totscl,
                hoverinfo = 'y'
  ) %>%
    layout(
      yaxis = list(title = 'Scallops per boat', rangemode = 'nonnegative'),
      xaxis = list(title = ''),
      showlegend = FALSE
    )

  out <- subplot(p1, p2, p3, shareX = TRUE, titleY = TRUE, nrows = 3)

  return(out)

}
