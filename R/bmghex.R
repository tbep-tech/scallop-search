library(sf)
library(leaflet)
library(mapedit)
library(here)
library(dplyr)
library(ggplot2)
library(ggspatial)

data(tbseg, package = 'tbeptools')

ex <- st_read(here('data-raw', '2024_GBSS_Hex_Update.shp'))

# draw polygon for hex boundaries ----------------------------------------

# m <- leaflet(st_transform(ex, 4326)) |>
#   addProviderTiles('USGS.USImageryTopo') |>
#   addPolygons(weight = 0.5) 

# egspoly <- drawFeatures(m)

# egspoly <- st_make_valid(egspoly) |> 
#   st_transform(crs = 6443)

# save(egspoly, file = here('data-raw', 'egspoly.RData'))

# create hexes -----------------------------------------------------------

load(file = here('data-raw', 'egspoly.RData'))

# match hex size/angle to the existing grid in data/hex.RData: hexagons there
# have a center-to-vertex radius of ~2482 ft (cellsize = radius * sqrt(3))
# and are rotated ~10.5 degrees counter-clockwise from st_make_grid's default
# orientation (default puts a vertex due north)
cellsize <- 4299
rot_ang <- 10.5

rotate_geom <- function(geom, ang_deg, ctr) {
  a <- -ang_deg * pi / 180
  rotmat <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  st_set_crs((geom - ctr) * rotmat + ctr, st_crs(geom))
}

ctr <- st_centroid(st_union(egspoly))

egspoly_rot <- st_set_geometry(egspoly, rotate_geom(st_geometry(egspoly), -rot_ang, ctr))

egshex <- st_make_grid(egspoly_rot, cellsize = cellsize, what = "polygons", square = F) |>
  st_make_valid()
egshex <- st_as_sf(egshex[egspoly_rot, ])
egshex <- st_set_geometry(egshex, rotate_geom(st_geometry(egshex), rot_ang, ctr))
egshex$id <- seq_len(nrow(egshex))

m <- m |>
  addPolygons(data = st_transform(egspoly, 4326), weight = 2, color = 'blue') |>
  addPolygons(data = st_transform(egshex, 4326), weight = 0.5, color = 'red')

egscent <- st_sf(id = egshex$id, geometry = st_centroid(st_geometry(egshex)))

tbsegtrn <- st_transform(tbseg, crs = 6443)
egscent <- egscent[tbsegtrn, ]

egshex <- egshex[egshex$id %in% egscent$id, ]
egshex$id <- seq_len(nrow(egshex))

m <- leaflet(st_transform(ex, 4326)) |>
  addProviderTiles('USGS.USImageryTopo') |>
  addPolygons(weight = 0.5) |> 
  addPolygons(data = st_transform(tbseg, 4326), weight = 2, color = 'blue') |>
  # addPolygons(data = st_transform(egspoly, 4326), weight = 2, color = 'blue') |>
  addPolygons(data = st_transform(egshex, 4326), weight = 0.5, color = 'red')

save(egshex, file = here('data', 'egshex.RData'))

# select big bayou hexes -------------------------------------------------

m <- leaflet(st_transform(ex, 4326)) |>
    addProviderTiles('USGS.USImageryTopo') |>
    addPolygons(weight = 0.5)
exsel <- selectFeatures(ex, map = m)

bbhex <- exsel

save(bbhex, file = here('data', 'bbhex.RData'))

# combine egs and bb hexes -----------------------------------------------

load(file = here('data', 'egshex.RData'))
load(file = here('data', 'bbhex.RData'))

egshex <- st_transform(egshex, 4326) |> 
  select(geometry = x) |> 
  mutate(
    site = 'EG Simmons'
  )

bbhex <- st_transform(bbhex, 4326) |> 
  select(geometry) |> 
  mutate(
    site = 'Big Bayou'
  )

bmghex <- bind_rows(egshex, bbhex) |> 
  mutate(
    id = 1:n(), 
    lat = st_coordinates(st_centroid(geometry))[, 2],
    lon = st_coordinates(st_centroid(geometry))[, 1]
  )

save(bmghex, file = here('data', 'bmghex.RData'))

# save as csv ------------------------------------------------------------

tosv <- bmghex |> 
  st_set_geometry(NULL) |> 
  select(id, site, lat, lon)

write.csv(tosv, file = here('data-raw', 'bmghex.csv'), row.names = FALSE)

#  map for eg simmons ----------------------------------------------------

tomap <- bmghex |> 
  filter(site == 'EG Simmons')

m1 <- ggplot() +
  ggspatial::annotation_map_tile(
    zoom = 13,
    type = 'cartolight',
    cachedir = system.file("rosm.cache", package = "ggspatial")
  ) +
  annotation_north_arrow(
    location = 'tl',
    style = north_arrow_orienteering(fill = c('black', 'black'), text_col = NA),
    height = unit(0.5, "cm"),
    width = unit(0.5, "cm")
  ) +
  annotation_scale(location = 'br', text_cex = 1) +
  geom_sf(data = tomap, color = 'black', inherit.aes = F, alpha = 0) +
  geom_sf_text(
    data = st_centroid(tomap),
    aes(label = id),
    size = 5,
    color = 'black',
    inherit.aes = F
  ) +
  labs(
    x = NULL, 
    y = NULL, 
    title = 'EG Simmons'
  )

png(here('figs', 'egshex.png'), width = 6, height = 6, units = 'in', res = 300)
print(m1)
dev.off()

# map for big bayou ------------------------------------------------------

tomap <- bmghex |> 
  filter(site == 'Big Bayou')

m2 <- ggplot() +
  ggspatial::annotation_map_tile(
    zoom = 13,
    type = 'cartolight',
    cachedir = system.file("rosm.cache", package = "ggspatial")
  ) +
  annotation_north_arrow(
    location = 'tl',
    style = north_arrow_orienteering(fill = c('black', 'black'), text_col = NA),
    height = unit(0.5, "cm"),
    width = unit(0.5, "cm")
  ) +
  annotation_scale(location = 'br', text_cex = 1) +
  geom_sf(data = tomap, color = 'black', inherit.aes = F, alpha = 0) +
  geom_sf_text(
    data = st_centroid(tomap),
    aes(label = id),
    size = 5,
    color = 'black',
    inherit.aes = F
  ) +
  labs(
    x = NULL, 
    y = NULL, 
    title = 'Big Bayou'
  )

png(here('figs', 'bbhex.png'), width = 6, height = 6, units = 'in', res = 300)
print(m2)
dev.off()