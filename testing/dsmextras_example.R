
library(sf)
library(terra)
library(mapview)
library(concaveman)
library(rmapshaper)
library(dsmextraLite)
# devtools::load_all(".")


#' -----------------------------------------------------------------------------
#' Load and extract the data
#' -----------------------------------------------------------------------------
data("spermwhales")


#' -----------------------------------------------------------------------------
#' Create a raster prediction grid for demonstration
#' -----------------------------------------------------------------------------
study_area <- concaveman::concaveman(predgrid) %>%
  ms_simplify(keep=0.15) %>% st_buffer(10000)
predgrid <- dsmextraLite::sf_to_rast(predgrid)

#' -----------------------------------------------------------------------------
#' Compute extrapolation
#' -----------------------------------------------------------------------------

spermwhale.extrapolation <- compute_extrapolation(
  samples = segments,
  covariate.names = c("Depth", "SST", "NPP", "DistToCAS", "EKE"),
  prediction.grid = predgrid
)

summary(spermwhale.extrapolation)

#' -----------------------------------------------------------------------------
#' High definition fixed plot for export
#' -----------------------------------------------------------------------------

library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(ggnewscale)
library(paletteer)
library(ggthemes)
library(RColorBrewer)

land <- st_geometry(ne_countries(scale=10))


## ExDet plot
ggplot() + theme_bw() +
  annotation_spatial(land, fill="grey30") +
  #layer_spatial(study_area, fill=NA) +
  layer_spatial(spermwhale.extrapolation$rasters$ExDet$combinatorial) +
  scale_fill_distiller(palette = "Purples", direction = 2, na.value=NA) +
  guides(fill=guide_legend(title="Combinatorial")) +
  new_scale_fill() +
  layer_spatial(spermwhale.extrapolation$rasters$ExDet$univariate) +
  scale_fill_distiller(palette = "Oranges", na.value=NA) +
  guides(fill=guide_legend(title="Univariate")) +
  new_scale_fill() +
  layer_spatial(spermwhale.extrapolation$rasters$ExDet$analogue) +
  scale_fill_distiller(palette = "Greens", direction = 2, na.value=NA) +
  guides(fill=guide_legend(title="Analogue")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## MIC plot
disc_pal <- c("gray90", paletteer_d("ggthemes::calc", 5))
ggplot() + theme_bw() +
  annotation_spatial(land, fill="gray30") +
  layer_spatial(spermwhale.extrapolation$rasters$mic$all) +
  scale_fill_manual(values=disc_pal, na.translate=FALSE) +
  guides(fill=guide_legend(title="MIC")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#' -----------------------------------------------------------------------------
#' Interactive plot using the {mapview} package
#' -----------------------------------------------------------------------------
library(mapview)
## ExDet
mapview(spermwhale.extrapolation$rasters$ExDet$combinatorial,
        layer.name="Combinatorial", na.color=NA, col.regions=brewer.pal(9, "Purples")) +
  mapview(spermwhale.extrapolation$rasters$ExDet$univariate,
          layer.name="Univariate", na.color=NA, col.regions=rev(brewer.pal(9, "Oranges"))) +
  mapview(spermwhale.extrapolation$rasters$ExDet$analogue,
          layer.name="Analogue", na.color=NA, col.regions=brewer.pal(9, "Greens"))

## MIC -- currently doesn't work, legend is wrong
  # mapview(spermwhale.extrapolation$rasters$mic$all, layer.name="MIC", na.color=NA)

