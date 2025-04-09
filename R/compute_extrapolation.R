#' Quantify extrapolation in multivariate environmental space
#'
#' Assesses univariate (Type I) and combinatorial (Type II) extrapolation in spatial ecological models such as density surface models of line transect data. Models are built in a reference (calibration) system and projected into one or more target (prediction) system(s). The function is based on original code from the \href{https://cran.r-project.org/web/packages/ecospat/index.html}{ecospat} package (Broennimann et al. 2016). Although the required inputs mirror those of the \href{https://cran.r-project.org/web/packages/dsm/index.html}{dsm} package (Miller et al. 2015), the function is not restricted to line/strip-transect data and can be applied to other survey types and predictive modelling scenarios. See the 'Examples' section and Bouchet et al. (2019) for more information.
#'
#' The function calculates values of the ExDet (EXtrapolation DETection) metric as originally proposed by Mesgaran et al. (2014). ExDet takes on strictly negative values during univariate extrapolation (i.e. when predictions are made outside the range of individual covariates), is strictly >1 during combinatorial extrapolation (i.e. when predictions are made within the range of individual covariates, but for combinations of environmental conditions not encountered in the sample), and lies within the range 0-1 when predictions are made in conditions analogous to those found in the reference system. The function also determines which covariates make the largest contribution to each type of extrapolation; this is the most influential covariate (MIC). See Mesgaran et al. (2014) for details.
#'
#' Note that \code{compute_extrapolation} returns results in both numerical and raster format. The latter is used to support mapping functions and requires the locations in \code{prediction.grid} to be evenly spaced. If this is not the case, \code{dsmextra} will attempt to automatically generate a raster with a resolution given by the \code{resolution} argument (and expressed in the units of \code{coordinate.system}). An error may be returned if no \code{resolution} is specified.
#'
#' The \code{data} list captures ExDet values at prediction locations (i.e. cells in \code{prediction.grid}) and is organised into multiple \code{data.frame} objects, as follows:
#'
#' \tabular{ll}{
#'   \code{all} \tab All prediction locations\cr
#'   \code{univariate} \tab Prediction locations subject to univariate extrapolation (only)\cr
#'   \code{combinatorial} \tab Prediction locations subject to combinatorial extrapolation (only)\cr
#'   \code{analogue} \tab Prediction locations where conditions are analogous to sampled conditions (only)\cr
#'  }
#'
#'  Each \code{data.frame} contains four columns:
#'
#'  \tabular{ll}{
#'   \code{ExDet} \tab ExDet values\cr
#'   \code{mic_univariate} \tab Integer identifying the univariate MIC\cr
#'   \code{mic_combinatorial} \tab Integer identifying the combinatorial MIC\cr
#'   \code{mic} \tab Integer identifying the MIC\cr
#'  }
#'
#' The \code{rasters} list comprises two elements, named \code{ExDet} and \code{mic}. Each contains individual rasters mapping ExDet and MIC values, respectively.
#'
#' @param samples Sample (reference) \code{sf} dataset used for model building and calibration. This corresponds to the \code{segment.data} used when building density surface models in \code{dsm}. It must contain one column for each of the covariates in \code{covariate.names}.
#' @param covariate.names Character string. Names of the covariates of interest.
#' @param prediction.grid Prediction data.frame. This contains both geographic coordinates (\code{x}, \code{y}) and covariate values associated with the target locations for which predictions are desired. Typically, these locations are taken as the centroids of the grid cells in a spatial prediction grid/raster. See \code{\link[dsm]{predict.dsm}}.
#' @param coordinate.system Projected coordinate system relevant to the study location. Can be either a character string or an object of class \code{\link[sp]{CRS}}.
# @param print.summary Logical, defaults to \code{TRUE}. Outputs a summary of the results to the R console.
# @param print.precision Integer. Number of significant figures to be used when printing the summary. Default value of 2.
# @param save.summary Logical, defaults to \code{FALSE}. Adds summary statistics to the output list.
#' @param resolution Resolution of the output raster (in units relevant to \code{coordinate.system}). Only required if \code{prediction.grid} is irregular, and thus needs to be rasterised. Defaults to \code{NULL}.
#' @param verbose Logical. Show or hide possible warnings and messages.
#'
#' @return A list object containing extrapolation values in both \code{data.frame} and \code{\link[raster]{raster}} format. Also included are a summary object of class \code{extrapolation_results_summary} and a copy of function inputs (i.e, \code{coordinate.system}, \code{covariate.names}, and \code{prediction.grid}).
#'
#' @author Phil J. Bouchet and Devin S. Johnson
#'
#' @references Bouchet PJ, Miller DL, Roberts JJ, Mannocci L, Harris CM and Thomas L (2019). From here and now to there and then: Practical recommendations for extrapolating cetacean density surface models to novel conditions. CREEM Technical Report 2019-01, 59 p. \href{https://research-repository.st-andrews.ac.uk/handle/10023/18509}{https://research-repository.st-andrews.ac.uk/handle/10023/18509}
#'
#' Broennimann O, Di Cola V, Guisan A (2016). ecospat: Spatial Ecology Miscellaneous Methods. R package version 2.1.1. \href{https://CRAN.R-project.org/package=ecospat}{https://CRAN.R-project.org/package=ecospat}
#'
#' Mesgaran MB, Cousens RD, Webber BL (2014). Here be dragons: a tool for quantifying novelty due to covariate range and correlation change when projecting species distribution models. Diversity & Distributions, 20: 1147-115. DOI: \href{https://onlinelibrary.wiley.com/doi/full/10.1111/ddi.12209}{10.1111/ddi.12209}
#'
#' Miller DL, Rexstad E, Burt L, Bravington MV, Hedley S (2015). dsm: Density Surface Modelling of Distance Sampling Data. R package version 2.2.9. \href{https://CRAN.R-project.org/package=dsm}{https://CRAN.R-project.org/package=dsm}
#'
#' @rawNamespace import(terra, except = c(na.omit, quantile))
#' @import sf
#' @export

compute_extrapolation <- function(samples,
                                  covariate.names,
                                  prediction.grid,
                                  coordinate.system,
                                  resolution = NULL,
                                  verbose = TRUE){

  #---------------------------------------------
  # Perform function checks
  #---------------------------------------------
  # covariate.names <- sort(covariate.names)
  if(!inherits(samples, "sf")) stop("samples must be an sf data set")
  class(prediction.grid)
  if(!any(c("sf","SpatRaster")%in%class(prediction.grid))) stop("prediction.grid must be an sf data set or a SpatRaster")

  if(!all(covariate.names%in%names(samples))) stop("Missing/unrecognised covariates in the sample data")
  if(!all(covariate.names%in%names(prediction.grid))) stop("Missing/unrecognised covariates in the prediction grid")

  prj_samples <- st_crs(samples)
  prj_pred <- st_crs(prediction.grid)
  if(prj_samples != prj_pred) stop("CRS of samples and prediction.grid is not the same!")

  samples <- samples[,covariate.names]
  samples <- na.omit(samples) # Cannot have NA values
  if(inherits(prediction.grid,"sf")){
    prediction.grid <- prediction.grid[,covariate.names]
    prediction.grid <- stats::na.omit(prediction.grid)
  } else{
    prediction.grid <- prediction.grid[[covariate.names]]
  }

  if(verbose) message("Computing ...")

  #---------------------------------------------
  # Define reference and target systems
  #---------------------------------------------

  reference <- st_drop_geometry(samples[, covariate.names])
  if(inherits(prediction.grid, "sf")){
    target <- st_drop_geometry(prediction.grid[, covariate.names])
  } else{
    target <- terra::as.data.frame(prediction.grid, cells=TRUE)
    xy <- cbind(terra::xyFromCell(prediction.grid, target$cell), cell=target$cell)
    target <- target[,covariate.names]
  }

  #---------------------------------------------
  # Run the exdet tool from Mesgaran et al. (2014)
  #---------------------------------------------

  mesgaran <- ExDet(ref = reference,
                    tg = target,
                    xp = covariate.names) |> as.data.frame()

  #---------------------------------------------
  # Add coordinates
  #---------------------------------------------

  if(inherits(prediction.grid, "sf")){
    mesgaran <- cbind(st_coordinates(prediction.grid), mesgaran)
  } else{
    mesgaran <- cbind(xy,mesgaran)
  }
  names(mesgaran)[1:2] <- c("x","y")
  mesgaran <- st_as_sf(mesgaran, coords = c("x","y"), crs=prj_pred)
  # mesgaran$mic <- ifelse(mesgaran$mic==0, NA, mesgaran$mic)
  # mesgaran$mic_univariate <- factor(covariate.names[mesgaran$mic_univariate], levels=levels(mesgaran$mic))
  # mesgaran$mic_combinatorial <- factor(covariate.names[mesgaran$mic_combinatorial], levels=levels(mesgaran$mic))

  #---------------------------------------------
  # Return a list with univariate, combinatorial, and analog conditions as separate elements
  #---------------------------------------------

  reslist <- list(data=NULL, rasters=NULL)

  reslist$data$all <- mesgaran
  reslist$data$univariate <- mesgaran %>%
    dplyr::filter(., ExDet < 0)
  reslist$data$combinatorial <- mesgaran %>%
    dplyr::filter(., ExDet > 1)
  reslist$data$analogue <- mesgaran %>%
    dplyr::filter(., ExDet >= 0 & ExDet <= 1)

  #---------------------------------------------
  # Create rasters from extrapolation/MIC values
  #---------------------------------------------

  if(inherits(prediction.grid, "SpatRaster")){
    cov_df <- data.frame(id=0:length(covariate.names), covariate=c("None",covariate.names))
    nms <- names(reslist$data)
    r <- terra::rast(terra::ext(prediction.grid), resolution=terra::res(prediction.grid))
    terra::crs(r) <- terra::crs(prediction.grid)
    # ExDet
    r_lst <- vector("list", length(nms))
    names(r_lst) <- nms
    for(i in 1:length(r_lst)){
      r_lst[[i]] <- r
      r_lst[[i]][reslist$data[[i]]$cell] <-  reslist$data[[i]]$ExDet
    }
    reslist$rasters$ExDet <- rast(r_lst)

    #MIC
    for(i in 1:length(r_lst)){
      r_lst[[i]] <- r
      r_lst[[i]][reslist$data[[i]]$cell] <-  reslist$data[[i]]$mic
      levels( r_lst[[i]]) <- cov_df
    }
    reslist$rasters$mic <- rast(r_lst)
  }

#  #---------------------------------------------
#  # Print/save summary
#  #---------------------------------------------

  sumres <- summarise_extrapolation(extrapolation.object = reslist,
                                    covariate.names = covariate.names,
                                    extrapolation = TRUE,
                                    mic = TRUE)

  class(sumres) <- c("extrapolation_results_summary", class(sumres))
  reslist <- append(x = reslist, values = list(summary = sumres))

  # Add function inputs to obviate need to specify them in map()
  reslist <- append(x = reslist, values = list(
                                  covariate.names = covariate.names,
                                  samples = samples,
                                  prediction.grid = prediction.grid
                                  ))

  reslist <- append(list(type = c("extrapolation", "mic")), reslist)

  # Keep it classy
  class(reslist) <- c("extrapolation_results", class(reslist))

  if(verbose) message("Done!")
  return(reslist)

}
