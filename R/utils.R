
sf_to_rast <- function(x, digits=6){
  if(!inherits(x,"sf")) stop("x must be an sf object.")
  prj <- st_crs(x)
  x <- cbind(st_coordinates(x), st_drop_geometry(x))
  colnames(x)[1:2] <- c("x","y")
  x$x <- round(x$x,digits)
  x$y <- round(x$y,digits)
  r <- terra::rast(x, type="xyz", crs=prj)
  crs(r) <- prj$wkt
  return(r)
}

#---------------------------------------------
# Function to tally the number/% of cells subject to extrapolation
#---------------------------------------------

n_and_p <- function(x){

  exl <- list(univariate.n = length(x[x < 0]),
              univariate.p = 100 * length(x[x < 0])/length(x),
              combinatorial.n = length(x[x > 1]),
              combinatorial.p = 100 * length(x[x > 1])/length(x),
              analogue.n = length(x[x >= 0 & x <=1]),
              analogue.p = 100 * length(x[x >= 0 & x <=1])/length(x))
  return(exl)
}


