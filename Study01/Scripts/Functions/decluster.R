# Declustering spatial points


# Edited by Anders Bjørn Møller, 2019-07-04, anbm@agro.au.dk


# Functuion for declustering points using shifted grids

# pts:        SpatialPoints or a SpatialPointsDataFrame for which to run declustering.
# rows:       Number of rows in the declustering grid. Not used if gridspace is specified.
# cols:       Number of columns in the declustering grid. Not used if gridspace is specified.
# nshifts:    Number of shifts in the grid.
# xlim:       Optional vector of length 2, specifying the limits of the x axis.
# ylim:       Optional vector of length 2, specifying the limits of the y axis.
# gridspace:  Optional vector of length 2, specifying the spacing of the grid. Uses the unit of the coordinate system.
# savegrids   Should the grids used for declustering be saved to the output? If FALSE (default), the output will be a vector with weights for pts. If TRUE, the output will be a list containing the weights for pts (w) and a list with the grids used for declustering, saved as SpatialPolygons.


decluster <- function(pts, rows = 5, cols = 5, nshifts = 4,
                      xlim = NULL, ylim = NULL, gridspace = NULL,
                      savegrids = FALSE, cores = 1) {
  require(rgdal)
  require(raster)
  require(parallel)
  
  options('mc.cores' = cores)
  
  # Function to make grids
  
  mkgrid <- function(pts, h = 5, v = 5, h_shift = 0, v_shift = 0, xlim = NULL, ylim = NULL, grdspc = NULL) {
    
    ext <- extent(pts)
    
    # Calculate x coordinates of the grid boundaries
    # If grid spacing is specified, use it.
    if(!is.null(grdspc)) {
      seq_x <- seq(from = ext@xmin, by = grdspc[1], length.out = ((ext@xmax - ext@xmin)/ grdspc[1]) + 1)
    } else {
      # Otherwise use the specified number of columns
      seq_x <- seq(ext@xmin, ext@xmax, length.out = h - 1)
    }
    seq_x <- c(seq_x[1] - diff(seq_x)[1], seq_x, seq_x[length(seq_x)] + diff(seq_x)[1])
    seq_x <- seq_x + diff(seq_x)[1] * h_shift
    # If x limits are specified, truncate the grid
    if(!is.null(xlim)) {
      seq_x <- clamp(seq_x, xlim[1], xlim[2])
    }
    
    # Calculate y coordinates of the grid boundaries
    # If grid spacing is specified, use it.
    if(!is.null(grdspc)) {
      seq_y <- seq(from = ext@ymin, by = grdspc[2], length.out = ((ext@ymax - ext@ymin)/ grdspc[2]) + 1)
    } else {
      # Otherwise use the specified number of rows
      seq_y <- seq(ext@ymin, ext@ymax, length.out = v - 1)
    }
    seq_y <- c(seq_y[1] - diff(seq_y)[1], seq_y, seq_y[length(seq_y)] + diff(seq_y)[1])
    seq_y <- seq_y + diff(seq_y)[1] * v_shift
    # If y limits are specified, truncate the grid
    if(!is.null(ylim)) {
      seq_y <- clamp(seq_y, ylim[1], ylim[2])
    }
    
    # Create spatial polygons
    exts <- lapply(1:(length(seq_x) - 1), function(ix) {
      mclapply(1:(length(seq_y) - 1), function(iy) {
        df <- data.frame(x = seq_x[ix:(ix + 1)], y = seq_y[iy:(iy + 1)])
        ext_ <- extent(df)
        ext_ <- as(ext_, 'SpatialPolygons')
        proj4string(ext_) <- proj4string(pts)
        ext_
      })
    })
    
    Reduce(union, mclapply(exts, function(e) Reduce(union, e)))
  }
  
  # Calculate relative shift values, depending on the number of shifts
  shiftvalues <- seq(-0.5, 0.5, length.out = nshifts + 1)[-1]
  
  shifts <- expand.grid(h_shift = shiftvalues, v_shift = shiftvalues)
  grids <- apply(shifts, 1, function(x) {
    h_shift <- x[1]
    v_shift <- x[2]
    mkgrid(pts, h = rows, v = cols, h_shift = h_shift, v_shift = v_shift, grdspc = gridspace, xlim = xlim, ylim = ylim)
  })
  
  # Calculate weights for each grid
  message('Calculating weights')
  w <- sapply(grids, function(g) {
    names <- as.character(1:length(g))
    group <- over(pts, g)
    count <- table(group)[names]
    names(count) <- names
    count[is.na(count)] <- 0
    
    w <- 1 / count
    
    w[w == Inf] <- 0
    
    w <- w / max(w)
    w[group]
  })
  
  # Calculate mean weights across the grids.
  w <- rowMeans(w)
  names(w) <- row.names(pts@data)
  
  if(savegrids == TRUE) {
    out <- list()
    out$w <- w
    out$grids <- grids
    return(out)
  } else {
    return(w)
  }
}

# # Small example using meuse dataset 
# 
# data(meuse)
# coordinates(meuse) <- ~x+y
# proj4string(meuse) <- CRS('+init=epsg:28992')
# 
# # ws <- decluster(pts = meuse, nshifts = 3, gridspace = c(500, 500), savegrids = TRUE)
# ws <- decluster(pts = meuse, nshifts = 3, savegrids = TRUE)
# 
# rbPal <- colorRampPalette(c('red','blue'))
# 
# par(mfrow=c(3, 3), mar = c(0, 0, 0, 0))
# 
# for(i in 1:length(ws$grids)){
#   plot(meuse, pch = 20, cex = 2, col = rbPal(10)[as.numeric(cut(ws$w, breaks = 10))])
#   plot(ws$grids[[i]], add = TRUE)
# }

# END
