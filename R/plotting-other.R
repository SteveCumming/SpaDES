################################################################################
#' Clear plotting device
#'
#' Under some conditions, a device and its metadata need to be cleared manually.
#' This can be done with either the \code{new = TRUE} argument within the call to
#' \code{Plot}.
#' Sometimes, the metadata of a previous plot will prevent correct plotting of
#' a new \code{Plot} call.
#' Use \code{clearPlot} to clear the device and all the associated metadata
#' manually.
#'
#' @param dev Numeric. Device number to clear.
#'
#' @param removeData Logical indicating whether any data that was stored in the
#' \code{.spadesEnv} should also be removed; i.e., not just the plot window wiped.
#'
#' @export
#' @importFrom grDevices dev.cur
#' @importFrom grid grid.newpage
#' @docType methods
#' @rdname clearPlot
#' @include plotting-classes.R
#' @author Eliot McIntire
setGeneric("clearPlot", function(dev = dev.cur(), removeData = TRUE) {
  standardGeneric("clearPlot")
})

#' @export
#' @rdname clearPlot
setMethod(
  "clearPlot",
  signature = c("numeric", "logical"),
  definition = function(dev, removeData) {
    suppressWarnings(
      try(rm(list = paste0("spadesPlot", dev), envir = .spadesEnv))
    )
    if (removeData) {
      suppressWarnings(
        try(rm(list = ls(.spadesEnv[[paste0("dev", dev)]]),
               envir = .spadesEnv[[paste0("dev", dev)]]), silent = TRUE)
      )
    }
    devActive <- dev.cur()
    if (devActive == 1) { return(invisible()) }
    dev(dev)
    grid.newpage()
    dev(devActive)
  }
)

#' @export
#' @rdname clearPlot
setMethod("clearPlot",
          signature = c("numeric", "missing"),
          definition = function(dev) {
            clearPlot(dev, removeData = FALSE)
})

#' @export
#' @rdname clearPlot
setMethod("clearPlot",
          signature = c("missing","logical"),
          definition =  function(removeData) {
            clearPlot(dev = dev.cur(), removeData = removeData)
})

#' @export
#' @rdname clearPlot
setMethod("clearPlot",
          signature = c("missing","missing"),
          definition =  function(dev, removeData) {
            clearPlot(dev.cur(), removeData = FALSE)
})

################################################################################
#' Convert \code{grid.locator} units
#'
#' Internal function from example in \code{?grid.locator}.
#' Converts \code{grid.locator} units to meaningful units.
#' Used within \code{.clickCoord}
#'
#' @param grid.locator an object that was output by a call to \code{grid.locator}
#'                     and mouse click(s).
#'
#' @docType methods
#' @export
#' @rdname unittrim
#' @author Paul Murrell
#'
.unittrim <- function(grid.locator) {
  as.numeric(sub("^([0-9]+|[0-9]+[.][0-9])[0-9]*", "\\1",
                 as.character(grid.locator)))
}

################################################################################
#' Mouse interactions with Plots
#'
#' These functions use \code{grid.locator}. The primary two user-level functions are
#' \code{clickValues} and \code{clickExtent}. These functions automatically select
#' the correct viewport (i.e., map) where the mouse clicks occured so the user
#' does not have to manually specify which map is being clicked on.
#' This works for \code{Raster*}, \code{SpatialPoints*}, and \code{SpatialPolygons*} objects.
#'
#' \code{clickValues} is equivalent to running \code{X[SpatialPoints(locator(n))]}, where
#' X is the raster being clicked on, in base graphics. This function determines which place in the
#' grid.layout was clicked and makes all appropriate calculations to determine the value
#' on the raster(s) at that or those location(s). It should be noted that when zooming in
#' to rasters, plotting of rasters will only allow for complete pixels to be plotted, even
#' if the extent is not perfectly in line with pixel edges. As a result, when values
#' returned by this function may be slightly off (<0.5 pixel width).
#'
#' \code{clickExtent} is for drawing an extent with two mouse clicks on a given Plotted map.
#'
#' \code{clickCoordinates} is the workhorse function that determines which plot has been
#' clicked on and passes this plot name and the clicked coordinates to \code{.clickCoord}.
#'
#' \code{.clickCoord} is intended for internal use and is called by other functions here.
#'
#' @param n The number of mouse clicks to do.
#'
#' @return \code{clickValues} returns the layer names and values at the clicked points.
#' \code{clickExtent} invisibly returns the extent object, and optionally plots it in a new device window.
#' \code{clickCoordinates} returns the xy coordinates in the units of the plot clicked on.
#'
#' @export
#' @include plotting-classes.R
#' @docType methods
#' @author Eliot McIntire
#' @rdname spadesMouseClicks
#'
clickValues <- function(n = 1) {
  coords <- clickCoordinates(n = n)
  objLay <- strsplit(coords$map, "\\$")
  objNames <- sapply(objLay, function(x) { x[1] })
  layNames <- sapply(objLay, function(x) { x[2] })
  for (i in 1:n) {
    if (!is.na(layNames[i])) {
      coords$coords$value <- sapply(seq_len(n), function(i) {
        eval(parse(text = objNames[i]),
             envir = coords$envir[[i]])[[layNames[i]]][cellFromXY(
               eval(parse(text = objNames[i]),
                    envir = coords$envir[[i]])[[layNames[i]]],
               coords$coords[i,1:2])]
      })
    } else {
      coords$coords$value <- sapply(seq_len(n), function(i) {
        eval(parse(text = objNames[i]),
             envir = coords$envir[[i]])[cellFromXY(
               eval(parse(text = objNames[i]),
                    envir = coords$envir[[i]]),
               coords$coords[i,1:2])]
      })
    }
  }
  return(coords$coords)
}

#' @param devNum The device number for the new plot to be plotted on.
#'
#' @param plot.it Logical. If \code{TRUE} a new plotting window is made for the
#'                new extent. Default \code{TRUE}.
#'
#' @export
#' @docType methods
#' @importFrom grDevices dev.cur
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @rdname spadesMouseClicks
clickExtent <- function(devNum = NULL, plot.it = TRUE) {

  corners <- clickCoordinates(2)
  zoom <- extent(c(sort(corners[[3]]$x), sort(corners[[3]]$y)))

  if (plot.it) {
    devActive <- dev.cur()
    if (is.null(devNum)) {
      newPlot()
    } else {
      dev(devNum)
    }

    objLay <- strsplit(corners$map, "\\$")
    objNames <- unique(sapply(objLay, function(x) x[1]))
    layNames <- unique(sapply(objLay, function(x) x[2]))
    if (!is.na(layNames)) {
      Plot(eval(parse(text = objNames), envir = corners$envir[[1]])[[layNames]],
           zoomExtent = zoom, new = TRUE)
    } else {
      Plot(get(objNames, envir = corners$envir[[1]]), zoomExtent = zoom, new = TRUE)
    }

    dev(devActive)
    return(invisible(zoom))
  } else {
    return(zoom)
  }
}

#' @export
#' @include environment.R
#' @include plotting-classes.R
#' @docType methods
#' @author Eliot McIntire
#' @rdname spadesMouseClicks
#' @importFrom grid grid.layout grid.locator unit
#' @importFrom grDevices dev.cur
# igraph exports %>% from magrittr
clickCoordinates <- function(n = 1) {
  dc <- dev.cur()

  arr <- try(.getSpaDES(paste0("spadesPlot", dc)))
  if (is(arr, "try-error")) {
    stop(paste("Plot does not already exist on current device.",
               "Try new = TRUE, clearPlot() or change device to",
               "one that has objects from a call to Plot()."))
  }
  gl <- grid.layout(nrow = arr@arr@rows*3+2,
                    ncol = arr@arr@columns*3+2,
                    widths = arr@arr@layout$wdth,
                    heights = arr@arr@layout$ht)

  grepNullsW <- grep("null$", gl$widths)
  grepNpcsW <- grep("npc$", gl$widths)
  #nulls <- as.numeric(unlist(strsplit(as.character(gl$widths)[grepNullsW], "null") ))
  nulls <- as.character(gl$widths)[grepNullsW] %>%
    strsplit(., "null") %>%
    unlist %>%
    as.numeric
  #npcs <- as.numeric(unlist(strsplit(as.character(gl$widths)[grepNpcsW], "npc") ))
  npcs <- as.character(gl$widths)[grepNpcsW] %>%
    strsplit(., "npc") %>%
    unlist %>%
    as.numeric
  remaining <- 1 - sum(npcs)
  npcForNulls <- nulls * remaining / sum(nulls)
  widthNpcs <- c(npcs, npcForNulls)[order(c(grepNpcsW, grepNullsW))]

  grepNullsH <- grep("null$", gl$heights)
  grepNpcsH <- grep("npc$", gl$heights)
  #nulls <- as.numeric(unlist(strsplit(as.character(gl$heights)[grepNullsH], "null") ))
  nulls <- as.character(gl$heights)[grepNullsH] %>%
    strsplit(., "null") %>%
    unlist %>%
    as.numeric
  #npcs <- as.numeric(unlist(strsplit(as.character(gl$heights)[grepNpcsH], "npc") ))
  npcs <- as.character(gl$heights)[grepNpcsH] %>%
    strsplit(., "npc") %>%
    unlist() %>%
    as.numeric()
  remaining <- 1 - sum(npcs)
  npcForNulls <- nulls * remaining / sum(nulls)
  heightNpcs <- c(npcs, npcForNulls)[order(c(grepNpcsH, grepNullsH))]

  clickCoords <- data.frame(x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
  mapNames <- character(n)
  envs <- list()

  grobLoc <- list()

  for (i in 1:n) {
    seekViewport("top")
    gloc <- grid.locator(unit = "npc")
    xInt <- findInterval(as.numeric(strsplit(as.character(gloc$x), "npc")[[1]]),
                         c(0, cumsum(widthNpcs)))
    # for the y, grid package treats bottom left as origin, Plot treats top left
    #  as origin... so, require 1-
    yInt <- findInterval(as.numeric(strsplit(as.character(gloc$y), "npc")[[1]]),
                         c(0, cumsum(heightNpcs)))
    if (!(xInt %in% grepNpcsW) & !(yInt %in% grepNpcsH)) {
      stop("No plot at those coordinates")
    }
    column <-  which(xInt == grepNpcsW)
    row <- which((yInt == grepNpcsH)[length(grepNpcsH):1])
    map <- column + (row - 1) * arr@arr@columns

    maxLayX <- cumsum(widthNpcs)[xInt]
    minLayX <- cumsum(widthNpcs)[xInt - 1]
    grobLoc$x <- unit((as.numeric(strsplit(
      as.character(gloc$x), "npc"
      )[[1]]) - minLayX) / (maxLayX - minLayX), "npc")

    maxLayY <- cumsum(heightNpcs)[yInt]
    minLayY <- cumsum(heightNpcs)[yInt - 1]
    grobLoc$y <- unit((as.numeric(strsplit(
      as.character(gloc$y), "npc"
      )[[1]]) - minLayY) / (maxLayY - minLayY), "npc")

    clickCoords[i, ] <- .clickCoord(arr@spadesGrobList[[map]][[1]]@plotName,
                                    n = 1, gl = grobLoc)
    mapNames[i] <- arr@spadesGrobList[[map]][[1]]@plotName
    envs[[i]] <- arr@spadesGrobList[[map]][[1]]@envir
  }
  return(list(map = mapNames, envir = envs, coords = clickCoords))
}

#' @param X The raster object whose values will be returned where mouse clicks occur.
#'
#' @param gl An object created by a call to \code{grid.locator}.
#'
#' @export
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @docType methods
#' @rdname spadesMouseClicks
#' @importFrom grid seekViewport grid.locator convertX convertY
.clickCoord <- function(X, n = 1, gl = NULL) {
  pts <- data.frame(x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
  seekViewport(X)
  for (i in 1:n) {
    if (is.null(gl)) {
      gl <- grid.locator()
      pts[i, ] <- .unittrim(gl)
    } else {
      pts[i, ] <- c(convertX(gl$x, "native"), convertY(gl$y, "native"))
    }
  }
  return(pts)
}

################################################################################
#' Specify where to plot
#'
#' Switch to an existing plot device, or if not already open,
#' launch a new graphics device based on operating system used.
#'
#' For example, \code{dev(6)} switches the active plot device to device #6.
#' If it doesn't exist, it opens it. NOTE: if devices 1-5 don't exist
#' they will be opened too.
#'
#' @param x   The number of a plot device. If missing, will open a new
#'            non-RStudio plotting device
#'
#' @param ... Additional arguments passed to \code{\link{newPlot}}.
#'
#' @return Opens a new plot device on the screen. Invisibly returns the
#' device number selected.
#'
#' @export
#' @include plotting-classes.R
#' @importFrom grDevices dev.list dev.set
#' @docType methods
#' @rdname dev
#' @author Eliot McIntire and Alex Chubaty
#'
dev <- function(x, ...) {
  if (missing(x)) {
    if (is.null(dev.list())) {
      x <- 2L
    } else {
      if (any(names(dev.list()) == "RStudioGD")) {
        x <- min(max(dev.list()) + 1,
                 which(names(dev.list()) == "RStudioGD") + 3L)
        dev(x)
      } else {
        x <- max(dev.list())
        dev(x)
      }
    }
  }
  if (is.null(dev.list())) newPlot(...)
  while (dev.set(x) < x) newPlot(...)
  return(invisible(dev.cur()))
}

################################################################################
#' Open a new plotting window
#'
#' @param noRStudioGD Logical Passed to dev.new. Default is TRUE to avoid using
#'                    RStudio graphics device, which is slow.
#' @param ...         Additional arguments.
#'
#' @note \code{\link{dev.new}} is supposed to be the correct way to open a new
#' window in a platform-generic way; however, doesn't work in RStudio (#116).
#'
#' @seealso \code{\link{dev}}.
#'
#' @author Eliot McIntire and Alex Chubaty
#'
#' @export
#' @importFrom grDevices dev.new
#' @docType methods
#' @rdname newPlot
newPlot <- function(noRStudioGD = TRUE, ...) {
  dev.new(noRStudioGD = TRUE, ...)
}
