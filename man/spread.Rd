% Generated by roxygen2 (4.0.1): do not edit by hand
\docType{methods}
\name{spread}
\alias{spread}
\title{Simulate a spread process on a landscape.}
\usage{
spread(landscape, loci, spreadProb, persistance, mask, maxSize, directions,
  iterations, ...)

\S4method{spread}{RasterLayer}(landscape, loci, spreadProb, persistance, mask,
  maxSize, directions, iterations, plot.it = FALSE, ...)
}
\arguments{
\item{landscape}{A \code{RasterLayer} object.}

\item{loci}{A vector of locations in \code{landscape}}

\item{spreadProb}{The probability of spreading.}

\item{persistance}{A probability that a burning cell will continue to burn, per time step.}

\item{mask}{non-NULL, a \code{RasterLayer} object congruent with \code{landscape}
                     whose elements are \code{0,1}, where 1 indicates "cannot spread to".}

\item{maxSize}{The maximum number of pixels for a fire. This is currently
                     only a single number, not one for each spread event}

\item{directions}{The number adjacent cells in which to look; default is 8 (Queen case).}

\item{iterations}{Number of iterations to spread. Leaving this \code{NULL} allows the spread
                     to continue until stops spreading itself (i.e., exhausts itself).}

\item{plot.it}{If TRUE, then plot the raster at every iteraction, so one can watch the
spread event grow.}
}
\value{
A \code{RasterLayer} indicating the spread of the process in the landscape.
}
\description{
More detailed description needed here.
}
\examples{
library(raster)
library(RColorBrewer)

# Make random forest cover map
fc = GaussMap(raster(extent(0, 1e3, 0, 1e3), res=1), speedup=10)
names(fc)="fc"
# make 10 fires
loci = as.integer(sample(1:ncell(fc), 10))
# create a mask used to omit spreading
mask = raster(fc)
mask = setValues(mask, 0)
mask[1:5000] <- 1

numCol <- ncol(fc)
numCell <- ncell(fc)
directions=8

# Transparency involves adding 2 extra hex digits on the
# 6 digit color code, 00 is fully transparent.
# This first colour code will give the lowest value on the
# scale a value of "transparent". This can be used for
# overlaying a raster onto another raster.
cols = list(c("#00000000", brewer.pal(8,"RdYlGn")[8:1]),
              brewer.pal(9,"Greys"), brewer.pal(8,"Spectral"))

newPlot()
simPlot(fc,col=cols[[3]],speedup=10)
names(fc) <- "fc" # required to name the layer if there is a need to plot one raster over another

fire2 <- spread(landscape=fc, loci=loci, spreadProb=0.235, persistance=0,
                     mask=NULL, maxSize=1e3, directions=8,
                     iterations=1e6, plot.it=FALSE, speedup=10)
names(fire2) <- "fire"

simPlot(stack(fire2, fc), col=cols[1:2], speedup=10)
#simPlot(fire2, col=cols[[1]], speedup=10, add=TRUE, on.which.to.plot="fc", delete.previous=FALSE)

# Here, watch the fire grow
fire2 <- spread(landscape=fc, loci=loci, spreadProb=0.235, persistance=0,
                     mask=NULL, maxSize=1e6, directions=8,
                     iterations=1e2, plot.it=TRUE, speedup=20, col=cols[[1]],
                     delete.previous=FALSE)
}
\author{
Steve Cumming \email{Steve.Cumming@sbf.ulaval.ca}

Eliot McIntire
}
