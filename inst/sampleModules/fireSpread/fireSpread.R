###
### Specify module (and dependencies) definitions:
###
### name:         fireSpread
###
### description:  Simulate fire ignition and spread on a landscape, where
###               spread probability varies according to percent pine.
###               Fire size statistics are collected immediately after each burn event.
###               Requires a global simulation parameter `.stackName` be set.
###
### keywords:     fire; percolation model; spread algorithm
###
### authors:      Alex M. Chubaty <Alexander.Chubaty@NRCan.gc.ca>
###               Eliot J. B. McIntire <Eliot.McIntire@NRCan.gc.ca>
###               Steve Cumming <Steve.Cumming@sbf.ulaval.ca>
###
### version:      0.1.0
###
### spatialExtent: NA
###
### timeframe:    NA
###
### timestep:     NA
###
### citation:     NA
###
### reqdPkgs:     raster; RColorBrewer; tkrplot; RandomFields
###
### inputObjects: objectName: simGlobals(sim)$.stackName
###                objectClass: RasterStack
###
### outputObjects: objectName: simGlobals(sim)$.stackName
###                objectClass: RasterStack
###
### fireSpread module metadata
defineModule(sim, list(
  name="fireSpread",
  description="Simulate fire ignition and spread on a landscape, where spread probability varies according to percent pine. Fire size statistics are collected immediately after each burn event. Requires a global simulation parameter `.stackName` be set.",
  keywords=c("random map", "random landscape"),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="Steve.Cumming@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timestep=NA_real_,
  citation=list(),
  reqdPkgs=list("methods", "raster", "RColorBrewer"),
  inputObjects=data.frame(name=simGlobals(sim)$.stackName, class="RasterStack", stringsAsFactors=FALSE),
  outputObjects=data.frame(name=simGlobals(sim)$.stackName, class="RasterStack", stringsAsFactors=FALSE)
))

### event functions
doEvent.fireSpread <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more object dependencies:
    ### (use `checkObject` or similar)
    checkObject(simGlobals(sim)$.stackName, layer="habitatQuality")

    if (!exists(simGlobals(sim)$burnStats, envir=.GlobalEnv)) {
      assignGlobal(simGlobals(sim)$burnStats, numeric())
    } else {
      npix <- getGlobal(simGlobals(sim)$burnStats)
      stopifnot("numeric" %in% is(npix), "vector" %in% is(npix))
    }

    # do stuff for this event
    sim <- fireSpreadInit(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, simParams(sim)$fireSpread$startTime, "fireSpread", "burn")
    sim <- scheduleEvent(sim, simParams(sim)$fireSpread$.saveInterval, "fireSpread", "save")
    sim <- scheduleEvent(sim, simParams(sim)$fireSpread$.plotInitialTime, "fireSpread", "plot.init")

  } else if (eventType=="burn") {
    # do stuff for this event
    sim <- fireSpreadBurn(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSpread", "stats") # do stats immediately following burn
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$returnInterval, "fireSpread", "burn")
  } else if (eventType=="stats") {
    # do stuff for this event
    sim <- fireSpreadStats(sim)

    # schedule the next event
    ## stats scheduling done by burn event
  } else if (eventType=="plot.init") {
    # do stuff for this event
    maps <- getGlobal(simGlobals(sim)$.stackName)
    setColors(maps) <- list(DEM=terrain.colors(100),
                                forestAge=brewer.pal(9,"BuGn"),
                                forestCover=brewer.pal(8,"BrBG"),
                                habitatQuality=brewer.pal(8,"Spectral"),
                                percentPine=brewer.pal(9,"Greens"),
                                Fires=c("white", rev(heat.colors(9)))
                            )
    assignGlobal(simGlobals(sim)$.stackName, maps)
    Plot(getGlobal(simGlobals(sim)$.stackName), new=TRUE)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.plotInterval, "fireSpread", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(get(simGlobals(sim)$.stackName)$Fires, new=FALSE)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.plotInterval, "fireSpread", "plot")
  } else if (eventType=="save") {
    # do stuff for this event
    saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.saveInterval, "fireSpread", "save")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
  }
  return(invisible(sim))
}

fireSpreadInit <- function(sim) {
  landscapes <- getGlobal(simGlobals(sim)$.stackName)

  ### create burn map that tracks fire locations over time
  Fires <- raster(extent(landscapes), ncol=ncol(landscapes), nrow=nrow(landscapes), vals=0)
  names(Fires) <- "Fires"
  setColors(Fires) <- c("white", rev(heat.colors(9)))
  Fires <- setValues(Fires, 0)

  # add Fires map to global$.stackName stack
  assignGlobal(simGlobals(sim)$.stackName, addLayer(landscapes,Fires))

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "fireSpread")

  return(invisible(sim))
}


fireSpreadBurn <- function(sim) {
  landscapes <- getGlobal(simGlobals(sim)$.stackName)

  Fires <- spread(landscapes[[1]],
                   loci=as.integer(sample(1:ncell(landscapes), simParams(sim)$fireSpread$nFires)),
                   spreadProb=simParams(sim)$fireSpread$spreadprob,
                   persistance=simParams(sim)$fireSpread$persistprob,
                   mask=NULL,
                   maxSize=1e8,
                   directions=8,
                   iterations=simParams(sim)$fireSpread$its,
                   plot.it=FALSE,
                   mapID=TRUE)
  names(Fires) <- "Fires"
  setColors(Fires) <- c("white", rev(heat.colors(9)))
  landscapes$Fires <- Fires

  assignGlobal(simGlobals(sim)$.stackName, landscapes)

  return(invisible(sim))
}

fireSpreadStats <- function(sim) {
  npix <- getGlobal(simGlobals(sim)$burnStats)

  landscapes <- getGlobal(simGlobals(sim)$.stackName)

  assignGlobal("nPixelsBurned", c(npix, length(which(values(landscapes$Fires)>0))))

  return(invisible(sim))
}