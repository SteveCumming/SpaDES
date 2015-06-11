test_that("defineModule correctly handles different inputs", {
  tmp <- simInit()

  x1 <- list(
    name="testModule",
    description="this is a test.",
    keywords=c("test"),
    authors=c(person(c("Alex", "M"), "Chubaty", email="achubaty@nrcan.gc.ca", role=c("aut", "cre"))),
    version=numeric_version("0.0.1"),
    spatialExtent=raster::extent(rep(NA_real_, 4)),
    timeframe=as.POSIXlt(c(NA, NA)),
    timestep=NA_real_,
    citation=list(),
    reqdPkgs=list("grid", "raster", "sp"),
    parameters=rbind(defineParameter("dummyVal", "numeric", 1.0, NA, NA)),
    inputObjects=data.frame(objectName="testInput",
                            objectClass="list",
                            other=NA_character_,
                            stringsAsFactors=FALSE),
    outputObjects=data.frame(objectName="testOutput",
                             objectClass="list",
                             other=NA_character_,
                             stringsAsFactors=FALSE)
  )

  ## check name
  x2 <- x1
  x2$name <- list("testModule") # not a character
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check description
  x2 <- x1
  x2$description <- list("this is a test.") # not a character vector
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check keywords
  x2 <- x1
  x2$keywords <- list("test") # not a character vector
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check authors
  x2 <- x1
  x2$authors <- "not a person class"
  expect_error(defineModule(tmp, x2), paste0("invalid module definition: ",
                                             x2$name,
                                             ": authors must be a `person` class."))

  ## check version
  x2 <- x1
  x2$version <- "0.0.1"
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check spatialExtent
  x2 <- x1
  x2$spatialExtent <- NA
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check timeframe
  x2 <- x1
  x2$timeframe <- NA
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check timestep
  x2 <- x1
  x2$timestep <- NA
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check citation
  x2 <- x1
  x2$citation <- character() # not a list
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check reqdPkgs
  x2 <- x1
  x2$reqdPkgs <- c("grid", "raster", "sp") # not a list
  expect_identical(defineModule(tmp, x1), defineModule(tmp, x2))

  ## check parameters
  x2 <- x1
  x2$parameters <- "not a data.frame"
  expect_error(defineModule(tmp, x2), paste0("invalid module definition: ",
                                             x2$name,
                                             ": parameters must be a `data.frame`."))

  ## check inputObjects
  x2 <- x1
  x2$inputObjects <- "not a data.frame"
  expect_error(defineModule(tmp, x2), paste0("invalid module definition: ",
                                             x2$name,
                                             ": inputObjects must be a `data.frame`."))
  ## check authors
  x2 <- x1
  x2$outputObjects <- "not a person class"
  expect_error(defineModule(tmp, x2), paste0("invalid module definition: ",
                                             x2$name,
                                             ": outputObjects must be a `data.frame`."))
})


test_that("depsEdgeList and depsGraph work", {
  times <- list(start=0.0, stop=10)
  params <- list(.globals=list(burnStats="npixelsburned", stackName="landscape"),
                 randomLandscapes=list(.plotInitialTime=NA, .plotInterval=NA),
                 caribouMovement=list(.plotInitialTime=NA, .plotInterval=NA),
                 fireSpread=list(.plotInitialTime=NA, .plotInterval=NA))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  path <- system.file("sampleModules", package="SpaDES")

  mySim <- simInit(times, params, modules, objects=list(), path)

  # depsEdgeList
  el <- depsEdgeList(mySim)
  el_from <- c("caribouMovement", "caribouMovement", "fireSpread", "fireSpread",
               "fireSpread", "randomLandscapes", "randomLandscapes")
  el_to <- c("caribouMovement", "fireSpread", "caribouMovement", "fireSpread",
             "fireSpread", "caribouMovement", "fireSpread")
  el_objName <- c("landscape", "landscape", "landscape", "landscape", "npixelsburned",
                  "landscape", "landscape")
  el_objClass <- c("RasterStack", "RasterStack", "RasterStack", "RasterStack",
                   "numeric", "RasterStack", "RasterStack")

  expect_is(el, "data.table")
  expect_equal(names(el), c("from", "to", "objName", "objClass"))
  expect_equal(el$from, el_from)
  expect_equal(el$to, el_to)
  expect_equal(el$objName, el_objName)
  expect_equal(el$objClass, el_objClass)

  # .depsPruneEdges
  p <- .depsPruneEdges(el)
  p_from <- c("randomLandscapes", "randomLandscapes")
  p_to <- c("caribouMovement", "fireSpread")
  p_objName <- c("landscape", "landscape")
  p_objClass <- c("RasterStack", "RasterStack")
  p_ <- data.table(from=p_from, to=p_to, objName=p_objName, objClass=p_objClass)

  expect_is(p, "data.table")
  expect_equivalent(p, p_)

  # depsGraph
  expect_is(depsGraph(mySim), "igraph")
})