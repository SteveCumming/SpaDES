################################################################################
#' Thin method for sp objects
#'
#' This wraps the low level \code{thin} function in the fastshp
#' package for use on \code{sp} classes of spatial objects (specifically
#' \code{SpatialPolygon*} and \code{SpatialLines*})
#'
#' @param spGeom A sp class object.
#'
#' @param tol Numeric. Transfered to \code{tolerance} arg of \code{\link[fastshp]{thin}}
#'
#' @param method Integer. Passed to \code{method} are of  \code{\link[fastshp]{thin}}
#'
#' @return An object of the same class as the input \code{s
#' pGeom}, but
#' with thinned points
#'
#' @seealso \code{\link[fastshp]{thin}}.
#' @export
#' @importFrom fastshp thin
#' @docType methods
#' @rdname thin
#' @author Eliot McIntire
setGeneric("thin", function(spGeom, tol=100, method=2L) {
  thin(spGeom, tol, method)
})

#' @export
#' @rdname thin
setMethod(
  "thin",
  signature="SpatialPolygons",
  definition=function(spGeom, tol, method) {

    mat <- lapply(1:length(spGeom@polygons), function(i) {
      matInner <- lapply(1:length(spGeom@polygons[[i]]@Polygons), function(j) {
         matInner2 <- cbind(rep(j,NROW(spGeom@polygons[[i]]@Polygons[[j]]@coords)),
                                spGeom@polygons[[i]]@Polygons[[j]]@coords)
        return(matInner2)
      })
      matInner <- do.call(rbind,matInner)
      matInner <- cbind(id=rep(i,NROW(matInner)), matInner)
      return(matInner)
    }) %>%
      do.call(rbind, .)




    mat <- cbind(mat[,1],rep(5,NROW(mat)),mat[,2],mat[,3], mat[,4])
    colnames(mat) <- c("id","type","part","x","y")

    matThin <- thin.shp(mat %>% data.table, max.width=25L, tolerance=100)
    mat <- cbind(mat, matThin)

    a = mat %>% data.table %>% group_by(id, part)
    aSum <- a %>% summarise(sumMatThin=sum(matThin))
    whNotEnough <- which(aSum$sumMatThin==1)
    aTmp <- a %>% inner_join(aSum[whNotEnough]) %>% group_by(id, part) %>%
      filter(first(sumMatThin)) %>% mutate(matThin=1)%>% select(-sumMatThin)
    b <- anti_join(a, aTmp) %>% bind_rows(aTmp)

    traits <- mat %>% data.table %>%
      group_by(id,part) %>%
      summarise(len=length(x))

    stopIndex <- cumsum(traits$len)
    startIndex <- c(1,cumsum(traits$len)+1)
    startIndex <- startIndex[-length(startIndex)]

#    browser()
#    for(i in 10217:10220) {
     for(i in 1:(NROW(traits))) {
      spGeom@polygons[[traits[i,id]]]@Polygons[[traits[i,part]]]@coords <-
        b[((startIndex[i]:stopIndex[i]) * b$matThin[startIndex[i]:stopIndex[i]]),] %>%
        as.matrix
#      browser(expr=i==10217)
#      print(i)
    }

#     spGeom@polygons <- lapply(1:length(spGeom@polygons), function(i) {
#       spGeom@polygons[[i]]@Polygons <- lapply(1:length(spGeom@polygons[[i]]@Polygons), function(j) {
#         spGeom@polygons[[i]]@Polygons[[j]]@coords <-
#                              mat[, c("x","y")]
#         return(spGeom@polygons[[i]]@Polygons[[j]])
#
#       })
#       return(spGeom@polygons[[i]])
#     })
#
#     spGeom@polygons <- lapply(spGeom@polygons, function(i) {
#      i@Polygons <- lapply(i@Polygons, function(j) {
#        j@coords <- j@coords[fastshp::thin(j@coords[,1],j@coords[,2], method=method, tol=tol),]
#        return(j)
#      })
#      return(i)
#  })

 # matThin <- fastshp::thin(mat[,1],mat[,2], method=2L, tol=1e3, id = TRUE)


  return(spGeom)
})

#' @export
#' @rdname thin
setMethod(
  "thin",
  signature="SpatialLines",
  definition=function(spGeom, tol, method) {
    spGeom@lines <- lapply(spGeom@lines, function(i) {
      i@Lines <- lapply(i@Lines, function(j) {
        tmp <- fastshp::thin(j@coords[,1],j@coords[,2], method=method, tol=tol)
        tmp[c(1,length(tmp))] <- TRUE
        j@coords <-   j@coords[tmp,]
        return(j)
      })
      return(i)
    })

    return(spGeom)
  })
#
# hole <- lapply(1:length(sp), function(x) {
#   lapply(sp@polygons[[x]]@Polygons, function(x)
#     x@hole)
# }) %>%
#   unlist
#
# ord <- sp@plotOrder
#
# ordInner <- lapply(1:length(sp), function(x) {
#   sp@polygons[[x]]@plotOrder
# })
#
# xyOrd.l <- lapply(ord, function(i) {
#   xy[[i]][ordInner[[i]]]
# })
#
# idLength <- lapply(xyOrd.l, function(i) { lapply(i, NROW) }) %>%
#   unlist %>%
# #  `/`(., 2) %>%
#   data.table(V1 = .)
#
# idLength2 <- lapply(xyOrd.l, function(i) { sapply(i, NROW) }) %>%
#   lapply(., function(i) rep(1:length(i), i)) %>%
#   unlist %>%
#   data.table(group2=.)
#
# idLength3 <- sapply(1:length(xyOrd.l), function(i) {
#   rep(i, NROW(xyOrd.l[[i]]))}) %>% unlist %>%
#   data.table(group=.)
#
# #   lapply(., function(i) rep(1:length(i), i)) %>%
# #   unlist %>%
# #   data.table(group2=.)
#
# # lapply(xyOrd.l, function(i) sapply(i, NROW) %>% rep(1:length(i), .))
# # idL <- unlist(idLength2)
# # rep(1:NROW(xyOrd.l), ))
#
#
# # spRes <- lapply(1:length(xyOrd.l), function(i) {
# #   Polygons(lapply(xyOrd.l[[i]], Polygon), as.character(i))}) %>%
# #   SpatialPolygons(., 1:length(xyOrd.l))
#
# xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) { do.call(rbind, i) }))
#
# thinned <- data.table(
#   thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
#                        tolerance = speedupScale * speedup,
#                        id=rep(1:length(idLength$V1), idLength$V1))
# )
# thinned[, `:=`(groups= rep(idLength3$group, idLength$V1),
#                groups2=idLength2$group2,
#                #groupd3=cumsum(idLength2$group2),
#                id=rep(1:length(idLength$V1), idLength$V1))]
# idLength <- thinned[, sum(thin),by = list(groups, groups2,id)]
# xyOrd2 <- xyOrd[thinned$thin, ]
#
#
# stopIndex <- cumsum(idLength$V1)
# startIndex <- c(1,cumsum(idLength$V1)+1)
# startIndex <- startIndex[-length(startIndex)]
# #browser()
# a = lapply(1:length(startIndex), function(x){
#   Polygon(matrix(xyOrd[startIndex[x]:stopIndex[x],],ncol=2))
# })
# b = lapply(1:max(idLength$groups), function(x)
#   Polygons(a[idLength[groups==x,id]], as.character(x))) %>%
#   SpatialPolygons(., 1:max(idLength$groups))
# return(invisible(b))
#}

