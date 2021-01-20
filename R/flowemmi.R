
#' Run the flowEMMi algorithm (with reasonable defaults)
#'
#' Once this function has run to completion, the user can get the label for each data point using
#' @seealso [getLabels]. Once flowEMMI is done, both the internal data and weight information are
#' removed, as they use quite a bit of memory. data is of course the fdo object handed to the
#' function, while the weights can be recovered using @seealso [eigenDensitiesAtSamples] followed by
#' normaliziation using @seealso [eigenRowNormalize].
#'
#' @export
flowEMMi <- function ( fdo
                      , xMin=0, xMax=4095, yMin=700, yMax=4095
                      , useLogScale=TRUE, diff.ll=1
                      , initFraction=0.01
                      , finalFraction=1.0
                      , minClusters=3, maxClusters=20, clusterbracket=3
                      , numberOfInits=5, total=FALSE, imageFormat="png"
                      , verbose=F
                      , parallel=F
                      , convergenceEpsilon=0.01
) {
  stopifnot ( initFraction >  0.0
             , minClusters > 1
             )
  numCores <- if (parallel) {max(1, detectCores())} else {1}

  # run for each number of clusters
  tic(msg="sampled subset EM")
  parSampled <- function (c)
  {
    em <- flowEMMiSampled( flowDataObject=fdo, initFraction=initFraction, inits=numberOfInits
                         , numClusters=c, useLogScale=useLogScale, imageFormat=imageFormat
                         , xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax
                         , epsilon=convergenceEpsilon / (max (sqrt(initFraction), 1.0))
                         , verbose=verbose)
    return (em)
  }
  ems<-mclapply (minClusters:maxClusters, parSampled, mc.cores=numCores)
  toc(quiet= ! verbose)

  # find best number of clusters
  bestLL <-1
  for (b in 1:length(ems))
  {
    if (bic(ems[[bestLL]]) < bic(ems[[b]]))
    {
      bestLL <- b
    }
  }
  emsbestll <- ems[[bestLL]]

  # around best number of clusters, run flowEMMi again
  tic(msg="full EM run on best subset of clusters")
  minStart <- max(1,bestLL-clusterbracket)
  maxStart <- min(length(ems),bestLL+clusterbracket)
  parFull <- function (idx)
  {
    c <- minClusters + idx - 1
    if (verbose) {
      cat(sprintf("full calculation with %d clusters, idx %d\n",c,idx))
    }
    em_<-ems[[idx]]
    pd <- mkFractionedFlowData (fdo=fdo
                               ,fraction = finalFraction
                               ,xMin=xMin, xMax=xMax
                               ,yMin=yMin, yMax=yMax)
    emI <- emInitWithPrior (em=em_, flowData=pd)
    em <- emDensitiesLogL(em=emI, flowData=pd, mu=em_@mu, sigma=em_@sigma, clusterProbs=em_@clusterProbs)
    em@data<-list(pd)
    em <- flowEMMiFull( em=em, flowDataObject=fdo,
                      , finalFraction=finalFraction
                      , numClusters=c
                      , xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax
                      , epsilon=convergenceEpsilon
                      , verbose=verbose
                      )
    return (em)
  }
  emsFull<-mclapply (minStart:maxStart, parFull, mc.cores=numCores)
  toc(quiet= ! verbose)

  # kill "data" within emsFull
  #lapply(emsFull, function(e){ x <- e; x$data<-list(); return (x)})
  # kill weight matrix
  # TODO allow keeping with flag
  #lapply(emsFull, function(e){ x <- e; x$weight=matrix(); return (x)})

  # TODO hand bestem back to the user
  lls <- lapply(emsFull, bic)
  llmax <- which.max (lls) + minStart-1
  bestem <- emsFull[[which.max(lls)]]

  # relabel data, in particular if we have inclusion/exclusion boxes
  #flowEMMiRelabel <- function (em_,c) {
  #  pd <- mkFractionedFlowData (fdo=fdo
  #                             ,fraction = 1.0 # finalFraction
  #                             ,xMin=0, xMax=100000
  #                             ,yMin=0, yMax=100000)
  #  emI <- emInitWithPrior (em=em_, flowData=pd)
  #  em <- emDensitiesLogL(em=emI, flowData=pd, mu=em_@mu, sigma=em_@sigma, clusterProbs=em_@clusterProbs)
  #  em@data<-list(pd)
  #  return(em)
  #} # relabelling

  #tic(msg="rerun with full relabelling")
  #bestrel <- flowEMMiRelabel(bestem,llmax)
  #toc(quiet= ! verbose)

  return (list(best=bestem, all=emsFull))
} # flowEMMi



# Run the flowEMMi algorithm on the full input data.

flowEMMiFull<-function ( em, flowDataObject
                        , finalFraction
                        , numClusters
                        , xMin, xMax, yMin, yMax
                        , epsilon
                        , verbose=F
                        ,...
) {
  pd <- mkFractionedFlowData (fdo=flowDataObject
                             ,fraction = finalFraction
                             ,xMin=xMin, xMax=xMax
                             ,yMin=yMin, yMax=yMax)
  em <- emInitWithPrior (em=em, flowData=pd)
  em <- iterateInitedEM(em=em, deltaThreshold=epsilon, numClusters=numClusters, flowData=pd, verbose=verbose)
  em@data = list(pd)
  return (em)
}



#' run the sampled algorithm
#'
#' Finds for a given number of clusters the best initialization based on
#' log-likelihood.
#'
#' @export
flowEMMiSampled<-function ( flowDataObject, initFraction, inits, numClusters, useLogScale, imageFormat, xMin, xMax, yMin, yMax, epsilon, verbose=F)
{
  em<-NULL
  # run inits with fraction of points
  for (i in 1:inits)
  {
    pd <- mkFractionedFlowData (fdo=flowDataObject
                               ,fraction = initFraction
                               ,xMin=xMin, xMax=xMax
                               ,yMin=yMin, yMax=yMax)
    em_ <- iterateEM (deltaThreshold = epsilon, numClusters=numClusters, flowData = pd, verbose=verbose)
    if (is.null(em) || em@logL < em_@logL)
    {
      em <- em_
      em@data = list(pd)
    }
  } # i in inits
  return (em)
}



#' BIC criterion
#' @export
bic <- function (em) {
  numPoints <- nrow(em@weight)
  numParams <- (ncol(em@mu)-1) * 6 # 2 mu, 3 sigma, 1 weight # without background distribution
  logL <- em@logL
  # ln(n)*k - 2ln(LL), but we still want to maximize
  bic <- logL - 0.5 * (log(numPoints) * numParams)
  return(bic)
}

