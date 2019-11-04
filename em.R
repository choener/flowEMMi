#!/usr/bin/env Rscript

# Start this program via @./em.R@ (in the directory of the program)

# TODO modify sourceCpp to includes the full directory we are in
# TODO consider using rPackages.littler

library (colortools)
library (flowCore)
library (flowViz)
library (ggplot2)
library (gtools)
library (mixtools)
library (mvtnorm)
library (optparse)
library (parallel)
library (Rcpp)
library (tictoc)

source("classes.R")
source("plotting.R")
source("writestats.R")
source("emwrapper.R")

parser <- OptionParser ()
parser <- add_option (parser, c ("-v", "--verbose"), action = "store_true", default=FALSE, help="be very verbose")
parser <- add_option (parser, c ("-f", "--file"), type="character", default = "", help="file to process")
parser <- add_option (parser, c ("-x", "--channelx"), type="character", default = "PMT.1", help="first channel")
parser <- add_option (parser, c ("-y", "--channely"), type="character", default = "PMT.9", help="second channel")
parser <- add_option (parser, c ("--xstart"), type="integer", default = 1500, help="")
parser <- add_option (parser, c ("--xend"), type="integer", default = 40000, help="")
parser <- add_option (parser, c ("--ystart"), type="integer", default = 5000, help="")
parser <- add_option (parser, c ("--yend"), type="integer", default = 38000, help="")
parser <- add_option (parser, c ("--convergence"), type="double", default = 0.01, help="")
parser <- add_option (parser, c ("--initfraction"), type="double", default = 0.01, help="")
parser <- add_option (parser, c ("--finalfraction"), type="double", default = 1.0, help="")
parser <- add_option (parser, c ("--prior"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--separation"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--inits"), type="integer", default = 10, help="")
parser <- add_option (parser, c ("--log"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--alpha"), type="double", default = 0.5, help="")
parser <- add_option (parser, c ("--imgformat"), type="character", default = "png", help="")
parser <- add_option (parser, c ("--mincluster"), type="integer", default = 2, help="")
parser <- add_option (parser, c ("--maxcluster"), type="integer", default = 5, help="")
parser <- add_option (parser, c ("--clusterbracket"), type="integer", default = 3, help="")
parser <- add_option (parser, c ("--disableparallelism"), action="store_true", default = FALSE, help="")

opts <- parse_args(parser)



# BIC criterion

bic <- function (em) {
  numPoints <- nrow(em@weight)
  numParams <- (ncol(em@mu)-1) * 6 # 2 mu, 3 sigma, 1 weight # without background distribution
  logL <- em@logL
  # ln(n)*k - 2ln(LL), but we still want to maximize
  bic <- logL - 0.5 * (log(numPoints) * numParams)
  return(bic)
}

# run the sampled algorithm
#
# Finds for a given number of clusters the best initialization based on
# log-likelihood.

flowEMMiSampled<-function ( flowDataObject, initFraction, inits, numClusters, useLogScale, imageFormat, xMin, xMax, yMin, yMax, epsilon, verbose=FALSE)
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

# Run the flowEMMi algorithm on the full input data.

flowEMMiFull<-function ( em, flowDataObject
                        , finalFraction
                        , numClusters
                        , xMin, xMax, yMin, yMax
                        , epsilon
                        , verbose=TRUE
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

# run the complete flowEMMi algorithm

flowEMMi<-function( frame, ch1="FS.Log", ch2="FL.4.Log"
                         , xMin=0, xMax=4095,yMin=700,yMax=4095
                         ,useLogScale=TRUE,diff.ll=1
                         ,initFraction=0.01
                         ,finalFraction=1.0
                         ,minClusters=8,maxClusters=15,clusterbracket=3
                         ,prior=FALSE
                         ,pi_prior,mu_prior,sigma_prior,separation=TRUE,numberOfInits=5,total=FALSE,alpha=.05,imageFormat="png",verbose=TRUE
                         ,disableParallelism=TRUE
                         ,convergenceEpsilon=0.01 )
{
  #mat<-exprs(frame)
  stopifnot (initFraction >  0.0)

  # assert that we have data in the channels
  stopifnot ( ch1 %in% colnames(frame) )
  stopifnot ( ch2 %in% colnames(frame) )

  # need at least two clusters, because we need background
  stopifnot ( minClusters>1 )

  # the full flow data
  fdo <- mkFlowDataObject(frame=frame,xChannel=ch1, yChannel=ch2)

  # setup parallelism
  numCores <- if (disableParallelism) {1} else {max(1, detectCores())}

  # run for each number of clusters
  tic(msg="sampled subset EM")
  parSampled <- function (c)
  {
    em <- flowEMMiSampled( flowDataObject=fdo, initFraction=initFraction, inits=numberOfInits
                         , numClusters=c, useLogScale=useLogScale, imageFormat=imageFormat
                         , xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax
                         , epsilon=convergenceEpsilon / (max (sqrt(initFraction), 1.0))
                         , verbose=T)
    return (em)
  }
  ems<-mclapply (minClusters:maxClusters, parSampled, mc.cores=numCores)
  toc()

  # find best number of clusters
  tic(msg="finding best LL in ems")
  bestLL <-1
  for (b in 1:length(ems))
  {
    # if (is.null(bestLL) || ems[[bestLL]]@logL < ems[[b]]@logL)
    if (bic(ems[[bestLL]]) < bic(ems[[b]]))
    {
      bestLL <- b
    }
  }
  print(length(ems))
  print(bestLL)
  print(bic(ems[[bestLL]]))
  toc()
  emsbestll <- ems[[bestLL]]
  tic(msg="plotting sampled took ...")
  plotInputData(emsbestll@data[[1]], labels = getLabels(emsbestll), mu=emsbestll@mu, sigma=emsbestll@sigma,
                logScaled = useLogScale, imageFormat = imageFormat, prefix=sprintf("sampled-%d-labels", bestLL)
  )
  toc()

  # around best number of clusters, run flowEMMi again
  tic(msg="full EM run on best subset of clusters")
  minStart <- max(1,bestLL-clusterbracket)
  maxStart <- min(length(ems),bestLL+clusterbracket)
  parFull <- function (idx)
  {
    c <- minClusters + idx - 1
    cat(sprintf("full calculation with %d clusters, idx %d\n",c,idx))
    em_<-ems[[idx]]
    # set up labels in a 1-step em
    # em <- flowEMMiFull( em=em_, flowDataObject=fdo,
    #                   , finalFraction=finalFraction
    #                   , numClusters=c
    #                   , xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax
    #                   , epsilon=(100 * convergenceEpsilon)
    #                   , verbose=TRUE
    #                   )
    pd <- mkFractionedFlowData (fdo=fdo
                               ,fraction = finalFraction
                               ,xMin=xMin, xMax=xMax
                               ,yMin=yMin, yMax=yMax)
    tic(msg="rederived densities")
    emI <- emInitWithPrior (em=em_, flowData=pd)
    em <- emDensitiesLogL(em=emI, flowData=pd, mu=em_@mu, sigma=em_@sigma, clusterProbs=em_@clusterProbs)
    em@data<-list(pd)
    pre <- getLabels(em)
    toc()
    em <- flowEMMiFull( em=em, flowDataObject=fdo,
                      , finalFraction=finalFraction
                      , numClusters=c
                      , xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax
                      , epsilon=convergenceEpsilon
                      , verbose=TRUE
                      #, useLogScale=useLogScale, imageFormat=imageFormat
                      )
    ls <- getLabels(em)
    tic(msg="crosses")
    crosses <- table(pre,ls)
    print(crosses)
    print(table(ls))
    print(em@mu)
    toc()
    write(ls, file=sprintf("label_assignment_%d.dat",c-1))
    return (em)
  }
  emsFull<-mclapply (minStart:maxStart, parFull, mc.cores=numCores)
  toc()

  # TODO find best LL
  tic(msg="finding and plotting best LL took ...")
  lls <- lapply(emsFull, bic)
  llmax <- which.max (lls) + minStart-1
  bestem <- emsFull[[which.max(lls)]]
  write(getLabels(bestem), file=sprintf("best_%d_labels_%f_logL.dat", llmax-1, bestem@logL))

  # plot bestem data
  plotInputData(bestem@data[[1]], labels = getLabels(bestem), mu=bestem@mu, sigma=bestem@sigma,
                logScaled = useLogScale, imageFormat = imageFormat, prefix=sprintf("best-%d-labels",llmax-1)
  )
  toc()

  # relabel data, in particular if we have inclusion/exclusion boxes
  flowEMMiRelabel <- function (em_,c) {
    print("starting relabel")
    pd <- mkFractionedFlowData (fdo=fdo
                               ,fraction = 1.0 # finalFraction
                               ,xMin=0, xMax=100000
                               ,yMin=0, yMax=100000)
    emI <- emInitWithPrior (em=em_, flowData=pd)
    em <- emDensitiesLogL(em=emI, flowData=pd, mu=em_@mu, sigma=em_@sigma, clusterProbs=em_@clusterProbs)
    em@data<-list(pd)
    #em <- flowEMMiFull( em=em_, flowDataObject=fdo,
    #                  , finalFraction=finalFraction
    #                  , numClusters=c
    #                  , xMin=0, xMax=100000
    #                  , yMin=0, yMax=100000
    #                  #, xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax
    #                  , epsilon=10000 # * convergenceEpsilon
    #                  #, useLogScale=useLogScale, imageFormat=imageFormat
    #                  #, verbose=T
    #                  )
    return(em)
  } # relabelling
  tic(msg="rerun with full relabelling")
  bestrel <- flowEMMiRelabel(bestem,llmax)
  toc()
  tic(msg="plotting best relabelled took ...")
  write(getLabels(bestrel), file=sprintf("best_relabel_%d_labels_%f_logL.dat", llmax-1, bestrel@logL))
  plotInputData(bestrel@data[[1]], labels=getLabels(bestrel), mu=bestrel@mu, sigma=bestrel@sigma,
                logScaled = useLogScale, imageFormat = imageFormat, prefix=sprintf("relabel-%d-labels",llmax-1)
  )
  toc()
} # flowEMMi


# load sample
if (opts$file != "") {
  fcsData <- read.FCS(opts$file,alter.names = TRUE,transformation = FALSE)
  # run actual flowEMMi algorithm
  results <- flowEMMi( frame = fcsData
                     , ch1=opts$channelx, ch2=opts$channely
                     , xMin = opts$xstart, xMax = opts$xend, yMin=opts$ystart, yMax=opts$yend
                     , initFraction = opts$initfraction
                     , finalFraction = opts$finalfraction
                     , prior = opts$prior
                     , separation = opts$separation
                     , numberOfInits = opts$inits
                     , useLogScale = opts$log
                     , alpha = opts$alpha, imageFormat = opts$imgformat
                     , minClusters = opts$mincluster, maxClusters = opts$maxcluster, clusterbracket=opts$clusterbracket
                     , disableParallelism = opts$disableparallelism
                     , convergenceEpsilon = opts$convergence
                     )
}
