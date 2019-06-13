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
parser <- add_option (parser, c ("--initfraction"), type="double", default = 0.02, help="")
parser <- add_option (parser, c ("--prior"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--separation"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--inits"), type="integer", default = 10, help="")
parser <- add_option (parser, c ("--log"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--alpha"), type="double", default = 0.5, help="")
parser <- add_option (parser, c ("--imgformat"), type="character", default = "png", help="")
parser <- add_option (parser, c ("--startcluster"), type="integer", default = 2, help="")
parser <- add_option (parser, c ("--endcluster"), type="integer", default = 20, help="")

opts <- parse_args(parser)



# run the sampled algorithm
#
# Finds for a given number of clusters the best initialization based on
# log-likelihood.

flowEMMiSampled<-function ( flowDataObject, initFraction, inits, numClusters, useLogScale, imageFormat, xMin, xMax, yMin, yMax)
{
  em<-NULL
  # run inits with fraction of points
  for (i in 1:inits)
  {
    pd <- mkFractionedFlowData (fdo=flowDataObject
                               ,fraction = initFraction
                               ,xMin=xMin, xMax=xMax
                               ,yMin=yMin, yMax=yMax)
    em_ <- iterateEM (deltaThreshold = 0.01, numClusters=numClusters, flowData = pd)
    if (is.null(em) || em@logL < em_@logL)
    {
      em <- em_
      #tic(msg="plotting input data")
      #plotInputData(pd, logScaled = useLogScale, imageFormat = imageFormat)
      #toc()
    }
  } # i in inits
  return (em)
}

# Run the flowEMMi algorithm on the full input data.

flowEMMiFull<-function ( em, flowDataObject, numClusters, useLogScale, imageFormat, xMin, xMax, yMin, yMax, ...)
{
  pd <- mkFractionedFlowData (fdo=flowDataObject
                             ,fraction = 1.0
                             ,xMin=xMin, xMax=xMax
                             ,yMin=yMin, yMax=yMax)
  em <- emInitWithPrior (em=em, flowData=pd)
  em <- iterateInitedEM(em=em, deltaThreshold=0.01, numClusters=numClusters, flowData=pd)
  return (em)
}

# run the complete flowEMMi algorithm

flowEMMi<-function( frame, ch1="FS.Log", ch2="FL.4.Log"
                         , xMin=0, xMax=4095,yMin=700,yMax=4095
                         ,useLogScale=TRUE,diff.ll=1
                         ,initFraction=0.02
                         ,minClusters=8,maxClusters=15,prior=FALSE
                         ,pi_prior,mu_prior,sigma_prior,separation=TRUE,numberOfInits=5,total=FALSE,alpha=.05,imageFormat="png",verbose=TRUE)
{
  #mat<-exprs(frame)
  stopifnot (initFraction >  0.0)

  # the full flow data
  fdo <- mkFlowDataObject(frame=frame,xChannel=ch1, yChannel=ch2)

  # setup parallelism
  # numCores <- detectCores() # -1
  # cluster  <- makeCluster(numCores, type="FORK")

  # run for each number of clusters
  parSampled <- function (c)
  {
    em <- flowEMMiSampled( flowDataObject=fdo, initFraction=initFraction, inits=numberOfInits
                         , numClusters=c, useLogScale=useLogScale, imageFormat=imageFormat
                         , xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax)
    return (em)
  }
  ems<-lapply (minClusters:maxClusters, parSampled)

  # find best number of clusters
  bestLL <-NULL
  for (b in 1:length(ems))
  {
    if (is.null(bestLL) || ems[[bestLL]]@logL < ems[[b]]@logL)
    {
      bestLL <- b
    }
  }

  # around best number of clusters, run flowEMMi again
  parFull <- function (c)
  {
    idx<-c-max(minClusters,bestLL-3)+1
    em_<-ems[[idx]]
    em <- flowEMMiFull( em=em_, flowDataObject=fdo,
                      , numClusters=c, useLogScale=useLogScale, imageFormat=imageFormat
                      , xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax)
    return (em)
  }
  emsFull<-lapply (max(minClusters,bestLL-3):min(maxClusters,bestLL+3), parFull)

  error ()

  #for (c in minClusters:maxClusters)
  #{
  #  parFunc (c)
  #} # for c in clusters

  # for each fraction, run the flowEMMi algorithm
  for (f in fractions)
  {
    #pd <- mkFractionedFlowData (fdo=fdo
    #                           ,fraction = f
    #                           ,xMin=xMin, xMax=xMax
    #                           ,yMin=yMin, yMax=yMax)

    #plotInputData(pd, logScaled = useLogScale, imageFormat = imageFormat)

    BIC<-rep(0,maxClusters)
    palette <- wheel ("steelblue", num = maxClusters)
    act_T<-list()
    act_P_mat<-list()
    act_pi<-list()
    pis<-list()
    act_mu<-list()
    mus<-list()
    act_sigma<-list()
    sigmas<-list()
    act_loglik<-list()
    act_iterations<-list()
    probs<-list()
    ll<-list()
    newList<-list()

    # TODO parallelization (but consider what exactly to parallelize)
    #
    # numCores <- detectCores() # -1
    # cluster  <- makeCluster(numCores)
    # parLapply (cluster, minClusters:maxClusters, functionToCall)

    for(c in minClusters:maxClusters)
    {
      #ll[c][1]<-0
      #counter<-2
      for (iteration in 1:numberOfInits)
      {
        # initialize the EM with lowest count of elements
        em <- iterateEM (deltaThreshold = 0.01, numClusters = c, flowData = pd)
        # increase element count
        # for each initialization, run the algorithm
        if (prior) {
          # this variant assumes that we collected start points from somewhere else
          # TODO re-use iterateEM ...
        } else {
          # this variant randomly selects start points for the EM algorithm
        }
      } # initiations
    } # for minClusters ... maxClusters
    plotBIC (newList$BIC)
    statBIC (newList$BIC)
    for(c in minClusters:maxClusters)
    {
      statCluster(newList$mu[[c]], newList$sigma[[c]])
    }
  } # for f in fractions
} # flowEMMi


# load sample
fcsData <- read.FCS(opts$file,alter.names = TRUE,transformation = FALSE)
# run actual flowEMMi algorithm
results <- flowEMMi( frame = fcsData
                   , ch1=opts$channelx, ch2=opts$channely
                   , xMin = opts$xstart, xMax = opts$xend, yMin=opts$ystart, yMax=opts$yend
                   , initFraction = opts$initfraction
                   , prior = opts$prior
                   , separation = opts$separation
                   , numberOfInits = opts$inits
                   , useLogScale = opts$log
                   , alpha = opts$alpha, imageFormat = opts$imgformat
                   , minClusters = opts$startcluster, maxClusters = opts$endcluster
                   )

