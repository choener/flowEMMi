#!/usr/bin/env Rscript

# Start this program via @./em.R@ (in the directory of the program)

# TODO modify sourceCpp to includes the full directory we are in
# TODO consider using rPackages.littler

library (flowCore)
library (flowViz)
library (ggplot2)
library (gtools)
library (mixtools)
library (mvtnorm)
library (optparse)
library (colortools)
library (Rcpp)

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
parser <- add_option (parser, c ("--fraction"), type="double", default = 0.02, help="")
parser <- add_option (parser, c ("--fracmult"), type="integer", default = 5, help="")
parser <- add_option (parser, c ("--prior"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--separation"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--inits"), type="integer", default = 10, help="")
parser <- add_option (parser, c ("--log"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--alpha"), type="double", default = 0.5, help="")
parser <- add_option (parser, c ("--imgformat"), type="character", default = "png", help="")
parser <- add_option (parser, c ("--startcluster"), type="integer", default = 2, help="")
parser <- add_option (parser, c ("--endcluster"), type="integer", default = 20, help="")

opts <- parse_args(parser)




# run the sampling function

flowEMMi_sample<-function( frame, ch1="FS.Log", ch2="FL.4.Log"
                         , x_start=0, x_end=4095,y_start=700,y_end=4095
                         ,use_log=TRUE,diff.ll=1
                         ,fraction=0.02
                         ,fractmult=5
                         ,start_cluster=8,end_cluster=15,prior=FALSE
                         ,pi_prior,mu_prior,sigma_prior,separation=TRUE,max_inits=5,total=FALSE,alpha=.05,img_format="png",verbose=TRUE)
{
  #mat<-exprs(frame)

  # the full flow data
  fdo <- mkFlowDataObject(frame=frame,xChannel=ch1, yChannel=ch2)

  pd <- mkFractionedFlowData (fdo=fdo
                             ,fraction = fraction
                             ,xMin=x_start, xMax=x_end
                             ,yMin=y_start, yMax=y_end)
  dimensions <- pd@data
  dimensionsSample <- pd@sampled

  plotInputData(pd@sampled, fraction=pd@fraction, logScaled = use_log, imageFormat = img_format)
  n<-nrow(dimensionsSample)

  BIC<-rep(0,end_cluster)
  #palette <- distinctColorPalette(end_cluster)
  palette <- wheel ("steelblue", num = end_cluster)
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
  # parLapply (cluster, start_cluster:end_cluster, functionToCall)

  for(c in start_cluster:end_cluster)
  {
    #ll[c][1]<-0
    #counter<-2
    for (iteration in 1:max_inits)
    {
      # initialize the EM with lowest count of elements
      em <- iterateEM (deltaThreshold = 0.01, numClusters = c, flowDataObj = pd@sampled)
      # increase element count
      # for each initialization, run the algorithm
      if (prior) {
        # this variant assumes that we collected start points from somewhere else
        # TODO re-use iterateEM ...
      } else {
        # this variant randomly selects start points for the EM algorithm
      }
    } # initiations
  } # for start_cluster ... end_cluster
  plotBIC (newList$BIC)
  statBIC (newList$BIC)
  for(c in start_cluster:end_cluster)
  {
    statCluster(newList$mu[[c]], newList$sigma[[c]])
  }
} # flowEMMi_sample


# load sample
fcsData <- read.FCS(opts$file,alter.names = TRUE,transformation = FALSE)
# run actual flowEMMi algorithm
results <- flowEMMi_sample( frame = fcsData
                          , ch1=opts$channelx, ch2=opts$channely
                          , x_start = opts$xstart, x_end = opts$xend, y_start=opts$ystart, y_end=opts$yend
                          , fraction = opts$fraction
#                          , fracmult = opts$fracmult
                          , prior = opts$prior
                          , separation = opts$separation
                          , max_inits = opts$inits
                          , use_log = opts$log
                          , alpha = opts$alpha, img_format = opts$imgformat
                          , start_cluster = opts$startcluster, end_cluster = opts$endcluster
                          )

