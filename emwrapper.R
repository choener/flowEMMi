
# Wrap the cpp-based EM functionality and provide fix-point functions. The EM
# has reached a fixed point, if the log-likelihood does not improve
# significantly anymore.

library("tictoc")

source("classes.R")

sourceCpp(file = "./em_fast_sigma.cpp", cacheDir = "./.cacheDir")



# run the EM until a fixed point is reached, requires the threshold for the LL
# function, the number of clusters to try to fit, as well as the flow data
# object, to work on.
#
# TODO have tictoc write to a file with additional information. Develop perf.R
# module for this.

iterateEM <- function (deltaThreshold, numClusters, flowData)
{
  em <- emInit (numClusters = numClusters, flowData = flowData)
  em <- iterateInitedEM (em=em, deltaThreshold=deltaThreshold, numCluster=numClusters, flowData=flowData)
  return (em)
}



iterateInitedEM <- function (em,deltaThreshold, numClusters, flowData)
{
  tic(msg="timing iterateEM")
  cat (sprintf("Starting EM with threshold %.4f threshold, %d clusters\n", deltaThreshold, numClusters))
  cat (sprintf("Iteration       Δ LL\n"))
  stepDelta <- Inf
  iteration <- 0
  while (stepDelta > deltaThreshold)
  {
    prevLL <- em@logL
    emNew <- emStep (em, flowData)
    if (! (is.nan(emNew@logL)))
    {
      em<-emNew
    }
    curLL <- em@logL
    stepDelta <- curLL - prevLL
    ws_ <- c(em@clusterProbs, 0, 0, 0)
    weights <-sprintf("%4.3f %4.3f %4.3f ...",ws_[1], ws_[2], ws_[3])
    poss <- "" # sprintf("%020s", toString (em@mu))
    cat (sprintf("%5d %14.4f    %s %s\n", iteration, stepDelta, weights, poss))
    iteration <- iteration +1
  }
  toc()
  return (em)
}





# the first EM step to set everything up

emInit <- function (numClusters, flowData)
{
  em <- mkEMRun()
  # draw random pi weights, for each row (data point), and each cluster, draw a
  # dirichlet-distributed weight.
  sampleClusterWeight <- rdirichlet(nrow(flowData@sampled),rep(1,numClusters))
  # draw random start positions from the given data points
  start<-flowData@sampled[sample(nrow(flowData@sampled),size=numClusters,replace=FALSE),]
  # transpose... (rows: dimensions (2), named; cols: clusters(numClusters))
  # PMT.1 2000 2300 ...
  # PMT.9 4000 9000 ...
  mu<-t(start)
  # run the common part of the em algorithm
  return (emCommon(em, flowData, sampleClusterWeight, mu))
}



# initialize from prior em

emInitWithPrior <- function (emOld, flowData)
{
  em <- mkEMRun()
  return (emCommon(em, flowData, emOld@weight, emOld@mu))
}



# a single EM step

emStep <- function (em,flowData)
{
  # calculate mu based on cluster-weight for each data point, and actual data
  # points
  mu<-eigenMu(em@weight,flowData@sampled)
  return (emCommon(em, flowData, em@weight, mu))
}



# shared between init/step

emCommon <- function(em, flowData, weight, mu)
{
  # average cluster probability, averaged over all samples
  clusterProbs    <- eigenMeanClusterProb(weight)
  # calculate the sample covariance matrices, one for each cluster, as
  # sigma[1]...sigma[n]
  sigma           <- eigenSigma(weight,mu,flowData@sampled)
  densities       <- eigenDensitiesAtSamples(clusterProbs ,mu,sigma,flowData@sampled)
  logL            <- eigenLogLikelihood(densities) #compute log likelihood
  normedDensities <- eigenRowNormalize(densities)
  return (updateEMRun(em=em, mu=mu, sigma=sigma, weight=normedDensities, clusterProbs=clusterProbs, logL=logL))
}

