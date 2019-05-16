
# Wrap the cpp-based EM functionality and provide fix-point functions. The EM
# has reached a fixed point, if the log-likelihood does not improve
# significantly anymore.

source("classes.R")

sourceCpp(file = "./em_fast_sigma.cpp", cacheDir = "./.cacheDir")



# run the EM until a fixed point is reached, requires the threshold for the LL
# function, the number of clusters to try to fit, as well as the flow data
# object, to work on.

iterateEM <- function (deltaThreshold, numClusters, flowDataObj)
{
  cat (sprintf("Starting EM with threshold %.4f threshold, %d clusters\n", deltaThreshold, numClusters))
  cat (sprintf("Iteration       Δ LL\n"))
  em <- emInit (numClusters = numClusters, flowDataObj = flowDataObj)
  stepDelta <- Inf
  iteration <- 0
  while (stepDelta > deltaThreshold)
  {
    prevLL <- em@logL
    em <- emStep (em, flowDataObj)
    curLL <- em@logL
    stepDelta <- curLL - prevLL
    cat (sprintf("%5d %14.4f\n", iteration, stepDelta))
    iteration <- iteration +1
  }
}



# the first EM step to set everything up

emInit <- function (numClusters, flowDataObj)
{
  em <- mkEMRun()
  # draw random pi weights, for each row (data point), and each cluster, draw a
  # dirichlet-distributed weight.
  sampleClusterWeight <- rdirichlet(nrow(flowDataObj@data),rep(1,numClusters))
  # draw random start positions from the given data points
  start<-flowDataObj@data[sample(nrow(flowDataObj@data),size=numClusters,replace=FALSE),]
  # transpose... (rows: dimensions (2), named; cols: clusters(numClusters))
  # PMT.1 2000 2300 ...
  # PMT.9 4000 9000 ...
  mu<-t(start)
  # run the common part of the em algorithm
  return (emCommon(em, flowDataObj, sampleClusterWeight, mu))
}



# a single EM step

emStep <- function (em,flowDataObj)
{
  # calculate mu based on cluster-weight for each data point, and actual data
  # points
  mu<-eigenMu(em@weight,flowDataObj@data)
  return (emCommon(em, flowDataObj, em@weight, mu))
}



# shared between init/step

emCommon <- function(em, flowDataObj, weight, mu)
{
  # average cluster probability, averaged over all samples
  clusterProbs    <- eigenMeanClusterProb(weight)
  # calculate the sample covariance matrices, one for each cluster, as
  # sigma[1]...sigma[n]
  sigma           <- eigenSigma(weight,mu,flowDataObj@data)
  densities       <- eigenDensitiesAtSamples(clusterProbs ,mu,sigma,flowDataObj@data)
  logL            <- eigenLogLikelihood(densities) #compute log likelihood
  normedDensities <- eigenRowNormalize(densities)
  return (updateEMRun(em=em, mu=mu, sigma=sigma, weight=normedDensities, logL=logL))
}

