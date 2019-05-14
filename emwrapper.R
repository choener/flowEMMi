
# Wrap the cpp-based EM functionality and provide fix-point functions. The EM
# has reached a fixed point, if the log-likelihood does not improve
# significantly anymore.

source("classes.R")

sourceCpp(file = "./em_fast_sigma.cpp", cacheDir = "./.cacheDir")



# run the EM until a fixed point is reached, requires the threshold for the LL
# function, the number of clusters to try to fit, as well as the flow data
# object, to work on.

iterateEM <- function (deltaThreshold, numClusters, fdo)
{
  em <- emInit (numClusters = numClusters, fdo = fdo)
  stepDelta <- Inf
  while (stepDelta > deltaThreshold)
  {
    prevLL <- em@logL
    em <- emStep (em, fdo)
    curLL <- em@logL
    stepDelta <- curLL - prevLL
  }
}



# the first EM step to set everything up

emInit <- function (numClusters, fdo)
{
  em <- mkEMRun()
  # draw random pi weights, for each row (data point), and each cluster, draw a
  # dirichlet-distributed weight
  dcw <- rdirichlet(nrow(fdo@data),rep(1,numClusters))
  # draw random start positions from the given data points
  start<-fdo@data[sample(nrow(fdo@data),size=numClusters,replace=FALSE),]
  mu<-t(start)
  return (emShared(em, fdo, dcw, mu))
}



# a single EM step

emStep <- function (em,fdo)
{
  mu<-eigenMu(P_mat,fdo@data)
  return (emShared(em, fdo, P_mat, mu))
}



# shared between init/step

emShared <- function(em, fdo, dcw, mu)
{
  clusterProbs <- eigenMeanClusterProb(dcw)
  sigma<-eigenSigma(P_mat,mu,fdo@data)
  T<-calc_T(clusterProbs ,mu,sigma,fdo@data)
  logL <- eigenLogLikelihood(T) #compute log likelihood
  P_mat<-calc_Pmat(T)
  return (updateEMRun(em, mu, sigma, logL))
}

