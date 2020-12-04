
# Wrap the cpp-based EM functionality and provide fix-point functions. The EM
# has reached a fixed point, if the log-likelihood does not improve
# significantly anymore.

library("tictoc")
library (Rcpp)

source("./R/classes.R")

sourceCpp(file = "./src/em.cpp", cacheDir = "./.cacheDir")



# run the EM until a fixed point is reached, requires the threshold for the LL
# function, the number of clusters to try to fit, as well as the flow data
# object, to work on.
#
# TODO have tictoc write to a file with additional information. Develop perf.R
# module for this.
#

#' @export
iterateEM <- function (deltaThreshold, numClusters, flowData, verbose=FALSE)
{
  em <- emInit (numClusters = numClusters, flowData = flowData)
  em <- iterateInitedEM (em=em, deltaThreshold=deltaThreshold, numCluster=numClusters, flowData=flowData, verbose=verbose)
  return (em)
}



#' @export
iterateInitedEM <- function (em,deltaThreshold, numClusters, flowData,verbose=FALSE)
{
  tic(msg="timing iterateEM")
  if (verbose) {
    cat (sprintf("Starting EM with threshold %.4f threshold, %d clusters\n", deltaThreshold, numClusters))
    cat (sprintf("Iteration       Δ LL\n"))
  }
  stepDelta <- Inf
  iteration <- 0
  while ((iteration<1000) && (iteration < 11 || abs(stepDelta) > deltaThreshold))
  {
    prevLL <- em@logL
    emNew <- emStep (em, flowData, iteration)
    if (! (is.nan(emNew@logL)))
    {
      em<-emNew
    }
    curLL <- em@logL
    stepDelta <- curLL - prevLL
    ws_ <- c(em@clusterProbs, 0, 0, 0)
    weights <-sprintf("%4.3f %4.3f %4.3f ...",ws_[1], ws_[2], ws_[3])
    poss <- "" # sprintf("%020s", toString (em@mu))
    if (verbose && (iteration %% 10 == 0)) {
      cat (sprintf("%5d %14.4f    %s %s\n", iteration, stepDelta, weights, poss))
    }
    iteration <- iteration +1
  }
  if (verbose) {
    cat (sprintf("%5d %14.4f\n", iteration, stepDelta))
  }
  toc(quiet = !verbose)
  return (em)
}





# the first EM step to set everything up

#' @export
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

#' @export
emInitWithPrior <- function (emOld, flowData)
{
  em <- mkEMRun()
  return (emCommon(em, flowData, emOld@weight, emOld@mu))
}



# a single EM step

#' @export
emStep <- function (em,flowData, iteration)
{
  # calculate mu based on cluster-weight for each data point, and actual data
  # points
  mu<-eigenMu(em@weight,flowData@sampled)
  # CHZS fixing cluster 1
  xmean <- mean(flowData@sampled[,1])
  ymean <- mean(flowData@sampled[,2])
  mu[,1] <- c(xmean,ymean)
  # CHZS
  return (emCommon(em, flowData, em@weight, mu, iteration))
}



# shared between init/step

#' @export
emCommon <- function(em, flowData, weight, mu, iteration=100)
{
  itr <- 0.1 * min(1000, max (10,iteration))
  # clamp <- max(1, maxIteration / (max(1,iteration)))
  #clamp <- (itr / (1+itr))^2 # [0.25 .. 1.0]
  clamp <- (0.5 + (itr / (1+itr))) # [1.0 .. 1.5]
  # average cluster probability, averaged over all samples
  clusterProbs    <- eigenMeanClusterProb(weight)
  # calculate the sample covariance matrices, one for each cluster, as
  # sigma[1]...sigma[n]
  sigma           <- eigenSigma(weight,mu,flowData@sampled)

  # CHZS and CB
  clusterProbs[[1]] <- max (0.01, clusterProbs[[1]]) # at least 1% background
  sigmaclamped <- lapply(sigma, function(s) {
    variance1 <- s[1,1] # sigma_1 ^2
    sigma1 <- sqrt(variance1)
    c <- s[1,2] # = s[2,1] = rho*sigma_1 * sigma_2 with rho in [-1,1]
    variance2 <- s[2,2] # sigma_2 ^2
    sigma2 <- sqrt(variance2)
    if(variance1 >= 100 && variance2 >= 100)
    {
      rho <- c/(sigma1*sigma2) # only in this case all the values are fine
      if(rho < -0.95){rho <- -0.95}
      if(rho > 0.95){rho <- 0.95}
    }
    if(variance1 < 100) # then we have no clue about rho
    {
      sigma1 <- 10
      rho <- 0 # assume no correlation
    }
    if(variance2 < 100) # then we have no clue about rho
    {
      sigma2 <- 10
      rho <- 0 # assume no correlation
    }
    if(sigma1 > 25000){sigma1 <- 25000} # limit sigma1
    if(sigma2 > 25000){sigma2 <- 25000} # limit sigma2
    variance1 <- sigma1^2
    variance2 <- sigma2^2
    c <- rho*sigma1*sigma2
    t <- matrix(c(variance1,c,c,variance2),nrow=2,ncol=2)
    return (t)
  }) # for each sigma
  #  the background cluster stays the same in each iteration
  sigmaclamped[[1]] <- 25000^2 * matrix(c(1,0,0,1), nrow=2, ncol=2)
  # # CHZS and CB

  emNew <- emDensitiesLogL (em, flowData, mu, sigmaclamped, clusterProbs)
  return (emNew)
}

#' @export
emDensitiesLogL <- function (em, flowData, mu, sigma, clusterProbs)
{
  densities       <- eigenDensitiesAtSamples(clusterProbs ,mu,sigma,flowData@sampled)
  logL            <- eigenLogLikelihood(densities) #compute log likelihood
  normedDensities <- eigenRowNormalize(densities)
  return (updateEMRun(em=em, mu=mu, sigma=sigma, weight=normedDensities, clusterProbs=clusterProbs, logL=logL))
}

