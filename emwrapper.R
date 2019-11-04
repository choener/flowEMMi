
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

iterateEM <- function (deltaThreshold, numClusters, flowData, verbose=FALSE)
{
  em <- emInit (numClusters = numClusters, flowData = flowData)
  em <- iterateInitedEM (em=em, deltaThreshold=deltaThreshold, numCluster=numClusters, flowData=flowData, verbose=verbose)
  return (em)
}



iterateInitedEM <- function (em,deltaThreshold, numClusters, flowData,verbose=FALSE)
{
  tic(msg="timing iterateEM")
  if (verbose) {
    cat (sprintf("Starting EM with threshold %.4f threshold, %d clusters\n", deltaThreshold, numClusters))
    cat (sprintf("Iteration       ?? LL\n"))
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
  cat (sprintf("%5d %14.4f\n", iteration, stepDelta))
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

emStep <- function (em,flowData, iteration)
{
  # calculate mu based on cluster-weight for each data point, and actual data
  # points
  mu<-eigenMu(em@weight,flowData@sampled)
  # CHZS fixing cluster 1
  # hier aendern
    xmean <- mean(flowData@sampled[,1])
    ymean <- mean(flowData@sampled[,2])
    mu[,1] <- c(xmean,ymean)
  # CHZS
  return (emCommon(em, flowData, em@weight, mu, iteration))
}



# shared between init/step

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
  # CHZS
  # hier aendern
  clusterProbs[[1]] <- max (0.01, clusterProbs[[1]]) # at least 1% background
  # apply the inline function on each covariance matrix in the list sigma
  sigmaclamped <- lapply(sigma, function(s) {
                         x <- s[[1,1]]
                         x <- max(1,min(x, (2500*clamp)^2))
                         y <- s[[2,2]]
                         y <- max(1,min(y, (2500*clamp)^2))
                         c <- s[[1,2]]
                         t <- matrix(c(x,c,c,y),nrow=2,ncol=2)
                         # for each covariance matrix s, run the inline function over each scalar.
                         #t <- apply(s, 1:2, function(v) {
                         #  sign(v) * min(abs(v),(2500*clamp)^2)
                         #})
                         return (t)
    })
  # hier aendern (entfernen)
  sigmaclamped[[1]] <- 25000^2 * matrix(c(1,0,0,1), nrow=2, ncol=2)
  # CHZS
  emNew <- emDensitiesLogL (em, flowData, mu, sigmaclamped, clusterProbs)
  return (emNew)
}

emDensitiesLogL <- function (em, flowData, mu, sigma, clusterProbs)
{
  densities       <- eigenDensitiesAtSamples(clusterProbs ,mu,sigma,flowData@sampled)
  background <- clusterProbs[1]
  logL            <- eigenLogLikelihood(densities, backgroundProportion = background) #compute log likelihood
  # + background hier
  normedDensities <- eigenRowNormalize(densities)
  if (is.nan(logL)) {
    cat(sprintf("NaN in loglikelihood calculation, which should not happen anymore!"))
    exit()
  }
  return (updateEMRun(em=em, mu=mu, sigma=sigma, weight=normedDensities, clusterProbs=clusterProbs, logL=logL))
}

