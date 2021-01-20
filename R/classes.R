
requireNamespace("flowCore")
requireNamespace("mvtnorm")

# limits
setClass (Class="Limits", slots=c(channel="character", min="numeric", max="numeric"))

# creation function
mkLimits <- function(channel, vs)
{
  minvs <- min (vs)
  maxvs <- max (vs)
  return(new("Limits", channel=channel, min=minvs, max=maxvs))
}

limitsC <- function(l)
{
  return (c(l@min,l@max))
}



# a data object, including its limits
setClass (Class="FlowDataObject", slots=c(flowFrame="flowFrame", data="matrix", xChannel="character", yChannel="character", x="Limits", y="Limits"))

#' Creates a flow data object from a frame.
#'
#' This frame is typically the result of a call to read.FCS().
#'
#' @param frame FCS data frame.
#' @param xChannel "x"-channel to extract.
#' @param yChannel "y"-channel to extract.
#'
#' @return flow data object
#'
#' @export
mkFlowDataObject <- function(frame, xChannel, yChannel)
{
  data <- exprs(frame)
  xs <- data[,xChannel]
  ys <- data[,yChannel]
  return(new("FlowDataObject", flowFrame=frame, data=data, xChannel=xChannel, yChannel=yChannel
             , x=mkLimits(channel=xChannel,xs), y=mkLimits(channel=yChannel,ys)))
}



# input parameters, as a class
validFlowData <- function(object)
{
  #TODO we should check certain things
  TRUE
}
FlowData <- setClass (Class="FlowData", slots=c(data="FlowDataObject", sampled="matrix", fraction="numeric"), validity=validFlowData)

# create flow data object, including correct subsampling, etc
# fraction is the subsampling parameter, 0 < fraction <= 1
# note that the "sampled" structure retains only two dimensions

# TODO move denoised in own function, used by mkFlowDataObject

#' Sample a subset of data points
#'
#' This function subsamples a flow data object, given both a fraction to sample and xmin/xmax,
#' ymin/ymax values as limits.
#'
#' @param fdo flow data object
#' @param fraction fraction
#' @param xMin x-min
#' @param xMax x-max
#' @param yMin y-min
#' @param yMax y-max
#'
#' @return subsampled flow data object
#'
#' @export
mkFractionedFlowData <- function(fdo, fraction=1.0, xMin, xMax, yMin, yMax)
{
  # prepare subset extraction without border machine noise
  border <- list(c(xMin,xMax), c(yMin,yMax)) # define subset area
  names(border) <- c(fdo@xChannel, fdo@yChannel)
  denoised <- rectangleGate(filterId="Noise",  .gate = border) # filter noise
  denoised.subset <- Subset(fdo@flowFrame, denoised)
  denoisedData<-mkFlowDataObject(frame=denoised.subset, xChannel=fdo@xChannel, yChannel=fdo@yChannel)
  # subsample every nth element
  if (  length (denoisedData@data[,fdo@xChannel]) < 1
     || length (denoisedData@data[,fdo@yChannel]) < 1 ) {
    cat(sprintf("subset area selection leaves no elements left, modify xMin/xMax, yMin/yMax"))
    stop()
  }
  vs<-cbind(denoisedData@data[,fdo@xChannel],denoisedData@data[,fdo@yChannel]) #both dimensions as matrix
  # make sure to keep data ordered if no subsampled is requested
  if (fraction>=1) {
    subsampled<-vs
  } else {
    subsampled<-vs[sample(nrow(vs),size=nrow(vs) * fraction,replace=FALSE),]
  }
  colnames(subsampled) <- list(fdo@xChannel, fdo@yChannel)

  return (new("FlowData"
              , data=denoisedData
              , sampled=subsampled
              , fraction=fraction
              ))
}



# a single run of the EM algorithm with a given number of clusters
EMRun <- setClass (Class="EMRun", slots=c(mu="matrix", sigma="list", weight="matrix", clusterProbs="numeric", logL="numeric", data="list"
                                 ))

#' Creates an object of type EMRun
#'
#' @return A new object of type EMRun, with a matrix mu, covariance matrix sigma, a matrix with weights for each sample point (weights), a probability for each cluster (clusterProbs), a log-likelihhod (logL) and a list of data
#' @export
mkEMRun <- function ()
{
  return (new("EMRun"
              , mu=matrix()
              , sigma=list()
              , weight=matrix()
              , clusterProbs=0
              , logL=Inf
              , data=list()
              ))
}



#' include the newest mu, sigma, logL values in the EMRun
#'
#' @param em the EM object
#' @param mu mean values
#' @param sigma covariance matrices
#' @param weight the new weight vector
#' @param clusterProbs 
#' @param logL log-likelihood of the current state of the EM model given the current data
#'
#' @return updated EM object
#'
#' @export
updateEMRun <- function (em, mu, sigma, weight, clusterProbs, logL)
{
  em@mu <- mu
  em@sigma <- sigma
  em@weight <- weight
  em@clusterProbs <- clusterProbs
  em@logL <- logL
  return (em)
}



#' Provide a label vector for each data element in an @em@ structure
#'
#' If below the cutoff, the label is set "0" to indicate background.
#'
#' @param em the current EM object
#' @param cutoff at which responsibility weight should a point no longer be associated with a
#' foreground Gaussian
#' @param ksigma How many standard deviations from the mean score may we be away?
#'
#' @export
getLabels <- function (em, cutoff=0.05, ksigma=2.0) {
  # TODO
  data <- em@data[[1]]
  # for each cluster, calculate empirical mean and standard deviation of the log-likelihoods of the
  # given Gaussians. This gives us a sense of the "average" score/log-likelihood a point belonging
  # to this Gaussian should have.
  llss <- sapply(1:length(em@sigma), function(i)
                 {
                   m <- em@mu[,i]
                   s <- em@sigma[[i]]
                   # generate 1000 data points (two-dimensional) from mu and sigma of cluster i
                   xs <- rmvnorm(1000, mean=m, sigma=s)
                   # for each of the generated (two-dimensional) xs calculate (back) the
                   # log-likelihood log(P(xs | m,s)), but why log?
                   ls <- dmvnorm(xs, mean=m, sigma=s, log=TRUE)
                   # mean of the  probabability over all data points to be in cluster i
                   ls.mean <- mean(ls)
                   # standard deviation of the probabability over all data points to be in cluster i
                   ls.sd   <- sd(ls)
                   return(c(ls.mean,ls.sd))
                 })
  # For each data point, the Gaussian which provides the maximal responsibility weight.
  ms <- apply(em@weight, 1, which.max)
  # apply this function to each data point i.
  f <- function (i) {
    # maximal weight
    l <- ms[i]
    # mean
    m <- em@mu[,l]
    # covariance matrix
    s <- em@sigma[[l]]
    # what is the log-likelihood of the point i, given that i has highest responsibility value with
    # Gaussian l, whose mean and covariance matrix we just extracted above.
    bestll <- NULL
    if (!is.null(data)) {
      bestll <- dmvnorm(data@sampled[i,], mean=m, sigma=s, log=TRUE)
    }
    # background cluster is indicated with 1 before, will now be indicated by 0, afterwards there
    # will be no data point belonging to cluster 1
    if (l == 1 || em@weight[[i,l]] < cutoff) {
      l <- 0
    }
    # If something went wrong, i.e. l>number of clusters or l<0, set l <- 0. If the log-likelihood
    # of the best-fitting cluster of the current data point is less than the mean log-likelihood of
    # that theoretical cluster - 2*sd of that theoretical cluster, then also set l <- 0 (to
    # background cluster), because then the point has no cluster it really belongs to.
    #
    # This makes the responsibility matrix "more sparse", fewer Gaussians have responsibility to
    # each data point.
    if (!(is.null(bestll)) && (l<=0 || l>ncol(llss) || bestll < llss[1,l] - ksigma*llss[2,l])) {
      l <- 0
    }
    l
  }
  ms <- sapply(1:length(ms), f)
  return (ms)
}

