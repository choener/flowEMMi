
library(tictoc)
library(flowCore)
library(mvtnorm)

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

mkFractionedFlowData <- function(fdo, fraction=1.0, xMin, xMax, yMin, yMax)
{
  tic(msg="mkFractionedFlowData")
  # prepare subset extraction without border machine noise
  border <- list(c(xMin,xMax), c(yMin,yMax)) # define subset area
  names(border) <- c(fdo@xChannel, fdo@yChannel)
  denoised <- rectangleGate(filterId="Noise",  .gate = border) # filter noise
  denoised.subset <- Subset(fdo@flowFrame, denoised)
  denoisedData<-mkFlowDataObject(frame=denoised.subset, xChannel=fdo@xChannel, yChannel=fdo@yChannel)
  # subsample every nth element
  vs<-cbind(denoisedData@data[,fdo@xChannel],denoisedData@data[,fdo@yChannel]) #both dimensions as matrix
  # make sure to keep data ordered if no subsampled is requested
  if (fraction>=1) {
    subsampled<-vs
  } else {
    subsampled<-vs[sample(nrow(vs),size=nrow(vs) * fraction,replace=FALSE),]
  }
  colnames(subsampled) <- list(fdo@xChannel, fdo@yChannel)
  toc()

  return (new("FlowData"
              , data=denoisedData
              , sampled=subsampled
              , fraction=fraction
              ))
}



# a single run of the EM algorithm with a given number of clusters
EMRun <- setClass (Class="EMRun", slots=c(mu="matrix", sigma="list", weight="matrix", clusterProbs="numeric", logL="numeric", data="list"
                                 ))
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



# include the newest mu, sigma, logL values in the EMRun
updateEMRun <- function (em, mu, sigma, weight, clusterProbs, logL)
{
  em@mu <- mu
  em@sigma <- sigma
  em@weight <- weight
  em@clusterProbs <- clusterProbs
  em@logL <- logL
  return (em)
}



# provide a label vector for each data element in an @em@ structure. If below
# the cutoff, the label is set "0" to indicate background.
getLabels <- function (em, cutoff=0.05) {
  ms <- apply(em@weight, 1, which.max)
  f <- function (i) {
    l <- ms[i]
    if (l == 1 || em@weight[[i,l]] < cutoff) {
      l <- 0
    }
    l
  }
  ms <- sapply(1:length(ms), f)
  return (ms)
}

