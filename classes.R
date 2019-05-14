
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
setClass (Class="FlowDataObject", slots=c(data="matrix", x="Limits", y="Limits"))
mkFlowDataObject <- function(data, xChannel, yChannel)
{
  xs <- data[,xChannel]
  ys <- data[,yChannel]
  return(new("FlowDataObject", data=data, x=mkLimits(channel=xChannel,xs), y=mkLimits(channel=yChannel,ys)))
}



# input parameters, as a class
setClass (Class="FlowData", slots=c(data="FlowDataObject", sampled="FlowDataObject", nth="numeric"), validity=validFlowData)
validFlowData <- function(object)
{
  #TODO we should check certain things
  TRUE
}

# create flow data object, including correct subsampling, etc
# "nth" is the subsampling parameters >= 1
# note that the "sampled" structure retains only two dimensions
mkFlowData <- function(nth=1, xChannel, yChannel, xMin, xMax, yMin, yMax, data)
{
  origData<-mkFlowDataObject(data=exprs(data), xChannel=xChannel, yChannel=yChannel)

  # prepare subset extraction without border machine noise
  border <- list(c(xMin,xMax+origData@x@min), c(yMin,yMax+origData@y@min)) # define subset area
  names(border) <- c(xChannel, yChannel)
  denoised <- rectangleGate(filterId="Noise",  .gate = border) # filter noise
  denoised.subset <- Subset(data, denoised)
  denoisedData<-mkFlowDataObject(data=exprs(denoised.subset), xChannel=xChannel, yChannel=yChannel)

  # subsample every nth element
  vs<-cbind(denoisedData@data[,xChannel],denoisedData@data[,yChannel]) #both dimensions as matrix
  subsampled<-vs[sample(nrow(vs),size=nrow(vs)/nth,replace=FALSE),]
  colnames(subsampled) <- list(xChannel, yChannel)

  return (new("FlowData"
              , data=denoisedData
              , sampled=mkFlowDataObject(data=subsampled, xChannel=xChannel, yChannel=yChannel)
              , nth=nth
              ))
}



# a single run of the EM algorithm with a given number of clusters
setClass (Class="EMRun", slots=c(mu="matrix", sigma="matrix", weight="matrix", logL="numeric"
                                 ,mus="list", sigmas="list", weights="list", logLs="list"))
mkEMRun <- function ()
{
  return (new("EMRun"
              , mu=c()
              , sigma=c()
              , weight=c()
              , logL=Inf
              , mus=list()
              , sigmas=list()
              , logLs=list()
              , weights=list()))
}



# include the newest mu, sigma, logL values in the EMRun
updateEMRun <- function (em, mu, sigma, w, logL)
{
  n <- 1 + error(em@mu)
  # store old data
  em@mus[n] <- em@mu
  em@sigmas[n] <- error()
  em@weights[n] <- error()
  em@logLs[n] <- error()
  #
  em@mu <- mu
  em@sigma <- sigma
  em@weight <- w
  em@logL <- logL
  return em
}

