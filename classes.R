
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
validFlowData <- function(object)
{
  #TODO we should check certain things
  TRUE
}
setClass (Class="FlowData", slots=c(data="FlowDataObject", sampled="FlowDataObject", fraction="numeric"), validity=validFlowData)

# create flow data object, including correct subsampling, etc
# fraction is the subsampling parameter, 0 < fraction <= 1
# note that the "sampled" structure retains only two dimensions
mkFlowData <- function(fraction=1.0, xChannel, yChannel, xMin, xMax, yMin, yMax, data)
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
  subsampled<-vs[sample(nrow(vs),size=nrow(vs) * fraction,replace=FALSE),]
  colnames(subsampled) <- list(xChannel, yChannel)

  return (new("FlowData"
              , data=denoisedData
              , sampled=mkFlowDataObject(data=subsampled, xChannel=xChannel, yChannel=yChannel)
              , fraction=fraction
              ))
}



# a single run of the EM algorithm with a given number of clusters
setClass (Class="EMRun", slots=c(mu="matrix", sigma="list", weight="matrix", clusterProbs="numeric", logL="numeric"
                                 ))
mkEMRun <- function ()
{
  return (new("EMRun"
              , mu=matrix()
              , sigma=list()
              , weight=matrix()
              , clusterProbs=0
              , logL=Inf
              ))
}



# include the newest mu, sigma, logL values in the EMRun
updateEMRun <- function (em, mu, sigma, weight, clusterProbs, logL)
{
#  n <- 1 + length (em@mu)
#  # store old data
#  em@mus[n] <- mu
#  em@sigmas[n] <- sigma
#  em@weights[n] <- w
#  em@logLs[n] <- logL
  # setup new
  em@mu <- mu
  em@sigma <- sigma
  em@weight <- weight
  em@clusterProbs <- clusterProbs
  em@logL <- logL
  return (em)
}

