
# limits
setClass (Class="Limits", representation(channel="character", min="numeric", max="numeric"))

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
setClass (Class="FlowDataObject", representation(data="matrix", x="Limits", y="Limits"))
mkFlowDataObject <- function(data, xChannel, yChannel)
{
  xs <- data[,xChannel]
  ys <- data[,yChannel]
  return(new("FlowDataObject", data=data, x=mkLimits(channel=xChannel,xs), y=mkLimits(channel=yChannel,ys)))
}



# input parameters, as a class
setClass (Class="FlowData", representation(data="FlowDataObject", sampled="FlowDataObject", nth="numeric"))

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

