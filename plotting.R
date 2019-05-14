
source("classes.R")

# create plot of the (possibly sampling reduced) input data.

plotInputData <- function (data, nth, logScaled, imageFormat = "png")
{
  # needed, because some data objects will hold more than 2 dimensions
  xy <- cbind(data@data[,data@x@channel], data@data[,data@y@channel])
  # pretty picture
  if (imageFormat=="png")
  {
    png(file=sprintf("%d_sample.png", nth), bg = "white", width = 12, height = 12, units = 'in', res = 300)
  } else if (imageFormat=="svg")
  {
    svg(filename=sprintf("%d_sample.svg", nth),width = 12, height = 12, pointsize = 12, bg = "white")
  }
  if(logScaled==TRUE)
  {
    plot(xy
         ,yaxt="n",xaxt="n",log="xy",type="p",cex=.6,pch=19
         ,xlim=limitsC(data@x)
         ,ylim=limitsC(data@y)
         ,xlab=data@x@channel
         ,ylab=data@y@channel)
    strLabels <- c(expression(paste("10"^"0")) , expression(paste("10"^"1"))
                  ,expression(paste("10"^"2")) , expression(paste("10"^"3"))
                  ,expression(paste("10"^"4")))
    axis(1, at=c(1,10,100,1000,10000), labels=strLabels )
    axis(2, at=c(1,10,100,1000,10000), labels=strLabels )
  }else
  {
    plot(xy
         ,type="p",cex=.6,pch=19
         ,xlim=limitsC(data@x)
         ,ylim=limitsC(data@y)
         ,xlab=data@x@channel
         ,ylab=data@y@channel)
  }
  dev.off()
}

