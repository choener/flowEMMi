
library(colortools)
library(mixtools)

source("classes.R")



# create plot of the (possibly sampling reduced) input data.
# if gates exists, plot them into the graph.
# if labels exist, use the labels to color

plotInputData <- function (data,
                           labels=NULL,
                           mu=NULL,
                           sigma=NULL,
                           logScaled=FALSE,
                           imageFormat = "png",
                           prefix="")
{
  # needed, because some data objects will hold more than 2 dimensions
  xchan <- data@data@x
  ychan <- data@data@y
  xy <- data@sampled
  cfacts <- as.numeric(as.factor(labels))
  colorfactors <- NULL
  if (is.null(labels)) {
    colorfactors <- "black"
  } else {
    pdf(file=NULL)
    w <- wheel("darkblue", num=length(table(labels)))
    dev.off()
    w[[1]] <- "grey"
    colorfactors <- w[cfacts]
  }
  #xy <- cbind(data@data[,data@x@channel], data@data[,data@y@channel])
  # pretty picture
  if (imageFormat=="png")
  {
    png(file=sprintf("%s%5.3f_sample.png", prefix, data@fraction), bg = "white", width = 12, height = 12, units = 'in', res = 300)
  } else if (imageFormat=="svg")
  {
    svg(filename=sprintf("%5.3f_sample.svg",data@fraction),width = 12, height = 12, pointsize = 12, bg = "white")
  }
  if(logScaled==TRUE)
  {
    plot(xy
         ,yaxt="n",xaxt="n",log="xy",type="p",cex=.6,pch=19
         ,xlim=limitsC(xchan)
         ,ylim=limitsC(ychan)
         ,xlab=xchan@channel
         ,col=colorfactors
         ,ylab=ychan@channel)
    strLabels <- c(expression(paste("10"^"0")) , expression(paste("10"^"1"))
                  ,expression(paste("10"^"2")) , expression(paste("10"^"3"))
                  ,expression(paste("10"^"4")))
    axis(1, at=c(1,10,100,1000,10000), labels=strLabels )
    axis(2, at=c(1,10,100,1000,10000), labels=strLabels )
  }else
  {
    plot(xy
         ,type="p",cex=.6,pch=19
         ,col=colorfactors
         ,xlim=limitsC(xchan)
         ,ylim=limitsC(ychan)
         ,xlab=xchan@channel
         ,ylab=ychan@channel)
    if (! is.null(mu)) { # we have gates and shall draw them
      for (i in 2:length(sigma)) {
        ellipse(mu = mu[,i],
                sigma = sigma[[i]],
                alpha = 0.05
                )
        #lines(ellipse(sigma=matrix(unlist(act_sigma[[c]][j]), ncol = 2, byrow = TRUE),mu = act_mu[[c]][,j],alpha=alpha,npoints = 100), col="black")
      }
    }
  }
  dev.off()
}

# plot the BIC information
plotBIC <- function (bic)
{
  png(file="bic.png",bg="white",width = 12, height = 12, units = 'in', res = 300)
  plot(newList$BIC)
  dev.off()
}

