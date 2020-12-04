
library(colortools)
library(mixtools)

source("./R/classes.R")



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

### new plotting functions ###

plotEllipse <- function(A,B,phi,c, numPoints=200, newplot=TRUE, color="black", plot=TRUE)
{
  h <- c[1]
  k <- c[2]
  # t should be in [0,2*pi]
  t <- seq(from = 0, to = 2*pi, by = 2*pi/numPoints)
  x <- numeric()
  y <- numeric()
  counter <- 1
  for (i in t) {
    x[counter] <- A*cos(i)*cos(phi) - B*sin(i)*sin(phi) + h
    y[counter] <- A*cos(i)*sin(phi) + B*sin(i)*cos(phi) + k
    counter=counter+1
  }
  if(plot)
  {
    if(newplot)
    {
      plot(x,y, col=color, cex=0.05, asp=1)
    }
    else
    {
      points(x,y, col=color, cex=0.05)
    }
  }


  return(data.frame(x=x,y=y))
}

# plotte Ellipsen nach ihrer Wichtigkeit gefaerbt
plotDensityAndEllipsesByRelevance <- function(results, alpha=0.95, data, addLegend=TRUE, plot=TRUE)
{
  centers <- split(results$mu, col(results$mu)) # returns a list
  sigmas <- results$sigma



  myEllipses <- function(i)
  {
    parameters <- getExtremes(mu=centers[[i]], sigma = sigmas[[i]], alpha = alpha, plot=FALSE)
    return(list(major=parameters$major, minor=parameters$minor, angle=parameters$angle, center=centers[[i]], relevance=results$clusterProbs[i]))
  }

  indices <- 1:(length(results$sigma)) # we do not care about the background cluster
  ellipses <- mapply(myEllipses, indices)
  ellipses <- as.data.frame(t(ellipses))
  ellipses$index <- indices
  relevance <- data.frame(index=indices, rel=unlist(ellipses$relevance))
  relevance <- relevance[order(relevance$rel),]
  relevance$color <- bluered(length(results$sigma))
  relevance <- relevance[order(relevance$index),]

  ellipsePoints <- function(i)
  {
    tmp <- plotEllipse(A=ellipses$major[i][[1]], B=ellipses$minor[i][[1]], c=ellipses$center[i][[1]], phi = ellipses$angle[i][[1]], plot = FALSE)
    return(tmp)
  }
  list <- mapply(ellipsePoints,indices)

  # start at 2, because we don't want to see the background cluster
  xs <- list[,2]$x
  ys <- list[,2]$y


  # skip the background cluster
  colors <- rep(relevance$color[2], length(list[,2]$x))

  if(length(results$sigma) >=3)
  {
    for (i in 3:length(results$sigma)){
      xs <- append(xs, list[,i]$x)
      ys <- append(ys, list[,i]$y)

      colors <- append(colors, rep(relevance$color[i], length(list[,i]$x)))
    }
  }

  # now plot
  x <- data[,1]
  y <- data[,2]
  good <- which(x > 1500 & y > 1500)
  x <- x[good]
  y <- y[good]

  if(length(x) > 0 && length(y) > 0)
  {
    df2 <- data.frame(x = x, y = y,
                      d = densCols(x, y, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
    p <- ggplot(df2) + geom_point(aes(x, y, col = d), size = 0.1) + coord_fixed(ratio = 1) +
      scale_color_identity() + annotate("point", x = xs, y = ys, colour = colors, size=0.1) + theme_bw() +
      xlab("PMT1 bzw. Forward Scatter") + ylab("PMT9 bzw. Fluoreszenz")
    # if(addLegend)
    # {
    #   p <- p + annotate("rect", xmin = 50000, xmax = 60000, ymin = -5000, ymax = 40000,
    #                     alpha = 0.5, color="black")
    # }

    for (i in 2:length(results$sigma))
    {
      currColor <- which(relevance$index==i)

      if(addLegend)
      {
        # p <- p+ annotate(geom = "text", label=paste("E", i, sep=""), x=55000, y=40000- i*2000, color=relevance$color[i])
        # add a legend for the relevance of the ellipses
        p <- p + annotate("rect", xmin = 64000, xmax = 67000, ymin = 29000-i*1000, ymax = 30000-i*1000,
                          alpha = 1, color=redblue(length(results$sigma))[i], fill=redblue(length(results$sigma))[i] )
      }

    }
    if(addLegend)
    {
      p <- p+ annotate(geom = "text", label="ellipse-weight", x=67000, y=30000, color="black", fontface="italic")
      p <- p+ annotate(geom = "text", label="high", x=70000, y=27000, color="black")
      p <- p+ annotate(geom = "text", label="low", x=70000, y=29000- (length(results$sigma)-1)*1000, color="black")
    }
    if(plot)
    {
      print(p)
    }

    return(df2)

  }# end if length(x)>0 && length(y)>0
  else
  {
    print("No data > 1500")
    df2 <- data.frame(x=numeric(), y=numeric(), d=factor())
    return(df2)
  }

} # end plotDensityAndEllipsesByRelevance

plotDensityAndEllipses <- function(data, results, alpha=0.95)
{
  centers <- split(results$mu, col(results$mu)) # returns a list
  sigmas <- results$sigma

  myEllipses <- function(i)
  {
    parameters <- getExtremes(mu=centers[[i]], sigma = sigmas[[i]], alpha = alpha, plot=FALSE)
    return(list(major=parameters$major, minor=parameters$minor, angle=parameters$angle, center=centers[[i]]))
  }
  indices <- 1:(length(results$sigma))
  ellipses <- mapply(myEllipses, indices)
  ellipses <- as.data.frame(t(ellipses))

  ellipsePoints <- function(i)
  {
    tmp <- plotEllipse(A=ellipses$major[i][[1]], B=ellipses$minor[i][[1]], c=ellipses$center[i][[1]], phi = ellipses$angle[i][[1]], plot = FALSE)
    return(tmp)
  }
  # ellipsePoints(3)
  list <- mapply(ellipsePoints,indices)
  #list <- t(list)
  # xs <- unlist(list[1,])
  # ys <- unlist(list[2,])

  # start at 2, because we don't want to see the background cluster
  xs <- list[,2]$x
  ys <- list[,2]$y

  for (i in 3:length(results$sigma)){
    xs <- append(xs, list[,i]$x)
    ys <- append(ys, list[,i]$y)
  }

  # now plot
  x <- data[,1]
  y <- data[,2]
  good <- which(x > 1500 & y > 1500)
  x <- x[good]
  y <- y[good]

  df2 <- data.frame(x = x, y = y,
                    d = densCols(x, y, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))
  p <- ggplot(df2) +
    geom_point(aes(x, y, col = d), size = 0.1) + coord_fixed(ratio = 1) +
    scale_color_identity() + annotate("point", x = xs, y = ys, colour = "black", size=0.1) + theme_bw() +
    xlab("PMT1 bzw. Forward Scatter") + ylab("PMT9 bzw. Fluoreszenz")
  # p <- ggplot(df2) +
  #   geom_point(aes(x, y, col = d), size = 0.1) +
  #   scale_color_identity() + theme_bw()
  print(p)
} # end plotDensityAndEllipses

## needed for plotDensityAndEllipsesByRelevance and plotDensityAndElipses
getExtremes <- function(mu, sigma, alpha=0.95, plot=FALSE, newplot=TRUE)
{
  ev <- eigen(sigma)
  eigenvalues <- ev$values
  eigenvectors <- ev$vectors # each column is an eigenvector

  s <- -2*log(1-alpha)
  # derive the length of the half-major axis
  major <- sqrt(s*eigenvalues[1]) # 12393.97
  A <- major
  # derive the length of the half-minor axis
  minor <- sqrt(s*eigenvalues[2]) # 3248.652
  B <- minor

  # normalize the longest eigenvector
  bigEV <- eigenvectors[,1]
  lengthBigEV <- sqrt(bigEV[1]*bigEV[1] + bigEV[2]*bigEV[2])
  normBigEV <- 1/lengthBigEV * bigEV

  # normalize the smaller eigenvector
  smallEV <- eigenvectors[,2]
  lengthSmallEV <- sqrt(smallEV[1]*smallEV[1] + smallEV[2]*smallEV[2])
  normSmallEV <- 1/lengthSmallEV * smallEV

  extremes1 <- mu + major*normBigEV
  extremes2 <- mu - major*normBigEV

  extremes3 <- mu + minor*normSmallEV
  extremes4 <- mu - minor*normSmallEV

  if(plot)
  {
    mixtools::ellipse(mu=mu,sigma=sigma,alpha=1-alpha, npoints=200, newplot = newplot)
    points(extremes1[1], extremes1[2], col="red", type='p', lwd=5)
    points(extremes2[1], extremes2[2], col="red", type='p', lwd=5)
    points(extremes3[1], extremes3[2], col="red", type='p', lwd=5)
    points(extremes4[1], extremes4[2], col="red", type='p', lwd=5)
    points(mu[1], mu[2], col="green", type='p', lwd=5) # center of the ellipse in green
  }

  # angle of the ellipse is the angle of the bigger eigenvector to the x-axis
  angle <- atan(bigEV[2] / bigEV[1])

  # A is the semi-axis length along the x-axis, and B is the semi-axis length along the y-axis
  # so, does major belong to the x-axis or to the y-axis?
  # if abs(bigEV[2]) is bigger than abs(bigEV[1]), then major belongs to the y-axis!
  # if(abs(bigEV[1]) > abs(bigEV[2]))
  # {
  #   B <- major
  #   A <- minor
  # }



  return(list(extreme1=extremes1, extreme2=extremes2, extreme3=extremes3, extreme4=extremes4, major=major, minor=minor, angle=angle, A=A,B=B))

}
