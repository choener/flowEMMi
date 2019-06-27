#!/usr/bin/env Rscript

# This small wrapper script compares "flowMerge" with "flowEMMi".

library (flowCore)
library (flowMerge)
library (optparse)
library (tictoc)

source ("classes.R")

parser <- OptionParser ()
parser <- add_option (parser, c ("-v", "--verbose"), action = "store_true", default=FALSE, help="be very verbose")
parser <- add_option (parser, c ("-f", "--file"), type="character", default = "", help="file to process")
parser <- add_option (parser, c ("-x", "--channelx"), type="character", default = "PMT.1", help="first channel")
parser <- add_option (parser, c ("-y", "--channely"), type="character", default = "PMT.9", help="second channel")
parser <- add_option (parser, c ("--xstart"), type="integer", default = 1500, help="")
parser <- add_option (parser, c ("--xend"), type="integer", default = 40000, help="")
parser <- add_option (parser, c ("--ystart"), type="integer", default = 5000, help="")
parser <- add_option (parser, c ("--yend"), type="integer", default = 38000, help="")
parser <- add_option (parser, c ("--initfraction"), type="double", default = 0.02, help="")
parser <- add_option (parser, c ("--prior"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--separation"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--inits"), type="integer", default = 10, help="")
parser <- add_option (parser, c ("--log"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--alpha"), type="double", default = 0.5, help="")
parser <- add_option (parser, c ("--imgformat"), type="character", default = "png", help="")
parser <- add_option (parser, c ("--mincluster"), type="integer", default = 2, help="")
parser <- add_option (parser, c ("--maxcluster"), type="integer", default = 20, help="")
parser <- add_option (parser, c ("--clusterbracket"), type="integer", default = 3, help="")
parser <- add_option (parser, c ("--disableparallelism"), action="store_true", default = FALSE, help="")
parser <- add_option (parser, c ("--convergence"), type="double", default = 10e-5, help="")
parser <- add_option (parser, c ("--prefix"), type="character", default = "", help="output prefix")
parser <- add_option (parser, c ("--suffix"), type="character", default = "", help="output suffix")

opts <- parse_args(parser)



# prepare our data, by setting up things in parent score

prepareData <- function( filename, channelx="PMT.1", channely="PMT.9" ) {
  fcsData <<- read.FCS(filename,alter.names = TRUE,transformation = FALSE)
  summary (fcsData)
  colnames(fcsData)
  origNRow <<- nrow(fcsData@exprs)
  fdo <<- mkFlowDataObject(frame=fcsData,xChannel=channelx, yChannel=channely)
  pd <<- mkFractionedFlowData (fdo=fdo
                              ,fraction = opts$initfraction
                              ,xMin=opts$xstart, xMax=opts$xend
                              ,yMin=opts$ystart, yMax=opts$yend)
  # TODO remove when actually measuring; flowClust is slower than flowEMMi (by a
  # huge factor), this way we can test the performance script at least

  fcsData@exprs <<- pd@sampled
  sampledNRow <<- nrow(fcsData@exprs)
  cat (sprintf("number of rows originally: %.0f, sampled: %.0f\n", origNRow, sampledNRow))
} # prepareData



# run flowmerge

runFlowMerge <- function ( ) {
  tic(msg="flowClust call")
  flowClust.res <<- if (opts$disableparallelism)
  {
    pFlowMerge(cl=NULL, fcsData, varNames=c(opts$channelx, opts$channely), K=(opts$mincluster):(opts$maxcluster),trans=1,nu.est=1,randomStart=opts$inits);
  } else
  {
    flowClust(fcsData, varNames=c(opts$channelx, opts$channely), K=(opts$mincluster):(opts$maxcluster),trans=1,nu.est=1,randomStart=opts$inits
              ,tol=opts$convergence # flowClust default is 10e-5 and flowEMMi uses 10e-2
              );
  }
  toc()
  summary (flowClust.res)
} # runFlowMerge



# produce the optimal clustering result

runFlowMergeOpt <- function () {
  flowClust.maxBIC <<- flowClust.res[[which.max(BIC(flowClust.res))]]
  flowClust.flowobj <<- flowObj(flowClust.maxBIC,fcsData);
  flowClust.merge <<- merge(flowClust.flowobj,metric="entropy")
  i <- fitPiecewiseLinreg(flowClust.merge) # local only
  flowClust.mergeopt <<- flowClust.merge[[i]]
} # runFlowMergeOpt



runFlowMergePlots <- function () {
  png(file=sprintf("./%s/flowmerge-bic-%s.png",opts$prefix,opts$suffix), bg = "white", width = 12, height = 12, units = 'in', res = 300)
  plot(flowMerge:::BIC(flowClust.res))
  dev.off()

  png(file=sprintf("./%s/flowmerge-maxbic-solution-%s.png",opts$prefix,opts$suffix), bg = "white", width = 12, height = 12, units = 'in', res = 300, pointsize=2)
  plot(flowClust.res[[4]], data=fcsData, main="Max BIC solution")
  dev.off()

  png(file=sprintf("./%s/flowmerge-maxicl-solution-%s.png",opts$prefix,opts$suffix), bg = "white", width = 12, height = 12, units = 'in', res = 300, pointsize=2)
  plot(flowClust.res[[which.max(flowMerge:::ICL(flowClust.res))]],data=fcsData,main="Max ICL solution");
  dev.off()

  png(file=sprintf("./%s/flowmerge-merged-solution-%s.png",opts$prefix,opts$suffix), bg = "white", width = 12, height = 12, units = 'in', res = 300)
  plot(flowClust.mergeopt,level=0.75,pch=20,main="Merged Solution");
  dev.off()

  #write (flowClust.mergeopt@mu, file=   sprintf("./%s/flowmerge-mu-%s.dat",opts$prefix,opts$suffix))
  #write (flowClust.mergeopt@sigma, file=sprintf("./%s/flowmerge-sigma-%s.dat",opts$prefix,opts$suffix))

  print(flowClust.mergeopt)
  print(flowClust.mergeopt@mu)
  print(flowClust.mergeopt@sigma)
}



# write out the labels for the optimal solution. These labels are then to be used

writeFlowMergeLabels <- function ( filename, xs ) {
  write (xs@label, file = filename, ncolumns = 1)
}


# depending on if we are interactive or not, run automatically or have the user do stuff

if (interactive()){
} else { # interfactive
  print("preparing data")
  prepareData (opts$file, opts$channelx, opts$channely)
  print("flowMerge")
  runFlowMerge ()
  print("flowMergeOpt")
  runFlowMergeOpt()
  print("flowMergePlots")
  runFlowMergePlots()
  print("writing label file")
  writeFlowMergeLabels("flowmergeopt.dat", flowClust.mergeopt)

#fcsData@exprs[,opts$channelx]
#vs<-cbind(denoisedData@data[,fdo@xChannel],denoisedData@data[,fdo@yChannel]) #both dimensions as matrix
#subsampled<-vs[sample(nrow(vs),size=nrow(vs) * fraction,replace=FALSE),]

# subsample artificially to have reasonable running times with flowClust

# subsampled<-vs[sample(nrow(vs),size=nrow(vs) * fraction,replace=FALSE),]



# call flowClust with the same parameters as flowEMMi, in particular, the
# chosen channels, the number of clusters to choose from, and the number of
# random starts

# TODO in case of disableparallelism, call pFlowClust with cl=NULL, to be able
# to compare both approaches.

#ests<-getEstimates(flowClust.mergeopt) # $proportions $locations
#
#write (ests$proportions, file=sprintf("./%s/flowmerge-proportions-%s.dat",opts$prefix,opts$suffix))
#write (ests$locations, file=sprintf("./%s/flowmerge-locations-%s.dat",opts$prefix,opts$suffix))

} # not interactive
