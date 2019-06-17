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

opts <- parse_args(parser)


fcsData <- read.FCS(opts$file,alter.names = TRUE,transformation = FALSE)
summary (fcsData)
colnames(fcsData)
origNRow <- nrow(fcsData@exprs)
fdo <- mkFlowDataObject(frame=fcsData,xChannel=opts$channelx, yChannel=opts$channely)
pd <- mkFractionedFlowData (fdo=fdo
                           ,fraction = opts$initfraction
                           ,xMin=opts$xstart, xMax=opts$xend
                           ,yMin=opts$ystart, yMax=opts$yend)

# TODO remove when actually measuring; flowClust is slower than flowEMMi (by a
# huge factor), this way we can test the performance script at least

fcsData@exprs <- pd@sampled
sampledNRow <- nrow(fcsData@exprs)
cat (sprintf("number of rows originally: %.0f, sampled: %.0f\n", origNRow, sampledNRow))

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

tic(msg="flowClust call")
flowClust.res <- if (opts$disableparallelism)
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

print ("plotting")

# plot of flowClust BIC values
png(file="flowmerge_bic.png", bg = "white", width = 12, height = 12, units = 'in', res = 300)
plot(flowMerge:::BIC(flowClust.res))
dev.off()

flowClust.maxBIC<-flowClust.res[[which.max(BIC(flowClust.res))]]
flowClust.flowobj<-flowObj(flowClust.maxBIC,fcsData);
flowClust.merge<-merge(flowClust.flowobj,metric="entropy");



