library(flowCore)
setwd("~/Dokumente/GitHub/ZishusPaper/data")
fcsData <- read.FCS(paste("./C1/20160222_C1.fcs", sep = ""), alter.names = TRUE, transformation = FALSE)
# fcsData <- read.FCS(opts$file,alter.names = TRUE,transformation = FALSE)
print (colnames(fcsData))
fdo <- mkFlowDataObject(fcsData, xChannel="PMT.1", yChannel="PMT.9")
setwd("~/Dokumente/GitHub/packaging")
source("./R/flowemmi.R")

# run actual flowEMMi algorithm
tic(msg="EM")
results <- flowEMMi( fdo=fdo
                     , xMin = 1500, xMax = 40000, yMin=1500, yMax=40000
                     , minClusters = 2, maxClusters = 20, clusterbracket=3
                     , parallel = FALSE
                     , verbose = TRUE
)
toc() #EM: 170.154 sec elapsed, EM: 274.37 sec elapsed
results_plot <- results$best
plotDensityAndEllipses(data = fcsData@exprs[,c("PMT.1","PMT.9")], results = results_plot)
plotDensityAndEllipsesByRelevance(data = fcsData@exprs[,c("PMT.1","PMT.9")], results = results_plot)
plotDensityAndEllipsesByRelevance(results = results_plot, data= fcsData@exprs[,c("PMT.1", "PMT.9")])
# how long does it take if verbose=FALSE?
tic(msg="EM")
results2 <- flowEMMi( fdo=fdo
                     , xMin = 1500, xMax = 40000, yMin=1500, yMax=40000
                     , minClusters = 2, maxClusters = 20, clusterbracket=3
                     , parallel = FALSE
                     , verbose = FALSE
)
toc() # EM: 317.762 sec elapsed

# does parallel computation work?
tic(msg="EM parallel")
results_parallel <- flowEMMi( fdo=fdo
                     , xMin = 1500, xMax = 40000, yMin=1500, yMax=40000
                     , minClusters = 2, maxClusters = 20, clusterbracket=3
                     , parallel = TRUE
                     , verbose = TRUE
)
toc()# nope!
