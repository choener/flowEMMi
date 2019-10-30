fcsData <- read.FCS("InTH_160712_025.fcs",alter.names = TRUE,transformation = FALSE)
summary(fcsData)
plot(fcsData@exprs[,"PMT.1"], fcsData@exprs[,"PMT.9"])
set.seed(1)
results <- flowEMMi( frame = fcsData
                     , ch1="PMT.1", ch2="PMT.9"
                     , xMin = 100, xMax = opts$xend, yMin=100, yMax=opts$yend
                     , initFraction = opts$initfraction
                     , finalFraction = opts$finalfraction
                     , prior = opts$prior
                     , separation = opts$separation
                     , numberOfInits = opts$inits
                     , useLogScale = opts$log
                     , alpha = opts$alpha, imageFormat = opts$imgformat
                     , minClusters = opts$mincluster, maxClusters = opts$maxcluster, clusterbracket=opts$clusterbracket
                     , disableParallelism = TRUE
                     , convergenceEpsilon = opts$convergence
    )
  # full calculation with 15 clusters, idx 14
  # mkFractionedFlowData: 0.314 sec elapsed
  # rederived densities: 42.23 sec elapsed
  # mkFractionedFlowData: 0.316 sec elapsed
  # Starting EM with threshold 0.0100 threshold, 15 clusters
  # Iteration       ?? LL
  # 0            NaN    0.062 0.010 0.072 ... 
  # 10            NaN    0.062 0.010 0.072 ... 
  # Error in while ((iteration < 1000) && (iteration < 11 || abs(stepDelta) >  : 
  #                                        missing value where TRUE/FALSE needed
  #                                        In addition: Warning messages:
  #                                          1: In rmvnorm(1000, mean = m, sigma = s) :
  #                                          sigma is numerically not positive semidefinite
  #                                        2: In sqrt(es$val) :
  #                                          Show Traceback
  #                                        
  #                                        Rerun with Debug
  #                                        Error in while ((iteration < 1000) && (iteration < 11 || abs(stepDelta) >  : 
  #                                                                               missing value where TRUE/FALSE needed                            missing value where TRUE/FALSE needed 

# use the default parameters for start of x and y
set.seed(1)
results <- flowEMMi( frame = fcsData
                     , ch1="PMT.1", ch2="PMT.9"
                     , xMin = opts$xstart, xMax = opts$xend, yMin=opts$ystart, yMax=opts$yend
                     , initFraction = opts$initfraction
                     , finalFraction = opts$finalfraction
                     , prior = opts$prior
                     , separation = opts$separation
                     , numberOfInits = opts$inits
                     , useLogScale = opts$log
                     , alpha = opts$alpha, imageFormat = opts$imgformat
                     , minClusters = opts$mincluster, maxClusters = opts$maxcluster, clusterbracket=opts$clusterbracket
                     , disableParallelism = TRUE
                     , convergenceEpsilon = opts$convergence
)
  # full calculation with 16 clusters, idx 15
  # mkFractionedFlowData: 0.299 sec elapsed
  # rederived densities: 29.712 sec elapsed
  # mkFractionedFlowData: 0.299 sec elapsed
  # Starting EM with threshold 0.0100 threshold, 16 clusters
  # Iteration       ?? LL
  # 0         0.0000    0.010 0.071 0.156 ... 
  # 10         0.0000    0.010 0.071 0.156 ... 
  # 11         0.0000
  # timing iterateEM: 1.779 sec elapsed
  # Error in if (!(is.null(bestll)) && (l <= 0 || l > ncol(llss) || bestll <  : 
  #                                     missing value where TRUE/FALSE needed
  #                                     In addition: Warning message:
  #                                       In sqrt(es$val) :
  #                                       Show Traceback
  #                                     
  #                                     Rerun with Debug
  #                                     Error in if (!(is.null(bestll)) && (l <= 0 || l > ncol(llss) || bestll <  : 
  #                                                                         missing value where TRUE/FALSE needed 
myMatrix <- rbind(c(0.5,0.3, 0.2), c(0.3,0.3,0.4),c(0.2,0,0.8))
myMatrix
colnames(myMatrix) <- c("Cluster1","Cluster2","Cluster3")
rownames(myMatrix) <- c("sample1", "sample2", "sample3")
myMatrix

fcsData <- read.FCS("BCK_30_Sep_11_100.fcs",alter.names = TRUE,transformation = FALSE)
summary(fcsData)
plot(fcsData@exprs[,"FS.Log"], fcsData@exprs[,"SS.Log"])
set.seed(1)
results <- flowEMMi( frame = fcsData
                     , ch1="FS.Log", ch2="SS.Log"
                     , xMin = 100, xMax = opts$xend, yMin=100, yMax=opts$yend
                     , initFraction = opts$initfraction
                     , finalFraction = opts$finalfraction
                     , prior = opts$prior
                     , separation = opts$separation
                     , numberOfInits = opts$inits
                     , useLogScale = opts$log
                     , alpha = opts$alpha, imageFormat = opts$imgformat
                     , minClusters = opts$mincluster, maxClusters = opts$maxcluster, clusterbracket=opts$clusterbracket
                     , disableParallelism = TRUE
                     , convergenceEpsilon = opts$convergence
)
  # full calculation with 8 clusters, idx 7
  # mkFractionedFlowData: 0.285 sec elapsed
  # rederived densities: 32.687 sec elapsed
  # mkFractionedFlowData: 0.283 sec elapsed
  # Starting EM with threshold 0.0100 threshold, 8 clusters
  # Iteration       ?? LL
  # 0            NaN    0.010 0.007 0.056 ... 
  # 10            NaN    0.010 0.007 0.056 ... 
  # Show Traceback
  # 
  # Rerun with Debug
  # Error in while ((iteration < 1000) && (iteration < 11 || abs(stepDelta) >  : 
  #                                        missing value where TRUE/FALSE needed 