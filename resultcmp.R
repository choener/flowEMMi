#!/usr/bin/env Rscript

# In principle, this script only needs a vector of *known* assignments, and a
# vector of *assumed* assignments, then greedily matches up the labels as best
# as possible, and calculates the confusion. As part of this, we return the
# actual assignment.

library (lattice)
library (flowWorkspace)
library (CytoML)
library (stringr)


# given two vectors of label assignments, calculate the TP, FT, TN, FN absolute
# counts. The first three values are the relative rates, summed over all
# absolutes, then made relative.
#
# TP: [[1]], FN:[[2]], FP:[[3]]

calcConfusion <- function (ts, xs) {
  # true classes (from experts)
  tcs <- as.vector(table(ts)) # , useNA="ifany")
  # classes in the predicted data
  xcs <- as.vector(table(xs))
  # cross of tables, but without NA, since that is the background label
  ovr <- table(ts, xs) # , useNA="ifany")

  # TP matrix calculation, greedy best assignment of labels

  tp <- matrix(0, nrow=nrow(ovr), ncol=ncol(ovr)) # prepare true positive count
  # we have n classes, and greedily find the best matching now
  for (i in 1:min(nrow(ovr),ncol(ovr))) {
    # highest fraction of assigned labels
    best <- which(ovr==max(ovr), arr.ind=T)
    t <- best[[1,1]]
    x <- best[[1,2]]
    # indicate label assignment, giving the actual TP count
    tp[[t,x]] <- ovr[[t,x]] # / tcs[[t]]
    # clean row
    for (j in 1:ncol(ovr)) {
      ovr[[t,j]] <- 0
    }
    # clean column
    for (j in 1:nrow(ovr)) {
      ovr[[j,x]] <- 0
    }
  } # for min nrow,ncol

  # recreate the table of crosses, to calculate FN and FP
  ovr <- table(ts, xs)
  # FN calculation: this is the remaining mass of tcs[t] - sum(tp[t,])
  fn <- sapply (1:length(tcs), function(t){ tcs[t] - sum(tp[t,]) })
  # FP
  fp <- sapply (1:length(xcs), function(x){ xcs[x] - sum(tp[,x]) })

  # make relative
  rtp <- tp
  for (i in 1:nrow(tp)) {
    rtp[i,] <- rtp[i,] / tcs[i]
  }
  #fn <- fn / tcs
  #fp <- fp / xcs
  list( sum(tp)/sum(tcs), sum(fn)/sum(tcs), sum(fp)/sum(xcs) # relative TP,FN,FP with prior summing over absolute values
      , tcs,xcs,tp,fn,fp  # absolute values, vectors for truths, vector of assumed, tp matrix, fn vector, fp vector
      , rtp, fn/tcs, fp/xcs) # relativized
}

fOneFromCon <- function (c, beta=1.0) {
  tp <- c[[1]]
  fn <- c[[2]]
  fp <- c[[3]]
  betatp <- (1+beta*beta) * tp
  betafn <- (beta*beta) * fn
  fbeta <- betatp / (betatp + betafn + fp)
}


# Given a single sample, (say gs[[1]]) produce a label vector.

getLabelVector <- function (gatingH) {
  idx <- 1
  l <- nrow(getData(gatingH))
  lvec <- integer(l)
  # loop over all gates
  for (g in tail(getNodes(gatingH, path=1), n=-6)) {
    # extract indices boolean vector
    bs <- getIndices(gatingH, g)
    # replace True by 'idx' in the target
    lvec <- mapply (function (c,b) { if (b) {idx} else {c} }, lvec, bs)
    idx <- idx+1
  } # for gates
  lvec
} # getLabelVector


# create plot given a csv (can be wrapped in svg/png and dev.off)
createPlot <- function (fname) {
  xs <- read.csv(fname, comment.char='#')
  plot(xs[[1]] ~ xs[[2]], data = xs)
  text(xs[[2]], xs[[1]], labels=xs[[3]])
}

# create the plotting data
createData <- function () {
  thesefcs <- list.files("./fcs")
  print(thesefcs)
  # known gates
  # TODO rename users to shortened letters?
  # TODO johannes, thomas
  users <- c("florian")
  # users <- c("zishu", "florian", "susanne")
  usergates <- list()
  for (user in users) {
    print(user)
    # gating.wsp should be symlink to correct flowjo file
    wspfname <-sprintf ("./wsp/%s/gating.wsp", user)
    wsp <- openWorkspace(wspfname)
    gs <- parseWorkspace(wsp, path="./fcs", name=1)
    print(sampleNames(gs))
    usergates[user] <- gs
  }
  print(usergates)

  # we now have loaded all user gates and can proceed with f-measure calculations

  df <- data.frame(time=double(), f1=double(), epsilon=double(), label=character(),stringsAsFactors=FALSE)

  # flowEMMI and flowMerge sources
  #algos <- c("emmi")
  algos <- c("merge", "emmi")
  tests <- c("12", "25", "26", "39")
  # testNames <- c("InTH_160713_012.fcs_225880", "InTH_160712_025.fcs_252227", "InTH_160720_026.fcs_212880", "InTH_160719_039.fcs_303271")
  es <- c(1e+0, 1e-2, 1e-5)
  esName <- c("+0", "-2", "-5")
  for (user in users) {
    for (testidx in 1:length(tests)) {
      test <- tests[[testidx]]
      #testName <- testNames[[testidx]]
      for (algo in algos) {
        for (e in 1:length(es)) {
          eV <- es[[e]]
          eN <- esName[[e]]
          cat(sprintf("user %s, test %s, algo %s, epsilon %s\n", user, test, algo, eN))
          # we now need to load the appropriate data from the automated tools
          algofname <- ""
          if (algo=="emmi") {
            algofname <- Sys.glob(sprintf("./out-flow%s/0%s-1e%s/best*dat",algo, test, eN))
          }
          if (algo=="merge") {
            algofname <- Sys.glob(sprintf("./out-flow%s/0%s-1e%s/flowmergeopt*dat",algo, test, eN))
          }
          print(algofname)
          if (length(algofname) > 0 && file.exists(algofname)) {
            algolabels <- scan(algofname)
            # get gating information, by finding the correct sample name
            gates <- usergates[[user]]
            print(sampleNames(gates))
            print(paste(test,".fcs", sep=""))
            sext <- str_extract(sampleNames(gates), paste(test,".fcs", sep=""))
            print (sext)
            sidx <- which.min(is.na(sext))
            print (sidx)
            gate <- gates[[sidx]]
            print(gate)
            gLabelVec <- getLabelVector(gate)
            cat (sprintf("user label vector length: %d, algo label vector length: %d\n", length(gLabelVec), length(algolabels)))
            c <- calcConfusion(gLabelVec, algolabels)
            f1 <- fOneFromCon(c)
            print(f1)
            lbl <- sprintf("%s", algo)
            timefname <- Sys.glob(sprintf("./out-flow%s/0%s-1e%s/test.out",algo, test, eN))
            timelines <- readLines(timefname)
            seconds <- as.double(str_extract(timelines[[1]],"(\\.|[:digit:])+"))
            df[nrow(df)+1,] = list(seconds, f1, eV, lbl)
            print(df)
          } # file.exists (algofname)
        } # es
      } # algos
    } # tests
  } #users
  print(df)
  write.csv(df, file="vs.csv")
}

t1 <- c(1,1,1,1,2,2,2,3,3,NA)
x1 <- c(3,3,3,2,2,2,1,1,2,NA)

plotFromData <- function (fname) {
  csv <- read.csv (fname)
  print(csv)
  print(as.factor(csv$epsilon))
  xmin = min(csv$f1)
  xmax = max(csv$f1)
  ymin = min(csv$time)
  ymax = max(csv$time)
  es <- csv[csv$label == "emmi",]
  ms <- csv[csv$label == "merge",]
  plot( time ~ f1, data=es, xlim=c(xmin,xmax), ylim=c(ymin,ymax), pch=c("ε","e","E")[as.numeric(as.factor(epsilon))], col="blue" )
  points ( time ~ f1, data=ms, pch=c("μ","m","M")[as.numeric(as.factor(epsilon))], col="red" )
  # points ( time ~ f1, data=ms, pch="M", col=c("red","green","blue")[as.numeric(as.factor(epsilon))] )
}

#ws <- openWorkspace("./wsp/florian/Gating_Test_FS.wsp")
#print (ws)
#summary (ws)
#
#gs <- parseWorkspace(ws, path="./fcs")
#summary (gs)
#sampleNames(gs)
#
## single run
#gh <- gs[[1]]

# # nodes / leading to gates
# ghns <- getNodes (gh)
# 
# # get a single gate
# gate <- getGate (gh, "G1")
# 
# g1data <- getData (gh, "G2")
# print(g1data@exprs)
# 
# inv_trans <- lapply(gs, function(gh){
#   invs <- getTransformations(gh, inverse=TRUE)
#   invs <- transformList(names(invs), invs)
# })
# 
# inv_single <- function (gh) {
#   invs <- getTransformations(gh, inverse=TRUE)
#   invs <- transformList(names(invs), invs)
# }
# 
# # gives original-transformed single gate data
# t <- transform(getData(gh, "G14"), inv_single(gh))
# # plot (t@exprs[,"PMT 1"])
# 
# # g1data_ <- transform(
# 
# #fs_transformed <- getData(gs)
# #fs_inverted <- transform(fs_transformed, inv_trans)
# #write.flowSet(fs_inverted)
# 
# # g1data_inv <- transform(g1data, inv_trans)
# 
