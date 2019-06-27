#!/usr/bin/env Rscript

# In principle, this script only needs a vector of *known* assignments, and a
# vector of *assumed* assignments, then greedily matches up the labels as best
# as possible, and calculates the confusion. As part of this, we return the
# actual assignment.

library (flowWorkspace)
library (CytoML)


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


t1 <- c(1,1,1,1,2,2,2,3,3,NA)
x1 <- c(3,3,3,2,2,2,1,1,2,NA)

ws <- openWorkspace("./wsp/florian/Gating_Test_FS.wsp")
print (ws)
summary (ws)

gs <- parseWorkspace(ws, path="./fcs")
summary (gs)
sampleNames(gs)

# single run
gh <- gs[[1]]

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
