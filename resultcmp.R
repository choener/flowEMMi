#!/usr/bin/env Rscript

library (flowWorkspace)
library (CytoML)

ws <- openWorkspace("./wsp/florian/Gating_Test_FS.wsp")
print (ws)
summary (ws)

gs <- parseWorkspace(ws, path="./fcs")
summary (gs)
sampleNames(gs)

# single run
gh <- gs[[1]]

# nodes / leading to gates
ghns <- getNodes (gh)

# get a single gate
gate <- getGate (gh, "G1")

g1data <- getData (gh, "G2")
print(g1data@exprs)

inv_trans <- lapply(gs, function(gh){
  invs <- getTransformations(gh, inverse=TRUE)
  invs <- transformList(names(invs), invs)
})

inv_single <- function (gh) {
  invs <- getTransformations(gh, inverse=TRUE)
  invs <- transformList(names(invs), invs)
}

# gives original-transformed single gate data
t <- transform(getData(gh, "G14"), inv_single(gh))
# plot (t@exprs[,"PMT 1"])

# g1data_ <- transform(

#fs_transformed <- getData(gs)
#fs_inverted <- transform(fs_transformed, inv_trans)
#write.flowSet(fs_inverted)

# g1data_inv <- transform(g1data, inv_trans)

