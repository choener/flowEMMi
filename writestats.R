
# write statistics from the flowEMMi runs

# TODO should later work on the class, with run information

source("classes.R")



# textual output of the BIC information

statBIC <- function (bic)
{
  sink("bic.txt")
  print (bic)
  sink()
}



# textual output of each cluster position
statCluster <- function (mu, sigma)
{
  sink(sprintf("cluster-positions-%03d.txt",c))
  cat("Positions\n")
  print(mu)
  cat("\nCovariance Matrices\n")
  print(sigma)
  sink()
}

