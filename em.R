#!/usr/bin/env Rscript

# Start this program via @./em.R@ (in the directory of the program)

# TODO modify sourceCpp to includes the full directory we are in
# TODO consider using rPackages.littler

library (flowCore)
library (flowViz)
library (ggplot2)
library (gtools)
library (mixtools)
library (mvtnorm)
library (optparse)
library (colortools)
library (Rcpp)

source("classes.R")
source("plotting.R")
source("writestats.R")
source("emwrapper.R")

parser <- OptionParser ()
parser <- add_option (parser, c ("-v", "--verbose"), action = "store_true", default=FALSE, help="be very verbose")
parser <- add_option (parser, c ("-f", "--file"), type="character", default = "", help="file to process")
parser <- add_option (parser, c ("-x", "--channelx"), type="character", default = "PMT.1", help="first channel")
parser <- add_option (parser, c ("-y", "--channely"), type="character", default = "PMT.9", help="second channel")
parser <- add_option (parser, c ("--xstart"), type="integer", default = 1500, help="")
parser <- add_option (parser, c ("--xend"), type="integer", default = 40000, help="")
parser <- add_option (parser, c ("--ystart"), type="integer", default = 5000, help="")
parser <- add_option (parser, c ("--yend"), type="integer", default = 38000, help="")
parser <- add_option (parser, c ("--samples"), type="integer", default = 50, help="")
parser <- add_option (parser, c ("--prior"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--separation"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--inits"), type="integer", default = 10, help="")
parser <- add_option (parser, c ("--log"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--alpha"), type="double", default = 0.5, help="")
parser <- add_option (parser, c ("--imgformat"), type="character", default = "png", help="")
parser <- add_option (parser, c ("--startcluster"), type="integer", default = 2, help="")
parser <- add_option (parser, c ("--endcluster"), type="integer", default = 20, help="")

opts <- parse_args(parser)




# run the sampling function

flowEMMi_sample<-function( frame, ch1="FS.Log", ch2="FL.4.Log"
                         , x_start=0, x_end=4095,y_start=700,y_end=4095
                         ,use_log=TRUE,diff.ll=1,sample_size=10,start_cluster=8,end_cluster=15,prior=FALSE
                         ,pi_prior,mu_prior,sigma_prior,separation=TRUE,max_inits=5,total=FALSE,alpha=.05,img_format="png",verbose=TRUE)
{
  mat<-exprs(frame)
  pd <- mkFlowData(nth = sample_size
                   , xChannel=ch1, yChannel=ch2
                   , xMin=x_start, xMax=x_end
                   , yMin=y_start, yMax=y_end
                   , data=frame)
  dimensions <- pd@data
  dimensionsSample <- pd@sampled

  plotInputData(pd@sampled, nth=pd@nth, logScaled = use_log, imageFormat = img_format)
  n<-nrow(dimensionsSample)

  BIC<-rep(0,end_cluster)
  #palette <- distinctColorPalette(end_cluster)
  palette <- wheel ("steelblue", num = end_cluster)
  act_T<-list()
  act_P_mat<-list()
  act_pi<-list()
  pis<-list()
  act_mu<-list()
  mus<-list()
  act_sigma<-list()
  sigmas<-list()
  act_loglik<-list()
  act_iterations<-list()
  probs<-list()
  ll<-list()
  newList<-list()

  for(c in start_cluster:end_cluster)
  {
    number_of_inits<-1
    ll[c][1]<-0
    counter<-2
    repeat
    {
      # for each initialization, run the algorithm
      if (prior) {
        # this variant assumes that we collected start points from somewhere else
        # TODO re-use iterateEM ...
      } else {
        # this variant randomly selects start points for the EM algorithm
        iterateEM (deltaThreshold = 0.01, numClusters = c, flowDataObj = pd@sampled)
        error ()
      }
            #
            if(number_of_inits==1){
              # stores the final init=1 values
              print("First initialization.")
              act_T[[c]]<-T
              act_P_mat[[c]]<-P_mat
              act_pi[[c]]<-pi
              pis[[number_of_inits]]<-act_pi[[c]]
              act_mu[[c]]<-mu
              mus[[number_of_inits]]<-act_mu[[c]]
              act_sigma[[c]]<-sigma
              sigmas[[number_of_inits]]<-act_sigma[[c]]
              act_loglik[[c]]<-loglikelihood
              act_iterations[[c]]<-it
              print(paste0(it, " iterations."))
              number_of_inits<-number_of_inits+1
            }else if (number_of_inits>=2 && number_of_inits<max_inits)
            {
              # stores the final init>1 && init<max values
                print(paste0(number_of_inits, ". initialization"))
                if(loglikelihood>act_loglik[[c]]){
                  act_T[[c]]<-T
                  act_P_mat[[c]]<-P_mat
                  act_pi[[c]]<-pi
                  act_mu[[c]]<-mu
                  act_sigma[[c]]<-sigma
                  act_loglik[[c]]<-loglikelihood
                  act_iterations[[c]]<-it
                  print(paste0(it, " iterations."))
                }else{
                  print(paste0(it, " iterations."))
                }
                number_of_inits<-number_of_inits+1
            }else if(number_of_inits==max_inits)
            {
                  if(prior==TRUE){
                    print("First initialization.")
                    act_T[[c]]<-T
                    act_P_mat[[c]]<-P_mat
                    act_pi[[c]]<-pi
                    act_mu[[c]]<-mu
                    act_sigma[[c]]<-sigma
                    act_loglik[[c]]<-loglikelihood
                    act_iterations[[c]]<-it 
                    print(paste0(it, " iterations."))
                  }else{
                    print(paste0(number_of_inits, ". initialization"))
                    if(loglikelihood>act_loglik[[c]]){
                      act_T[[c]]<-T
                      act_P_mat[[c]]<-P_mat
                      act_pi[[c]]<-pi
                      act_mu[[c]]<-mu
                      act_sigma[[c]]<-sigma
                      act_loglik[[c]]<-loglikelihood
                      act_iterations[[c]]<-it
                      print(paste0(it, " iterations."))
                    }else{
                      act_T[[c]]<-act_T[[c]]
                      act_P_mat[[c]]<-act_P_mat[[c]]
                      act_pi[[c]]<-act_pi[[c]]
                      act_mu[[c]]<-act_mu[[c]]
                      act_sigma[[c]]<-act_sigma[[c]]
                      act_loglik[[c]]<-act_loglik[[c]]
                      act_iterations[[c]]<-act_iterations[[c]]
                      print(paste0(it, " iterations."))
                    }}
##############sigma test
                  #abs_sigma<-numeric()
                    #for(i in 1:length(act_sigma[[c]])){
                    #  abs_sigma[i]<-abs(act_sigma[[c]][[i]][1,2])
                    #}
                    #mean_sigma<-mean(abs_sigma)
###############sigma test
                      BIC[c]<- (-2*act_loglik[[c]])+((c*7)*log(nrow(dimensionssample)))
                      color_cluster_matrix<-matrix(ncol = 1,nrow=length(act_P_mat[[c]][,1]))
                      for(i in 1:length(act_P_mat[[c]][,1])){
                        max<-which.max(act_P_mat[[c]][i,])
                        if(separation==TRUE){
                          #if(abs(act_sigma[[c]][[max]][1,2])<0.3*mean_sigma){
                          if(min(sqrt(act_sigma[[c]][[max]][1,1]),sqrt(act_sigma[[c]][[max]][2,2]))<2500){
                            color_cluster_matrix[i,1]<-palette[max]
                          } else {
                            color_cluster_matrix[i,1]<-"white"
                          }
                        }else{
                          color_cluster_matrix[i,1]<-palette[max] 
                        }
                      }
                      colnames(color_cluster_matrix)="color_cluster"
                      plot_matrix<-cbind(dimensionssample,color_cluster_matrix)
                        t<-table(color_cluster_matrix)
                        if(separation==TRUE){
                          print(paste0("Found ",length(t)-1," real cell clusters. ", "Background: ",c - (length(t)-1)," of ", c, " clusters."))  
                          s<-sum(t[1:length(t)])
                          l<-length(t)-1
                          names<-rownames(t)
                          sum_foreground<-sum(t[1:l])
                          sum_background<-t[length(t)]
                          print(paste0("Foreground: ", sum_foreground, " events (", round(sum_foreground*100/n,digits=3)," %) of ", n , " total events."))
                          print(paste0("Background: ", sum_background, " events (", round(sum_background*100/n,digits=3)," %) of ", n , " total events."))
                          for(i in 1:l){
                            print(paste0("Cluster ", i , " : ", t[i], " events(", round(t[i]*100/sum_foreground,digits=3)," %) of ", sum_foreground, " foreground events."))
                          }
                          colors<-character()
                            a<-1
                          for (i in names(t)){
                            if(i=="white"){
                              colors[a]<-"Background"	
                            }else{
                              colors[a]<-paste0("Cluster ", a)
                              a<-a+1
                            }
                            }
                          if(img_format=="png"){
                            png(file=paste0(c,"_clusterssample_separation.png"),bg="white",width = 12, height = 12, units = 'in', res = 300)
                          }else if(img_format=="svg"){
                            svg(filename=paste0(c,"_clusterssample_separation.svg"),width = 12, height = 12, pointsize = 12, bg = "white")   
                          }
                          if(use_log==TRUE){
                            plot(x=plot_matrix[,1], y=plot_matrix[,2], col=plot_matrix[,3],pch=19,cex=.6,yaxt="n",xaxt="n",log="xy",xlab=ch1,ylab=ch2,xlim=c(min(mFSC),max(mFSC)),ylim=c(min(mFL),max(mFL)))
                            axis(1,at=c(1,10,100,1000,10000), labels=c(expression(paste("10"^"0")),expression(paste("10"^"1")),expression(paste("10"^"2")),expression(paste("10"^"3")),expression(paste("10"^"4"))))
                            axis(2,at=c(1,10,100,1000,10000), labels=c(expression(paste("10"^"0")),expression(paste("10"^"1")),expression(paste("10"^"2")),expression(paste("10"^"3")),expression(paste("10"^"4"))))
                            for(j in 1:c){
                              #if(abs(act_sigma[[c]][[max]][1,2])<0.3*mean_sigma){
                              if(min(sqrt(act_sigma[[c]][[j]][1,1]),sqrt(act_sigma[[c]][[j]][2,2]))<2500){
                                lines(ellipse(sigma=matrix(unlist(act_sigma[[c]][j]), ncol = 2, byrow = TRUE),mu = act_mu[[c]][,j],alpha=alpha,npoints = 100), col="black")
                              }
                            }
                          }else{
                            plot(x=plot_matrix[,1], y=plot_matrix[,2], col=plot_matrix[,3],pch=19,cex=.6,xlab=ch1,ylab=ch2,xlim=c(min(mFSC),max(mFSC)),ylim=c(min(mFL),max(mFL)))
                            for(j in 1:c){
                              if(min(sqrt(act_sigma[[c]][[j]][1,1]),sqrt(act_sigma[[c]][[j]][2,2]))<2500){
                              #for(k in 1:length(act_sigma[[c]])){
                                lines(ellipse(sigma=matrix(unlist(act_sigma[[c]][j]), ncol = 2, byrow = TRUE),mu = act_mu[[c]][,j],alpha=.3*(1 - act_pi[[c]][j]),npoints = 100), col="black")
                              }
                              }
                            }
                          legend(x="topleft",pch=rep(19,l), cex=0.6,col=sort(unique(plot_matrix[,3])),legend=colors,bty = "n")
                            dev.off()
                            if(total){
                              tt<-unname(t(t))
                              cols<-character()
                              for(a in 1:length(t)){
                                if(a<length(t))
                                {cols[a]<-paste0("G",a)
                                }else{
                                  cols[a]<-"Background"}
                              }
                              colnames(tt)<-cols
                            }else{
                              tt<-unname(t(t))
                              for(l in 1:length(t)){
                                if(l<length(t)){
                                  tt[l]<-round(t[l]/sum_foreground,digits=3)
                                }else{tt[l]<-round(t[l]/n,digits=3)}
                              }
                              cols<-character()
                                for(a in 1:length(t)){
                                  if(a<length(t))
                                  {cols[a]<-paste0("G",a)
                                  }else{
                                    cols[a]<-"Background"}
                                }
                                colnames(tt)<-cols  
                            }
                            write.table(x=tt,file=paste0("cell_numbers_separation_",c,"_clusters.txt"),sep="\t",quote = FALSE,row.names = FALSE)
                          }else{
                            print(paste0("Found ",length(t)," real cell clusters. ")) 
                            sum_foreground<-sum(t[1:length(t)])
                            print(paste0("Foreground: ", sum_foreground, " events (", round(sum_foreground*100/n,digits=3)," %) of ", n , " total events."))
                            for(i in 1:length(t)){
                              print(paste0("Cluster ", i , " : ", t[i], " events(", round(t[i]*100/sum_foreground,digits=3)," %) of ", sum_foreground, " foreground events."))
                            }
                            colors<-character()
                              for (i in 1:length(t)){
                                colors[i]<-paste0("Cluster ", i)
                              }
                              if(img_format=="png"){
                                png(file=paste0(c,"_clusterssample.png"),bg="white",width = 12, height = 12, units = 'in', res = 300)
                              }else if(img_format=="svg"){
                                svg(filename=paste0(c,"_clusterssample.svg"), width = 12, height = 12, pointsize = 12, bg = "white") 
                              }
                              if(use_log==TRUE){  
                                plot(x=plot_matrix[,1], y=plot_matrix[,2], col=plot_matrix[,3],pch=19,cex=.6,yaxt="n",xaxt="n",log="xy",xlab=ch1,ylab=ch2,xlim=c(min(mFSC),max(mFSC)),ylim=c(min(mFL),max(mFL)))
                                axis(1,at=c(1,10,100,1000,10000), labels=c(expression(paste("10"^"0")),expression(paste("10"^"1")),expression(paste("10"^"2")),expression(paste("10"^"3")),expression(paste("10"^"4"))))
                                axis(2,at=c(1,10,100,1000,10000), labels=c(expression(paste("10"^"0")),expression(paste("10"^"1")),expression(paste("10"^"2")),expression(paste("10"^"3")),expression(paste("10"^"4"))))
                                for(j in 1:length(t)){
                                  lines(ellipse(sigma=matrix(unlist(act_sigma[[c]][j]), ncol = 2, byrow = TRUE),mu = act_mu[[c]][,j],alpha=alpha,npoints = 100), col="black")
                                }
                              }else{
                                plot(x=plot_matrix[,1], y=plot_matrix[,2], col=plot_matrix[,3],pch=19,cex=.6,xlab=ch1,ylab=ch2,xlim=c(min(mFSC),max(mFSC)),ylim=c(min(mFL),max(mFL))) 
                                for(j in 1:length(t)){
                                  lines(ellipse(sigma=matrix(unlist(act_sigma[[c]][j]), ncol = 2, byrow = TRUE),mu = act_mu[[c]][,j],alpha=alpha,npoints = 100), col="black")
                                }
                              }
                              legend(x="topleft",pch=rep(19,length(t)), cex=0.6,col=sort(unique(plot_matrix[,3])),legend=colors,bty = "n")
                                dev.off()
                                if(total){
                                  tt<-unname(t(t))
                                  cols<-character()
                                  for(a in 1:length(t)){
                                    cols[a]<-paste0("G",a)
                                  }
                                  colnames(tt)<-cols
                                }else{
                                  tt<-unname(t(t))
                                  for(l in 1:length(t)){
                                    tt[l]<-round(t[l]/sum_foreground,digits=3)
                                  }
                                  cols<-character()
                                    for(a in 1:length(t)){
                                      cols[a]<-paste0("G",a)
                                    }
                                    colnames(tt)<-cols  
                                }
                                write.table(x=tt,file=paste0("cell_numbers_",c,"_clusters.txt"),sep="\t",quote = FALSE,row.names = FALSE)
                          }
                          probs[[c]]<-length(t)-1
                          newList$mu[[c]]<-act_mu[[c]]
                          newList$sigma[[c]]<-act_sigma[[c]]
                          newList$pi[[c]]<-act_pi[[c]]
                          newList$BIC[c]<-BIC[c]
                          newList$ll[c]<-ll[c]
                          newList$probs[[c]]<-probs[[c]]
                          newList$matrix<-plot_matrix
                            break
                          }
    } # repeat
  } # for start_cluster ... end_cluster
  plotBIC (newList$BIC)
  statBIC (newList$BIC)
  for(c in start_cluster:end_cluster)
  {
    statCluster(newList$mu[[c]], newList$sigma[[c]])
  }
} # flowEMMi_sample


# load sample
fcsData <- read.FCS(opts$file,alter.names = TRUE,transformation = FALSE)
# run actual flowEMMi algorithm
results <- flowEMMi_sample( frame = fcsData
                          , ch1=opts$channelx, ch2=opts$channely
                          , x_start = opts$xstart, x_end = opts$xend, y_start=opts$ystart, y_end=opts$yend
                          , sample_size = opts$samples
                          , prior = opts$prior
                          , separation = opts$separation
                          , max_inits = opts$inits
                          , use_log = opts$log
                          , alpha = opts$alpha, img_format = opts$imgformat
                          , start_cluster = opts$startcluster, end_cluster = opts$endcluster
                          )

