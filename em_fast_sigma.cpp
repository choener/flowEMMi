// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
#include <RcppEigen.h>
#include <list>
#include <iostream>
using namespace std;
using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
///////////////////////////////// Calculate means of probability matrix
NumericVector colMeans(NumericMatrix P_mat) {
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(P_mat);
  Eigen::VectorXd means(P.cols());
  means = P.colwise().mean();
  return Rcpp::NumericVector(wrap(means));
}
/////////////////////////////////

// [[Rcpp::export]]
///////////////////////////////// Calculate mean values of each cluster
NumericMatrix calc_mu(NumericMatrix P_mat,NumericMatrix dimensionssample) {
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(P_mat);
  Eigen::Map<Eigen::MatrixXd> D = as<Eigen::Map<Eigen::MatrixXd> >(dimensionssample);
  Eigen::VectorXd w(P.cols());
  w = P.colwise().sum();
  Eigen::MatrixXd mu = D.transpose()*P;
  for(int i=0;i<w.rows();i++){
    mu(0,i) /= w(i);
    mu(1,i) /= w(i);
  }
  return Rcpp::NumericMatrix(wrap(mu));
}
/////////////////////////////////

///////////////////////////////// Calculate covariance matrices
// [[Rcpp::export]]
List calc_sigma(NumericMatrix P_mat,NumericMatrix mu,NumericMatrix dimensionssample) {
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(P_mat);
  Eigen::Map<Eigen::MatrixXd> M = as<Eigen::Map<Eigen::MatrixXd> >(mu);
  Eigen::Map<Eigen::MatrixXd> D = as<Eigen::Map<Eigen::MatrixXd> >(dimensionssample);
  List listOfSigmas(M.cols());
  Eigen::VectorXd mean;
  Eigen::VectorXd P_i;
  for(int i=0;i<M.cols();i++){
    P_i=P.col(i);
    double w_i=P_i.sum();
    mean = M.col(i);
    Eigen::MatrixXd centered = D.rowwise() - mean.transpose();
    Eigen::MatrixXd cajw = centered.array().colwise() * P_i.array();
    cajw.adjointInPlace();
    Eigen::MatrixXd cov = (cajw * centered) / w_i;
    listOfSigmas(i)=cov;
  }
  return Rcpp::List(wrap(listOfSigmas));
}
/////////////////////////////////

// [[Rcpp::export]]
///////////////////////////////// Calculate T
NumericMatrix calc_T(NumericVector pi,NumericMatrix mu, List sigma, NumericMatrix dimensionssample) {
  Eigen::Map<Eigen::VectorXd> p = as<Eigen::Map<Eigen::VectorXd> >(pi); 
  Eigen::Map<Eigen::MatrixXd> M = as<Eigen::Map<Eigen::MatrixXd> >(mu);
  Eigen::Map<Eigen::MatrixXd> D = as<Eigen::Map<Eigen::MatrixXd> >(dimensionssample);
  Eigen::VectorXd mean;
  Eigen::MatrixXd T(D.rows(),M.cols());
  double logSqrt2Pi=log(sqrt(2*M_PI));
  double cons = D.cols()*logSqrt2Pi;
  for(int i=0;i<M.cols();i++){
    mean = M.col(i);
    Eigen::Map<Eigen::MatrixXd> S = as<Eigen::Map<Eigen::MatrixXd> >(sigma(i));
    typedef Eigen::LLT<Eigen::MatrixXd> Chol;
    Chol chol(S);
    const Chol::Traits::MatrixL& L = chol.matrixL();
    Eigen::VectorXd quadform = ((((L.solve(D.transpose().colwise() - mean).colwise().squaredNorm())*-0.5).array()-cons).array().exp()).array()/L.determinant();
    T.col(i) = quadform.array() * p(i);
  }
  return Rcpp::NumericMatrix(wrap(T));
}
///////////////////////////////// 

// [[Rcpp::export]]
///////////////////////////////// Calculate log-likelihood
double calc_loglik(NumericMatrix T) {
  Eigen::Map<Eigen::MatrixXd> T_mat = as<Eigen::Map<Eigen::MatrixXd> >(T); 
  Eigen::VectorXd lse = T_mat.array().rowwise().sum().array().log();
  return lse.sum();
}
/////////////////////////////////  

// [[Rcpp::export]]
///////////////////////////////// Calculate means of probability matrix
NumericMatrix calc_Pmat(NumericMatrix T) {
  Eigen::Map<Eigen::MatrixXd> T_mat = as<Eigen::Map<Eigen::MatrixXd> >(T); 
  Eigen::MatrixXd T_new = T_mat.array();
  for(int i=0;i<T_mat.rows();i++){
    T_new.row(i) /= T_new.row(i).sum();
  }
  return Rcpp::NumericMatrix(wrap(T_new));
}


/*** R
setwd("C:/users/schumanj/Desktop/EM/for Joachim/new")
  library("gtools")
  #library("mvtnorm")
  library("flowCore")
  library("ggplot2")
  #library("hexbin")
  library("flowViz")
  library("randomcoloR")
  flowEMMi_sample<-function(frame,ch1="FS.Log",ch2="FL.4.Log",x_start=0,
                            x_end=4095,y_start=700,y_end=4095,use_log=TRUE,diff.ll=1,sample_size=10,start_cluster=8,end_cluster=15,prior=FALSE,pi_prior,mu_prior,sigma_prior,separation=TRUE,max_inits=5,total=FALSE,alpha=.05,img_format="png",verbose=TRUE){
    mat<-exprs(fr)
    mFSC<-mat[,ch1]
    mFL<-mat[,ch2]
    coords <- list(c(x_start,x_end+min(mFSC)), c(y_start,y_end+min(mFL))) # define subset area
    names(coords) <- c(ch1, ch2)
    Noise <- rectangleGate(filterId="Noise",  .gate = coords) # filter noise
    Noise.subset <- Subset(fr, Noise)
    matNoise<-exprs(Noise.subset)
    mNoiseFSC<-matNoise[,ch1]
    mNoiseFL<-matNoise[,ch2]
    dim1<- mNoiseFSC
    dim2<- mNoiseFL
    dimensions<-cbind(dim1,dim2) #both dimensions as matrix
########### Sampling
    if(img_format=="png"){
      png(file=paste0(sample_size,"_sample.png"),bg="white",width = 12, height = 12, units = 'in', res = 300)
    }else if(img_format=="svg"){
      svg(filename=paste0(sample_size,"_sample.svg"),width = 12, height = 12,pointsize = 12, bg = "white")
    }
    dimensionssample<-dimensions[sample(nrow(dimensions),size=nrow(dimensions)/sample_size,replace=FALSE),]
    colnames(dimensionssample)<-c(ch1,ch2)
      if(use_log==TRUE){
        plot(dimensionssample,yaxt="n",xaxt="n",log="xy",type="p",cex=.6,pch=19,xlim=c(min(mFSC),max(mFSC)),ylim=c(min(mFL),max(mFL)),xlab=ch1,ylab=ch2)
        axis(1,at=c(1,10,100,1000,10000), labels=c(expression(paste("10"^"0")),expression(paste("10"^"1")),expression(paste("10"^"2")),expression(paste("10"^"3")),expression(paste("10"^"4"))))
        axis(2,at=c(1,10,100,1000,10000), labels=c(expression(paste("10"^"0")),expression(paste("10"^"1")),expression(paste("10"^"2")),expression(paste("10"^"3")),expression(paste("10"^"4"))))
      }else{
        plot(dimensionssample,type="p",cex=.6,pch=19,xlim=c(min(mFSC),max(mFSC)),ylim=c(min(mFL),max(mFL)),xlab=ch1,ylab=ch2)
      }
      dev.off()
        n<-nrow(dimensionssample)
####################
        BIC<-rep(0,end_cluster)
        palette <- distinctColorPalette(end_cluster)
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
        for(c in start_cluster:end_cluster){
          print(paste0("Number of clusters: ",c))
          number_of_inits<-1
          ll[c][1]<-0
          counter<-2
          repeat{
            if(prior==FALSE){
              loglik<- c()
              loglik[1]<-0
              iterations<-1
              diff.tmp <- 1000
              P_mat<-rdirichlet(n,rep(1,c))
              while(diff.tmp > diff.ll) {
                if(iterations==1){
                  pi<-colMeans(P_mat)
                  start<-dimensionssample[sample(nrow(dimensionssample),size=c,replace=FALSE),]
                  mu<-t(start)
                  sigma<-calc_sigma(P_mat,mu,dimensionssample)
                  T<-calc_T(pi,mu,sigma,dimensionssample)
                  loglik[iterations+1] <- calc_loglik(T) #compute log likelihood
                  ll[[c]][counter]<-loglik[iterations+1]
                  if(iterations >= 2){
                    diff.tmp <- abs(loglik[iterations+1]-loglik[iterations])
                  }
                  P_mat<-calc_Pmat(T)
                    loglikelihood=loglik[iterations+1]
                  it<-iterations
                    iterations<-iterations+1
                  counter<-counter+1
                }else{
################## M-step
                  pi<-colMeans(P_mat)
                  mu<-calc_mu(P_mat,dimensionssample)
                  sigma<-calc_sigma(P_mat,mu,dimensionssample)
                  T<-calc_T(pi,mu,sigma,dimensionssample)
                  loglik[iterations+1] <- calc_loglik(T) #compute log likelihood
                  ll[[c]][counter]<- loglik[iterations+1]
                  if(iterations >= 2){
                    diff.tmp <- abs(loglik[iterations+1]-loglik[iterations])
                  }
                  P_mat<-calc_Pmat(T)
                    loglikelihood=loglik[iterations+1]
                  it<-iterations
                    iterations<-iterations+1
                  counter<-counter+1
                }
              }
            }else if(prior==TRUE){
              number_of_inits<-max_inits
              loglik<- c()
              loglik[1]<-0
              iterations<-1
              diff.tmp <- 1000  
              while(diff.tmp > diff.ll) { 
                print(iterations)
                if(iterations==1){
                  pi<-pi_prior[[c]]
                  mu<-mu_prior[[c]]
                  sigma<-sigma_prior[[c]]
                  tic(msg="Build T[,m]")
                  T<-calc_T(pi,mu,sigma,dimensionssample)
                  toc()
                  tic(msg="Compute log-likelihood")
                  loglik[iterations+1] <- calc_loglik(T) #compute log likelihood
                  toc()
                  ll[[c]][counter]<-loglik[iterations+1]
                  tic(msg="Compute new P_mat")
                  P_mat<-calc_Pmat(T)
                  toc()
                  loglikelihood=loglik[iterations+1]
                  it<-iterations
                  iterations<-iterations+1
                  counter<-counter+1  
                }else{
                  tic(msg="Calculate pi's")
                  pi<-colMeans(P_mat)
                  toc()
                  tic(msg="calculate new mu")
                  mu<-calc_mu(P_mat,dimensionssample)
                  toc()
                  tic(msg="calculate new sigma")
                  sigma<-calc_sigma(P_mat,mu,dimensionssample)
                  toc()
                  tic("Calculate T")
                  T<-calc_T(pi,mu,sigma,dimensionssample)
                  toc()
                  tic(msg="Compute log-likelihood")
                  loglik[iterations+1] <- calc_loglik(T) #compute log likelihood
                  toc()
                  ll[[c]][counter]<-loglik[iterations+1]
                  diff.tmp <- abs(loglik[iterations+1]-loglik[iterations])
                  tic(msg="Compute new P_mat")
                  P_mat<-calc_Pmat(T)
                  toc()
                  loglikelihood=loglik[iterations+1]
                  it<-iterations
                  iterations<-iterations+1
                  counter<-counter+1
                }
              }
            }
            if(number_of_inits==1){
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
              number_of_inits<-number_of_inits+1}else if(number_of_inits>=2 && number_of_inits<max_inits){
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
                number_of_inits<-number_of_inits+1}else if(number_of_inits==max_inits){
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
#if(abs(BIC[c]-BIC[c-1])<abs(.0002*BIC[c])){
#if(abs(BIC[c]-BIC[c-1])<100){
#  print(paste0("Best number of cluster: ",c))
#}#png(filename=paste0(c,"_clusters_new.png"),width = 800, height = 600, units = "px")
                      color_cluster_matrix<-matrix(ncol = 1,nrow=length(act_P_mat[[c]][,1]))
                      for(i in 1:length(act_P_mat[[c]][,1])){
                        max<-which.max(act_P_mat[[c]][i,])
                        if(separation==TRUE){
                          #if(abs(act_sigma[[c]][[max]][1,2])<0.3*mean_sigma){
                          if(min(sqrt(act_sigma[[c]][[max]][1,1]),sqrt(act_sigma[[c]][[max]][2,2]))<2500){
                            color_cluster_matrix[i,1]<-palette[max]
                          } else {
#color_cluster_matrix[i,1]<-"white"
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
#if(i=="white"){
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
#plot(x=plot_matrix[,1], y=plot_matrix[,2], col=plot_matrix[,3],pch=19,cex=.6,xlab=ch1,ylab=ch2,xlim=c(min(mFSC),150),ylim=c(min(mFL),150))
                            plot(x=plot_matrix[,1], y=plot_matrix[,2], col=plot_matrix[,3],pch=19,cex=.6,xlab=ch1,ylab=ch2,xlim=c(min(mFSC),max(mFSC)),ylim=c(min(mFL),max(mFL)))
                            for(j in 1:c){
#if(abs(act_sigma[[c]][[j]][1,2])<0.3*mean_sigma){
                              if(min(sqrt(act_sigma[[c]][[j]][1,1]),sqrt(act_sigma[[c]][[j]][2,2]))<2500){
                              #for(k in 1:length(act_sigma[[c]])){
#lines(ellipse(sigma=matrix(unlist(act_sigma[[c]][j]), ncol = 2, byrow = TRUE),mu = act_mu[[c]][,j],alpha=.1*(1 - act_pi[[c]][j]),npoints = 100), col="black")
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
                        }
}
        if(verbose)
          return (newList)
                }
################ Linear without prior
fr <- read.FCS("InTH_160712_025.fcs",alter.names = TRUE,transformation = FALSE)
flowEMMi_sample_results<-flowEMMi_sample(frame=fr,ch1="PMT.1",ch2="PMT.9",x_start=1500,
                                           x_end=40000,y_start=5000,y_end=38000,sample_size=50,prior=FALSE,separation = FALSE,max_inits = 10,use_log = FALSE,alpha = .5,img_format = "svg",start_cluster = 2,end_cluster = 20)
############### Linear with prior
#pi_prior<-flowEMMi_sample_results$pi
#mu_prior<-flowEMMi_sample_results$mu
#sigma_prior<-flowEMMi_sample_results$sigma
#flowEMMi_sample_results<-flowEMMi_sample(frame=fr,ch1="PMT.1",ch2="PMT.9",x_start=1500,
#                                        x_end=40000,y_start=5000,y_end=38000,sample_size=1,prior=TRUE,diff.ll = 1,pi_prior=pi_prior,mu_prior=mu_prior,sigma_prior=sigma_prior,separation = TRUE,use_log = FALSE,alpha = .7,img_format = "png",start_cluster = 10,end_cluster = 20)
###############
  
############### Logarithmic without prior
#fr <- read.FCS("InTH_160713_012.fcs",alter.names = TRUE)
#flowEMMi_sample_results<-flowEMMi_sample(frame=fr,ch1="PMT.1",ch2="PMT.9",x_start=1.05,
#x_end=1000,y_start=1.05,y_end=250,sample_size=50,prior=FALSE,separation = TRUE,max_inits = 10,img_format = "svg",start_cluster = 12,end_cluster = 20,use_log = TRUE)
############### Linear with prior
#pi_prior<-flowEMMi_sample_results$pi
#mu_prior<-flowEMMi_sample_results$mu
#sigma_prior<-flowEMMi_sample_results$sigma
#flowEMMi_sample_results<-flowEMMi_sample(frame=fr,ch1="PMT.1",ch2="PMT.9",x_start=1.05,
#                                        x_end=1000,y_start=1.05,y_end=250,sample_size=1,prior=TRUE,diff.ll = 1,pi_prior=pi_prior,mu_prior=mu_prior,sigma_prior=sigma_prior,separation = TRUE,max_inits = 3,alpha = .2,img_format = "png",start_cluster = 12,end_cluster = 20,use_log = TRUE)
  
  */

