
# calculation with prior information

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
            loglik[iterations+1] <- eigenLogLikelihood(T) #compute log likelihood
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
            pi<-eigenMeanClusterProb(P_mat)
            toc()
            tic(msg="calculate new mu")
            mu<-eigenMu(P_mat,dimensionssample)
            toc()
            tic(msg="calculate new sigma")
            sigma<-eigenSigma(P_mat,mu,dimensionssample)
            toc()
            tic("Calculate T")
            T<-calc_T(pi,mu,sigma,dimensionssample)
            toc()
            tic(msg="Compute log-likelihood")
            loglik[iterations+1] <- eigenLogLikelihood(T) #compute log likelihood
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


# needs to be transferred

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
