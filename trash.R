
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
