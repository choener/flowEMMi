// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
#include <RcppEigen.h>
#include <list>
#include <cmath>
#include <iostream>
using namespace std;
using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]



/*
 * Given a matrix, that holds for each data point (row) the probability of
 * belonging to a certain cluster (column), calculate the column-wise means.
 * This gives for each cluster, the average probability that a data point
 * belongs to the cluster.
 */

// [[Rcpp::export]]
NumericVector eigenMeanClusterProb(NumericMatrix sampleClusterWeight) {
  // the incoming sampleClusterWeight matrix has size: samples >< clusters
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(sampleClusterWeight);
  // how many clusters do we have?
  Eigen::VectorXd means(P.cols());
  // for each cluster, we take the mean over all the column data, that is the
  // mean over the probability weights for each data element
  means = P.colwise().mean();
  // yields a vector with length in the number of clusters
  return Rcpp::NumericVector(wrap(means));
}



/* 
 * Calculate mean values of each cluster
 */

// [[Rcpp::export]]
NumericMatrix eigenMu(NumericMatrix sampleClusterWeight, NumericMatrix xs) {
  // the incoming sampleClusterWeight matrix has size: samples >< clusters
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(sampleClusterWeight);
  // the incoming xs matrix has size: samples >< 2
  Eigen::Map<Eigen::MatrixXd> D = as<Eigen::Map<Eigen::MatrixXd> >(xs);
  // we need a total weight for each column
  Eigen::VectorXd w(P.cols());
  // that is just the sum over all entries
  w = P.colwise().sum();
  // TODO ???
  Eigen::MatrixXd mu = D.transpose()*P;
  // re-weight each mu by the summed up weights, iterating over i in number of
  // clusters
  for(int i=0;i<w.rows();i++){
    mu(0,i) /= w(i);
    mu(1,i) /= w(i);
  }
  // yields the new mu matrix, which is 2 >< clusters
  return Rcpp::NumericMatrix(wrap(mu));
}



/*
 * Calculate covariance matrices. Requires the sample-cluster weight matrix,
 * the current means, as well as the data points xs.
 */

// [[Rcpp::export]]
List eigenSigma(NumericMatrix sampleClusterWeight, NumericMatrix mu, NumericMatrix xs) {
  // the incoming sampleClusterWeight matrix has size: samples >< clusters
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(sampleClusterWeight);
  // the incoming mu matrix has size: 2 >< clusters
  Eigen::Map<Eigen::MatrixXd> M = as<Eigen::Map<Eigen::MatrixXd> >(mu);
  // the incoming xs matrix has size: samples >< 2
  Eigen::Map<Eigen::MatrixXd> D = as<Eigen::Map<Eigen::MatrixXd> >(xs);
  // one covariance matrix for each cluster
  List sigma(M.cols());
  // temporary storage for the mu[i] vectors
  Eigen::VectorXd mean;
  // temporary storage for the per-cluster weights (column vector)
  Eigen::VectorXd P_i;
  // for each cluster ...
  for(int i=0;i<M.cols();i++){
    P_i=P.col(i);
    double w_i=P_i.sum(); // total weight for cluster
    mean = M.col(i);
    // efficient covariation calculation
    Eigen::MatrixXd centered = D.rowwise() - mean.transpose();
    Eigen::MatrixXd cajw = centered.array().colwise() * P_i.array();
    cajw.adjointInPlace();
    Eigen::MatrixXd cov = (cajw * centered) / w_i;
    // done for cluster [i]
    sigma(i)=cov;
  }
  return Rcpp::List(wrap(sigma));
}



/*
 * Calculate the updated density of each Gaussian at each data point. That is,
 * for each data point, the probability of belonging to a certain cluster.
 */

// [[Rcpp::export]]
NumericMatrix eigenDensitiesAtSamples(NumericVector clusterProbs, NumericMatrix mu, List sigma, NumericMatrix xs) {
  // cluster probabilities, one double for each cluster, summing to 1.0
  Eigen::Map<Eigen::VectorXd> p = as<Eigen::Map<Eigen::VectorXd> >(clusterProbs); 
  // the incoming mu, or cluster mean vector, matrix has size: 2 >< clusters
  Eigen::Map<Eigen::MatrixXd> M = as<Eigen::Map<Eigen::MatrixXd> >(mu);
  // the incoming xs matrix has size: samples >< 2
  Eigen::Map<Eigen::MatrixXd> D = as<Eigen::Map<Eigen::MatrixXd> >(xs);
  // temporary storage for each cluster i's mu[i] vector
  Eigen::VectorXd mean;
  // prepare to return a matrix of size: samples >< clusters
  Eigen::MatrixXd responsibilities(D.rows(),M.cols());
  double logSqrt2Pi=log(sqrt(2*M_PI));
  double cons = D.cols()*logSqrt2Pi;
  // for each cluster ...
  for(int i=0;i<M.cols();i++){
    mean = M.col(i);
    // extract the covariance matrix
    Eigen::Map<Eigen::MatrixXd> covMat = as<Eigen::Map<Eigen::MatrixXd> >(sigma(i));
    typedef Eigen::LLT<Eigen::MatrixXd> Chol;
    Chol chol(covMat);
    const Chol::Traits::MatrixL& L = chol.matrixL();
    Eigen::VectorXd quadform
        = ((((L.solve(D.transpose().colwise() - mean).colwise().squaredNorm())*-0.5).array()-cons).array().exp()).array()
        / L.determinant();
    responsibilities.col(i) = quadform.array() * p(i);
  }
  // chzs, this is a really bad idea ...!
  double konst = (65535-115)*(65535-143);
  responsibilities.col(0).setConstant(p(0) / konst);
  // no, really that was a very bad idea!
  return Rcpp::NumericMatrix(wrap(responsibilities));
}



/*
 * Calculate log-likelihood based on matrix of densities.
 */

// [[Rcpp::export]]
double eigenLogLikelihood(NumericMatrix densities, double backgroundProportion) {
  // incoming weighted cluster probabilities for each data point: samples >< clusters
  Eigen::Map<Eigen::MatrixXd> ds = as<Eigen::Map<Eigen::MatrixXd> >(densities);
  // row-wise (i.e. for each data point), sum over the weighted normal
  // distribution probabilities
  //
  // then take the log of this sum
  // hier: delete .log, add background, then log
  // Eigen::VectorXd lse = ds.array().rowwise().sum().array().log();
  Eigen::VectorXd lse = ds.array().rowwise().sum().array();
  // cb: die erste Spalte von ds (fuer background) muss wieder abgezogen werden:
  Eigen::VectorXd backgroundProbs = ds.col(0);
  lse = lse-backgroundProbs;
  // hier tats??chliche xMax,xMin,yMax und yMin einfuegen
  double konst = (65535-115)*(65535-143);
  double tmp = backgroundProportion/konst;
  // wie erstelle ich einen vektor, bei dem in jedem Eintrag tmp steht?
  Eigen::VectorXd tmps(lse.size());
  for (int i = 0; i <lse.size(); i++) {
    tmps[i] <- tmp;
  }
  
  lse = lse+tmps;
  lse = lse.array().log();
    
  
  // now, we have for each data-point the log(sum(weightedGaussian)), these
  // need to be summed up.
  // It is possible that some values, due to numerical inaccuracies, produce NaNs. We replace those
  // by "0" before summing up.
  double r = 0;
  int nana = 0;
  for (int i = 0; i <lse.size(); i++) {
    double k = lse[i];
    if (isfinite (k) )
    {
      r += k;
    } else
    {
      nana += 1;
    }
  };
  if (nana) {
    cout << lse.size() << " values, and no. nans: " << nana << endl;
  };
  return r;
}



/*
 * Row-Normalize density matrix.
 */

// [[Rcpp::export]]
NumericMatrix eigenRowNormalize(NumericMatrix densities) {
  Eigen::Map<Eigen::MatrixXd> ds = as<Eigen::Map<Eigen::MatrixXd> >(densities); 
  Eigen::MatrixXd dsArr = ds.array();
  for(int i=0; i<ds.rows(); i++){
    dsArr.row(i) /= dsArr.row(i).sum();
  }
  return Rcpp::NumericMatrix(wrap(dsArr));
}

