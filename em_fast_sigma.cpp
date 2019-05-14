// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
#include <RcppEigen.h>
#include <list>
#include <iostream>
using namespace std;
using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
///////////////////////////////// Calculate means of probability matrix
NumericVector eigenMeanClusterProb(NumericMatrix dcw) {
  // the incoming dcw matrix has size: samples >< clusters
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(dcw);
  // how many clusters do we have?
  Eigen::VectorXd means(P.cols());
  // for each cluster, we take the mean over all the column data, that is the
  // mean over the probability weights for each data element
  means = P.colwise().mean();
  // yields a vector with length in the number of clusters
  return Rcpp::NumericVector(wrap(means));
}
/////////////////////////////////

// [[Rcpp::export]]
///////////////////////////////// Calculate mean values of each cluster
NumericMatrix eigenMu(NumericMatrix dcw, NumericMatrix xs) {
  // the incoming dcw matrix has size: samples >< clusters
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(dcw);
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
/////////////////////////////////

///////////////////////////////// Calculate covariance matrices
// [[Rcpp::export]]
List eigenSigma(NumericMatrix dcw, NumericMatrix mu, NumericMatrix xs) {
  // the incoming dcw matrix has size: samples >< clusters
  Eigen::Map<Eigen::MatrixXd> P = as<Eigen::Map<Eigen::MatrixXd> >(dcw);
  // the incoming mu matrix has size: 2 >< clusters
  Eigen::Map<Eigen::MatrixXd> M = as<Eigen::Map<Eigen::MatrixXd> >(mu);
  // the incoming xs matrix has size: samples >< 2
  Eigen::Map<Eigen::MatrixXd> D = as<Eigen::Map<Eigen::MatrixXd> >(xs);
  // one covariance matrix for each cluster
  List ss(M.cols());
  // TODO ???
  Eigen::VectorXd mean;
  // TODO ???
  Eigen::VectorXd P_i;
  // for each cluster ...
  for(int i=0;i<M.cols();i++){
    P_i=P.col(i);
    double w_i=P_i.sum();
    mean = M.col(i);
    Eigen::MatrixXd centered = D.rowwise() - mean.transpose();
    Eigen::MatrixXd cajw = centered.array().colwise() * P_i.array();
    cajw.adjointInPlace();
    Eigen::MatrixXd cov = (cajw * centered) / w_i;
    ss(i)=cov;
  }
  return Rcpp::List(wrap(ss));
}
/////////////////////////////////

// [[Rcpp::export]]
///////////////////////////////// Calculate T
NumericMatrix calc_T(NumericVector pi, NumericMatrix mu, List sigma, NumericMatrix xs) {
  Eigen::Map<Eigen::VectorXd> p = as<Eigen::Map<Eigen::VectorXd> >(pi); 
  // the incoming mu matrix has size: 2 >< clusters
  Eigen::Map<Eigen::MatrixXd> M = as<Eigen::Map<Eigen::MatrixXd> >(mu);
  // the incoming xs matrix has size: samples >< 2
  Eigen::Map<Eigen::MatrixXd> D = as<Eigen::Map<Eigen::MatrixXd> >(xs);
  Eigen::VectorXd mean;
  // prepare to return a matrix of size: samples >< clusters
  Eigen::MatrixXd T(D.rows(),M.cols());
  double logSqrt2Pi=log(sqrt(2*M_PI));
  double cons = D.cols()*logSqrt2Pi;
  // for each cluster ...
  for(int i=0;i<M.cols();i++){
    mean = M.col(i);
    // extract correct covariance matrix
    Eigen::Map<Eigen::MatrixXd> S = as<Eigen::Map<Eigen::MatrixXd> >(sigma(i));
    typedef Eigen::LLT<Eigen::MatrixXd> Chol;
    Chol chol(S);
    const Chol::Traits::MatrixL& L = chol.matrixL();
    Eigen::VectorXd quadform
        = ((((L.solve(D.transpose().colwise() - mean).colwise().squaredNorm())*-0.5).array()-cons).array().exp()).array()
        / L.determinant();
    T.col(i) = quadform.array() * p(i);
  }
  return Rcpp::NumericMatrix(wrap(T));
}
///////////////////////////////// 

// [[Rcpp::export]]
///////////////////////////////// Calculate log-likelihood
double eigenLogLikelihood(NumericMatrix T) {
  // incoming weighted cluster probabilities for each data point: samples >< clusters
  Eigen::Map<Eigen::MatrixXd> T_mat = as<Eigen::Map<Eigen::MatrixXd> >(T); 
  // row-wise (i.e. for each data point), sum over the weighted normal
  // distribution probabilities
  //
  // then take the log of this sum
  Eigen::VectorXd lse = T_mat.array().rowwise().sum().array().log();
  // now, we have for each data-point the log(sum(weightedGaussian)), these
  // need to be summed up.
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

