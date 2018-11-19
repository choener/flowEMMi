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


