#include <Rcpp.h>
#include "penmap.h"

Rcpp::NumericVector helpful(penmap *inst){ 
  int Nrow = inst->helpful_list.size();
  Rcpp::NumericVector penalty_vec(Nrow);
  Losses::iterator it=inst->helpful_list.begin();
  for(int i=0; i<Nrow; i++){
    penalty_vec[i] = it->get_penalty();
    it++; 
  }
  return penalty_vec;
}

Rcpp::DataFrame df(penmap *inst){ 
  int Nrow = inst->breakpoints.size();
  Rcpp::NumericVector penalty(Nrow);
  Rcpp::NumericVector loss_on(Nrow);
  Rcpp::IntegerVector size_on(Nrow);
  Rcpp::NumericVector loss_after(Nrow);
  Rcpp::IntegerVector size_after(Nrow);
  BreakpointTree::iterator it=inst->breakpoints.begin();
  for(int i=0; i<Nrow; i++){
    penalty[i] = it->penalty;
    loss_on[i] = it->on->loss;
    size_on[i] = it->on->size;
    loss_after[i] = it->after->loss;
    size_after[i] = it->after->size;
    it++;
  }
  return Rcpp::DataFrame::create
    (Rcpp::Named("penalty", penalty),
     Rcpp::Named("loss_on", loss_on),
     Rcpp::Named("size_on", size_on),
     Rcpp::Named("loss_after", loss_after),
     Rcpp::Named("size_after", size_after)
     );
}

RCPP_MODULE(penmap_module){
  Rcpp::class_<penmap>("penmap")
    .constructor()
    .method("insert", &penmap::insert_loss_size)
    .method("df", &df)
    .method("helpful", &helpful)
    ;
}
