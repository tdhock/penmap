/* -*- compile-command: "R -e 'Rcpp::compileAttributes(\"..\")' && R CMD INSTALL .. && R --vanilla < ../tests/testthat/test-CRAN-penmap.R" -*- */
#include <Rcpp.h>
#define UNKNOWN loss_list.end()
#define LOSS(it) ((it)==(UNKNOWN)) ? (INFINITY) : ((it)->loss)
#define SIZE(it) ((it)==(UNKNOWN)) ? (-1) : ((it)->size)

class lossSize {
public:
  double loss;
  int size;
  lossSize(double l_, int s_) :
    loss(l_), size(s_) {}
};

typedef std::list<lossSize> Losses;

class breakInfo {
public:
  mutable Losses::iterator on, after;
  mutable double penalty;
  breakInfo(double p){
    penalty = p;
  }
  breakInfo
  (double p_,
   Losses::iterator on_it,
   Losses::iterator after_it) {
    penalty = p_;
    on = on_it; 
    after = after_it;
  }
  friend bool operator<(const breakInfo& l, const breakInfo& r)
  {
    return l.penalty < r.penalty;
  }  
};

typedef std::set<breakInfo> BreakpointTree;

double crossing_point(double l1, double l2, int c1, int c2){
  return (l1-l2)/(c2-c1);
}

class msMap {
public:
  BreakpointTree breakpoints;
  Losses loss_list;
  BreakpointTree::iterator smaller_pen, larger_pen;
  void insert_simplify
  (double penalty,
   Losses::iterator on,
   Losses::iterator after){
    if(larger_pen != breakpoints.end() &&
       larger_pen->after == after &&
       after != UNKNOWN){
      larger_pen->penalty = penalty;
      larger_pen->on = on;
      return;
    }
    if(smaller_pen != breakpoints.end() &&
       smaller_pen != breakpoints.begin() &&
       prev(smaller_pen)->after == on){
      smaller_pen->penalty = penalty;
      smaller_pen->after = after;
      return;
    }
    if(smaller_pen != breakpoints.end() && smaller_pen->on == on){
      smaller_pen->after = on;
    }
    if(larger_pen != breakpoints.end() && larger_pen->on == on){
      after = on;
    }
    breakpoints.emplace_hint(larger_pen, penalty, on, after);
  }
  bool smaller_is_interval(){
    if(smaller_pen == breakpoints.begin()){
      return false;
    }else{
      return prev(smaller_pen)->after == smaller_pen->on;
    }
  }
  void insert(double penalty, double loss, int size){
    breakInfo new_break(penalty);
    // An iterator to the the first element in the container which is
    // not considered to go before val (can be same value), or
    // set::end if all elements are considered to go before val.
    double larger_lambda, smaller_lambda;
    larger_pen = breakpoints.lower_bound(new_break);
    double larger_pen_size_diff = INFINITY;
    double smaller_pen_size_diff = INFINITY;
    // Compute larger/smaller size differences.
    if(larger_pen == breakpoints.begin()){
      smaller_pen = breakpoints.end();//marker for does not exist.
    }else{
      smaller_pen = prev(larger_pen);
      if(smaller_pen->after != UNKNOWN){
	throw std::range_error("penalty already known");
      }
      smaller_lambda = crossing_point
	(loss, smaller_pen->on->loss,
	 size, smaller_pen->on->size);
      smaller_pen_size_diff = smaller_pen->on->size - size;
    }
    if(larger_pen != breakpoints.end()){
      if(larger_pen->penalty == penalty){
	throw std::range_error("penalty already known");
      }	
      larger_lambda = crossing_point
	(loss, larger_pen->on->loss,
	 size, larger_pen->on->size);
      larger_pen_size_diff = size - larger_pen->on->size;
    }
    if(larger_pen != breakpoints.end() && larger_pen != breakpoints.begin()){
      double other_lambda = crossing_point
	(smaller_pen->on->loss, larger_pen->on->loss, 
	 smaller_pen->on->size, larger_pen->on->size);
      bool adjacent_same_size =
	smaller_pen_size_diff==0 ||
	larger_pen_size_diff==0;
      if(penalty == other_lambda && adjacent_same_size){
	// there are no other models between smaller and larger.
	insert_simplify(other_lambda, smaller_pen->on, larger_pen->on);
	return;
      }
    }
    if(smaller_pen_size_diff == 0){
      insert_simplify(penalty, smaller_pen->on, UNKNOWN);
      return;
    }
    if(larger_pen_size_diff == 0){
      insert_simplify(penalty, larger_pen->on, larger_pen->on);
      return;
    }
    loss_list.emplace_front(loss, size);
    if(larger_pen_size_diff == 1 && smaller_pen_size_diff == 1){
      insert_simplify(smaller_lambda, smaller_pen->on, loss_list.begin());
      insert_simplify(larger_lambda, loss_list.begin(), larger_pen->on);
      return;
    }
    if(larger_pen_size_diff == 1){
      if(penalty < larger_lambda){
	insert_simplify(penalty, loss_list.begin(), loss_list.begin());
	insert_simplify(larger_lambda, larger_pen->on, larger_pen->on);
      }else{
	insert_simplify(penalty, loss_list.begin(), larger_pen->on);
      }
      return;
    }
    if(smaller_pen_size_diff == 1){
      if(smaller_lambda < penalty){
	insert_simplify(smaller_lambda, smaller_pen->on, loss_list.begin());
      }else{
	smaller_pen->after = smaller_pen->on;
      }
      insert_simplify(penalty, loss_list.begin(), UNKNOWN);
      return;
    }
    //last case: empty set, just insert.
    breakpoints.emplace_hint(larger_pen, penalty, loss_list.begin(), UNKNOWN);
  }
  Rcpp::DataFrame df(){ 
    int Nrow = breakpoints.size();
    Rcpp::NumericVector penalty(Nrow);
    Rcpp::NumericVector loss_on(Nrow);
    Rcpp::IntegerVector size_on(Nrow);
    Rcpp::NumericVector loss_after(Nrow);
    Rcpp::IntegerVector size_after(Nrow);
    BreakpointTree::iterator it=breakpoints.begin();
    for(int i=0; i<Nrow; i++){
      penalty[i] = it->penalty;
      loss_on[i] = LOSS(it->on);
      size_on[i] = SIZE(it->on);
      loss_after[i] = LOSS(it->after);
      size_after[i] = SIZE(it->after);
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
};

RCPP_MODULE(map_module){
  Rcpp::class_<msMap>("penmap")
    .constructor()
    .method("insert", &msMap::insert)
    .method("df", &msMap::df)
    ;
}
