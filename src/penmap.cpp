/* -*- compile-command: "R -e 'Rcpp::compileAttributes(\"..\")' && R CMD INSTALL .. && R --vanilla < ../tests/testthat/test-CRAN-penmap.R" -*- */
#include <map>
#include <Rcpp.h>

class lossSize {
public:
  double loss;
  int model_size;
  lossSize(double l_, int s_) :
    loss(l_), model_size(s_) {}
};

typedef std::list<lossSize> Losses;

class breakInfo {
public:
  mutable Losses::iterator model;
  mutable bool after;
  mutable double penalty;
  breakInfo(double p){
    penalty = p;
  }
  breakInfo
  (double p_,
   Losses::iterator it,
   bool after_) {
    penalty = p_;
    model = it; 
    after = after_;
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
  void insert(double penalty, double loss, int model_size){
    breakInfo new_break(penalty);
    // An iterator to the the first element in the container which is
    // not considered to go before val (can be same value), or
    // set::end if all elements are considered to go before val.
    double larger_lambda, smaller_lambda;
    BreakpointTree::iterator larger_pen = breakpoints.lower_bound(new_break);
    BreakpointTree::iterator smaller_pen;
    BreakpointTree::iterator before_smaller_pen;
    bool smaller_is_interval;
    double larger_pen_size_diff = INFINITY;
    double smaller_pen_size_diff = INFINITY;
    if(larger_pen != breakpoints.begin()){
      smaller_pen = prev(larger_pen);
      smaller_lambda = crossing_point
	(loss, smaller_pen->model->loss,
	 model_size, smaller_pen->model->model_size);
      if(smaller_pen->after){
	throw std::range_error("penalty already known");
      }
      smaller_pen_size_diff = smaller_pen->model->model_size - model_size;
      if(smaller_pen == breakpoints.begin()){
	smaller_is_interval = false;
      }else{
	before_smaller_pen = prev(smaller_pen);
	smaller_is_interval =
	  before_smaller_pen->after &&
	  before_smaller_pen->model->model_size==smaller_pen->model->model_size;
      }
    }
    if(larger_pen != breakpoints.end()){
      larger_lambda = crossing_point
	(loss, larger_pen->model->loss,
	 model_size, larger_pen->model->model_size);      
      if(larger_pen->penalty == penalty){
	throw std::range_error("penalty already known");
      }	
      larger_pen_size_diff = model_size - larger_pen->model->model_size;
    }
    if(larger_pen != breakpoints.end() && larger_pen != breakpoints.begin()){
      double other_lambda = crossing_point
	(larger_pen->model->loss, smaller_pen->model->loss,
	 larger_pen->model->model_size, smaller_pen->model->model_size);
      bool adjacent_same_size =
	smaller_pen_size_diff==0 ||
	larger_pen_size_diff==0;
      if(penalty == other_lambda && adjacent_same_size){
	// now we know there are no other models between smaller and
	// larger, so we should add a breakpoint at other_lambda.
	smaller_pen->after = true;
	if(larger_pen->after){
	  // case smaller(after=false), larger(after=true), convert to
	  // smaller(after=true), breakpoint(after=true)
	  larger_pen->penalty = other_lambda;
	}else if(smaller_is_interval){
	  smaller_pen->penalty = other_lambda;
	  smaller_pen->model = larger_pen->model;
	}else{//smaller point.
	  breakpoints.emplace_hint
	    (larger_pen, other_lambda, larger_pen->model, true);
	}
	return;
      }
    }
    if(smaller_pen_size_diff == 0){
      if(smaller_is_interval){
	smaller_pen->penalty = penalty;
      }else{
	breakpoints.emplace_hint
	  (larger_pen, penalty, smaller_pen->model, false);
	smaller_pen->after = true;
      }
      return;
    }
    if(larger_pen_size_diff == 0){
      if(larger_pen->after){
	// larger pen is already known to be optimal on interval, so
	// expand that interval by decreasing the breakpoint.
	larger_pen->penalty = penalty;
      }else{
	// larger pen is only known to be optimal at its penalty
	// value, so we need to create a new breakpoint representing
	// the optimal interval.
	breakpoints.emplace_hint(larger_pen, penalty, larger_pen->model, true);
	// The function optimizes its insertion time if position
	// points to the element that will follow the inserted element
	// (or to the end, if it would be the last).
      }
      return;
    }
    if(smaller_pen_size_diff == 1){
      if(smaller_is_interval){
	smaller_pen->penalty = smaller_lambda;
      }else{
	// emplace new breakpoint to create interval.
	smaller_pen->after = true;
	smaller_pen = breakpoints.emplace_hint
	  (larger_pen, smaller_lambda, smaller_pen->model, false);
      }
      // at this point smaller_pen is the end of an interval with
      // after=false.
      if(larger_pen_size_diff == 1){
	loss_list.emplace_front(loss, model_size);
	Losses::iterator new_model = loss_list.begin();
	if(larger_pen->after){
	  larger_pen->penalty = larger_lambda;
	}else{
	  breakpoints.emplace_hint
	    (larger_pen, larger_lambda, larger_pen->model, true);
	}
	smaller_pen->model = new_model;
	smaller_pen->after = true;
      }else if(smaller_lambda < penalty){
	smaller_pen->after = true;
	loss_list.emplace_front(loss, model_size);
	Losses::iterator new_model = loss_list.begin();
	smaller_pen->model = new_model;
	breakpoints.emplace_hint
	  (larger_pen, penalty, new_model, false);
      }else if(model_size != smaller_pen->model->model_size){
	loss_list.emplace_front(loss, model_size);
	Losses::iterator new_model = loss_list.begin();
	smaller_pen->model = new_model;
      }
      return;
    }
    if(larger_pen_size_diff == 1){
      BreakpointTree::iterator hint;
      if(larger_pen->after){
	larger_pen->penalty = larger_lambda;
	hint = larger_pen;
      }else{
	hint = breakpoints.emplace_hint
	  (larger_pen, larger_lambda, larger_pen->model, true);
      }
      if(penalty < larger_lambda){
	loss_list.emplace_front(loss, model_size);
	Losses::iterator new_model = loss_list.begin();
	breakpoints.emplace_hint
	  (hint, penalty, new_model, true);
      }
      return;
    }
    loss_list.emplace_front(loss, model_size);
    Losses::iterator new_model = loss_list.begin();
    breakpoints.emplace_hint(larger_pen, penalty, new_model, false);
  }
  Rcpp::DataFrame df(){ 
    int Nrow = breakpoints.size();
    Rcpp::NumericVector penalty(Nrow);
    Rcpp::NumericVector loss(Nrow);
    Rcpp::IntegerVector model_size(Nrow);
    Rcpp::IntegerVector after(Nrow);
    BreakpointTree::iterator it=breakpoints.begin();
    for(int i=0; i<Nrow; i++){
      penalty[i] = it->penalty;
      loss[i] = it->model->loss;
      model_size[i] = it->model->model_size;
      after[i] = it->after;
      it++;
    }
    return Rcpp::DataFrame::create
      (Rcpp::Named("penalty", penalty),
       Rcpp::Named("loss", loss),
       Rcpp::Named("model_size", model_size),
       Rcpp::Named("after", after)
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
