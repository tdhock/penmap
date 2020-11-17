#include "penmap.h"
#include <math.h>//INFINITY
#include <stdexcept>

selectedModel::selectedModel(double l, int s) 
  : loss(l), size(s) {}

double selectedModel::get_penalty(){
  return loss;
}

bool selectedModel::unknown(){
  return size == UNKNOWN_SIZE || size == HELPFUL_SIZE;
}

bool selectedModel::known(){
  return !unknown();
}

breakInfo::breakInfo(double p)
  : penalty(p) {}

breakInfo::breakInfo(double p, Losses::iterator o, Losses::iterator a)
  : penalty(p), on(o), after(a) {}


bool operator<(const breakInfo& l, const breakInfo& r){
  return l.penalty < r.penalty;
}

double crossing_point(double l1, double l2, int c1, int c2){
  return (l1-l2)/(c2-c1);
}

Losses::iterator penmap::new_optimal(double loss, int size){
  optimal_list.emplace_front(loss, size);
  return optimal_list.begin();
}

Losses::iterator penmap::new_helpful(double penalty){
  helpful_list.emplace_front(penalty, HELPFUL_SIZE);
  return helpful_list.begin();
}

penmap::penmap(){
  UNKNOWN = new_optimal(INFINITY, UNKNOWN_SIZE);
  BOTH = new_optimal(INFINITY, BOTH_SIZE);
  breakpoints.emplace_hint
    (breakpoints.end(), 0, UNKNOWN, new_helpful(0));
  breakpoints.emplace_hint
    (breakpoints.end(), INFINITY, UNKNOWN, new_helpful(INFINITY));
}

void penmap::set_after
(BreakpointTree::iterator pen, Losses::iterator new_after, bool ok_to_erase) {
  if(pen->after->helpful() && ok_to_erase){
    helpful_list.erase(pen->after);
  }
  pen->after = new_after;
}

bool selectedModel::helpful(){
  return size == HELPFUL_SIZE;
}

void penmap::insert_on_after
(double penalty, 
 Losses::iterator on, 
 Losses::iterator after){
  if(smaller_pen != breakpoints.end() && smaller_pen->after == on &&
     larger_pen != breakpoints.end() && larger_pen->on == on){
    //already optimal, nothing to do.
    return;
  }
  if(larger_pen != breakpoints.end() &&
     larger_pen->after == after &&
     after->known()){
    larger_pen->penalty = penalty;
    larger_pen->on = on;
    return;
  }
  if(smaller_pen != breakpoints.end() &&
     smaller_pen != breakpoints.begin() &&
     prev(smaller_pen)->after == on){
    smaller_pen->penalty = penalty;
    smaller_pen->on = on;
    set_after(smaller_pen, after, true);
    return;
  }
  if(larger_pen != breakpoints.end() &&
     larger_pen->penalty == penalty){
    larger_pen->on = on;
    set_after(larger_pen, after, true);
    return;
  }
  if(smaller_pen != breakpoints.end() &&
     smaller_pen->penalty == penalty){
    smaller_pen->on = on;
    set_after(smaller_pen, after, true);
    return;
  }
  // no way to resize neighboring entries, now see if there is a
  // nearby UNKNOWN to update and then insert a new entry.
  if(smaller_pen != breakpoints.end() &&
     smaller_pen->after->unknown()){
    bool ok_to_erase = smaller_pen->after != after;
    if(smaller_pen->on == on){
      set_after(smaller_pen, on, ok_to_erase);
    }
    if(smaller_pen->on == BOTH && on != BOTH){
      set_after(smaller_pen, on, ok_to_erase);
    }
    if(smaller_pen->on != BOTH && on == BOTH){
      set_after(smaller_pen, smaller_pen->on, ok_to_erase);
    }
  }
  if(smaller_pen != breakpoints.end() &&
     smaller_pen != breakpoints.begin() &&
     prev(smaller_pen)->after != smaller_pen->after &&
     prev(smaller_pen)->after->known() &&
     smaller_pen->after->known()){
    smaller_pen->on = BOTH;
  }
  smaller_pen = breakpoints.emplace_hint(larger_pen, penalty, on, after);
  if(larger_pen == breakpoints.end() && on->size==0 && penalty<INFINITY){
    insert_on_after(INFINITY, on, UNKNOWN);
  }
}

void penmap::insert_loss_size(double penalty, double loss, int size){
  if(penalty == INFINITY && 0 < size){
    throw std::domain_error("size should be zero with infinite penalty");
  }
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
    if(smaller_pen->after->known()){
      throw std::domain_error("penalty already known");
    }
    smaller_lambda = crossing_point
      (loss, smaller_pen->on->loss,
       size, smaller_pen->on->size);
    smaller_pen_size_diff = smaller_pen->on->size - size;
  }
  if(larger_pen != breakpoints.end()){
    if(larger_pen->penalty == penalty){
      if(larger_pen->on->known()){
	throw std::domain_error("penalty already known");
      }else{//case for penalty=0 or Inf
	insert_on_after(penalty, new_optimal(loss, size), UNKNOWN);
	return;
      }
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
      set_after(smaller_pen, smaller_pen->on, true);
      if(smaller_pen != breakpoints.begin() &&
	 smaller_pen->after == prev(smaller_pen)->after){
	smaller_pen = prev(smaller_pen);
	breakpoints.erase(next(smaller_pen));
      }
      insert_on_after(other_lambda, BOTH, larger_pen->on);
      return;
    }
  }
  if(smaller_pen_size_diff == 0){
    insert_on_after(penalty, smaller_pen->on, smaller_pen->after);
    return;
  }
  if(larger_pen_size_diff == 0){
    insert_on_after(penalty, larger_pen->on, larger_pen->on);
    return;
  }
  Losses::iterator m = new_optimal(loss, size);
  if(smaller_lambda < penalty && smaller_lambda < INFINITY &&
     1 < smaller_pen_size_diff){
    set_after(smaller_pen, new_helpful(smaller_lambda), true);
  }
  if(larger_pen_size_diff == 1 && smaller_pen_size_diff == 1){
    insert_on_after(smaller_lambda, BOTH, m);
    insert_on_after(larger_lambda, BOTH, larger_pen->on);
    return;
  }
  if(larger_pen_size_diff == 1){
    if(penalty < larger_lambda){
      insert_on_after(penalty, m, m);
      if(larger_lambda < larger_pen->penalty){
	insert_on_after(larger_lambda, BOTH, larger_pen->on);
      }
    }else{
      insert_on_after(penalty, m, larger_pen->on);
    }
    return;
  }
  // special case for size=0 which we know is optimal for all larger
  // penalties.
  Losses::iterator after;
  if(size == 0){
    insert_on_after(INFINITY, m, UNKNOWN);
    after = m;
  }else{
    if(larger_lambda == INFINITY){
      after = UNKNOWN;
    }else{
      after = new_helpful(larger_lambda);
    }
  }
  if(smaller_pen_size_diff == 1){
    if(smaller_lambda < penalty){
      insert_on_after(smaller_lambda, BOTH, m);
      insert_on_after(penalty, m, after);
    }else{
      set_after(smaller_pen, smaller_pen->on, true);
      insert_on_after(penalty, m, after);
    }
    return;
  }
  insert_on_after(penalty, m, after);
}
