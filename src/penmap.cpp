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

breakInfo::breakInfo(double p, Losses::iterator o, Losses::iterator a){
  penalty = p;
  on = o;
  after = a;
}


bool operator<(const breakInfo& l, const breakInfo& r){
  return l.penalty < r.penalty;
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

void penmap::set_on
(BreakpointTree::iterator pen, Losses::iterator new_on){
  set_after(pen, UNKNOWN);
  pen->on = new_on;
}

void penmap::set_after
(BreakpointTree::iterator pen, Losses::iterator new_after) {
  if(pen->after->helpful()){
    helpful_list.erase(pen->after);
  }
  pen->after = new_after;
}

bool selectedModel::helpful(){
  return size == HELPFUL_SIZE;
}

void penmap::add_breaks
(BreakpointTree::iterator smaller_pen,
 BreakpointTree::iterator larger_pen){
  if(smaller_pen->on->unknown() || larger_pen->on->unknown()){
    return;
  }
  int size_diff = smaller_pen->on->size - larger_pen->on->size;
  if(size_diff == 0){
    return;
  }
  double cross = (larger_pen->on->loss - smaller_pen->on->loss)/size_diff;
  if(size_diff == 1){
    if(smaller_pen->penalty < cross && cross < larger_pen->penalty){
      breakpoints.emplace_hint(larger_pen, cross, BOTH, larger_pen->on);
      //printf("cross BOTH\n");
    }else{
      set_after(smaller_pen, UNKNOWN);
    }
  }else{
    set_after(smaller_pen, new_helpful(cross));
  }
}

void penmap::fill_pair
(BreakpointTree::iterator smaller_pen,
 BreakpointTree::iterator larger_pen){
  if(smaller_pen->after->unknown()){
    // fill in unknown after if possible.
    if(smaller_pen->on->known() && larger_pen->on == BOTH){
      //printf("filling from smaller larger is BOTH\n");
      set_after(smaller_pen, smaller_pen->on);
    }
    if(smaller_pen->on == BOTH && larger_pen->on->known()){
      //printf("filling from larger smaller is BOTH\n");
      set_after(smaller_pen, larger_pen->on);
    }
    if(smaller_pen->on->known() && larger_pen->on->known() &&
       smaller_pen->on == larger_pen->on){
      //printf("filling from smaller equal\n");
      set_after(smaller_pen, smaller_pen->on);
    }
  }
}

void penmap::make_both
(BreakpointTree::iterator smaller_pen,
 BreakpointTree::iterator larger_pen){
  int size_diff = smaller_pen->on->size - larger_pen->on->size;
  double cross = (larger_pen->on->loss - smaller_pen->on->loss)/size_diff;
  if(cross < INFINITY){
    if(smaller_pen->penalty == cross){
      //printf("cross smaller pen=%f\n", smaller_pen->penalty);
      if(smaller_pen != breakpoints.begin() &&
	 prev(smaller_pen)->after->known()){
	// only write if we won't be losing information about this
	// model (it exists before).
	//printf("BOTH smaller pen=%f\n", smaller_pen->penalty);
	smaller_pen->on = BOTH;
      }
      set_after(smaller_pen, larger_pen->on);
    }
    if(larger_pen->penalty == cross){
      //printf("cross larger pen=%f\n", larger_pen->penalty);
      if(larger_pen->after->known()){
	//printf("BOTH larger pen=%f\n", larger_pen->penalty);
	larger_pen->on = BOTH;
      }
      set_after(smaller_pen, smaller_pen->on);
    }
  }
}  

void penmap::erase_pair
(BreakpointTree::iterator smaller_pen,
 BreakpointTree::iterator larger_pen){
  if(smaller_pen->after == larger_pen->after &&
     larger_pen->after->known()){
    breakpoints.erase(larger_pen);
  }
}

void already_known(){
  throw std::domain_error("penalty already known");
}

void error_if_inconsistent
(double penalty_before, double loss_before, int size_before,
 double penalty_after, double loss_after, int size_after){
  if(size_before < size_after){
    throw std::domain_error
      ("model sizes must be non-increasing as penalties increase");
  }else if(size_after < size_before && loss_after < loss_before){
    throw std::domain_error
      ("loss values must be non-decreasing as penalties increase");
  }
  int size_diff = size_before - size_after;
  if(0 < size_diff){
    double cross = (loss_after - loss_before)/size_diff;
    if(cross < penalty_before || penalty_after < cross){
      throw std::domain_error
	("model/penalty to insert inconsistent with previous data");
    }
  }
}  

void penmap::insert_loss_size(double penalty, double loss, int size){
  if(penalty == INFINITY && 0 < size){
    throw std::domain_error("size should be zero with infinite penalty");
  }
  if(size < 0){
    throw std::domain_error("size must be non-negative");
  }
  breakInfo new_break(penalty);
  // An iterator to the the first element in the container which is
  // not considered to go before val (can be same value), or
  // set::end if all elements are considered to go before val.
  BreakpointTree::iterator larger_or_same = breakpoints.lower_bound(new_break);
  bool do_insert = true;
  bool do_checks = true;
  if(larger_or_same != breakpoints.begin()){
    bool same_size =
      size == prev(larger_or_same)->on->size ||
      size == larger_or_same->on->size;
    bool penalty_is_helpful = 
      prev(larger_or_same)->after->helpful() &&
      penalty == prev(larger_or_same)->after->get_penalty();
    if(same_size && penalty_is_helpful){
      do_checks = false;
    }
  }
  if(do_checks){
    if(larger_or_same->on->known()){
      error_if_inconsistent
	(penalty, loss, size,
	 larger_or_same->penalty,
	 larger_or_same->on->loss,
	 larger_or_same->on->size);
    }
    if(larger_or_same->penalty == penalty){
      if(larger_or_same->on->known()){
	already_known();
      }else{//case for penalty=0 or Inf
	do_insert = false;
      }
    }
    if(larger_or_same != breakpoints.begin()){
      if(prev(larger_or_same)->after->size == size){
	already_known();
      }
      if(prev(larger_or_same)->on->known()){
	error_if_inconsistent
	  (prev(larger_or_same)->penalty,
	   prev(larger_or_same)->on->loss,
	   prev(larger_or_same)->on->size,
	   penalty, loss, size);
      }
    }
  }
  Losses::iterator m;
  if(larger_or_same->on->size == size){
    m = larger_or_same->on;
  }else if(larger_or_same != breakpoints.begin() &&
	   prev(larger_or_same)->on->size == size){
    m = prev(larger_or_same)->on;
  }else{
    m = new_optimal(loss, size);
  }
  BreakpointTree::iterator new_it;
  if(do_insert){
    new_it = breakpoints.emplace_hint(larger_or_same, penalty, m, UNKNOWN);
  }else{ 
    new_it = larger_or_same;
    set_on(new_it, m);
  }
  BreakpointTree::iterator next_it = next(new_it), last, first;
  if(new_it == breakpoints.begin()){
    first = new_it;
  }else{
    add_breaks(prev(new_it), new_it);
    if(prev(new_it) == breakpoints.begin()){
      first = prev(new_it);
    }else if(prev(prev(new_it)) == breakpoints.begin()){
      first = prev(prev(new_it));
    }else{
      first = prev(prev(prev(new_it)));
    }
  }
  if(next_it == breakpoints.end()){
    last = first;
  }else{
    add_breaks(new_it, next_it);
    if(next(next_it) == breakpoints.end()){
      last = next_it;
    }else{
      last = next(next_it);
    }
    if(size == 0 && next_it->penalty == INFINITY){
      set_on(next_it, new_it->on);
    }
  }
  for(BreakpointTree::iterator it=first; it != last; it++){
    fill_pair(it, next(it));
  }
  for(BreakpointTree::iterator it=first; it != last; it++){
    make_both(it, next(it));
  }
  for(BreakpointTree::iterator it=first; it != last && next(it) != breakpoints.end(); it++){
    erase_pair(it, next(it));
  }
}
