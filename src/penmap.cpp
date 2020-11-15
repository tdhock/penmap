#include "penmap.h"
#include <math.h>//INFINITY
#include <stdexcept>

lossSize::lossSize(double l, int s) 
  : loss(l), size(s) {}

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

penmap::penmap(){
  loss_list.emplace_front(INFINITY, -1);
  UNKNOWN = loss_list.begin();
  loss_list.emplace_front(INFINITY, -2);
  BOTH = loss_list.begin();
}

void penmap::insert_on_after
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
     prev(smaller_pen)->after == smaller_pen->on){
    smaller_pen->penalty = penalty;
    smaller_pen->on = on;
    smaller_pen->after = after;
    return;
  }
  if(smaller_pen != breakpoints.end() &&
     smaller_pen->penalty == penalty){
    smaller_pen->on = on;
    smaller_pen->after = after;
    return;
  }
  // no way to resize neighboring entries, now see if there is a
  // nearby UNKNOWN to update and then insert a new entry.
  if(smaller_pen != breakpoints.end() &&
     smaller_pen->after == UNKNOWN){
    if(smaller_pen->on == on){
      smaller_pen->after = on;
    }
    if(smaller_pen->on == BOTH && on != BOTH){
      smaller_pen->after = on;
    }
    if(smaller_pen->on != BOTH && on == BOTH){
      smaller_pen->after = smaller_pen->on;
    }
  }
  if(smaller_pen != breakpoints.end() &&
     smaller_pen != breakpoints.begin() &&
     prev(smaller_pen)->after != smaller_pen->after &&
     prev(smaller_pen)->after != UNKNOWN &&
     smaller_pen->after != UNKNOWN){
    smaller_pen->on = BOTH;
  }
  smaller_pen = breakpoints.emplace_hint(larger_pen, penalty, on, after);
}

void penmap::insert_loss_size(double penalty, double loss, int size){
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
      throw std::domain_error("penalty already known");
    }
    smaller_lambda = crossing_point
      (loss, smaller_pen->on->loss,
       size, smaller_pen->on->size);
    smaller_pen_size_diff = smaller_pen->on->size - size;
  }
  if(larger_pen != breakpoints.end()){
    if(larger_pen->penalty == penalty){
      throw std::domain_error("penalty already known");
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
      insert_on_after(other_lambda, BOTH, larger_pen->on);
      return;
    }
  }
  if(smaller_pen_size_diff == 0){
    insert_on_after(penalty, smaller_pen->on, UNKNOWN);
    return;
  }
  if(larger_pen_size_diff == 0){
    insert_on_after(penalty, larger_pen->on, larger_pen->on);
    return;
  }
  loss_list.emplace_front(loss, size);
  if(larger_pen_size_diff == 1 && smaller_pen_size_diff == 1){
    insert_on_after(smaller_lambda, BOTH, loss_list.begin());
    insert_on_after(larger_lambda, BOTH, larger_pen->on);
    return;
  }
  if(larger_pen_size_diff == 1){
    if(penalty < larger_lambda){
      insert_on_after(penalty, loss_list.begin(), loss_list.begin());
      insert_on_after(larger_lambda, BOTH, larger_pen->on);
    }else{
      insert_on_after(penalty, loss_list.begin(), larger_pen->on);
    }
    return;
  }
  if(smaller_pen_size_diff == 1){
    if(smaller_lambda < penalty){
      insert_on_after(smaller_lambda, BOTH, loss_list.begin());
      insert_on_after(penalty, loss_list.begin(), UNKNOWN);
    }else{
      smaller_pen->after = smaller_pen->on;
      insert_on_after(penalty, loss_list.begin(), UNKNOWN);
    }
    return;
  }
  //last case: empty set, just insert.
  breakpoints.emplace_hint(larger_pen, penalty, loss_list.begin(), UNKNOWN);
}
