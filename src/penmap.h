#include <set>
#include <list>

class lossSize {
public:
  double loss;
  int size;
  lossSize(double l_, int s_);
};

typedef std::list<lossSize> Losses;

class breakInfo {
public:
  mutable Losses::iterator on, after;
  mutable double penalty;
  breakInfo(double p);
  breakInfo(double p_, Losses::iterator on_it, Losses::iterator after_it);
};

typedef std::set<breakInfo> BreakpointTree;

class penmap {
public:
  BreakpointTree breakpoints;
  Losses loss_list;
  Losses::iterator UNKNOWN, BOTH;
  BreakpointTree::iterator smaller_pen, larger_pen;
  penmap();
  void insert_on_after
  (double penalty, Losses::iterator on, Losses::iterator after);
  void insert_loss_size(double penalty, double loss, int size);
};
