#include <set>
#include <list>
#define BOTH_SIZE -1
#define UNKNOWN_SIZE -2
#define HELPFUL_SIZE -3

class selectedModel {
public:
  double loss;
  int size;
  selectedModel(double l_, int s_);
  double get_penalty();
  bool unknown();
  bool known();
  bool helpful();
};

typedef std::list<selectedModel> Losses;

class breakInfo {
public:
  mutable Losses::iterator on, after;
  double penalty;
  breakInfo(double p);
  breakInfo(double, Losses::iterator on_it, Losses::iterator after_it);
};

typedef std::set<breakInfo> BreakpointTree;

class penmap {
public:
  BreakpointTree breakpoints;
  Losses::iterator UNKNOWN, BOTH;
  Losses optimal_list;
  Losses helpful_list;
  void set_after(BreakpointTree::iterator, Losses::iterator);
  void set_on(BreakpointTree::iterator, Losses::iterator);
  void add_breaks(BreakpointTree::iterator, BreakpointTree::iterator);
  void fill_pair(BreakpointTree::iterator, BreakpointTree::iterator);
  void erase_pair(BreakpointTree::iterator, BreakpointTree::iterator);
  void make_both(BreakpointTree::iterator, BreakpointTree::iterator);
  penmap();
  Losses::iterator new_optimal(double loss, int size);
  Losses::iterator new_helpful(double pen);
  void insert_loss_size(double penalty, double loss, int size);
  int num_helpful();
  int num_optimal();
  int num_breakpoints();
};
