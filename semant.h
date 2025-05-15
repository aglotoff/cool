#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class InheritanceNode;
typedef InheritanceNode *InheritanceNodeP;

class TypeEnvironment {
private:
  ClassTableP class_table;
  InheritanceNodeP node;
  SymbolTable<Symbol, Entry> object_table;
  SymbolTable<Symbol, method_class> method_table;

public:
  TypeEnvironment(ClassTableP, InheritanceNodeP);
  TypeEnvironment *copy_TypeEnvironment(InheritanceNodeP);
  InheritanceNode *lookup_class(Symbol);
  void enter_object_scope();
  void exit_object_scope();
  void add_object(Symbol, Symbol);
  Symbol lookup_object(Symbol);
  Symbol probe_object(Symbol);
  void enter_method_scope();
  void exit_method_scope();
  void add_method(Symbol, method_class *);
  method_class *lookup_method(Symbol);
  method_class *probe_method(Symbol);
  bool check_conformance(Symbol, Symbol);
  Symbol get_lub(Symbol, Symbol);
  ostream& semant_error();
  ostream& semant_error(tree_node *);
};

class InheritanceNode {
private:
  Class_ class_;
  bool basic;
  bool inheritable;
  bool reachable;
  InheritanceNode *parent;
  List<InheritanceNode> *children;
  TypeEnvironment *env;

public:
  InheritanceNode(Class_, bool, bool);
  Class_ get_class() { return class_; };
  bool is_basic() { return basic; }
  bool is_inheritable() { return inheritable; }
  bool is_reachable() { return reachable; }
  InheritanceNode *get_parent() { return parent; }
  void set_parent(InheritanceNode *);
  void add_child(InheritanceNode *);
  void mark_reachable();
  void init_env(ClassTableP);
  void build_feature_tables();
  void check_main_method();
  void type_check();
};

class ClassTable {
private:
  List<InheritanceNode> *list;
  SymbolTable<Symbol, InheritanceNode> *table;
  int semant_errors;
  ostream& error_stream;

  void install_basic_classes();
  void install_classes(Classes);
  void install_class(InheritanceNode *);
  void install_self_type();
  void check_inheritance();
  void build_inheritance_tree();
  void check_inheritance_cycles();
  void check_main();
  void type_check();

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  InheritanceNode *lookup(Symbol);
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

