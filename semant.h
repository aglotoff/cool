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
class InheritanceNode;
class TypeEnvironment;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.

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
  void build_feature_tables();
  void check_main();

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  InheritanceNode *lookup(Symbol);
  void type_check();
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
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
  void init_env(ClassTable *);
  TypeEnvironment *get_env() { return env; }
  void build_feature_tables();
  void check_main_method();
  void type_check();
};

class TypeEnvironment {
private:
  ClassTable *class_table;
  InheritanceNode *node;
  SymbolTable<Symbol, Entry> object_table;
  SymbolTable<Symbol, method_class> method_table;

public:
  TypeEnvironment(ClassTable *, InheritanceNode *);
  TypeEnvironment *copy_TypeEnvironment(InheritanceNode *);
  InheritanceNode *lookup_class(Symbol);

  bool type_conforms(Symbol, Symbol);
  Symbol get_lub(Symbol, Symbol);
  Symbol type_check_dispatch(Expression, Symbol, Symbol, Expressions);

  ostream& semant_error();
  ostream& semant_error(tree_node *);

  void enter_object_scope() { object_table.enterscope(); }
  void exit_object_scope() { object_table.exitscope(); }
  void add_object(Symbol name, Symbol type) { object_table.addid(name, type); }
  Symbol lookup_object(Symbol name) { return object_table.lookup(name); }
  Symbol probe_object(Symbol name) { return object_table.probe(name); }

  void enter_method_scope() { method_table.enterscope(); }
  void exit_method_scope() { method_table.exitscope(); }
  void add_method(Symbol name, method_class *method)
  { method_table.addid(name, method); }
  method_class *lookup_method(Symbol name) { return method_table.lookup(name); }
  method_class *probe_method(Symbol name) { return method_table.probe(name); }
};

#endif
