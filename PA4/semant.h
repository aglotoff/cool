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
class ClassTableEntry;
class TypeEnvironment;

class TypeEnvironment {
private:
  ClassTable *class_table;
  ClassTableEntry *class_entry;
  SymbolTable<Symbol, Entry> object_table;
  SymbolTable<Symbol, method_class> method_table;

public:
  TypeEnvironment(ClassTable *, ClassTableEntry *);
  TypeEnvironment *copy_TypeEnvironment(ClassTableEntry *);
  ClassTableEntry *lookup_class(Symbol);

  bool type_conforms(Symbol, Symbol);
  Symbol get_least_upper_bound(Symbol, Symbol);
  Symbol type_check_dispatch(Expression, Symbol, Symbol, Expressions);

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

// This is a structure that used to contain the semantic information such as
// the inheritance graph.
class ClassTable {
private:
  List<ClassTableEntry> *list;
  SymbolTable<Symbol, ClassTableEntry> *table;

  int semant_errors;
  ostream& error_stream;

  void install_basic_classes();
  void install_classes(Classes);
  void install_self_type();
  void install_entry(ClassTableEntry *);

  void check_inheritance();
  void build_inheritance_tree();
  void check_inheritance_cycles();

  void build_feature_tables();
  void type_check_features();

  void check_main();

public:
  ClassTable(Classes);

  ClassTableEntry *lookup(Symbol name) { return table->probe(name); }

  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  int errors() { return semant_errors; }
};

class ClassTableEntry {
private:
  Class_ node;
  bool basic;
  bool inheritable;

  ClassTableEntry *parent;
  List<ClassTableEntry> *children;
  bool reachable;

  TypeEnvironment *env;

  void build_feature_tables();

public:
  ClassTableEntry(Class_, bool, bool);

  Class_ get_node() { return node; };
  bool is_basic() { return basic; }
  bool is_inheritable() { return inheritable; }
  bool is_reachable() { return reachable; }

  ClassTableEntry *get_parent() { return parent; }
  void set_parent(ClassTableEntry *p) { parent = p; }

  void add_child(ClassTableEntry *);
  void mark_reachable();

  void init_env(ClassTable *);
  void check_main_method();
  void type_check_features();

  method_class *lookup_method(Symbol name)
  { return env->lookup_method(name); }
};

#endif
