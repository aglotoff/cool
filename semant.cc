

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
  arg,
  arg2,
  Bool,
  concat,
  cool_abort,
  copy,
  Int,
  in_int,
  in_string,
  IO,
  length,
  Main,
  main_meth,
  No_class,
  No_type,
  Object,
  out_int,
  out_string,
  prim_slot,
  self,
  SELF_TYPE,
  Str,
  str_field,
  substr,
  type_name,
  val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any 
  //   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

InheritanceNode::InheritanceNode(Class_ c, bool b, bool i):
  class_(c),
  basic(b),
  inheritable(i),
  reachable(false),
  parent(NULL),
  children(NULL) {}

void InheritanceNode::set_parent(InheritanceNode *p)
{
  parent = p;
}

void InheritanceNode::add_child(InheritanceNode *child)
{
  children = new List<InheritanceNode>(child, children);
}

void InheritanceNode::mark_reachable()
{
  reachable = true;

  for (List<InheritanceNode> *l = children; l != NULL; l = l->tl())
    l->hd()->mark_reachable();
}

void InheritanceNode::init_env(ClassTableP class_table)
{
  env = new TypeEnvironment(class_table, this);
}

void InheritanceNode::build_feature_tables()
{
  env->enter_object_scope();
  env->enter_method_scope();

  Features features = get_class()->get_features();

  for (int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->add_to_table(env);

  for (List<InheritanceNode> *l = children; l != NULL; l = l->tl()) {
    InheritanceNode *child = l->hd();
    child->env = env->copy_TypeEnvironment(child);
    child->build_feature_tables();
  }
}

void InheritanceNode::check_main_method()
{
  Symbol class_name = get_class()->get_name();

  method_class *main_method = env->probe_method(main_meth);
  if (main_method == NULL) {
    env->semant_error() << "No 'main' method in class " << class_name
      << "." << endl;
    return;
  }

  if (main_method->get_formals()->len() != 0) {
    env->semant_error(get_class()) << "'main' method in class " << class_name
      << " should have no arguments." << endl;
  }
}

void InheritanceNode::type_check()
{
  if (is_basic())
    return;

  Features features = get_class()->get_features();

  for (int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->type_check(env);
}

TypeEnvironment::TypeEnvironment(ClassTableP t, InheritanceNodeP n):
  class_table(t),
  node(n) {}

TypeEnvironment *TypeEnvironment::copy_TypeEnvironment(InheritanceNodeP node)
{
  TypeEnvironment *env = new TypeEnvironment(class_table, node);
  env->object_table = object_table;
  env->method_table = method_table;
  return env;
}

InheritanceNode *TypeEnvironment::lookup_class(Symbol name)
{
  if (name == SELF_TYPE)
    return node;

  return class_table->lookup(name);
}

void TypeEnvironment::enter_object_scope()
{
  object_table.enterscope();
}

void TypeEnvironment::exit_object_scope()
{
  object_table.exitscope();
}

void TypeEnvironment::add_object(Symbol name, Symbol type)
{
  object_table.addid(name, type);
}

Symbol TypeEnvironment::lookup_object(Symbol name)
{
  return object_table.lookup(name);
}

Symbol TypeEnvironment::probe_object(Symbol name)
{
  return object_table.probe(name);
}

void TypeEnvironment::enter_method_scope()
{
  method_table.enterscope();
}

void TypeEnvironment::exit_method_scope()
{
  method_table.exitscope();
}

void TypeEnvironment::add_method(Symbol name, method_class *method)
{
  method_table.addid(name, method);
}

method_class *TypeEnvironment::lookup_method(Symbol name)
{
  return method_table.lookup(name);
}

method_class *TypeEnvironment::probe_method(Symbol name)
{
  return method_table.probe(name);
}

ostream& TypeEnvironment::semant_error()
{
  return semant_error(node->get_class());
}

ostream& TypeEnvironment::semant_error(tree_node *t)
{
  return class_table->semant_error(node->get_class()->get_filename(), t);
}

bool TypeEnvironment::check_conformance(Symbol t1, Symbol t2)
{
  InheritanceNodeP n1 = lookup_class(t1);
  InheritanceNodeP n2 = lookup_class(t2);

  while (n1 != NULL) {
    if (n1 == n2)
      return true;
    n1 = n1->get_parent();
  }

  return false;
}

Symbol TypeEnvironment::get_lub(Symbol t1, Symbol t2)
{
  InheritanceNodeP n1 = lookup_class(t1);

  while (!check_conformance(t2, n1->get_class()->get_name()))
    n1 = n1->get_parent();

  return n1->get_class()->get_name();
}

ClassTable::ClassTable(Classes classes) :
    list(NULL),
    table(new SymbolTable<Symbol, InheritanceNode>()),
    semant_errors(0),
    error_stream(cerr)
{
  table->enterscope();

  install_self_type();
  install_basic_classes();
  install_classes(classes);

  check_inheritance();
  build_inheritance_tree();
  check_inheritance_cycles();

  InheritanceNode *root = table->probe(Object);
  root->init_env(this);
  root->build_feature_tables();

  check_main();

  type_check();

  //exit(0);
}

void ClassTable::install_basic_classes()
{
  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");
  
  // The following demonstrates how to create dummy parse trees to
  // refer to basic Cool classes.  There's no need for method
  // bodies -- these are already built into the runtime system.
  
  // IMPORTANT: The results of the following expressions are
  // stored in local variables.  You will want to do something
  // with those variables at the end of this method to make this
  // code meaningful.

  // 
  // The Object class has no parent class. Its methods are
  //    abort() : Object  aborts the program
  //    type_name() : Str   returns a string representation of class name
  //    copy() : SELF_TYPE  returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.

  Class_ Object_class =
    class_(Object, 
      No_class,
      append_Features(
        append_Features(
          single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
          single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
        single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
    filename);

  // 
  // The IO class inherits from Object. Its methods are
  //    out_string(Str) : SELF_TYPE     writes a string to the output
  //    out_int(Int) : SELF_TYPE      "  an int  "  "   "
  //    in_string() : Str         reads a string from the input
  //    in_int() : Int            "   an int   "  "   "
  //
  Class_ IO_class = 
  class_(IO, 
    Object,
    append_Features(
      append_Features(
        append_Features(
          single_Features(method(out_string, single_Formals(formal(arg, Str)),
            SELF_TYPE, no_expr())),
          single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
        single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
      single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
    filename);  

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer. 
  //
  Class_ Int_class =
  class_(Int, 
    Object,
    single_Features(attr(val, prim_slot, no_expr())),
    filename);

  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class =
    class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

  //
  // The class Str has a number of slots and operations:
  //     val                  the length of the string
  //     str_field              the string itself
  //     length() : Int             returns length of the string
  //     concat(arg: Str) : Str         performs string concatenation
  //     substr(arg: Int, arg2: Int): Str   substring selection
  //     
  Class_ Str_class =
  class_(Str, 
    Object,
    append_Features(
      append_Features(
        append_Features(
          append_Features(
            single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
          single_Features(method(length, nil_Formals(), Int, no_expr()))),
        single_Features(method(concat, 
          single_Formals(formal(arg, Str)),
          Str, 
          no_expr()))),
      single_Features(method(substr, 
        append_Formals(single_Formals(formal(arg, Int)), 
          single_Formals(formal(arg2, Int))),
        Str, 
        no_expr()))),
    filename);

  install_class(new InheritanceNode(Object_class, true, true));
  install_class(new InheritanceNode(IO_class, true, true));
  install_class(new InheritanceNode(Int_class, true, false));
  install_class(new InheritanceNode(Bool_class, true, false));
  install_class(new InheritanceNode(Str_class, true, false));
}

void ClassTable::install_classes(Classes classes)
{
  for (int i = classes->first(); classes->more(i); i = classes->next(i))
    install_class(new InheritanceNode(classes->nth(i), false, true));
}

void ClassTable::install_class(InheritanceNode *node)
{
  Class_ c = node->get_class();
  Symbol name = c->get_name();

  InheritanceNode *prev_node = table->probe(name);

  if (prev_node != NULL) {
    if (prev_node->is_basic())
      semant_error(c) << "Redefinition of basic class " << name << "." << endl;
    else
      semant_error(c) << "Class " << name << " was previously defined." << endl;
    return;
  }
  
  if (c != NULL)
    list = new List<InheritanceNode>(node, list);

  table->addid(name, node);
}

void ClassTable::install_self_type()
{
  table->addid(SELF_TYPE, new InheritanceNode(NULL, true, false));
}

void ClassTable::check_inheritance()
{
  for (List<InheritanceNode> *l = list; l != NULL; l = l->tl()) {
    Class_ c = l->hd()->get_class();
    Symbol name = c->get_name();
    Symbol parent = c->get_parent();

    if (parent == No_class)
      continue;
    
    InheritanceNode *parent_node = table->probe(parent);
    if (parent_node == NULL) {
      semant_error(c) << "Class " << name
        << " inherits from an undefined class " << parent << "." << endl;
    } else if (!parent_node->is_inheritable()) {
      semant_error(c) << "Class " << name 
        << " cannot inherit class " << parent << "." << endl;
    }
  }
}

void ClassTable::build_inheritance_tree()
{
  for (List<InheritanceNode> *l = list; l != NULL; l = l->tl()) {
    Class_ c = l->hd()->get_class();
    
    InheritanceNode *node = table->probe(c->get_name());
    InheritanceNode *parent_node = table->probe(c->get_parent());

    if (node != NULL && parent_node != NULL) {
      node->set_parent(parent_node);
      parent_node->add_child(node);
    }
  }
}

void ClassTable::check_inheritance_cycles()
{
  InheritanceNode *root = table->probe(Object);
  root->mark_reachable();

  for (List<InheritanceNode> *l = list; l != NULL; l = l->tl()) {
    Class_ c = l->hd()->get_class();
    
    InheritanceNode *node = table->probe(c->get_name());

    if (!node->is_reachable())
      semant_error(c) << "Class " << c->get_name() << ", or an ancestor of "
        << c->get_name() << ", is involved in an inheritance cycle."  << endl;
  }
}

void ClassTable::check_main()
{
  InheritanceNodeP main_node = table->probe(Main);
  if (main_node == NULL) {
    semant_error() << "Class Main is not defined." << endl;
    return;
  }

  main_node->check_main_method();
}

void ClassTable::type_check()
{
  for (List<InheritanceNode> *l = list; l != NULL; l = l->tl())
    l->hd()->type_check();
}

InheritanceNode *ClassTable::lookup(Symbol name)
{
  return table->probe(name);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()        
//
//    ostream& ClassTable::semant_error(Class_ c)
//     print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
  return semant_error(c->get_filename(),c);
}  

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream& ClassTable::semant_error()          
{                         
  semant_errors++;              
  return error_stream;
} 

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
  initialize_constants();

  /* ClassTable constructor may do some semantic analysis */
  ClassTable *classtable = new ClassTable(classes);

  /* some semantic analysis code may go here */

  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}

void method_class::add_to_table(TypeEnvironment *env)
{
  if (env->probe_method(name) != NULL) {
    env->semant_error(this) << "Method " << name
      << " is multiply defined." << endl;
    return;
  }
  
  method_class *original = env->lookup_method(name);
  if (original != NULL) {
    if (return_type != original->return_type) {
      env->semant_error(this) << "In redefined method " << name
        << ", return type " << return_type
        << " is different from original return type" << original->return_type
        << endl;
      return;
    }

    if (formals->len() != original->formals->len()) {
      env->semant_error(this)
        << "Incompatible number of formal parameters in redefined method "
        << name << "." << endl;
      return;
    }

    for (int i = formals->first(), j = original->formals->first();
         formals->more(i) && original->formals->more(j);
         i = formals->next(i), j = original->formals->next(j)) {
      Symbol redefined_type = formals->nth(i)->get_type();
      Symbol original_type = original->formals->nth(j)->get_type();

      if (redefined_type != original_type) {
        env->semant_error(this)
          << "In redefined method " << name << ", parameter type "
          << redefined_type << " is different from original type "
          << original_type << "." << endl;
        return;
      }
    }
  }

  env->add_method(name, this);
}

void method_class::type_check(TypeEnvironment *env)
{
  env->enter_object_scope();

  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    Formal formal = formals->nth(i);
    env->add_object(formal->get_name(), formal->get_type());
  }

  Symbol expr_type = expr->type_check(env);

  if (!env->check_conformance(expr_type, return_type)) {
    env->semant_error(this) << "Inferred return type " << expr_type
      << " of method " << name << " does not conform to declared return type "
      << return_type << "." << endl;
  }

  env->exit_object_scope();
}

void attr_class::add_to_table(TypeEnvironment *env)
{
  if (name == self) {
    env->semant_error(this) << "'self' cannot be the name of an attribute."
      << endl;
    return;
  }

  if (env->probe_object(name) != NULL) {
    env->semant_error(this) << "Attribute " << name
      << " is multiply defined in class." << endl;
    return;
  }
  
  if (env->lookup_object(name) != NULL) {
    env->semant_error(this) << "Attribute " << name
      << " is an attribute of an inherited class." << endl;
    return;
  }
  
  env->add_object(name, type_decl);
}

void attr_class::type_check(TypeEnvironment *env)
{
  init->type_check(env);
}

Symbol branch_class::type_check(TypeEnvironment *env)
{
  env->enter_object_scope();
  env->add_object(name, type_decl);

  Symbol t = expr->type_check(env);

  env->exit_object_scope();

  return t;
}

Symbol assign_class::type_check(TypeEnvironment *env)
{
  Symbol t = expr->type_check(env);
  Symbol decl_t = env->lookup_object(name);

  if (decl_t == NULL) {
    env->semant_error(this) << "Assignment to undeclared variable " << name << "." << endl;
  } else if (!env->check_conformance(t, decl_t)) {
    env->semant_error(this) << "Type " << t
      << " of assigned expression does not conform to declared type "
      << decl_t << " of identifier " << name << "." << endl;
  }

  set_type(t);
  return t;
}

Symbol static_dispatch_class::type_check(TypeEnvironment *env)
{
  expr->type_check(env);

  for (int i = actual->first(); actual->more(i); i = actual->next(i))
    actual->nth(i)->type_check(env);

  // TODO
  return No_type;
}

Symbol dispatch_class::type_check(TypeEnvironment *env)
{
  Symbol expr_type = expr->type_check(env);

  for (int i = actual->first(); actual->more(i); i = actual->next(i))
    actual->nth(i)->type_check(env);

  // TODO
  return No_type;
}

Symbol cond_class::type_check(TypeEnvironment *env)
{
  Symbol pred_type = pred->type_check(env);
  if (pred_type != Bool) {
    env->semant_error(this) << "Predicate of 'if' does not have type Bool."
      << endl;
  }

  Symbol then_type = then_exp->type_check(env);
  Symbol else_type = else_exp->type_check(env);

  set_type(env->get_lub(then_type, else_type));
  return get_type();
}

Symbol loop_class::type_check(TypeEnvironment *env)
{
  if (pred->type_check(env) != Bool) {
    env->semant_error(this) << "Loop condition does not have type Bool."
      << endl;
  }

  body->type_check(env);

  set_type(Object);
  return Object;
}

Symbol typcase_class::type_check(TypeEnvironment *env)
{
  expr->type_check(env);

  Symbol lub_type = No_type;

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Symbol t = cases->nth(i)->type_check(env);
    lub_type = (lub_type == No_type) ? t : env->get_lub(lub_type, t);
  }

  set_type(lub_type);
  return lub_type;
}

Symbol block_class::type_check(TypeEnvironment *env)
{
  Symbol last_type = Object;

  for (int i = body->first(); body->more(i); i = body->next(i))
    last_type = body->nth(i)->type_check(env);

  set_type(last_type);
  return last_type;
}

Symbol let_class::type_check(TypeEnvironment *env)
{
  env->enter_object_scope();

  Symbol init_type = init->type_check(env);

  if (identifier == self) {
    env->semant_error(this) << "'self' cannot be bound in a 'let' expression."
      << endl;
  } else {
    if (init_type != No_type && !env->check_conformance(init_type, type_decl)) {
      env->semant_error(this) << "Inferred type " << init_type
        << " of initialization of " << identifier
        << " does not conform to identifier's declared type " << type_decl
        << "." << endl;
    }

    env->add_object(identifier, type_decl);
  }

  Symbol body_type = body->type_check(env);

  env->exit_object_scope();

  set_type(body_type);
  return body_type;
}

Symbol plus_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);
  Symbol t2 = e2->type_check(env);

  if ((t1 != Int) || (t2 != Int)) {
    env->semant_error(this) << "non-Int arguments: " << t1 << " + " << t2
      << endl;
  }

  set_type(Int);
  return Int;
}

Symbol sub_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);
  Symbol t2 = e2->type_check(env);

  if ((t1 != Int) || (t2 != Int)) {
    env->semant_error(this) << "non-Int arguments: " << t1 << " - " << t2
      << endl;
  }

  set_type(Int);
  return Int;
}

Symbol mul_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);
  Symbol t2 = e2->type_check(env);

  if ((t1 != Int) || (t2 != Int)) {
    env->semant_error(this) << "non-Int arguments: " << t1 << " * " << t2
      << endl;
  }

  set_type(Int);
  return Int;
}

Symbol divide_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);
  Symbol t2 = e2->type_check(env);

  if ((t1 != Int) || (t2 != Int)) {
    env->semant_error(this) << "non-Int arguments: " << t1 << " / " << t2
      << endl;
  }

  set_type(Int);
  return Int;
}

Symbol neg_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);

  if (t1 != Int) {
    env->semant_error(this) << "Argument of '~' has type " << t1
      << " instead of Int." << endl;
  }

  set_type(Int);
  return Int;
}

Symbol lt_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);
  Symbol t2 = e2->type_check(env);

  if ((t1 != Int) || (t2 != Int)) {
    env->semant_error(this) << "non-Int arguments: " << t1 << " < " << t2
      << endl;
  }

  set_type(Bool);
  return Bool;
}

Symbol eq_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);
  Symbol t2 = e2->type_check(env);

  if ((t1 != Int) || (t2 != Int)) {
    env->semant_error(this) << "non-Int arguments: " << t1 << " = " << t2
      << endl;
  }

  set_type(Bool);
  return Bool;
}

Symbol leq_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);
  Symbol t2 = e2->type_check(env);

  if ((t1 != Int) || (t2 != Int)) {
    env->semant_error(this) << "non-Int arguments: " << t1 << " <= " << t2
      << endl;
  }

  set_type(Bool);
  return Bool;
}

Symbol comp_class::type_check(TypeEnvironment *env)
{
  Symbol t1 = e1->type_check(env);

  if (t1 != Bool) {
    env->semant_error(this) << "Argument of 'not' has type " << t1
      << " instead of Bool." << endl;
  }

  set_type(Bool);
  return Bool;
}

Symbol int_const_class::type_check(TypeEnvironment *env)
{
  set_type(Int);
  return Int;
}

Symbol bool_const_class::type_check(TypeEnvironment *env)
{
  set_type(Bool);
  return Bool;
}

Symbol string_const_class::type_check(TypeEnvironment *env)
{
  set_type(Str);
  return Str;
}

Symbol new__class::type_check(TypeEnvironment *env)
{
  if (env->lookup_class(type_name) != NULL) {
    set_type(type_name);
    return type_name;
  }

  env->semant_error(this) << "'new' used with undefined class " << type_name
    << "." << endl;

  set_type(Object);
  return Object;
}

Symbol isvoid_class::type_check(TypeEnvironment *env)
{
  e1->type_check(env);

  set_type(Bool);
  return Bool;
}

Symbol no_expr_class::type_check(TypeEnvironment *env)
{
  set_type(No_type);
  return No_type;
}

Symbol object_class::type_check(TypeEnvironment *env)
{
  Symbol object_type = env->lookup_object(name);

  if (object_type != NULL) {
    set_type(object_type);
    return object_type;
  }

  env->semant_error(this) << "Undeclared identifier " << name << "." << endl;

  set_type(Object);
  return Object;
}
