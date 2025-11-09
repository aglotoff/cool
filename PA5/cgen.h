#include <assert.h>
#include <stdio.h>

#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenClassTableEntry;
typedef CgenClassTableEntry *CgenClassTableEntryP;

class CgenEnvironment;
typedef CgenEnvironment *CgenEnvironmentP;

class MethodBinding {
private:
   Symbol class_name;
   Symbol method_name;

public:
   MethodBinding(Symbol, Symbol);
   void code_ref(ostream &);
};

class ObjectBinding {
public:
   virtual Symbol get_type() = 0;
   virtual void code_load(ostream &) = 0;
   virtual void code_update(ostream &) = 0;
};

class AttributeBinding : public ObjectBinding {
private:
   Symbol type;
   int offset;

public:
   AttributeBinding(Symbol, int);

   Symbol get_type();
   void code_load(ostream &);
   void code_update(ostream &);
};

class SelfBinding : public ObjectBinding {
public:
   Symbol get_type();
   void code_load(ostream &);
   void code_update(ostream &);
};

class LocalBinding : public ObjectBinding {
private:
   Symbol type;
   int offset;

public:
   LocalBinding(Symbol, int);

   Symbol get_type();
   void code_load(ostream &);
   void code_update(ostream &);
};

class CgenClassTable {
private:
   List<CgenClassTableEntry> *list;
   SymbolTable<Symbol, CgenClassTableEntry> *table;

   ostream& out;

   int next_class_tag;
   int string_class_tag;
   int int_class_tag;
   int bool_class_tag;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_class_nametab();
   void code_class_objtab();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenClassTableEntry', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenClassTableEntryP);
   void install_classes(Classes cs);
   void build_inheritance_tree();

public:
   CgenClassTable(Classes, ostream&);
   void code();
   CgenClassTableEntryP root();
   CgenClassTableEntry *lookup(Symbol name) { return table->probe(name); }

   int assign_class_tag(Symbol);
};


class CgenClassTableEntry {
private:
   Class_ tree_node;
   CgenClassTableEntryP parent;                        // Parent of class
   List<CgenClassTableEntry> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   
   int tag;
   CgenClassTable *class_table;

   CgenEnvironment *env;

   SymbolTable<int, Entry> method_name_table;
   SymbolTable<Symbol, MethodBinding> method_table;
   int dispatch_table_len;

   SymbolTable<int, Entry> attr_name_table;
   SymbolTable<Symbol, ObjectBinding> var_table;
   int attribute_count;

   int next_local_offset = -1;

public:
   CgenClassTableEntry(Class_, Basicness, CgenClassTable *);

   CgenClassTableEntryP get_parent() { return parent; }
   List<CgenClassTableEntry> *get_children() { return children; }
   Symbol get_name() { return tree_node->get_name(); }
   Symbol get_parent_name() { return tree_node->get_parent(); }
   Symbol get_file_name() { return tree_node->get_filename(); }
   int get_tag() const { return tag; }
   int is_basic() { return (basic_status == Basic); }

   void add_child(CgenClassTableEntryP);
   void set_parent(CgenClassTableEntryP);

   void init(int, SymbolTable<int, Entry>, SymbolTable<Symbol, MethodBinding>,
      int, SymbolTable<int, Entry>, SymbolTable<Symbol, ObjectBinding>);

   void add_method(Symbol);
   void add_attribute(Symbol, Symbol);

   void code_class_nametab(ostream&);
   void code_class_objtab(ostream&);
   void code_dispatch_table(ostream&);
   void code_prototype_object(ostream&);
   void code_init(ostream&);
   void code_methods(ostream&);

   ObjectBinding *lookup_object(Symbol name)
   { return var_table.lookup(name ); }

   int lookup_method(Symbol);
   int lookup_method(Symbol, Symbol);
   void add_formal(Symbol, Symbol, int);
   void enter_local(Symbol, Symbol);
   void exit_local();
   void enter_scope();
   void exit_scope();
};

class CgenEnvironment {
private:
   static int next_label;

   CgenClassTableEntry *entry;
   CgenClassTable *table;

public:
   CgenEnvironment(CgenClassTableEntry *e, CgenClassTable *t)
   : entry(e),
     table(t)
   {}

   int get_next_label() { return next_label++; }
   Symbol get_class_name() { return entry->get_name(); }
   char *get_file_name() { return entry->get_file_name()->get_string(); }

   ObjectBinding *lookup_object(Symbol);
   int lookup_method(Symbol, Symbol);
   int lookup_tag(Symbol);
   int lookup_max_child_tag(Symbol);

   void add_formal(Symbol, Symbol, int);
   void enter_local(Symbol, Symbol);
   void exit_local();
   void enter_scope();
   void exit_scope();
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

