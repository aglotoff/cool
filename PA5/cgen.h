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

class VarBinding {
public:
   virtual Symbol get_type() = 0;
   virtual void code_ref(ostream &) = 0;
   virtual void code_update(ostream &) = 0;
};

class AttributeBinding : public VarBinding {
private:
   Symbol name;
   attr_class *tree_node;
   int offset;

public:
   AttributeBinding(Symbol, attr_class *, int);

   Symbol get_type();
   void code_ref(ostream &);
   void code_update(ostream &);
};

class SelfBinding : public VarBinding {
public:
   Symbol get_type();
   void code_ref(ostream &);
   void code_update(ostream &);
};

class CgenClassTable {
private:
   List<CgenClassTableEntry> *list;
   SymbolTable<Symbol, CgenClassTableEntry> *table;

   ostream& str;

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
   Class_ node;
   CgenClassTableEntryP parent;                        // Parent of class
   List<CgenClassTableEntry> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   
   int tag;
   CgenClassTable *class_table;

   CgenEnvironment *env;

   SymbolTable<int, Entry> method_name_table;
   SymbolTable<Symbol, MethodBinding> method_table;
   int next_method_offset;

   SymbolTable<int, Entry> attr_name_table;
   SymbolTable<Symbol, VarBinding> var_table;
   int next_attr_offset;

public:
   CgenClassTableEntry(Class_, Basicness, CgenClassTable *);

   Class_ get_node() { return node; };
   CgenClassTableEntryP get_parent() { return parent; }
   List<CgenClassTableEntry> *get_children() { return children; }
   int get_tag() const { return tag; }
   int is_basic() { return (basic_status == Basic); }

   void add_child(CgenClassTableEntryP);
   void set_parent(CgenClassTableEntryP);

   void init(int, SymbolTable<int, Entry>, SymbolTable<Symbol, MethodBinding>,
      int, SymbolTable<int, Entry>, SymbolTable<Symbol, VarBinding>);

   void add_method(Symbol);
   void add_attribute(Symbol, attr_class *);

   void code_class_nametab(ostream&);
   void code_dispatch_table(ostream&);
   void code_prototype_object(ostream&);
   void code_init(ostream&);
   void code_methods(ostream&);

   VarBinding *lookup_var(Symbol name)
   { return var_table.lookup(name ); }
};

class CgenEnvironment {
public:
   CgenClassTableEntry *entry;
   CgenEnvironment(CgenClassTableEntry *e)
   : entry(e)
   {}

   VarBinding *lookup_var(Symbol name)
   { return entry->lookup_var(name); }
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

