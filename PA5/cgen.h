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

class CgenEnvironment {
private:
  CgenClassTable *class_table;
  CgenClassTableEntry *class_entry;

public:
  CgenEnvironment(CgenClassTable *, CgenClassTableEntry *);
  ostream& out_stream();
};

class CgenClassTable {
private:
   List<CgenClassTableEntry> *list;
   SymbolTable<Symbol,CgenClassTableEntry> *table;

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

   ostream& out_stream() { return str; }

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

   CgenEnvironment *env;

public:
   CgenClassTableEntry(Class_, Basicness);

   Class_ get_node() { return node; };
   CgenClassTableEntryP get_parent() { return parent; }
   List<CgenClassTableEntry> *get_children() { return children; }
   int get_tag() const { return tag; }
   int is_basic() { return (basic_status == Basic); }

   void add_child(CgenClassTableEntryP);
   void set_parent(CgenClassTableEntryP);

   void init(CgenClassTable *);
   void code_class_nametab();
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

