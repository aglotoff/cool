
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
    << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest, s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest, s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest, s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s); s << LABEL                                             // label
    << WORD << stringclasstag << endl                                 // tag
    << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
    << WORD;


 /***** Add dispatch information for class String ******/

  s << endl;                                              // dispatch table
  s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                            // ascii string
  s << ALIGN;                                             // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//
void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                              // label
    << WORD << intclasstag << endl                      // class tag
    << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
    << WORD; 

 /***** Add dispatch information for class Int ******/

  s << endl;                                          // dispatch table
  s << WORD << str << endl;                           // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s); s << LABEL                                  // label
    << WORD << boolclasstag << endl                       // class tag
    << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
    << WORD;

 /***** Add dispatch information for class Bool ******/

  s << endl;                                            // dispatch table
  s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

CgenClassTable::CgenClassTable(Classes classes, ostream& s)
: list(NULL),
  table(new SymbolTable<Symbol, CgenClassTableEntry>()),
  str(s)
{
  table->enterscope();

  if (cgen_debug) cout << "Building CgenClassTable" << endl;

  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  root()->init(this);

  string_class_tag = table->probe(Str)->get_tag();
  int_class_tag = table->probe(Int)->get_tag();
  bool_class_tag = table->probe(Bool)->get_tag();

  code();

  table->exitscope();
}

void CgenClassTable::install_basic_classes()
{
// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  table->addid(No_class,
	  new CgenClassTableEntry(
      class_(No_class,
        No_class,
        nil_Features(),
        filename),
			Basic));
  table->addid(SELF_TYPE,
	  new CgenClassTableEntry(
      class_(SELF_TYPE,
        No_class,
        nil_Features(),
        filename),
			Basic));
  table->addid(prim_slot,
	  new CgenClassTableEntry(
      class_(prim_slot,
        No_class,
        nil_Features(),
        filename),
			Basic));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
    new CgenClassTableEntry(
      class_(Object, 
	      No_class,
	      append_Features(
          append_Features(
            single_Features(
              method(cool_abort, nil_Formals(), Object, no_expr())),
            single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
          single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
  install_class(
    new CgenClassTableEntry(
      class_(IO, 
        Object,
        append_Features(
          append_Features(
            append_Features(
              single_Features(method(out_string,
                single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
              single_Features(method(out_int,
                single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
          single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
        filename),	    
    Basic));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
  install_class(
    new CgenClassTableEntry(
      class_(Int, 
        Object,
        single_Features(attr(val, prim_slot, no_expr())),
        filename),
     Basic));

//
// Bool also has only the "val" slot.
//
  install_class(
    new CgenClassTableEntry(
      class_(Bool,
        Object,
        single_Features(attr(val, prim_slot, no_expr())),
        filename),
      Basic));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenClassTableEntry(
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
	      filename),
      Basic));
}

//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_classes(Classes cs)
{
  for (int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenClassTableEntry(cs->nth(i), NotBasic));
}

void CgenClassTable::install_class(CgenClassTableEntryP entry)
{
  Symbol name = entry->get_node()->get_name();

  if (table->probe(name))
    return;

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  list = new List<CgenClassTableEntry>(entry, list);
  table->addid(name, entry);
}

void CgenClassTable::build_inheritance_tree()
{
  for (List<CgenClassTableEntry> *l = list; l; l = l->tl()) {
    Class_ node = l->hd()->get_node();
    
    CgenClassTableEntry *entry = table->probe(node->get_name());
    CgenClassTableEntry *parent_entry = table->probe(node->get_parent());

    if (entry && parent_entry) {
      entry->set_parent(parent_entry);
      parent_entry->add_child(entry);
    }
  }
}

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main = idtable.lookup_string(MAINNAME);
  Symbol string = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str); str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str); str << endl;
  str << GLOBAL; falsebool.code_ref(str); str << endl;
  str << GLOBAL; truebool.code_ref(str); str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << int_class_tag << endl;
  str << BOOLTAG << LABEL       
      << WORD << bool_class_tag << endl;
  str << STRINGTAG << LABEL 
      << WORD << string_class_tag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

void CgenClassTable::code_class_nametab()
{
  str << CLASSNAMETAB << LABEL;
  root()->code_class_nametab();
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,string_class_tag);
  inttable.code_string_table(str,int_class_tag);
  code_bools(bool_class_tag);
}

int CgenClassTable::assign_class_tag(Symbol name)
{
  // Add class name to string table
  stringtable.add_string(name->get_string());
  return next_class_tag++;
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - dispatch tables
//
  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nametab();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
}

CgenClassTableEntryP CgenClassTable::root()
{
   return table->probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenClassTableEntry methods
//
///////////////////////////////////////////////////////////////////////

CgenClassTableEntry::CgenClassTableEntry(Class_ nd, Basicness bstatus)
: parent(NULL),
  children(NULL),
  node(nd),
  basic_status(bstatus),
  tag(-1)
{}

void CgenClassTableEntry::add_child(CgenClassTableEntryP n)
{
  children = new List<CgenClassTableEntry>(n,children);
}

void CgenClassTableEntry::set_parent(CgenClassTableEntryP p)
{
  assert(!parent);
  assert(p);
  parent = p;
}

void CgenClassTableEntry::init(CgenClassTable *class_table)
{
  tag = class_table->assign_class_tag(node->get_name());
  env = new CgenEnvironment(class_table, this);

  for (List<CgenClassTableEntry> *l = children; l; l = l->tl())
    l->hd()->init(class_table);
}

void CgenClassTableEntry::code_class_nametab()
{
  StringEntry *s = stringtable.lookup_string(node->get_name()->get_string());

  env->out_stream() << WORD;
  s->code_ref(env->out_stream());
  env->out_stream() << endl;

  for (List<CgenClassTableEntry> *l = children; l; l = l->tl())
    l->hd()->code_class_nametab();
}

///////////////////////////////////////////////////////////////////////
//
// CgenEnvironment methods
//
///////////////////////////////////////////////////////////////////////

CgenEnvironment::CgenEnvironment(CgenClassTable *ct, CgenClassTableEntry *ce)
: class_table(ct),
  class_entry(ce)
{}

ostream& CgenEnvironment::out_stream()
{
  return class_table->out_stream();
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(CgenEnvironment *env)
{
}

void static_dispatch_class::code(CgenEnvironment *env)
{
}

void dispatch_class::code(CgenEnvironment *env)
{
}

void cond_class::code(CgenEnvironment *env)
{
}

void loop_class::code(CgenEnvironment *env)
{
}

void typcase_class::code(CgenEnvironment *env)
{
}

void block_class::code(CgenEnvironment *env)
{
}

void let_class::code(CgenEnvironment *env)
{
}

void plus_class::code(CgenEnvironment *env)
{
}

void sub_class::code(CgenEnvironment *env)
{
}

void mul_class::code(CgenEnvironment *env)
{
}

void divide_class::code(CgenEnvironment *env)
{
}

void neg_class::code(CgenEnvironment *env)
{
}

void lt_class::code(CgenEnvironment *env)
{
}

void eq_class::code(CgenEnvironment *env)
{
}

void leq_class::code(CgenEnvironment *env)
{
}

void comp_class::code(CgenEnvironment *env)
{
}

void int_const_class::code(CgenEnvironment *env)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(
    ACC,
    inttable.lookup_string(token->get_string()),
    env->out_stream());
}

void string_const_class::code(CgenEnvironment *env)
{
  emit_load_string(
    ACC,
    stringtable.lookup_string(token->get_string()),
    env->out_stream());
}

void bool_const_class::code(CgenEnvironment *env)
{
  emit_load_bool(ACC, BoolConst(val), env->out_stream());
}

void new__class::code(CgenEnvironment *env)
{
}

void isvoid_class::code(CgenEnvironment *env)
{
}

void no_expr_class::code(CgenEnvironment *env)
{
}

void object_class::code(CgenEnvironment *env)
{
}


