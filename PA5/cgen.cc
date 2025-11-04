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

// BoolConst is a class that implements code generation for operations
// on the two booleans, which are given global names here.
BoolConst false_bool(FALSE);
BoolConst true_bool(TRUE);

///////////////////////////////////////////////////////////////////////////////
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
///////////////////////////////////////////////////////////////////////////////

void program_class::cgen(ostream &out) 
{
  // spim wants comments to start with '#'
  out << "# start of generated code\n";

  initialize_constants();
  new CgenClassTable(classes, out);

  out << "\n# end of generated code\n";
}

///////////////////////////////////////////////////////////////////////////////
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
///////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg,
  ostream& out)
{
  out << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg,
  ostream& out)
{
  out << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
    << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& out)
{ 
  out << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream& out)
{ 
  out << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream& out)
{ 
  out << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst& b, ostream& out)
{
  emit_partial_load_address(dest, out);
  b.code_ref(out);
  out << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& out)
{
  emit_partial_load_address(dest, out);
  str->code_ref(out);
  out << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& out)
{
  emit_partial_load_address(dest, out);
  i->code_ref(out);
  out << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& out)
{
  out << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream& out)
{
  out << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream& out)
{
  out << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream& out)
{
  out << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream& out)
{ 
  out << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream& out)
{ 
  out << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream& out)
{
  out << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream& out)
{
  out << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream& out)
{
  out << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream& out)
{
  out << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream &out)
{
  out << JAL << address << endl;
}

static void emit_return(ostream& out)
{
  out << RET << endl;
}

static void emit_gc_assign(ostream& out)
{
  out << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream& out)
{
  out << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream& out)
{
  out << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &out)
{
  out << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream& out)
{
  out << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& out)
{
  out << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &out)
{
  emit_label_ref(l, out); out << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &out)
{
  out << BEQZ << source << " ";
  emit_label_ref(label, out);
  out << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &out)
{
  out << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, out);
  out << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &out)
{
  out << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label, out);
  out << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &out)
{
  out << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, out);
  out << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &out)
{
  out << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label, out);
  out << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &out)
{
  out << BLT << src1 << " " << imm << " ";
  emit_label_ref(label, out);
  out << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &out)
{
  out << BGT << src1 << " " << imm << " ";
  emit_label_ref(label, out);
  out << endl;
}

static void emit_branch(int l, ostream& out)
{
  out << BRANCH; emit_label_ref(l, out); out << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& out)
{
  emit_store(reg, 0, SP, out);
  emit_addiu(SP, SP, -4, out);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& out)
{
  emit_load(dest, DEFAULT_OBJFIELDS, source, out);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& out)
{
  emit_store(source, DEFAULT_OBJFIELDS, dest, out);
}

static void emit_test_collector(ostream &out)
{
  emit_push(ACC, out);
  emit_move(ACC, SP, out); // stack end
  emit_move(A1, ZERO, out); // allocate nothing
  out << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP, SP, 4, out);
  emit_load(ACC, 0, SP, out);
}

static void emit_gc_check(char *source, ostream &out)
{
  if (source != (char*) A1) emit_move(A1, source, out);
  out << JAL << "_gc_check" << endl;
}

//
// Setup stack frame and self pointer
//
static void emit_function_prologue(ostream &out)
{
  // allocate room for the stack frame
  emit_addiu(SP, SP, -FRAME_SIZE, out);

  emit_store(FP, FP_OFFSET, SP, out); // save caller's frame pointer
  emit_store(SELF, SELF_OFFSET, SP, out); // save caller's self
  emit_store(RA, RA_OFFSET, SP, out); // save return address

  // setup new frame pointer
  emit_addiu(FP, SP, 4, out);

  // set $s0 to point to self
  emit_move(SELF, ACC, out);
}

//
// Restore stack frame, pop the arguments, and return
//
static void emit_function_epilogue(int formal_count, ostream &out)
{
  emit_load(RA, RA_OFFSET, SP, out); // restore return address
  emit_load(SELF, SELF_OFFSET, SP, out); // restore caller's self
  emit_load(FP, FP_OFFSET, SP, out); // restore caller's frame pointer

  // pop the stack frame and the arguments
  emit_addiu(SP, SP, FRAME_SIZE + formal_count * WORD_SIZE, out);

  // return to the caller
  emit_return(out);
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
void StringEntry::code_ref(ostream& out)
{
  out << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
//
void StringEntry::code_def(ostream& out, int string_class_tag)
{
  IntEntryP len_sym = inttable.add_int(len);
  int object_size = DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4;

  // Add -1 eye catcher
  out << WORD << "-1" << endl;

  code_ref(out); out << LABEL; // label
  out << WORD << string_class_tag << endl; // class tag
  out << WORD << object_size << endl; // object size
  
  // dispatch table
  out << WORD; emit_disptable_ref(Str, out); out << endl;   

  out << WORD; len_sym->code_ref(out); out << endl; // string length
  emit_string_constant(out, str); // ascii string

  // align to word
  out << ALIGN;
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& out, int string_class_tag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(out, string_class_tag);
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
//
void IntEntry::code_def(ostream &out, int int_class_tag)
{
  // Add -1 eye catcher
  out << WORD << "-1" << endl;

  code_ref(out); out << LABEL; // label
  out << WORD << int_class_tag << endl; // class tag
  out << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl; // object size

  // dispatch table
  out << WORD; emit_disptable_ref(Int, out); out << endl;   

  // integer value
  out << WORD << str << endl;
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &out, int int_class_tag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(out, int_class_tag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& out) const
{
  out << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
//
void BoolConst::code_def(ostream& s, int bool_class_tag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s); s << LABEL; // label
  s << WORD << bool_class_tag << endl; // class tag
  s << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl; // object size

  // dispatch table
  s << WORD; emit_disptable_ref(Bool, s); s << endl;   

  // value (0 or 1)
  s << WORD << val << endl;
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

CgenClassTable::CgenClassTable(Classes classes, ostream& o)
: list(NULL),
  table(new SymbolTable<Symbol, CgenClassTableEntry>()),
  out(o)
{
  table->enterscope();

  if (cgen_debug) cout << "Building CgenClassTable" << endl;

  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  root()->init(
    0,
    SymbolTable<int, Entry>(),
    SymbolTable<Symbol, MethodBinding>(),
    0,
    SymbolTable<int, Entry>(),
    SymbolTable<Symbol, VarBinding>()
  );

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
			Basic,
      this));

  table->addid(SELF_TYPE,
	  new CgenClassTableEntry(
      class_(SELF_TYPE,
        No_class,
        nil_Features(),
        filename),
			Basic,
      this));

  table->addid(prim_slot,
	  new CgenClassTableEntry(
      class_(prim_slot,
        No_class,
        nil_Features(),
        filename),
			Basic,
      this));

  // 
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object  aborts the program
  //        type_name() : Str      returns a string representation of class name
  //        copy() : SELF_TYPE     returns a copy of the object
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
    Basic,
    this));

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
    Basic,
    this));

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
      Basic,
      this));

  //
  // Bool also has only the "val" slot.
  //
  install_class(
    new CgenClassTableEntry(
      class_(Bool,
        Object,
        single_Features(attr(val, prim_slot, no_expr())),
        filename),
      Basic,
      this));

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
      Basic,
      this));
}

//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_classes(Classes cs)
{
  for (int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenClassTableEntry(cs->nth(i), NotBasic, this));
}

void CgenClassTable::install_class(CgenClassTableEntryP entry)
{
  Symbol name = entry->get_name();

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
    CgenClassTableEntry *entry = l->hd();
    CgenClassTableEntry *parent_entry = table->probe(entry->get_parent_name());

    if (parent_entry) {
      entry->set_parent(parent_entry);
      parent_entry->add_child(entry);
    }
  }
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

  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nametab();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  root()->code_dispatch_table(out);

  if (cgen_debug) cout << "coding prototype objects" << endl;
  root()->code_prototype_object(out);

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding object initializers" << endl;
  root()->code_init(out);

  if (cgen_debug) cout << "coding class methods" << endl;
  root()->code_methods(out);
}

///////////////////////////////////////////////////////////////////////////////
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
///////////////////////////////////////////////////////////////////////////////

void CgenClassTable::code_global_data()
{
  Symbol main = idtable.lookup_string(MAINNAME);
  Symbol string = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc = idtable.lookup_string(BOOLNAME);

  out << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  out << GLOBAL << CLASSNAMETAB << endl;
  out << GLOBAL; emit_protobj_ref(main, out); out << endl;
  out << GLOBAL; emit_protobj_ref(integer, out); out << endl;
  out << GLOBAL; emit_protobj_ref(string, out); out << endl;
  out << GLOBAL; false_bool.code_ref(out); out << endl;
  out << GLOBAL; true_bool.code_ref(out); out << endl;
  out << GLOBAL << INTTAG << endl;
  out << GLOBAL << BOOLTAG << endl;
  out << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  out << INTTAG << LABEL;
  out << WORD << int_class_tag << endl;
  out << BOOLTAG << LABEL;       
  out << WORD << bool_class_tag << endl;
  out << STRINGTAG << LABEL; 
  out << WORD << string_class_tag << endl;    
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  out << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  out << "_MemMgr_INITIALIZER:" << endl;
  out << WORD << gc_init_names[cgen_Memmgr] << endl;
  out << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  out << "_MemMgr_COLLECTOR:" << endl;
  out << WORD << gc_collect_names[cgen_Memmgr] << endl;
  out << GLOBAL << "_MemMgr_TEST" << endl;
  out << "_MemMgr_TEST:" << endl;
  out << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

///////////////////////////////////////////////////////////////////////////////
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
///////////////////////////////////////////////////////////////////////////////

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(out, string_class_tag);
  inttable.code_string_table(out, int_class_tag);
  code_bools(bool_class_tag);
}

void CgenClassTable::code_bools(int bool_class_tag)
{
  false_bool.code_def(out, bool_class_tag);
  true_bool.code_def(out, bool_class_tag);
}

void CgenClassTable::code_class_nametab()
{
  out << CLASSNAMETAB << LABEL;
  root()->code_class_nametab(out);
}

///////////////////////////////////////////////////////////////////////////////
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
///////////////////////////////////////////////////////////////////////////////

void CgenClassTable::code_global_text()
{
  out << GLOBAL << HEAP_START << endl;
  out << HEAP_START << LABEL; 
  out << WORD << 0 << endl;
  out << "\t.text" << endl;
  out << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), out);
  out << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"), out);
  out << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"), out);
  out << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"), out);
  out << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), out);
  out << endl;
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

CgenClassTableEntry::CgenClassTableEntry(Class_ t, Basicness bstatus, CgenClassTable *ct)
: tree_node(t),
  basic_status(bstatus),
  tag(-1),
  class_table(ct)
{}

void CgenClassTableEntry::add_child(CgenClassTableEntryP entry)
{
  children = new List<CgenClassTableEntry>(entry, children);
}

void CgenClassTableEntry::set_parent(CgenClassTableEntryP p)
{
  assert(!parent);
  assert(p);
  parent = p;
}

void CgenClassTableEntry::init(
  int nmo,
  SymbolTable<int, Entry> mnt,
  SymbolTable<Symbol, MethodBinding> mt,
  int nao,
  SymbolTable<int, Entry> ant,
  SymbolTable<Symbol, VarBinding> at)
{
  tag = class_table->assign_class_tag(get_name());

  env = new CgenEnvironment(this, class_table);

  dispatch_table_len = nmo;
  method_name_table = mnt;
  method_table = mt;

  attribute_count = nao;
  attr_name_table = ant;
  var_table = at;

  method_name_table.enterscope();
  method_table.enterscope();

  attr_name_table.enterscope();
  var_table.enterscope();

  var_table.addid(self, new SelfBinding());

  Features features = tree_node->get_features();

  for (int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->add_feature(this);

  for (List<CgenClassTableEntry> *l = children; l; l = l->tl())
    l->hd()->init(dispatch_table_len, method_name_table, method_table,
      attribute_count, attr_name_table, var_table);
}

void CgenClassTableEntry::code_class_nametab(ostream& out)
{
  StringEntry *s = stringtable.lookup_string(get_name()->get_string());

  out << WORD; s->code_ref(out); out << endl;

  for (List<CgenClassTableEntry> *l = children; l; l = l->tl())
    l->hd()->code_class_nametab(out);
}

void CgenClassTableEntry::code_dispatch_table(ostream& out)
{
  emit_disptable_ref(get_name(), out); out << LABEL;

  for (int offset = 0; offset < dispatch_table_len; offset++) {
    Symbol method_name = method_name_table.lookup(offset);
    assert(method_name);
    MethodBinding *method_binding = method_table.lookup(method_name);
    assert(method_binding);

    out << WORD; method_binding->code_ref(out); out << endl;
  }

  for (List<CgenClassTableEntry> *l = children; l; l = l->tl())
    l->hd()->code_dispatch_table(out);
}

void CgenClassTableEntry::code_prototype_object(ostream &out)
{
  // Add -1 eye catcher
  out << WORD << "-1" << endl;

  emit_protobj_ref(get_name(), out); out << LABEL; // label
  out << WORD << tag << endl; // tag
  out << WORD << DEFAULT_OBJFIELDS + attribute_count << endl; // size

  // dispatch table
  out << WORD; emit_disptable_ref(get_name(), out); out << endl;

  // attributes
  for (int i = 0; i < attribute_count; i++) {
    Symbol attr_name = attr_name_table.lookup(i);
    assert(attr_name);
    VarBinding *attr_binding = var_table.lookup(attr_name);
    assert(attr_binding);

    out << WORD;

    if (attr_binding->get_type() == Int)
      inttable.lookup_string("0")->code_ref(out);
    else if (attr_binding->get_type() == Str)
      stringtable.lookup_string("")->code_ref(out);
    else if (attr_binding->get_type() == Bool)
      false_bool.code_ref(out);
    else
      out << "0";

    out << endl;
  }

  for (List<CgenClassTableEntry> *l = children; l; l = l->tl())
    l->hd()->code_prototype_object(out);
}

void CgenClassTableEntry::code_init(ostream &out)
{
  emit_init_ref(get_name(), out); out << LABEL;

  emit_function_prologue(out);

  if (get_parent_name() != No_class) {
    // initialize parent
    assert(parent);
    out << JAL; emit_init_ref(get_parent_name(), out); out << endl;
  }

  // initialize attributes
  Features features = tree_node->get_features();
  for (int i = features->first(); features->more(i); i = features->next(i))
    features->nth(i)->code_init(out, env);

  // return self
  emit_move(ACC, SELF, out);
  emit_function_epilogue(0, out);

  for (List<CgenClassTableEntry> *l = children; l; l = l->tl())
    l->hd()->code_init(out);
}

void CgenClassTableEntry::add_attribute(Symbol name, Symbol type)
{
  attr_name_table.addid(attribute_count, name);
  var_table.addid(name, new AttributeBinding(type, attribute_count++));
}

void CgenClassTableEntry::add_method(Symbol name)
{
  if (!method_table.lookup(name))
    method_name_table.addid(dispatch_table_len++, name);
  method_table.addid(name, new MethodBinding(get_name(), name));
}

void CgenClassTableEntry::code_methods(ostream &out)
{
  if (!is_basic()) {
    Features features = tree_node->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      var_table.enterscope();
      features->nth(i)->code_method(out, env);
      var_table.exitscope();
    }
  }

  for (List<CgenClassTableEntry> *l = children; l; l = l->tl())
    l->hd()->code_methods(out);
}

int CgenClassTableEntry::lookup_method(Symbol name)
{
  for (int offset = 0; offset < dispatch_table_len; offset++)
    if (method_name_table.lookup(offset) == name)
      return offset;
  return -1;
}

int CgenClassTableEntry::lookup_method(Symbol class_name, Symbol method_name)
{
  CgenClassTableEntry *entry = class_table->lookup(class_name);
  assert(entry);
  return entry->lookup_method(method_name);
}

void CgenClassTableEntry::add_local(Symbol name, Symbol type, int offset)
{
  var_table.addid(name, new LocalBinding(type, offset));
}

int CgenEnvironment::next_label = 0;

VarBinding *CgenEnvironment::lookup_var(Symbol name)
{
  return entry->lookup_var(name);
}

int CgenEnvironment::lookup_method(Symbol class_name, Symbol method_name)
{
  if (class_name == SELF_TYPE)
    return entry->lookup_method(method_name);

  CgenClassTableEntryP e = table->lookup(class_name);
  assert(e);
  return e->lookup_method(method_name);
}

void CgenEnvironment::add_local(Symbol name, Symbol type, int offset)
{
  entry->add_local(name, type, offset);
}

///////////////////////////////////////////////////////////////////////
//
// Variable bindings
//
///////////////////////////////////////////////////////////////////////

AttributeBinding::AttributeBinding(Symbol t, int o)
: type(t),
  offset(o)
{}

Symbol AttributeBinding::get_type()
{
  return type;
}

void AttributeBinding::code_read(ostream &out)
{
  emit_load(ACC, DEFAULT_OBJFIELDS + offset, SELF, out);
}

void AttributeBinding::code_update(ostream &out)
{
  emit_store(ACC, DEFAULT_OBJFIELDS + offset, SELF, out);
}

Symbol SelfBinding::get_type()
{
  return SELF_TYPE;
}

void SelfBinding::code_read(ostream &out)
{
  emit_move(ACC, SELF, out);
}

void SelfBinding::code_update(ostream &)
{
  // do nothing (cannot write to self!)
}

LocalBinding::LocalBinding(Symbol t, int o)
: type(t),
  offset(o)
{}

Symbol LocalBinding::get_type()
{
  return type;
}

void LocalBinding::code_read(ostream &out)
{
  emit_load(ACC, offset, FP, out);
}

void LocalBinding::code_update(ostream &out)
{
  emit_store(ACC, offset, FP, out);
}

///////////////////////////////////////////////////////////////////////
//
// MethodBinding methods
//
///////////////////////////////////////////////////////////////////////

MethodBinding::MethodBinding(Symbol cn, Symbol mn)
: class_name(cn),
  method_name(mn)
{}

void MethodBinding::code_ref(ostream &out)
{
  emit_method_ref(class_name, method_name, out);
}

///////////////////////////////////////////////////////////////////////
//
// Feature methods
//
///////////////////////////////////////////////////////////////////////

void attr_class::add_feature(CgenClassTableEntry *entry)
{
  entry->add_attribute(name, type_decl);
}

void attr_class::code_init(ostream &out, CgenEnvironment *env)
{
  if (init->type) {
    VarBinding *var = env->lookup_var(name);
    assert(var);

    init->code(out, env);
    var->code_update(out);
  }
}

void attr_class::code_method(ostream &, CgenEnvironment *)
{
  // do nothing
}

void method_class::add_feature(CgenClassTableEntry *entry)
{
  entry->add_method(this->name);
}

void method_class::code_init(ostream &, CgenEnvironment *)
{
  // do nothing
}

void method_class::code_method(ostream &out, CgenEnvironment *env)
{
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    Formal f = formals->nth(i);
    env->add_local(f->get_name(), f->get_type(), 3 + i);
  }

  // TODO: replace with real class name
  emit_method_ref(Main, name, out); out << LABEL;
  emit_function_prologue(out);
  expr->code(out, env);
  emit_function_epilogue(formals->len(), out);
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

void assign_class::code(ostream&, CgenEnvironment *)
{
}

void static_dispatch_class::code(ostream&, CgenEnvironment *)
{
}

void dispatch_class::code(ostream& out, CgenEnvironment *env)
{
  // Push arguments onto the stack
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(out, env);
    emit_push(ACC, out);
  }

  expr->code(out, env);

  int label = env->get_next_label();

  emit_bne(ACC, ZERO, label, out);

  // abort dispatch
  StringEntry *filename = stringtable.lookup_string(env->get_file_name());
  emit_load_string(ACC, filename, out); // filename in $a0
  emit_load_imm(T1, get_line_number(), out); // line number in $t1
  emit_jal("_dispatch_abort", out);

  emit_label_def(label, out);

  int offset = env->lookup_method(expr->get_type(), name);

  emit_load(T1, DISPTABLE_OFFSET, ACC, out);
  emit_load(T1, offset, T1, out);
  emit_jalr(T1, out);

  // The arguments are popped off the stack by the callee
}

void cond_class::code(ostream&, CgenEnvironment *)
{
}

void loop_class::code(ostream&, CgenEnvironment *)
{
}

void typcase_class::code(ostream&, CgenEnvironment *)
{
}

void block_class::code(ostream& out, CgenEnvironment *env)
{
  for (int i = body->first(); body->more(i); i = body->next(i))
    body->nth(i)->code(out, env);
}

void let_class::code(ostream&, CgenEnvironment *)
{
}

void plus_class::code(ostream&, CgenEnvironment *)
{
}

void sub_class::code(ostream&, CgenEnvironment *)
{
}

void mul_class::code(ostream&, CgenEnvironment *)
{
}

void divide_class::code(ostream&, CgenEnvironment *)
{
}

void neg_class::code(ostream&, CgenEnvironment *)
{
}

void lt_class::code(ostream&, CgenEnvironment *)
{
}

void eq_class::code(ostream&, CgenEnvironment *)
{
}

void leq_class::code(ostream&, CgenEnvironment *)
{
}

void comp_class::code(ostream&, CgenEnvironment *)
{
}

void int_const_class::code(ostream& out, CgenEnvironment *)  
{
  IntEntry *i = inttable.lookup_string(token->get_string());
  assert(i);
  emit_load_int(ACC, i, out);
}

void string_const_class::code(ostream& out, CgenEnvironment *)
{
  StringEntry *str = stringtable.lookup_string(token->get_string());
  assert(str);
  emit_load_string(ACC, str, out);
}

void bool_const_class::code(ostream& out, CgenEnvironment *)
{
  emit_load_bool(ACC, BoolConst(val), out);
}

void new__class::code(ostream& out, CgenEnvironment *env)
{
  emit_partial_load_address(ACC, out);
  emit_protobj_ref(type_name, out);
  out << endl;

  emit_jal("Object" METHOD_SEP "copy", out);

  out << JAL; emit_init_ref(type_name, out); out << endl;
}

void isvoid_class::code(ostream&, CgenEnvironment *)
{
}

void no_expr_class::code(ostream&, CgenEnvironment *)
{
}

void object_class::code(ostream& out, CgenEnvironment *env)
{
  VarBinding *var = env->lookup_var(name);
  assert(var);
  var->code_read(out);
}
