/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 *  to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int comment_nesting;

%}

/*
 *  Define names for regular expressions here.
 */
A               (a|A)
C               (c|C)
D               (d|D)
E               (e|E)
F               (f|F)
H               (h|H)
I               (i|I)
L               (l|L)
N               (n|N)
O               (o|O)
P               (p|P)
R               (r|R)
S               (s|S)
T               (t|T)
U               (u|U)
V               (v|V)
W               (w|W)

%x COMMENT STRING

%%

 /*
  *  Nested comments
  */
"(*"                {
                      BEGIN COMMENT;
                      comment_nesting++;
                    }
"*)"                {
                      cool_yylval.error_msg = "Unmatched *)";
                      return (ERROR);
                    }
<COMMENT>"(*"       {
                      comment_nesting++;
                    }
<COMMENT>"*)"       {
                      if (--comment_nesting == 0)
                        BEGIN INITIAL;
                    }
<COMMENT>[^*()\n]+  |
<COMMENT>[*()]      ;
<COMMENT>\n         { curr_lineno++; }
<COMMENT><<EOF>>    {
                      BEGIN INITIAL;

                      cool_yylval.error_msg = "EOF in comment";
                      return (ERROR);
                    }

 /*
  *  Single-line comments
  */
"--".*              ;

 /*
  *  The multiple-character operators
  */
"=>"                { return (DARROW); }
"<-"                { return (ASSIGN); }
"<="                { return (LE); }

 /*
  *  Single-character special symbols
  */
":"                 |
";"                 |
","                 |
"{"                 |
"}"                 |
"("                 |
")"                 |
"@"                 |
"."                 |
"+"                 |
"-"                 |
"*"                 |
"/"                 |
"~"                 |
"<"                 |
"="                 { return (*yytext); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{C}{A}{S}{E}        { return (CASE); }
{C}{L}{A}{S}{S}     { return (CLASS); }
{E}{L}{S}{E}        { return (ELSE); }
{E}{S}{A}{C}        { return (ESAC); }
{F}{I}              { return (FI); }
{I}{F}              { return (IF); }
{I}{N}              { return (IN); }
{I}{N}{H}{E}{R}{I}{T}{S}  { return (INHERITS); }
{I}{S}{V}{O}{I}{D}  { return (ISVOID); }
{L}{E}{T}           { return (LET); }
{L}{O}{O}{P}        { return (LOOP); }
{N}{E}{W}           { return (NEW); }
{N}{O}{T}           { return (NOT); }
{O}{F}              { return (OF); }
{P}{O}{O}{L}        { return (POOL); }
{T}{H}{E}{N}        { return (THEN); }
{W}{H}{I}{L}{E}     { return (WHILE); }
f{A}{L}{S}{E}       {
                      cool_yylval.boolean = 0;
                      return (BOOL_CONST);
                    }
t{R}{U}{E}          {
                      cool_yylval.boolean = 1;
                      return (BOOL_CONST);
                    }

 /*
  *  Type identifiers begin with a capital letter, object identifiers begin
  *  with a lower case letter.
  */
[A-Z][_a-zA-Z0-9]*  {
                      cool_yylval.symbol = idtable.add_string(yytext);
                      return (TYPEID);
                    }
[a-z][_a-zA-Z0-9]*  {
                      cool_yylval.symbol = idtable.add_string(yytext);
                      return (OBJECTID);
                    }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"                  {
                      BEGIN STRING;
                      string_buf_ptr = string_buf;
                    }
<STRING>\"          {
                      BEGIN INITIAL;

                      if (string_buf_ptr == NULL)
                        return (ERROR);
                      
                      *string_buf_ptr = '\0';
                      cool_yylval.symbol = stringtable.add_string(string_buf);
                      return (STR_CONST);
                    }
<STRING>\\[^\0]     {
                      if (string_buf_ptr != NULL) {
                        if (string_buf_ptr >= &string_buf[MAX_STR_CONST-1]) {
                          cool_yylval.error_msg = "String constant too long";
                          string_buf_ptr = NULL;
                        } else {
                          switch (yytext[1]) {
                          case 'b':
                            *string_buf_ptr++ = '\b';
                            break;
                          case 't':
                            *string_buf_ptr++ = '\t';
                            break;
                          case '\n':
                            curr_lineno++;
                            // fall through
                          case 'n':
                            *string_buf_ptr++ = '\n';
                            break;
                          case 'f':
                            *string_buf_ptr++ = '\f';
                            break;
                          default:
                            *string_buf_ptr++ = yytext[1];
                            break;
                          }
                        }
                      }
                    }
<STRING>\0          {
                      cool_yylval.error_msg = "String contains null character";
                      string_buf_ptr = NULL;
                    }
<STRING>\n          {
                      BEGIN INITIAL;

                      curr_lineno++;

                      cool_yylval.error_msg = "Unterminated string constant";
                      return (ERROR);
                    }
<STRING>.           {
                      if (string_buf_ptr != NULL) {
                        if (string_buf_ptr >= &string_buf[MAX_STR_CONST-1]) {
                          cool_yylval.error_msg = "String constant too long";
                          string_buf_ptr = NULL;
                        } else {
                          *string_buf_ptr++ = *yytext;
                        }
                      }
                    }
<STRING><<EOF>>     {
                      BEGIN INITIAL;

                      cool_yylval.error_msg = "EOF in string constant";
                      return (ERROR);
                    }

 /*
  *  Integer constants
  */
[0-9]+              {
                      cool_yylval.symbol = inttable.add_string(yytext);
                      return (INT_CONST);
                    }

 /*
  *  Whitespace
  */
\n                  { curr_lineno++; }
[ \f\r\t\v]+        ;

.                   {
                      cool_yylval.error_msg = strdup(yytext);
                      return (ERROR);
                    }

%%
