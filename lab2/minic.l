%{
#include "string.h"
#include "def.h"

extern int yycolumn;

#define YYSTYPE yylval
extern "C"
{	
	int yywrap(void);
	int yylex(void);
}

void flexout(std::string name,std::string value){
  std::cout<<name<<"\t:"<<value<<std::endl;
}
//flex为bison提供行号信息
%}
%option yylineno 

%%

"int" {flexout("TYPE","int");}
"float" {flexout("TYPE","float");}
"char" {flexout("TYPE","char");}
";" {{flexout("SEMI",";");}}
"," {{flexout("COMMA",",");}}
"=" {{flexout("ASSIGNOP","=");}}
">"|"<"|">="|"<="|"=="|"!=" {{flexout("RELOP",yytext);}}
"+" {{flexout("PLUS","+");}}
"-" {{flexout("MINUS","-");}}
"*" {{flexout("STAR","*");}}
"/" {{flexout("DIV","/");}}
"&&" {{flexout("AND","&&");}}
"." {{flexout("DOT",".");}}
"!" {{flexout("NOT","!");}}
"(" {{flexout("LP","(");}}
")" {{flexout("RP",")");}}
"[" {{flexout("LB","[");}}
"]" {{flexout("RB","]");}}
"{" {{flexout("LC","{");}}
"}" {{flexout("RC","}");}}
"struct" {{flexout("STRUCT","struct");}}
"return" {{flexout("RETURN","return");}}
"if" {{flexout("IF","if");}}
"else" {{flexout("ELSE","else");}}
"while" {{flexout("WHILE","while");}}

[-]?[1-9][0-9]*|0 {{flexout("INT",yytext);}}
[-]?(0\.[0-9]+|[1-9][0-9]*\.[0-9]+) {{flexout("FLOAT",yytext);}}
[a-zA-Z_][0-9a-zA-Z_]* {{flexout("ID",yytext);}}

[\n]		{yycolumn=1;}
[ \r\t]		{/*printf("过滤空格等字符\n");*/}
. {printf("Error type A at Line %d: Mysterious characters '%s'\n",yylineno,yytext);}

%%

int yywrap()
{
    return 1;
}