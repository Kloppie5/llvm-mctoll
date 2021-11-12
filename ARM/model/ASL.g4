grammar ASL;

start : statement*;

statement : ID;

ID : [a-z]+;

Whitespace : [ \t\n\r] + -> skip;
