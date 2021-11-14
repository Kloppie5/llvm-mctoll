grammar ASL;

/*
  This grammar is technically a subset of the ASL grammar;
  - line comments are not allowed
  - empty lines are not allowed
  - tabs are not allowed
*/

tokens { INDENT, DEDENT }

@lexer::header{
from antlr4.Token import CommonToken
import importlib
# Allow languages to extend the lexer and parser, by loading the parser dynamically
module_path = __name__[:-5]
language_name = __name__.split('.')[-1]
language_name = language_name[:-5]  # Remove Lexer from name
LanguageParser = getattr(importlib.import_module('{}Parser'.format(module_path)), '{}Parser'.format(language_name))
}

@lexer::members {
@property
def tokens(self):
    try:
        return self._tokens
    except AttributeError:
        self._tokens = []
        return self._tokens
@property
def indents(self):
    try:
        return self._indents
    except AttributeError:
        self._indents = []
        return self._indents
@property
def opened(self):
    try:
        return self._opened
    except AttributeError:
        self._opened = 0
        return self._opened
@opened.setter
def opened(self, value):
    self._opened = value
@property
def lastToken(self):
    try:
        return self._lastToken
    except AttributeError:
        self._lastToken = None
        return self._lastToken
@lastToken.setter
def lastToken(self, value):
    self._lastToken = value
def reset(self):
    super().reset()
    self.tokens = []
    self.indents = []
    self.opened = 0
    self.lastToken = None
def emitToken(self, t):
    super().emitToken(t)
    self.tokens.append(t)
def nextToken(self):
    if self._input.LA(1) == Token.EOF and self.indents:
        for i in range(len(self.tokens)-1,-1,-1):
            if self.tokens[i].type == Token.EOF:
                self.tokens.pop(i)
        self.emitToken(self.commonToken(LanguageParser.NEWLINE, '\n'))
        while self.indents:
            self.emitToken(self.createDedent())
            self.indents.pop()
        self.emitToken(self.commonToken(LanguageParser.EOF, "<EOF>"))
    next = super().nextToken()
    if next.channel == Token.DEFAULT_CHANNEL:
        self.lastToken = next
    return next if not self.tokens else self.tokens.pop(0)
def createDedent(self):
    dedent = self.commonToken(LanguageParser.DEDENT, "")
    dedent.line = self.lastToken.line
    return dedent
def commonToken(self, type, text, indent=0):
    stop = self.getCharIndex()-1-indent
    start = (stop - len(text) + 1) if text else stop
    return CommonToken(self._tokenFactorySourcePair, type, super().DEFAULT_TOKEN_CHANNEL, start, stop)
}

start : stmt* EOF;

stmt
  : caseStmt
  | forStmt
  | ifStmt
  | simpleStmt ';' NEWLINE?
;

caseStmt
  : 'case' rvalue 'of' NEWLINE
    INDENT ('when' literal stmtBlock)+
           ('otherwise' stmtBlock)?
    DEDENT
;
forStmt
  : 'for' lvalue
      '=' rvalue
      'to' rvalue stmtBlock
;
ifStmt
  : 'if' rvalue
      'then' stmtBlock
    ('elsif' rvalue
      'then' stmtBlock)*
    ('else' stmtBlock)?
;

stmtBlock
  : NEWLINE INDENT stmt+ DEDENT
  | (simpleStmt ';')* NEWLINE?
;

simpleStmt
  : typeDecl ID (',' ID)*
  | 'enumeration' ID LEFT_CURLY_BRACKET (ID (',' ID)*)? RIGHT_CURLY_BRACKET
  | 'assert' rvalue
  | typeDecl? lvalue '=' rvalue
  | '-' '=' functionCall
  | functionCall
  | 'UNDEFINED'
  | 'UNPREDICTABLE'
  | 'SEE' STRING
;

lvalue
  : ID
  | LEFT_BRACKET lvalue ',' lvalue RIGHT_BRACKET
  | lvalue '.' ID
  | ID '.' '<' ID (',' ID)* '>'
  | lvalue LEFT_SQUARE_BRACKET (rvalue (',' rvalue)*)? RIGHT_SQUARE_BRACKET
  | lvalue '<' rvalue (':' rvalue)? '>'
  | '<' rvalue ',' rvalue '>'
  | '-'
;

rvalue
  : lvalue
  | LEFT_BRACKET rvalue RIGHT_BRACKET
  | LEFT_BRACKET rvalue ',' rvalue RIGHT_BRACKET
  | rvalue '<' rvalue (':' rvalue)? '>'
  | functionCall
  | typeDecl 'UNKNOWN'
  | 'if' rvalue 'then' rvalue 'else' rvalue
  | rvalue 'IN' LEFT_CURLY_BRACKET rvalue (',' rvalue)* RIGHT_CURLY_BRACKET
  | rvalue ':' rvalue
  | rvalue ('&&' | '||') rvalue
  | rvalue ('AND' | 'OR' | 'EOR') rvalue
  | ('!' | '-') rvalue
  | rvalue ('==' | '!=') rvalue
  | rvalue ('<' | '<=' | '>=' | '>') rvalue
  | rvalue ('<<' | '>>') rvalue
  | rvalue ('+' | '-' | '*' | '/') rvalue
  | rvalue 'DIV' rvalue
  | literal
;

functionCall
  : lvalue LEFT_BRACKET (rvalue (',' rvalue)*)? RIGHT_BRACKET
;

typeDecl
  : 'bits' LEFT_BRACKET rvalue RIGHT_BRACKET
  | 'bit'
  | 'integer'
  | 'boolean'
  | 'Constraint'
;

literal
  : BITPATTERN
  | ID
  | NUMBER
  | HEXNUMBER
;

/*
  LEXING
*/

LEFT_BRACKET : '(' {self.opened += 1};
LEFT_CURLY_BRACKET : '{' {self.opened += 1};
LEFT_SQUARE_BRACKET : '[' {self.opened += 1};
RIGHT_BRACKET : ')' {self.opened -= 1};
RIGHT_CURLY_BRACKET : '}' {self.opened -= 1};
RIGHT_SQUARE_BRACKET : ']' {self.opened -= 1};

NEWLINE
  : '\n' [ ]*
  {
tempt = Lexer.text.fget(self)
la_char = ""
try:
    la = self._input.LA(1)
    la_char = chr(la)       # Python does not compare char to ints directly
except ValueError:          # End of file
    pass
try:
    nextnext_la = self._input.LA(2)
    nextnext_la_char = chr(nextnext_la)
except ValueError:
    nextnext_eof = True
else:
    nextnext_eof = False
if self.opened > 0 or nextnext_eof is False and la_char == '\n':
    self.skip()
else:
    indent = len(tempt)-1
    previous = self.indents[-1] if self.indents else 0
    self.emitToken(self.commonToken(self.NEWLINE, '\n', indent=indent))
    if indent == previous:
        self.skip()
    elif indent > previous:
        self.indents.append(indent)
        self.emitToken(self.commonToken(LanguageParser.INDENT, tempt[1:]))
    else:
        while self.indents and self.indents[-1] > indent:
            self.emitToken(self.createDedent())
            self.indents.pop()
  }
;

BITPATTERN
  : '\'' [01x ]+ '\''
;

ID
  : [a-zA-Z][a-zA-Z0-9_]*
;

STRING
  : '"' (~["])* '"'
;

NUMBER
  : '0'
  | [1-9][0-9]*
;

HEXNUMBER
  : '0x' [0-9a-fA-F]+
;

Whitespace
  : [ ]+ -> skip
;
