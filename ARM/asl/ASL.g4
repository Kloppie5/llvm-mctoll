grammar ASL;

// An ANTLR4 grammar for ARM ASL.

tokens { INDENT, DEDENT }

@lexer::header{
from antlr4.Token import CommonToken
import re
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
@staticmethod
def getIndentationCount(spaces):
    count = 0
    for ch in spaces:
        if ch == '\t':
            count += 8 - (count % 8)
        else:
            count += 1
    return count
def atStartOfInput(self):
    return Lexer.column.fget(self) == 0 and Lexer.line.fget(self) == 1
}

start: (stmt | NEWLINE)* EOF;

stmt
  : simple_stmt ';' NEWLINE?
  | case_stmt
  | for_stmt
  | if_stmt
;

case_stmt
  : 'case' rvalue 'of' NEWLINE
    INDENT ('when' literal (NEWLINE | stmt_block))+
           ('otherwise' stmt_block)?
    DEDENT
;

for_stmt
  : 'for' lvalue
      '=' rvalue
      'to' rvalue
      stmt_block
;

if_stmt
  : 'if' rvalue
      'then' stmt_block
    ('elsif' rvalue
      'then' stmt_block)*
    ('else' stmt_block)?
;

stmt_block
  : (simple_stmt ';')+ NEWLINE
  | NEWLINE INDENT stmt+ DEDENT
;

simple_stmt
  : lvalue '=' rvalue
  | type_decl IDENTIFIER (',' IDENTIFIER)*
  | type_decl IDENTIFIER '=' rvalue
  | 'enumeration' IDENTIFIER '{' IDENTIFIER (',' IDENTIFIER)* '}'
  | function_call
  | '-' '=' function_call
  | 'assert' rvalue
  | 'SEE' STRING
  | 'UNDEFINED'
  | 'UNPREDICTABLE'
;

rvalue
  : lvalue
  | literal
  | '(' rvalue (',' rvalue)* ')'
  | un_op rvalue
  | rvalue bin_op rvalue
  | rvalue 'IN' '{' rvalue (',' rvalue)* '}'
  | function_call
  | rvalue '<' rvalue '>'
  | rvalue ':' rvalue
  | 'if' rvalue 'then' rvalue 'else' rvalue
;

un_op
  : '!'
  | '-'
;

bin_op
  : '=='
  | '!='
  | '<='
  | '>='
  | '<'
  | '>'
  | '<<'
  | '>>'
  | '&&'
  | '||'
  | 'AND'
  | 'OR'
  | 'EOR'
  | 'DIV'
  | '+'
  | '-'
  | '*'
  | '/'
;

lvalue
  : IDENTIFIER
  | IDENTIFIER '.' IDENTIFIER
  | IDENTIFIER '.' '<' IDENTIFIER (',' IDENTIFIER)* '>'
  | IDENTIFIER '[' ']' '.' IDENTIFIER
  | IDENTIFIER '[' (rvalue (',' rvalue)*)? ']'
  | '(' lvalue ')'
  | '(' lvalue ',' lvalue ')'
  | '<' lvalue ',' lvalue '>'
  | lvalue '<' rvalue '>'
  | lvalue '<' rvalue ':' rvalue '>'
;

function_call
  : IDENTIFIER '(' (rvalue (',' rvalue)*)? ')'
  | IDENTIFIER '.' IDENTIFIER '(' (rvalue (',' rvalue)*)? ')'
;

literal
  : IDENTIFIER
  | INTEGER
  | HEX
  | BIT_VALUE
  | BIT_MASK
  | STRING
  | 'bits' '(' rvalue ')' 'UNKNOWN'
;

type_decl
  : 'bits' '(' rvalue ')'
  | 'bit'
  | 'integer'
  | 'boolean'
  | 'Constraint'
;

IDENTIFIER
  : [a-zA-Z_][a-zA-Z0-9_]*
;

INTEGER
  : '0' | [1-9][0-9]*
;

HEX
  : '0x' [0-9a-fA-F]+
;

BIT_VALUE
  : '\'' [01 ]+ '\''
;

BIT_MASK
  : '\'' [01x ]+ '\''
;

STRING
  : '"' ~["]* '"'
;

NEWLINE
 : ( {self.atStartOfInput()}?   SPACES
   | ( '\r'? '\n' | '\r' | '\f' ) SPACES?
   )
   {
tempt = Lexer.text.fget(self)
newLine = re.sub("[^\r\n\f]+", "", tempt)
spaces = re.sub("[\r\n\f]+", "", tempt)
la_char = ""
try:
    la = self._input.LA(1)
    la_char = chr(la)       # Python does not compare char to ints directly
except ValueError:          # End of file
    pass
# Strip newlines inside open clauses except if we are near EOF. We keep NEWLINEs near EOF to
# satisfy the final newline needed by the single_put rule used by the REPL.
try:
    nextnext_la = self._input.LA(2)
    nextnext_la_char = chr(nextnext_la)
except ValueError:
    nextnext_eof = True
else:
    nextnext_eof = False
if self.opened > 0 or nextnext_eof is False and (la_char == '\r' or la_char == '\n' or la_char == '\f' or la_char == '/'):
    self.skip()
else:
    indent = self.getIndentationCount(spaces)
    previous = self.indents[-1] if self.indents else 0
    self.emitToken(self.commonToken(self.NEWLINE, newLine, indent=indent))      # NEWLINE is actually the '\n' char
    if indent == previous:
        self.skip()
    elif indent > previous:
        self.indents.append(indent)
        self.emitToken(self.commonToken(LanguageParser.INDENT, spaces))
    else:
        while self.indents and self.indents[-1] > indent:
            self.emitToken(self.createDedent())
            self.indents.pop()
    }
 ;

LEFT_BRACKET : '(' {self.opened += 1};
LEFT_CURLY_BRACKET : '{' {self.opened += 1};
LEFT_SQUARE_BRACKET : '[' {self.opened += 1};
RIGHT_BRACKET : ')' {self.opened -= 1};
RIGHT_CURLY_BRACKET : '}' {self.opened -= 1};
RIGHT_SQUARE_BRACKET : ']' {self.opened -= 1};

SKIP_
  : ( SPACES | COMMENT ) -> skip
;

fragment SPACES
  : [ \t]+
;

fragment COMMENT
  : '//' ~[\r\n\f]*
;
