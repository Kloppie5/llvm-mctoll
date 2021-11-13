grammar ASL;

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
}

start : (NEWLINE | stmt)*;

stmt
  : caseStmt
  | forStmt
  | ifStmt
  | (simpleStmt SEMICOLON)+
;

caseStmt
  : CASE lvalue OF NEWLINE
    INDENT (WHEN literal stmtBlock)+
           (OTHERWISE stmtBlock)?
    DEDENT
;
forStmt
  : FOR lvalue
      ASSIGN rvalue
      TO rvalue stmtBlock
;
ifStmt
  : IF rvalue
      THEN stmtBlock
    (ELSIF rvalue
      THEN stmtBlock)*
    (ELSE stmtBlock)?
;

stmtBlock
  : NEWLINE INDENT stmt+ DEDENT
  | (simpleStmt SEMICOLON)+
;

simpleStmt
  : typeDecl ID
  | typeDecl? lvalue ASSIGN rvalue
  | '-' '=' functionCall
  | functionCall
  | UNDEFINED
  | UNPREDICTABLE
  | SEE STRING
;

lvalue
  : ID
  | '(' ID ',' ID ')'
  | lvalue '.' ID
  | ID '.' '<' ID (',' ID)* '>'
  | lvalue '[' (rvalue (COMMA rvalue)*)? ']'
  | lvalue '<' NUMBER (COLON NUMBER)? '>'
  | '-'
;

rvalue
  : lvalue
  | '(' rvalue ')'
  | functionCall
  | typeDecl UNKNOWN
  | IF rvalue THEN rvalue ELSE rvalue
  | rvalue IN LEFT_CURLY_BRACKET rvalue (COMMA rvalue)* RIGHT_CURLY_BRACKET
  | rvalue COLON rvalue
  | rvalue ('&&' | '||') rvalue
  | '!' rvalue
  | rvalue ('==' | '!=') rvalue
  | rvalue ('>=' | '>') rvalue
  | rvalue ('<<' | '>>') rvalue
  | rvalue ('+' | '-' | '*') rvalue
  | rvalue DIV rvalue
  | literal
;

functionCall
  : lvalue LEFT_BRACKET (rvalue (COMMA rvalue)*)? RIGHT_BRACKET
;

typeDecl
  : BITS LEFT_BRACKET literal RIGHT_BRACKET
  | INTEGERTYPEDECL
;

literal
  : BITPATTERN
  | ID
  | NUMBER
;

/*
  LEXING
*/

ASSIGN : '=';
COLON : ':';
COMMA : ',';
DOT : '.';
INTEGERTYPEDECL : 'integer';
LEFT_ANGLE_BRACKET : '<' {self.opened += 1};
LEFT_BRACKET : '(' {self.opened += 1};
LEFT_CURLY_BRACKET : '{' {self.opened += 1};
LEFT_SQUARE_BRACKET : '[' {self.opened += 1};
RIGHT_ANGLE_BRACKET : '>' {self.opened -= 1};
RIGHT_BRACKET : ')' {self.opened -= 1};
RIGHT_CURLY_BRACKET : '}' {self.opened -= 1};
RIGHT_SQUARE_BRACKET : ']' {self.opened -= 1};
SEMICOLON : ';';
TIMES : '*';

BITS : 'bits';
CASE : 'case';
DIV : 'DIV';
ELSE : 'else';
ELSIF : 'elsif';
FOR : 'for';
IF : 'if';
IN : 'IN';
OF : 'of';
OTHERWISE : 'otherwise';
SEE : 'SEE';
THEN : 'then';
TO : 'to';
UNDEFINED : 'UNDEFINED';
UNKNOWN : 'UNKNOWN';
UNPREDICTABLE : 'UNPREDICTABLE';
WHEN : 'when';

NEWLINE
  : ('\r'? '\n' | '\r' | '\f' ) [ \t]*
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
if self.opened > 0 or nextnext_eof is False and (la_char == '\r' or la_char == '\n' or la_char == '\f' or la_char == '#'):
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

Whitespace
  : ([ \t]+
    | '//' ~[\r\n]*
  ) -> skip
;
