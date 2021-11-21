from antlr4 import *
from ASLLexer import ASLLexer
from ASLParser import ASLParser
from ASLVisitor import ASLVisitor
from antlr4.tree.Trees import Trees
import sys

class ASLPrintVisitor(ASLVisitor):

  def printASL(self, asl):
    input = InputStream(asl+'\n')
    lexer = ASLLexer(input)
    stream = CommonTokenStream(lexer)
    parser = ASLParser(stream)
    tree = parser.start()
    self.visit(tree)
    return parser.getNumberOfSyntaxErrors()
