from antlr4 import *
from ASLLexer import ASLLexer
from ASLParser import ASLParser
from ASLVisitor import ASLVisitor
import sys

class ASLToULDLVisitor(ASLVisitor):
    def visitStatement(self, ctx):
        print("Hello: %s" % ctx.ID())

def main():
    lexer = ASLLexer(StdinStream())
    stream = CommonTokenStream(lexer)
    parser = ASLParser(stream)
    tree = parser.start()
    ASLToULDLVisitor().visit(tree)

if __name__ == '__main__':
    main()
