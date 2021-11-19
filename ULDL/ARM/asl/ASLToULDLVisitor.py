from antlr4 import *
from ASLLexer import ASLLexer
from ASLParser import ASLParser
from ASLVisitor import ASLVisitor
from antlr4.tree.Trees import Trees
import sys
import ULDL

class ASLToULDLVisitor(ASLVisitor):

  def __init__(self):
    self.pattern = []

  def visitStart(self, ctx:ASLParser.StartContext):
    result = []
    for stmt in ctx.stmt():
      print('Visiting stmt: ', stmt.getText())
      result += self.visit(stmt)
    return result

  def visitStmt(self, ctx:ASLParser.StmtContext):
    if ctx.caseStmt():
      return self.visit(ctx.caseStmt())
    elif ctx.forStmt():
      return self.visit(ctx.forStmt())
    elif ctx.ifStmt():
      return self.visit(ctx.ifStmt())
    elif ctx.simpleStmt():
      return self.visit(ctx.simpleStmt())
    else:
      exit('Unknown stmt: ' + ctx.getText())

  def visitCaseStmt(self, ctx:ASLParser.CaseStmtContext):
    print('Found Case', ctx.getText())

  def visitForStmt(self, ctx:ASLParser.ForStmtContext):
    print('Found For', ctx.getText())

  def visitIfStmt(self, ctx:ASLParser.IfStmtContext):
    result = []
    conditions = ctx.rvalue()
    blocks = ctx.stmtBlock()
    for i in range(len(conditions)):
      result.append(f'Operation* operation{i} = new Operation();')

  def visitStmtBlock(self, ctx:ASLParser.StmtBlockContext):
    return self.visitChildren(ctx)

  def visitDeclarationStatement(self, ctx:ASLParser.DeclarationStatementContext):
    print('Found DeclarationStatement: ', ctx.getText())

  def visitEnumerationStatement(self, ctx:ASLParser.EnumerationStatementContext):
    print('EnumerationStatement: ', ctx.getText())

  def visitAssertStatement(self, ctx:ASLParser.AssertStatementContext):
    print('AssertStatement: ', ctx.getText())

  def visitAssignmentStatement(self, ctx:ASLParser.AssignmentStatementContext):
    print('AssignmentStatement: ', ctx.getText())

  def visitFunctionCallStatement(self, ctx:ASLParser.FunctionCallStatementContext):
    print('FunctionCallStatement: ', ctx.getText())

  def visitUndefinedStatement(self, ctx:ASLParser.UndefinedStatementContext):
    print('UndefinedStatement: ', ctx.getText())

  def visitSeeStatement(self, ctx:ASLParser.SeeStatementContext):
    print('SeeStatement: ', ctx.getText())

  def visitSquareBracketPairValue(self, ctx:ASLParser.SquareBracketPairValueContext):
    print('SquareBracketPairValue: ', ctx.getText())

  def visitIdentifier(self, ctx:ASLParser.IdentifierContext):
    print('Identifier: ', ctx.getText())

  def visitFieldGroupValue(self, ctx:ASLParser.FieldGroupValueContext):
    print('FieldGroupValue: ', ctx.getText())

  def visitLPairValue(self, ctx:ASLParser.LPairValueContext):
    print('LPairValue: ', ctx.getText())

  def visitFieldValue(self, ctx:ASLParser.FieldValueContext):
    print('FieldValue: ', ctx.getText())

  def visitArrayValue(self, ctx:ASLParser.ArrayValueContext):
    print('ArrayValue: ', ctx.getText())

  def visitLSubValue(self, ctx:ASLParser.LSubValueContext):
    print('LSubValue: ', ctx.getText())

  def visitIgnoredValue(self, ctx:ASLParser.IgnoredValueContext):
    print('IgnoredValue: ', ctx.getText())

  def visitShiftValue(self, ctx:ASLParser.ShiftValueContext):
    print('ShiftValue: ', ctx.getText())

  def visitLogicalValue(self, ctx:ASLParser.LogicalValueContext):
    print('LogicalValue: ', ctx.getText())

  def visitDivValue(self, ctx:ASLParser.DivValueContext):
    print('DivValue: ', ctx.getText())

  def visitUnaryValue(self, ctx:ASLParser.UnaryValueContext):
    print('UnaryValue: ', ctx.getText())

  def visitUnknownValue(self, ctx:ASLParser.UnknownValueContext):
    print('UnknownValue: ', ctx.getText())

  def visitRSubValue(self, ctx:ASLParser.RSubValueContext):
    print('RSubValue: ', ctx.getText())

  def visitInValue(self, ctx:ASLParser.InValueContext):
    print('InValue: ', ctx.getText())

  def visitLiteralValue(self, ctx:ASLParser.LiteralValueContext):
    print('LiteralValue: ', ctx.getText())

  def visitNestedValue(self, ctx:ASLParser.NestedValueContext):
    print('NestedValue: ', ctx.getText())

  def visitTernaryValue(self, ctx:ASLParser.TernaryValueContext):
    print('TernaryValue: ', ctx.getText())

  def visitRLValue(self, ctx:ASLParser.RLValueContext):
    print('RLValue: ', ctx.getText())

  def visitMetaLogicalValue(self, ctx:ASLParser.MetaLogicalValueContext):
    print('MetaLogicalValue: ', ctx.getText())

  def visitEqualityValue(self, ctx:ASLParser.EqualityValueContext):
    print('EqualityValue: ', ctx.getText())

  def visitRelationalValue(self, ctx:ASLParser.RelationalValueContext):
    print('RelationalValue: ', ctx.getText())

  def visitArithmeticValue(self, ctx:ASLParser.ArithmeticValueContext):
    print('ArithmeticValue: ', ctx.getText())

  def visitFunctionCallValue(self, ctx:ASLParser.FunctionCallValueContext):
    print('FunctionCallValue: ', ctx.getText())

  def visitConcatValue(self, ctx:ASLParser.ConcatValueContext):
    print('ConcatValue: ', ctx.getText())

  def visitRPairValue(self, ctx:ASLParser.RPairValueContext):
    print('RPairValue: ', ctx.getText())

  def visitFunctionCall(self, ctx:ASLParser.FunctionCallContext):
    print('FunctionCall: ', ctx.getText())

  def visitTypeDecl(self, ctx:ASLParser.TypeDeclContext):
    print('TypeDecl: ', ctx.getText())

  def visitLiteral(self, ctx:ASLParser.LiteralContext):
    print('Literal: ', ctx.getText())

  def generateULDL(self, asl, pattern):
    textstream = InputStream(asl)
    lexer = ASLLexer(textstream)
    tokenstream = CommonTokenStream(lexer)

    self.pattern = pattern

    parser = ASLParser(tokenstream)
    tree = parser.start()
    assert parser.getNumberOfSyntaxErrors() == 0

    instruction = self.visit(tree)
    print(instruction)

    return instruction
