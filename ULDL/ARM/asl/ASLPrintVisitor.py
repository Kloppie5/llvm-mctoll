from antlr4 import *
from ASLLexer import ASLLexer
from ASLParser import ASLParser
from ASLVisitor import ASLVisitor
from antlr4.tree.Trees import Trees
import sys

class ASLPrintVisitor(ASLVisitor):

  def visitStart(self, ctx:ASLParser.StartContext):
    print('Start: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitStmt(self, ctx:ASLParser.StmtContext):
    print('Stmt: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitCaseStmt(self, ctx:ASLParser.CaseStmtContext):
    print('CaseStmt: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitForStmt(self, ctx:ASLParser.ForStmtContext):
    print('ForStmt: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitIfStmt(self, ctx:ASLParser.IfStmtContext):
    print('IfStmt: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitStmtBlock(self, ctx:ASLParser.StmtBlockContext):
    print('StmtBlock: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitDeclarationStatement(self, ctx:ASLParser.DeclarationStatementContext):
    print('DeclarationStatement: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitEnumerationStatement(self, ctx:ASLParser.EnumerationStatementContext):
    print('EnumerationStatement: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitAssertStatement(self, ctx:ASLParser.AssertStatementContext):
    print('AssertStatement: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitAssignmentStatement(self, ctx:ASLParser.AssignmentStatementContext):
    print('AssignmentStatement: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitFunctionCallStatement(self, ctx:ASLParser.FunctionCallStatementContext):
    print('FunctionCallStatement: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitUndefinedStatement(self, ctx:ASLParser.UndefinedStatementContext):
    print('UndefinedStatement: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitSeeStatement(self, ctx:ASLParser.SeeStatementContext):
    print('SeeStatement: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitSquareBracketPairValue(self, ctx:ASLParser.SquareBracketPairValueContext):
    print('SquareBracketPairValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitIdentifier(self, ctx:ASLParser.IdentifierContext):
    print('Identifier: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitFieldGroupValue(self, ctx:ASLParser.FieldGroupValueContext):
    print('FieldGroupValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitLPairValue(self, ctx:ASLParser.LPairValueContext):
    print('LPairValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitFieldValue(self, ctx:ASLParser.FieldValueContext):
    print('FieldValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitArrayValue(self, ctx:ASLParser.ArrayValueContext):
    print('ArrayValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitLSubValue(self, ctx:ASLParser.LSubValueContext):
    print('LSubValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitIgnoredValue(self, ctx:ASLParser.IgnoredValueContext):
    print('IgnoredValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitShiftValue(self, ctx:ASLParser.ShiftValueContext):
    print('ShiftValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitLogicalValue(self, ctx:ASLParser.LogicalValueContext):
    print('LogicalValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitDivValue(self, ctx:ASLParser.DivValueContext):
    print('DivValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitUnaryValue(self, ctx:ASLParser.UnaryValueContext):
    print('UnaryValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitUnknownValue(self, ctx:ASLParser.UnknownValueContext):
    print('UnknownValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitRSubValue(self, ctx:ASLParser.RSubValueContext):
    print('RSubValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitInValue(self, ctx:ASLParser.InValueContext):
    print('InValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitLiteralValue(self, ctx:ASLParser.LiteralValueContext):
    print('LiteralValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitNestedValue(self, ctx:ASLParser.NestedValueContext):
    print('NestedValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitTernaryValue(self, ctx:ASLParser.TernaryValueContext):
    print('TernaryValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitRLValue(self, ctx:ASLParser.RLValueContext):
    print('RLValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitMetaLogicalValue(self, ctx:ASLParser.MetaLogicalValueContext):
    print('MetaLogicalValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitEqualityValue(self, ctx:ASLParser.EqualityValueContext):
    print('EqualityValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitRelationalValue(self, ctx:ASLParser.RelationalValueContext):
    print('RelationalValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitArithmeticValue(self, ctx:ASLParser.ArithmeticValueContext):
    print('ArithmeticValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitFunctionCallValue(self, ctx:ASLParser.FunctionCallValueContext):
    print('FunctionCallValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitConcatValue(self, ctx:ASLParser.ConcatValueContext):
    print('ConcatValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitRPairValue(self, ctx:ASLParser.RPairValueContext):
    print('RPairValue: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitFunctionCall(self, ctx:ASLParser.FunctionCallContext):
    print('FunctionCall: ', ctx.getText())
    return self.visitChildren(ctx)

  def visitTypeDecl(self, ctx:ASLParser.TypeDeclContext):
    print('TypeDecl: ', ctx.getText())
    return self.visitChildren(ctx)


  def visitLiteral(self, ctx:ASLParser.LiteralContext):
    print('Literal: ', ctx.getText())
    return self.visitChildren(ctx)

  def printASL(self, asl):
    input = InputStream(asl)
    lexer = ASLLexer(input)
    stream = CommonTokenStream(lexer)
    parser = ASLParser(stream)
    tree = parser.start()
    self.visit(tree)
    return parser.getNumberOfSyntaxErrors()
