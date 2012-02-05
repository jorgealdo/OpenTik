{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 2012 - J. Aldo G. de Freitas Junior

{$mode objfpc}
{$H+}

Unit
	ExprParser;
	
Interface

Uses
	Classes,
	SysUtils,
	BaseException,
	ExprScanner,
	ExprNodes;

Type
	TExprParser = Class
	Private
		fOwnsSource : Boolean;
		fSource : TExprTokenIterator;
		fDestination : TExprNode;
	Public
		Property Source : TExprTokenIterator Read fSource;
		Constructor Create(Const aSource : TExprTokenIterator; Const aDestination : TExprNode; Const aOwnsSource : Boolean = True); Virtual;
		Destructor Destroy; Override;
		Procedure Mark(Const aNode : TExprNode);
		Function ParseUnaryMinus(Const aPrevious : TExprNode): TExprNode;
		Function ParseUnaryNot(Const aPrevious : TExprNode): TExprNode;
		Function ParseFunctionCall(Const aPrevious : TExprNode): TExprNode;
		Function ParseStringLiteral(Const aPrevious : TExprNode): TExprNode;
		Function ParseNumberLiteral(Const aPrevious : TExprNode): TExprNode;
		Function ParseFloatLiteral(Const aPrevious : TExprNode): TExprNode;
		Function ParseLiteral(Const aPrevious : TExprNode): TExprNode;
		Function ParseTerm(Const aPrevious : TExprNode): TExprNode;
		Function ParseMulExpression(Const aPrevious : TExprNode): TExprNode;
		Function ParseAddExpression(Const aPrevious : TExprNode): TExprNode;
		Function ParseExpression(Const aPrevious : TExprNode): TExprNode;
		Function ParseExpressionList(Const aPrevious : TExprNode): TExprNode;
		Function ParseParameters: TExprNode;
		Function ParseSingleExpression: TExprNode;
		Property Destination : TExprNode Read fDestination Write fDestination;
	End;

Implementation

Constructor TExprParser.Create(Const aSource : TExprTokenIterator; Const aDestination : TExprNode; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fOwnsSource := aOwnsSource;
	fSource := aSource;
	fDestination := aDestination;
End;

Destructor TExprParser.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	Inherited Destroy;
End;

Procedure TExprParser.Mark(Const aNode : TExprNode);
Begin
	aNode.Row := fSource.Row;
	aNode.Col := fSource.Col;
End;

Function TExprParser.ParseUnaryMinus(Const aPrevious : TExprNode): TExprNode;
Var
	lUnaryMinus : TUnaryMinusNode;
Begin
	// Debug WriteLn('ParseUnaryMinus');
	lUnaryMinus := TUnaryMinusNode.Create(aPrevious);
	Mark(lUnaryMinus);
	fSource.Consume('-');
	ParseTerm(lUnaryMinus);
	Result := lUnaryMinus;
End;

Function TExprParser.ParseUnaryNot(Const aPrevious : TExprNode): TExprNode;
Var
	lUnaryNot : TUnaryNotNode;
Begin
	// Debug WriteLn('ParseUnaryNot');
	lUnaryNot := TUnaryNotNode.Create(aPrevious);
	Mark(lUnaryNot);
	fSource.Consume('not');
	ParseTerm(lUnaryNot);
	Result := lUnaryNot;
End;

Function TExprParser.ParseFunctionCall(Const aPrevious : TExprNode): TExprNode;
Var
	lFunctionCall : TFunctionCallNode;
Begin
	// Debug WriteLn('ParseFunctionCall');
	lFunctionCall := TFunctionCallNode.Create(aPrevious);
	Mark(lFunctionCall);
	lFunctionCall.Name := fSource.Extract(tkExprIdentifier);
	If fSource.Consume('(', False) Then
	Begin
		If Not(fSource.Expected(')')) Then
			lFunctionCall.Parameters := ParseExpressionList(lFunctionCall) As TExpressionListNode;
		fSource.Consume(')');
	End
	Else
		lFunctionCall.Parameters := Nil;
	Result := lFunctionCall;
End;

Function TExprParser.ParseStringLiteral(Const aPrevious : TExprNode): TExprNode;
Var
	lStringLiteral : TStringLiteralNode;
Begin
	// Debug WriteLn('ParseStringLiteral');
	lStringLiteral := TStringLiteralNode.Create(aPrevious);
	Mark(lStringLiteral);
	lStringLiteral.Value := fSource.Extract(tkExprLiteralString);
	// Debug WriteLn(lStringLiteral.Value);
	Result := lStringLiteral;
End;

Function TExprParser.ParseNumberLiteral(Const aPrevious : TExprNode): TExprNode;
Var
	lNumberLiteral : TNumberLiteralNode;
Begin
	// Debug WriteLn('ParseNumberLiteral');
	lNumberLiteral := TNumberLiteralNode.Create(aPrevious);
	lNumberLiteral.IsFloat := False;
	Mark(lNumberLiteral);
	lNumberLiteral.Value := fSource.Extract(tkExprLiteralNumber);
	// Debug WriteLn(lNumberLiteral.Value);
	Result := lNumberLiteral;
End;

Function TExprParser.ParseNumberLiteral(Const aPrevious : TExprNode): TExprNode;
Var
	lNumberLiteral : TNumberLiteralNode;
Begin
	// Debug WriteLn('ParseNumberLiteral');
	lNumberLiteral := TNumberLiteralNode.Create(aPrevious);
	lNumberLiteral.IsFloat := True;
	Mark(lNumberLiteral);
	lNumberLiteral.Value := fSource.Extract(tkExprLiteralNumber);
	// Debug WriteLn(lNumberLiteral.Value);
	Result := lNumberLiteral;
End;

Function TExprParser.ParseLiteral(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug WriteLn('ParseLiteral');
	If fSource.Expected(tkExprLiteralString) Then
		Result := ParseStringLiteral(aPrevious)
	Else If fSource.Expected(tkExprLiteralNumber) Then
		Result := ParseNumberLiteral(aPrevious)
	Else
		fSource.RaiseError('Expected string or number, got "' + fSource.Literal + '"');
End;

Function TExprParser.ParseTerm(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug WriteLn('ParseTerm');
	If fSource.Expected('-') Then
		Result := ParseUnaryMinus(aPrevious)
	Else If fSource.Expected('not') Then
		Result := ParseUnaryNot(aPrevious)
	Else If fSource.Expected(tkExprIdentifier) Then
		Result := ParseFunctionCall(aPrevious)
	Else If fSource.Expected('(') Then
	Begin
		fSource.Consume('(');
		Result := ParseExpression(aPrevious);
		fSource.Consume(')');
	End
	Else
		Result := ParseLiteral(aPrevious);
End;

Function TExprParser.ParseMulExpression(Const aPrevious : TExprNode): TExprNode;
Var
	lMul : TMulExpressionNode;
Begin
	// Debug WriteLn('ParseMulExpression');
	lMul := TMulExpressionNode.Create(aPrevious);
	lMul.Operation := '';
	ParseTerm(lMul);
	If fSource.Expected(['*', '/', 'div', 'mod', 'and', '^']) Then
	Begin
		Mark(lMul);
		lMul.Operation := fSource.Extract(tkExprWord);
		ParseMulExpression(lMul);
	End;
	Result := lMul;
End;

Function TExprParser.ParseAddExpression(Const aPrevious : TExprNode): TExprNode;
Var
	lAdd : TAddExpressionNode;
Begin
	// Debug WriteLn('ParseAddExpression');
	lAdd := TAddExpressionNode.Create(aPrevious);
	lAdd.Operation := '';
	ParseMulExpression(lAdd);
	If fSource.Expected(['+', '-', 'or', 'xor']) Then
	Begin
		Mark(lAdd);
		lAdd.Operation := fSource.Extract(tkExprWord);
		ParseAddExpression(lAdd);
	End;
	Result := lAdd;
End;

Function TExprParser.ParseExpression(Const aPrevious : TExprNode): TExprNode;
Var
	lCompare : TExpressionNode;
Begin
	// Debug WriteLn('ParseExpression');
	lCompare := TExpressionNode.Create(aPrevious);
	lCompare.Operation := '';
	ParseAddExpression(lCompare);
	If fSource.Expected(['=', '<', '>', '<=', '>=', '<>']) Then
	Begin
		Mark(lCompare);
		lCompare.Operation := fSource.Extract(tkExprWord);
		ParseAddExpression(lCompare);
	End;
	Result := lCompare;
End;

Function TExprParser.ParseExpressionList(Const aPrevious : TExprNode): TExprNode;
Var
	lExpressionList : TExpressionListNode;
Begin
	// Debug WriteLn('ParseExpressionList');
	lExpressionList := TExpressionListNode.Create(aPrevious);
	Mark(lExpressionList);
	Repeat
		ParseExpression(lExpressionList);	
	Until Not(fSource.Consume(',', False));
	Result := lExpressionList;
End;

Function TExprParser.ParseParameters: TExprNode;
Begin
	// Debug WriteLn('ParseSource');
	Repeat
		ParseExpression(fDestination);
	Until Not(fSource.Consume(',', False));
	fSource.Consume(tkExprEOF);
	Result := fDestination;
End;

Function TExprParser.ParseSingleExpression: TExprNode;
Begin
	// Debug WriteLn('ParseSource');
	ParseExpression(fDestination);
	fSource.Consume(tkExprEOF);
	Result := fDestination;
End;

End.
