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
	CmdParser;

Interface

Uses
	Classes,
	ExprScanner,
	ExprNodes,
	ExprParser,
	CmdNodes;

Type
	TCommandParser = Class(TExprParser)
	Private
		fClassFactory : TCommandFactory;
	Public
		Function ParseProperty(Const aPrevious : TExprNode): TExprNode;
		Function ParsePropertyList(Const aPrevious : TExprNode): TExprNode;

		Function ParseNodeName(Const aPrevious : TExprNode): TExprNode;
		Function ParseIntrinsic(Const aPrevious : TExprNode): TExprNode;
		Function ParseNumericNodeReference(Const aPrevious : TExprNode): TExprNode;
		Function ParseExpressionNodeReference(Const aPrevious : TExprNode): TExprNode;
		Function ParseNodeReference(Const aPrevious : TExprNode): TExprNode;
		Function ParseDOM(Const aPrevious : TExprNode): TExprNode;

		Function ParseFocusCommand(Const aPrevious : TExprNode): TExprNode;
		Function ParseCommandWithoutParameters(Const aPrevious : TExprNode): TExprNode;
		Function ParseCommandWithDOM(Const aPrevious : TExprNode): TExprNode;
		Function ParseCommandWithProperties(Const aPrevious : TExprNode): TExprNode;
		Function ParseCommandWithClassNameAndProperties(Const aPrevious : TExprNode): TExprNode;
		Function ParseCommandWithIdentifier(Const aPrevious : TExprNode): TExprNode;
		Function ParseCommandWithExpression(Const aPrevious : TExprNode): TExprNode;
		Function ParseCommandWithExpressionList(Const aPrevious : TExprNode): TExprNode;
		Function ParseCommand(Const aPrevious : TExprNode): TExprNode;

		Function ParseInputCommand: TExprNode;
		Function ParseDOMExpression: TExprNode;

		Property ClassFactory : TCommandFactory Read fClassFactory Write fClassFactory;
	End;

Implementation

Function TCommandParser.ParseProperty(Const aPrevious : TExprNode): TExprNode;
Var
	lProperty : TPropertyNode;
Begin
	// Debug  WriteLn('ParseProperty');
	lProperty := TPropertyNode.Create(aPrevious);
	Mark(lProperty);
	lProperty.Name := Source.Extract(tkExprIdentifier);
	Source.Consume('=');
	lProperty.Value := ParseExpression(lProperty) As TExpressionNode;
	Result := lProperty;
End;

Function TCommandParser.ParsePropertyList(Const aPrevious : TExprNode): TExprNode;
Var
	lPropertyList : TPropertyListNode;
Begin
	// Debug  WriteLn('ParsePropertyList');
	lPropertyList := TPropertyListNode.Create(aPrevious);
	Mark(lPropertyList);
	Repeat
		ParseProperty(lPropertyList);
	Until Source.Consume(tkExprEOF, False) Or Source.Consume(';', False);
	Result := lPropertyList;
End;

Function TCommandParser.ParseNodeName(Const aPrevious : TExprNode): TExprNode;
Var
	lNodeName : TNodeNameNode;
Begin
	// Debug  WriteLn('ParseNodeName');	
	lNodeName := TNodeNameNode.Create(aPrevious);
	Mark(lNodeName);
	lNodeName.Name := Source.Extract(tkExprIdentifier);
	Result := lNodeName;
End;

Function TCommandParser.ParseIntrinsic(Const aPrevious : TExprNode): TExprNode;
Var
	lIntrinsicNode : TIntrinsicNode;
Begin
	// Debug  WriteLn('ParseIntrinsic');	
	lIntrinsicNode := TIntrinsicNode.Create(aPrevious);
	Mark(lIntrinsicNode);
	Source.Consume('$');
	lIntrinsicNode.Name := Source.Extract(tkExprIdentifier);
	Source.Consume('$');
	Result := lIntrinsicNode;
End;

Function TCommandParser.ParseNumericNodeReference(Const aPrevious : TExprNode): TExprNode;
Var
	lNumericReference : TNumericReferenceNode;
Begin
	// Debug  WriteLn('ParseNumericNodeReference');	
	lNumericReference := TNumericReferenceNode.Create(aPrevious);
	Mark(lNumericReference);
	Source.Consume('[');
	lNumericReference.Expression := ParseExpression(lNumericReference) As TExpressionNode;
	Source.Consume(']');
	Result := lNumericReference;
End;

Function TCommandParser.ParseExpressionNodeReference(Const aPrevious : TExprNode): TExprNode;
Var
	lExpressionReference : TExpressionReferenceNode;
Begin
	// Debug  WriteLn('ParseExpressionNodeReference');
	lExpressionReference := TExpressionReferenceNode.Create(aPrevious);
	Mark(lExpressionReference);
	Source.Consume('|');
	If Not Source.Expected('|') Then
		lExpressionReference.Expression := ParseExpression(lExpressionReference) As TExpressionNode
	Else
		Source.RaiseError('Expected evaluation expression.');
	Source.Consume('|');
	Result := lExpressionReference;
End;

Function TCommandParser.ParseNodeReference(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseNodeReference');
	If Source.Expected('$') Then
		Result := ParseIntrinsic(aPrevious)
	Else If Source.Expected('[') Then
		Result := ParseNumericNodeReference(aPrevious)
	Else If Source.Expected('|') Then
		Result := ParseExpressionNodeReference(aPrevious)
	Else
		Result := ParseNodeName(aPrevious);
End;

Function TCommandParser.ParseDOM(Const aPrevious : TExprNode): TExprNode;
Var
	lDOM : TDomainNameNode;
Begin
	// Debug  WriteLn('ParseDOM');
	lDOM := TDomainNameNode.Create(aPrevious);
	Mark(lDOM);
	Repeat
		ParseNodeReference(lDOM);
	Until Not Source.Consume('.', False);
	Result := lDOM;
End;

Function TCommandParser.ParseFocusCommand(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseFocusCommand');
	Result := TFocusCommand.Create(aPrevious) As TExprNode;
	Source.Consume('/');
	(Result As TCommandWithDOM).DOM := ParseDOM(Result) As TDomainNameNode;
End;

Function TCommandParser.ParseCommandWithoutParameters(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseCommandWithoutParameters');
	Result := fClassFactory.Build(Source.Extract(tkExprIdentifier), aPrevious) As TExprNode;
End;

Function TCommandParser.ParseCommandWithDOM(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseCommandWithDOM');
	Result := fClassFactory.Build(Source.Extract(tkExprIdentifier), aPrevious) As TExprNode;
	(Result As TCommandWithDOM).DOM := ParseDOM(Result) As TDomainNameNode;
End;

Function TCommandParser.ParseCommandWithProperties(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseCommandWithProperties');
	Result := fClassFactory.Build(Source.Extract(tkExprIdentifier), aPrevious) As TExprNode;
	(Result As TCommandWithProperties).Properties := ParsePropertyList(Result) As TPropertyListNode;
End;

Function TCommandParser.ParseCommandWithClassNameAndProperties(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseCommandWithClassNameAndProperties');
	Result := fClassFactory.Build(Source.Extract(tkExprIdentifier), aPrevious) As TExprNode;
	(Result As TCommandWithClassNameAndProperties).NameOfClass := Source.Extract(tkExprIdentifier);
	(Result As TCommandWithClassNameAndProperties).Properties := ParsePropertyList(Result) As TPropertyListNode;
End;

Function TCommandParser.ParseCommandWithIdentifier(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseCommandWithIdentifier');
	Result := fClassFactory.Build(Source.Extract(tkExprIdentifier), aPrevious) As TExprNode;
	(Result As TCommandWithIdentifier).PropertyName := Source.Extract(tkExprIdentifier);
End;

Function TCommandParser.ParseCommandWithExpression(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseCommandWithExpression');
	Result := fClassFactory.Build(Source.Extract(tkExprIdentifier), aPrevious) As TExprNode;
	(Result As TCommandWithExpression).Expression := ParseExpression(Result) As TExpressionNode;
End;

Function TCommandParser.ParseCommandWithExpressionList(Const aPrevious : TExprNode): TExprNode;
Begin
	// Debug  WriteLn('ParseCommandWithExpressionList');
	Result := fClassFactory.Build(Source.Extract(tkExprIdentifier), aPrevious) As TExprNode;
	(Result As TCommandWithExpressionList).ExpressionList := ParseExpressionList(Result) As TExpressionListNode;
End;

Function TCommandParser.ParseCommand(Const aPrevious : TExprNode): TExprNode;
Var
	lKind : TCommandKind;
Begin
	If Source.Literal = '/' Then
		Result := ParseFocusCommand(aPrevious)
	Else
	Begin
		// Debug  WriteLn('ParseCommand');
		lKind := fClassFactory.CommandKind(Source.Literal);
		// Debug  WriteLn(Source.Literal, '=', lKind);
		If lKind = ckCommandWithoutParameters Then
			Result := ParseCommandWithoutParameters(aPrevious)
		Else If lKind = ckCommandWithDOM Then
			Result := ParseCommandWithDOM(aPrevious)
		Else If lKind = ckCommandWithProperties Then
			Result := ParseCommandWithProperties(aPrevious) 
		Else If lKind = ckCommandWithClassNameAndProperties Then
			Result := ParseCommandWithClassNameAndProperties(aPrevious)
		Else If lKind = ckCommandWithIdentifier Then
			Result := ParseCommandWithIdentifier(aPrevious)
		Else If lKind = ckCommandWithExpression Then
			Result := ParseCommandWithExpression(aPrevious)
		Else If lKind = ckCommandWithExpressionList Then
			Result := ParseCommandWithExpressionList(aPrevious)
		Else
		Begin
			Result := Nil;
			Source.RaiseError('Unknown command, "' + Source.Literal + '".');
		End;
	End;
End;

Function TCommandParser.ParseInputCommand: TExprNode;
Begin
	// Debug  WriteLn('ParseInputCommand');
	Result := ParseCommand(Destination);
End;

Function TCommandParser.ParseDOMExpression: TExprNode;
Begin
	Result := ParseDOM(Destination);
End;

End.