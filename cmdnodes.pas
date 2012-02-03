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
	CmdNodes;

Interface

Uses
	Classes,
	SysUtils,
	BaseException,
	Tree,
	Stacks,
	NameValue,
	XMLNodes,
	XMLLoader,
	ExprNodes;

Type
	TPropertyNode = Class(TExprNode)
	Private
		fName : String;
		fValue : TExpressionNode;
	Public
		Procedure Evaluate; Override;
		Property Name : String Read fName Write fName;
		Property Value : TExpressionNode Read fValue Write fValue;
	End;

	TPropertyListNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
	End;
	
	TNodeNameNode = Class(TExprNode)
	Private
		fName : String;
	Public
		Procedure Evaluate; Override;
		Property Name : String Read fName Write fName;
	End;
	
	TIntrinsicNode = Class(TNodeNameNode)
	Public
		Procedure Evaluate; Override;
	End;
	
	TNumericReferenceNode = Class(TExprNode)
	Private
		fExpression : TExpressionNode;
	Public
		Procedure Evaluate; Override;
		Property Expression : TExpressionNode Read fExpression Write fExpression;
	End;
	
	TExpressionReferenceNode = Class(TExprNode)
	Private
		fExpression : TExpressionNode;
	Public
		Procedure Evaluate; Override;
		Property Expression : TExpressionNode Read fExpression Write fExpression;
	End;

	TDomainNameNode = Class(TExprNode)
	Public
		Procedure Evaluate; OVerride;
	End;
	
	// Basic command structures 
	
	TCommandWithoutParameters = Class(TExprNode);
	
	TCommandWithDOM = Class(TCommandWithoutParameters)
	Private
		fDOM : TDomainNameNode;
	Public
		Property DOM : TDomainNameNode Read fDOM Write fDOM;
	End;

	TCommandWithProperties = Class(TCommandWithoutParameters)
	Private
		fProperties : TPropertyListNode;
	Public
		Property Properties : TPropertyListNode Read fProperties Write fProperties;
	End;
	
	TCommandWithClassNameAndProperties = Class(TCommandWithProperties)
	Private
		fClassName : String;
	Public
		Property NameOfClass : String Read fClassName Write fClassName;
	End;
	
	TCommandWithIdentifier = Class(TCommandWithoutParameters)
	Private
		fProperty : String;
	Public
		Property PropertyName : String Read fProperty Write fProperty;
	End;
	
	TCommandWithExpression = Class(TCommandWithoutParameters)
	Private
		fExpression : TExpressionNode;
	Public
		Property Expression : TExpressionNode Read fExpression Write fExpression;
	End;
	
	TCommandWithExpressionList = Class(TCommandWithoutParameters)
	Private
		fExpressionList : TExpressionListNode;
	Public
		Property ExpressionList : TExpressionListNode Read fExpressionList Write fExpressionList;
	End;

	TFocusCommand = Class(TCommandWithDOM)
	Public
		Procedure Evaluate; Override;
	End;

	// Command factory related

	TCommandKind = (ckCommandWithoutParameters, ckCommandWithDOM, ckCommandWithProperties,
		ckCommandWithClassNameAndProperties, ckCommandWithIdentifier, ckCommandWithExpression,
		ckCommandWithExpressionList, ckUnknownCommand);

	TCommandFactory = Class(TTreeClassFactory)
	Public
		Function CommandKind(Const aName : String): TCommandKind;
		Function Build(Const aClassName : String; Const aOwner : TCommandWithoutParameters): TCommandWithoutParameters; Overload;
	End;
	
Implementation

// TPropertyNode

Procedure TPropertyNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	Try
		Stack.Enter(0);
		fValue.Evaluate;
		Stack.NameValueStack.Top.SetValue(fName, Stack.Top.Pop);
		Stack.Leave(0);
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

// TPropertyListNode

Procedure TPropertyListNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	Try
		Stack.NameValueStack.Enter;
		EvaluateChilds;
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

// TNodeNameNode

Procedure TNodeNameNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	Try
		Stack.Focused := Stack.Focused.Find(Name) As TXMLNode;
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

// TIntrinsicNode
	
Procedure TIntrinsicNode.Evaluate;
Var
	lName : String;
Begin
	// Debug  WriteLn(Self.ClassName);
	Try
		lName := UpperCase(Name);
		If lName = 'ROOT' Then
			Stack.Focused := Stack.Focused.FindRoot As TXMLNode
		Else If lName = 'FIRST' Then 
			Stack.Focused := Stack.Focused.GetFirst As TXMLNode
		Else If lName = 'CURRENT' Then
			Stack.Focused := Stack.Focused.GetCurrent As TXMLNode
		Else If lName = 'NEXT' Then 
			Stack.Focused := Stack.Focused.GetNext As TXMLNode
		Else If lName = 'PREVIOUS' Then
			Stack.Focused := Stack.Focused.GetPrevious As TXMLNode
		Else If lName = 'LAST' Then
			Stack.Focused := Stack.Focused.GetLast As TXMLNode
		Else If lName = 'CURINC' Then
			Stack.Focused := Stack.Focused.GetCurrentAndIncrement As TXMLNode
		Else If lName = 'CURDEC' Then
			Stack.Focused := Stack.Focused.GetCurrentAndDecrement As TXMLNode
		Else If lName = 'OWNER' Then
			Stack.Focused := Stack.Focused.Owner As TXMLNode
		Else
			RaiseError('Unknown intrinsics : ' + Name);
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

// TNumericReferenceNode
	
Procedure TNumericReferenceNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	Try
		Stack.Enter(0);
		fExpression.Evaluate;
		Stack.Focused := Stack.Focused.GetChildByNumber(Stack.Top.Pop) As TXMLNode;
		Stack.Leave(0);
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

// TExpressionReferenceNode

Procedure TExpressionReferenceNode.Evaluate;
Var
	lFound : Boolean;
Begin
	// Debug  WriteLn(Self.ClassName);
	Try
		lFound := False;
		Stack.Focused.First;
		While Not Stack.Focused.IsAfterLast Do
		Begin
			fExpression.Evaluate;
			If Stack.Top.Pop Then
			Begin
				lFound := True;
				Stack.Focused := Stack.Focused.GetCurrent As TXMLNode;
				Break;
			End;
			Stack.Focused.Next;
		End;
		If Not lFound Then
			RaiseError('Cant find the node especified by the expression.');
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

// TDomainNameNode

Procedure TDomainNameNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	Try
		EvaluateChilds;
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

// TFocusCommand

Procedure TFocusCommand.Evaluate;
Begin
	Dom.Evaluate;
End;

// TCommandFactory

Function TCommandFactory.CommandKind(Const aName : String): TCommandKind;
Var
	lIndex     : Integer;
	lClass     : TTreeNodeClass;
Begin
	lIndex := RegisteredClasses.FindIndexOf(aName);
	// Debug  WriteLn('lIndex ', lIndex);
	If lIndex >= 0 Then
		lClass := TTreeNodeClass(RegisteredClasses.Items[lIndex])
	Else
		lClass := DefaultClass;
	// Debug  WriteLn('lClass ', lClass.ClassName);
	Result := ckUnknownCommand;
	If lClass.InheritsFrom(TCommandWithoutParameters) Then
		Result := ckCommandWithoutParameters;
	If lClass.InheritsFrom(TCommandWithDOM) Then 
		Result := ckCommandWithDOM;
	If lClass.InheritsFrom(TCommandWithProperties) Then 
		Result := ckCommandWithProperties;
	If lClass.InheritsFrom(TCommandWithClassNameAndProperties) Then 
		Result := ckCommandWithClassNameAndProperties;
	If lClass.InheritsFrom(TCommandWithIdentifier) Then 
		Result := ckCommandWithIdentifier;
	If lClass.InheritsFrom(TCommandWithExpression) Then 
		Result := ckCommandWithExpression;
	If lClass.InheritsFrom(TCommandWithExpressionList) Then 
		Result := ckCommandWithExpressionList;
End;

Function TCommandFactory.Build(Const aClassName : String; Const aOwner : TCommandWithoutParameters): TCommandWithoutParameters;
Begin
	Result := Build(aClassName, aOwner As TTreeNode) As TCommandWithoutParameters;
End;

End.