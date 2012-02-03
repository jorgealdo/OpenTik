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
	ExprNodes;

Interface

Uses
	Classes,
	SysUtils,
	BaseException,
	Tree,
	Stacks;

Type
	TExprNode = Class;
	TExprNodeClass = Class Of TExprNode;
	TExprNodeList = Array Of TExprNode;

	TExpressionException = Class(TRowColException);

	TExprNode = Class(TTreeNode)
	Private
		fRow, fCol : Integer;
		fStack : TVirtualMachineStack;
	Public
		Procedure RaiseError(Const aMsg : String);
		Procedure EvaluateChilds; Virtual;
		Procedure Evaluate; Virtual;
		Procedure PropagateStack(Const aStack : TVirtualMachineStack);
		Property Row : Integer Read fRow Write fRow;
		Property Col : Integer Read fCol Write fCol;
		Property Stack : TVirtualMachineStack Read fStack Write fStack;
	End;

	TUnaryMinusNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
	End;

	TUnaryNotNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
	End;

	TExpressionListNode = Class;

	TFunctionCallNode = Class(TExprNode)
	Private
		fName : String;
		fParameters : TExpressionListNode;
	Public
		Procedure Evaluate; Override;
		Property Name : String Read fName Write fName;
		Property Parameters : TExpressionListNode Read fParameters Write fParameters;
	End;

	TStringLiteralNode = Class(TExprNode)
	Private
		fValue : String;
	Public
		Procedure Evaluate; Override;
		Property Value : String Read fValue Write fValue;
	End;

	TNumberLiteralNode = Class(TExprNode)
	Private
		fValue : Variant;
	Public
		Procedure Evaluate; Override;
		Property Value : Variant Read fValue Write fValue;
	End;

	TMulExpressionNode = Class(TExprNode)
	Private
		fOperation : String;
	Public
		Procedure Evaluate; Override;
		Property Operation : String Read fOperation Write fOperation;
	End;

	TAddExpressionNode = Class(TExprNode)
	Private
		fOperation : String;
	Public
		Procedure Evaluate; Override;
		Property Operation : String Read fOperation Write fOperation;
	End;

	TExpressionNode = Class(TExprNode)
	Private
		fOperation : String;
	Public
		Procedure Evaluate; Override;
		Property Operation : String Read fOperation Write fOperation;
	End;

	TExpressionListNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
	End;

Implementation

// TExprNode

Procedure TExprNode.RaiseError(Const aMsg : String);
Var
	lException : TExpressionException;
Begin
	lException := TExpressionException.Create(aMsg);
	lException.Row := fRow;
	lException.Col := fCol;
	Raise lException;
End;

Procedure TExprNode.EvaluateChilds;
Begin
	First;
	While Not IsAfterLast Do
	Begin
		// Debug  WriteLn('TExprNode.EvaluateChilds ', GetCurrent.ClassName);
		(GetCurrent As TExprNode).Evaluate;
		Next;
	End;
End;

Procedure TExprNode.PropagateStack(Const aStack : TVirtualMachineStack);
Begin
	fStack := aStack;
	First;
	While Not IsAfterLast Do
	Begin
		(GetCurrent As TExprNode).PropagateStack(aStack);
		Next;
	End;
End;

Procedure TExprNode.Evaluate;
Begin
	// Debug  WriteLn('TExprNode.Evaluate ', Self.ClassName);
	EvaluateChilds;
End;

{ TUnaryMinusNode }

Procedure TUnaryMinusNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	EvaluateChilds;
	Stack.Top.Push(-1);
	Try
		Stack.DoMul;
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

{ TUnaryNotNode }

Procedure TUnaryNotNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	Try
		Stack.DoNot;
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

{ TFunctionCallNode }

Procedure TFunctionCallNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	EvaluateChilds;
	Try
		If Assigned(fParameters) Then
			Stack.Enter(Length(fParameters.Childs))
		Else
			Stack.Enter(0);
		Stack.Call(fName);
		Stack.Leave(1);
	Except
		On E: Exception Do
			RaiseError(E.Message);
	End;
End;

{ TStringLiteralNode }

Procedure TStringLiteralNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	Stack.Top.Push(fValue);
End;

{ TNumberLiteralNode }

Procedure TNumberLiteralNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	Stack.Top.Push(fValue);
End;

{ TMulExpressionNode }

Procedure TMulExpressionNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	EvaluateChilds;
	If fOperation = '*' Then
		Try
			Stack.DoMul;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '/' Then
		Try
			Stack.DoDiv;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'div' Then
		Try
			Stack.DoIDiv;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'mod' Then
		Try
			Stack.DoMod;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'and' Then
		Try
			Stack.DoAnd;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '^' Then
		Try
			Stack.DoPow;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End;
End;

{ TAddExpressionNode }

Procedure TAddExpressionNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	EvaluateChilds;
	If fOperation = '+' Then
		Try
			Stack.DoAdd;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '-' Then
		Try
			Stack.DoSub;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'or' Then
		Try
			Stack.DoOr;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = 'xor' Then
		Try
			Stack.DoXOr;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End;
End;

{ TExpressionNode }

Procedure TExpressionNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	EvaluateChilds;
	If fOperation = '=' Then
		Try
			Stack.DoCmpEq;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '<' Then
		Try
			Stack.DoCmpSm;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '>' Then
		Try
			Stack.DoCmpGt;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '<>' Then
		Try
			Stack.DoCmpDif;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '<=' Then
		Try
			Stack.DoCmpEqSm;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End
	Else If fOperation = '>=' Then
		Try
			Stack.DoCmpGtSm;
		Except
			On E: Exception Do
				RaiseError(E.Message);
		End;
End;

{ TExpressionListNode }

Procedure TExpressionListNode.Evaluate;
Begin
	// Debug  WriteLn(Self.ClassName);
	EvaluateChilds;
End;

End.