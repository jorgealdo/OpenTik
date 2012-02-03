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
	ExprLoader;

Interface

Uses
	Classes,
	SysUtils,
	Tree,
	Stacks,
	ExprScanner,
	ExprParser,
	ExprNodes;

Type
	TExprRootNode = Class(TExprNode)
	Public
		Procedure Evaluate; Override;
		Procedure DoLoadExpression(Const aExpression : String; Const aStack : TVirtualMachineStack);
		Procedure DoLoadParameters(Const aParameters : String; Const aStack : TVirtualMachineStack);
	End;

Implementation

Procedure TExprRootNode.Evaluate;
Begin
	EvaluateChilds;
End;

Procedure TExprRootNode.DoLoadExpression(Const aExpression : String; Const aStack : TVirtualMachineStack);
Var
	lFile : TStringStream;
	lSource : TExprSource;
	lTokens : TMemoryStream;
	lScanner : TExprScanner;
	lTokenIterator : TExprTokenIterator;
	lParser : TExprParser;
Begin
	lFile := Nil;
	lSource := Nil;
	lTokens := Nil;
	lScanner := Nil;
	lTokenIterator := Nil;
	lParser := Nil;
	Try
		lFile := TStringStream.Create(aExpression);
		lSource := TExprSource.Create(lFile, False);
		lTokens := TMemoryStream.Create;
		lScanner := TExprScanner.Create(lSource, lTokens, False);
		lScanner.Scan;
		lTokenIterator := TExprTokenIterator.Create(lTokens, False);
		lParser := TExprParser.Create(lTokenIterator, Self, False);
		lParser.ParseSingleExpression;
		PropagateStack(aStack);
	Finally
		FreeAndNil(lFile);
		FreeAndNil(lSource);
		FreeAndNil(lTokens);
		FreeAndNil(lScanner);
		FreeAndNil(lTokenIterator);
		FreeAndNil(lParser);
	End;
End;

Procedure TExprRootNode.DoLoadParameters(Const aParameters : String; Const aStack : TVirtualMachineStack);
Var
	lFile : TStringStream;
	lSource : TExprSource;
	lTokens : TMemoryStream;
	lScanner : TExprScanner;
	lTokenIterator : TExprTokenIterator;
	lParser : TExprParser;
Begin
	lFile := Nil;
	lSource := Nil;
	lTokens := Nil;
	lScanner := Nil;
	lTokenIterator := Nil;
	lParser := Nil;
	Try
		lFile := TStringStream.Create(aParameters);
		lSource := TExprSource.Create(lFile, False);
		lTokens := TMemoryStream.Create;
		lScanner := TExprScanner.Create(lSource, lTokens, False);
		lScanner.Scan;
		lTokenIterator := TExprTokenIterator.Create(lTokens, False);
		lParser := TExprParser.Create(lTokenIterator, Self, False);
		lParser.ParseParameters;
		PropagateStack(aStack);
	Finally
		FreeAndNil(lFile);
		FreeAndNil(lSource);
		FreeAndNil(lTokens);
		FreeAndNil(lScanner);
		FreeAndNil(lTokenIterator);
		FreeAndNil(lParser);
	End;
End;

End.