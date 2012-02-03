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
	CmdLoader;

Interface

Uses
	Classes,
	SysUtils,
	Tree,
	Stacks,
	ExprScanner,
	ExprParser,
	ExprNodes,
	CmdParser,
	CmdNodes;

Type
	TCmdRootNode = Class(TExprNode)
	Private
		fCommandClassFactory : TCommandFactory;
	Public
		Constructor Create(Const aOwner : TTreeNode); Override;
		Destructor Destroy; Override;
		Procedure Evaluate; Override;
		Procedure DoLoadCommand(Const aCommandLine : String; Const aStack : TVirtualMachineStack);
		Procedure DoLoadDOM(Const aDOMNode : String; Const aStack : TVirtualMachineStack);
		Property ClassFactory : TCommandFactory Read fCommandClassFactory;
	End;

Implementation

Constructor TCmdRootNode.Create(Const aOwner : TTreeNode);
Begin
	Inherited Create(aOwner);
	fCommandClassFactory := TCommandFactory.Create;
	fCommandClassFactory.DefaultClass := TExprNode;
End;

Destructor TCmdRootNode.Destroy;
Begin
	FreeAndNil(fCommandClassFactory);
	Inherited Destroy;
End;

Procedure TCmdRootNode.Evaluate;
Begin
	// Debug  WriteLn('TCmdRootNode');
	EvaluateChilds;
End;

Procedure TCmdRootNode.DoLoadCommand(Const aCommandLine : String; Const aStack : TVirtualMachineStack);
Var
	lFile : TStringStream;
	lSource : TExprSource;
	lTokens : TMemoryStream;
	lScanner : TExprScanner;
	lTokenIterator : TExprTokenIterator;
	lParser : TCommandParser;
Begin
	lFile := Nil;
	lSource := Nil;
	lTokens := Nil;
	lScanner := Nil;
	lTokenIterator := Nil;
	lParser := Nil;
	Try
		lFile := TStringStream.Create(aCommandLine);
		lSource := TExprSource.Create(lFile, False);
		lTokens := TMemoryStream.Create;
		lScanner := TExprScanner.Create(lSource, lTokens, False);
		lScanner.Scan;
		lTokenIterator := TExprTokenIterator.Create(lTokens, False);
		lParser := TCommandParser.Create(lTokenIterator, Self, False);
		lParser.ClassFactory := fCommandClassFactory;
		lParser.ParseInputCommand;
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

Procedure TCmdRootNode.DoLoadDOM(Const aDOMNode : String; Const aStack : TVirtualMachineStack);
Var
	lFile : TStringStream;
	lSource : TExprSource;
	lTokens : TMemoryStream;
	lScanner : TExprScanner;
	lTokenIterator : TExprTokenIterator;
	lParser : TCommandParser;
Begin
	lFile := Nil;
	lSource := Nil;
	lTokens := Nil;
	lScanner := Nil;
	lTokenIterator := Nil;
	lParser := Nil;
	Try
		lFile := TStringStream.Create(aDOMNode);
		lSource := TExprSource.Create(lFile, False);
		lTokens := TMemoryStream.Create;
		lScanner := TExprScanner.Create(lSource, lTokens, False);
		lScanner.Scan;
		lTokenIterator := TExprTokenIterator.Create(lTokens, False);
		lParser := TCommandParser.Create(lTokenIterator, Self, False);
		lParser.ClassFactory := fCommandClassFactory;
		lParser.ParseDOMExpression;
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