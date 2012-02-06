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
	Functions;

Interface

Uses
	Classes,
	SysUtils,
	{StrUtils,}
	Stacks,
	CmdLoader;

Type
	TNodeFunctions = Class(TStackFunctor)
	Public
		Procedure PushTrue(Var aMessage); Message 'true';
		Procedure PushFalse(Var aMessage); Message 'false';
		Procedure FuncGetProperty(Var aMessage); Message 'getproperty';
		Procedure FuncGetText(Var aMessage); Message 'gettext';
		Procedure FuncGetNodeName(Var aMessage); Message 'getnodename';
		Procedure FuncGetNodeNumber(Var aMessage); Message 'getnodenumber';
		Procedure FuncConcat(Var aMessage); Message 'concat';
	End;

Implementation

// TNodeFunctions

Procedure TNodeFunctions.PushTrue(Var aMessage);
Begin
	Stack.DoPushTrue;
End;

Procedure TNodeFunctions.PushFalse(Var aMessage);
Begin
	Stack.DoPushFalse;
End;

Procedure TNodeFunctions.FuncGetProperty(Var aMessage);
Var
	lDomLoader : TCmdRootNode;
	lDom,
	lProp : String;
Begin
	Try
		lProp := Stack.Top.Pop;
		lDom := Stack.Top.Pop;
		Stack.XMLNodeStack.Push(Stack.Focused);
		lDomLoader := TCmdRootNode.Create(Nil);
		lDomLoader.DoLoadDom(lDom, Stack);
		lDomLoader.Evaluate;
		Stack.Top.Push(Stack.Focused.Properties.GetValue(lProp));
		Stack.Focused := Stack.XMLNodeStack.Pop;
	Finally
		FreeAndNil(lDomLoader);
	End;
End;

Procedure TNodeFunctions.FuncGetText(Var aMessage);
Var
	lDomLoader : TCmdRootNode;
	lDom : String;
Begin
	Try
		lDom := Stack.Top.Pop;
		Stack.XMLNodeStack.Push(Stack.Focused);
		lDomLoader := TCmdRootNode.Create(Nil);
		lDomLoader.DoLoadDom(lDom, Stack);
		lDomLoader.Evaluate;
		Stack.Top.Push(Stack.Focused.GetTextChild);
		Stack.Focused := Stack.XMLNodeStack.Pop;
	Finally
		FreeAndNil(lDomLoader);
	End;
End;

Procedure TNodeFunctions.FuncGetNodeName(Var aMessage);
Begin
	Stack.Top.Push(Stack.Focused.Name);
End;

Procedure TNodeFunctions.FuncGetNodeNumber(Var aMessage);
Begin
	Stack.Top.Push(Stack.Focused.GetCurrentIndex);
End;

Procedure TNodeFunctions.FuncConcat(Var aMessage);
Var
	lCount : Integer;
	lResult : String;
Begin
	lResult := '';
	While Stack.Top.Count > 0 Do
		AppendStr(lResult, Stack.Top.Pop);
End;

End.
