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
	XMLTemplate;

Interface

Uses
	Classes,
	SysUtils,
	StrUtils,
	BaseException,
	Tree,
	NameValue,
	Stacks,
	XMLNodes,
	XMLLoader,
	ExprNodes,
	ExprLoader,
	CmdNodes,
	CmdLoader,
	XMLParser,
	XMLScanner;

Const
	ccNewLine = '\n';
	ccSpace = '\s';
	ccTab = '\t';

Type
	TXMLTemplateNode = Class(TXMLNode)
	Private
		fStack : TVirtualMachineStack;
	Public
		Constructor Create(Const aOwner : TTreeNode); Override;
		Function Emit: String; Virtual;
		Function EmitChilds: String; Virtual;
		Procedure PropagateStack(Const aStack : TVirtualMachineStack); Virtual;
		Property Stack : TVirtualMachineStack Read fStack Write fStack;
	End;

	TXMLTemplateRootNode = Class(TXMLTemplateNode)
	Public
		Function IndexedName: String; Override;
		Function AsXML: String; Override;
		Function AsClosingXML: String; Override;
		Procedure LoadFromFile(Const aFileName : String);
	End;

	TXMLWith = Class(TXMLTemplateNode)
	Public
		Function Emit: String; Override;
	End;

	TXMLTemplateIf = Class(TXMLTemplateNode)
	Public
		Function Emit: String; Override;
	End;

	TXMLTemplateMakeVar = Class(TXMLTemplateNode)
	Public
		Function Emit: String; Override;
	End;

	TXMLTemplateForEach = Class(TXMLTemplateNode)
	Public
		Function Emit: String; Override;
	End;

	TXMLGetConfig = Class(TXMLTemplateNode)
	Public
		Function Emit: String; Override;
	End;

	TXMLGetText = Class(TXMLTemplateNode)
	Public
		Function Emit: String; Override;
	End;

Implementation

// TXMLTemplateNode

Constructor TXMLTemplateNode.Create(Const aOwner : TTreeNode);
Begin
	Inherited Create(aOwner);
	Factory.Register('with', TXMLWith);
	Factory.Register('if', TXMLTemplateIf);
	Factory.Register('makevar', TXMLTemplateMakeVar);
	Factory.Register('foreach', TXMLTemplateForEach);
	Factory.Register('get', TXMLGetConfig);
	Factory.Register('gettext', TXMLGetText);
	Factory.Register('then', TXMLTemplateNode);
	Factory.Register('else', TXMLTemplateNode);
End;

Function TXMLTemplateNode.Emit: String;
Var
	lCtrl : Integer;
Begin
	// Debug  WriteLn('Emitting template');
	If Length(Childs) > 0 Then
		Result := AsXML + EmitChilds + AsClosingXML
	Else
		Result := AsXML;
	For lCtrl := Low(Stack.NameValueStack.Top.Pairs) To High(Stack.NameValueStack.Top.Pairs) Do
		Result := StringReplace(Result, '{$' + UpperCase(Stack.NameValueStack.Top.Pairs[lCtrl].Name) + '$}', Stack.NameValueStack.Top.Pairs[lCtrl].Value, [ rfReplaceAll, rfIgnoreCase ]);
End;

Function TXMLTemplateNode.EmitChilds: String;
Begin
	// Debug  WriteLn('Emitting Childs (', Length(Childs), ')');
	First;
	While Not IsAfterLast Do
	Begin
		// Debug  WriteLn('Emitting child : ', GetCurrentIndex, ' class ', GetCurrent.ClassName);
		If GetCurrent Is TXMLTextNode Then
			Result := Result + (GetCurrent As TXMLTextNode).Content
		Else If GetCurrent Is TXMLTemplateNode Then
			Result := Result + (GetCurrent As TXMLTemplateNode).Emit;
		Next;
	End;
End;

Procedure TXMLTemplateNode.PropagateStack(Const aStack : TVirtualMachineStack);
Var
	lSave : Integer;
Begin
	fStack := aStack;
	lSave := GetCurrentIndex;
	First;
	While Not IsAfterLast Do
	Begin
		// Debug  WriteLn('Propagating stack to ', GetCurrentIndex, ' class ', GetCurrent.ClassName);
		If GetCurrent Is TXMLTemplateNode Then
			(GetCurrent As TXMLTemplateNode).PropagateStack(aStack);
		Next;
	End;
	SetcurrentIndex(lSave);
End;

// TXMLTemplateRootNode

Function TXMLTemplateRootNode.IndexedName: String;
Begin
	Result := '';
End;

Function TXMLTemplateRootNode.AsXML: String;
Begin
	Result := '';
End;

Function TXMLTemplateRootNode.AsClosingXML: String;
Begin
	Result := '';
End;

Procedure TXMLTemplateRootNode.LoadFromFile(Const aFileName : String);
Var
	lFile : TFileStream;
	lSource : TXMLSource;
	lTokens : TMemoryStream;
	lScanner : TXMLScanner;
	lTokenIterator : TXMLTokenIterator;
	lParser : TXMLParser;
Begin
	// Debug  WriteLn('TXMLTemplateRootNode');
	lFile := Nil;
	lSource := Nil;
	lTokens := Nil;
	lScanner := Nil;
	lTokenIterator := Nil;
	lParser := Nil;
	Try
		lFile := TFileStream.Create(aFileName, fmOpenRead);
		lSource := TXMLSource.Create(lFile, False);
		lTokens := TMemoryStream.Create;
		lScanner := TXMLScanner.Create(lSource, lTokens, False);
		lScanner.Scan;
		lTokenIterator := TXMLTokenIterator.Create(lTokens, False);
		lParser := TXMLParser.Create(lTokenIterator, Self, False);
		lParser.Parse;
	Finally
		FreeAndNil(lFile);
		FreeAndNil(lSource);
		FreeAndNil(lTokens);
		FreeAndNil(lScanner);
		FreeAndNil(lTokenIterator);
		FreeAndNil(lParser);
	End;
End;

// TXMLWith 

Function TXMLWith.Emit: String;
Var
	lLoader : TCmdRootNode;
	lSave : TXMLNode;
Begin
	// Debug  WriteLn('TXMLWith');
	lSave := Stack.Focused;
	lLoader := TCmdRootNode.Create(Nil);
	Try
		lLoader.DoLoadDOM(Properties.GetValue('node'), Stack);
		lLoader.Evaluate;
		Result := Result + EmitChilds;
	Finally
		FreeAndNil(lLoader);
		Stack.Focused := lSave;
	End;	
End;

// TXMLTemplateIf

Function TXMLTemplateIf.Emit: String;
Var
	lExpression : TExprRootNode;
Begin
	// Debug  WriteLn('TXMLTemplateIf');
	Try
		lExpression := TExprRootNode.Create(Nil);
		lExpression.DoLoadExpression(Properties.GetValue('expression'), Stack);
		Stack.Enter(0);
		lExpression.Evaluate;
		If Stack.Top.Pop = True Then
			If HasChildNamed('then') Then
				Result := (Find('then') As TXMLTemplateNode).EmitChilds
			Else
		Else
			If HasChildNamed('else') Then
				Result := (Find('else') As TXMLTemplateNode).EmitChilds;
		Stack.Leave(0);
	Finally
		FreeAndNil(lExpression);
	End;
End;

// TXMLTemplateMakeVar

Function TXMLTemplateMakeVar.Emit: String;
Var
	lExpression : TExprRootNode;
Begin
	// Debug  WriteLn('TXMLTemplateMakeVar');
	Result := '';
	If Assigned(Owner) Then
		Try
			lExpression := TExprRootNode.Create(Nil);
			lExpression.DoLoadExpression(Properties.GetValue('value'), Stack);
			Stack.Enter(0);
			lExpression.Evaluate;
			Stack.NameValueStack.Top.SetValue(LowerCase(Properties.GetValue('name')), Stack.Top.Pop);
			Stack.Leave(0);
		Finally
			FreeAndNil(lExpression);
		End;
End;

// TXMLTemplateForEach

Function TXMLTemplateForEach.Emit: String;
Var
	lLoader : TCmdRootNode;
	lSave : TXMLNode;
Begin
	// Debug  WriteLn('TXMLTemplateForEach');
	lSave := Stack.Focused;
	lLoader := TCmdRootNode.Create(Nil);
	Try
		lLoader.DoLoadDOM(Properties.GetValue('node'), Stack);
		lLoader.Evaluate;
		Stack.Focused.First;
		While Not Stack.Focused.IsAfterLast Do
		Begin
			Result := Result + Inherited EmitChilds;
			Stack.Focused.Next;
		End;
	Finally
		Stack.Focused := lSave;
		FreeAndNil(lLoader);
	End;	
End;

// TXMLGetConfig

Function TXMLGetConfig.Emit: String;
Var
	lLoader : TCmdRootNode;
	lSave : TXMLNode;
Begin
	// Debug  WriteLn('TXMLGetConfig');
	lSave := Stack.Focused;
	lLoader := TCmdRootNode.Create(Nil);
	Try
		lLoader.DoLoadDOM(Properties.GetValue('node'), Stack);
		lLoader.Evaluate;
		Result := Stack.Focused.Properties.GetValue(Properties.GetValue('property'));
	Finally
		Stack.Focused := lSave;
		FreeAndNil(lLoader);
	End;	
End;

// TXMLGetText
Function TXMLGetText.Emit: String;
Var
	lLoader : TCmdRootNode;
	lSave : TXMLNode;
Begin
	// Debug  WriteLn('TXMLGetText');
	lSave := Stack.Focused;
	lLoader := TCmdRootNode.Create(Nil);
	Try
		lLoader.DoLoadDOM(Properties.GetValue('node'), Stack);
		lLoader.Evaluate;
		If Stack.Focused.HasTextChild Then
			Result := Stack.Focused.GetTextChild
		Else
			RaiseError('Node has no text.');
	Finally
		Stack.Focused := lSave;
		FreeAndNil(lLoader);
	End;	
End;

End.