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
	Commands;

Interface

Uses
	Classes,
	SysUtils,
	StrUtils,
	Tree,
	XMLNodes,
	XMLLoader,
	TabularData,
	CmdNodes,
	XMLTemplate;
	
Type
	TPrintCommand = Class(TCommandWithoutParameters)
	Public
		Procedure Evaluate; Override;
	End;
	
	TListCommand = Class(TCommandWithoutParameters)
	Public
		Procedure Evaluate; Override;
	End;
	
	TAddCommand = Class(TCommandWithClassNameAndProperties)
	Public
		Procedure Evaluate; Override;
	End;
	
	TParseTemplate = Class(TCommandWithExpressionList)
	Public
		Procedure Evaluate; Override;
	End;

Implementation

// TListCommand

Procedure TPrintCommand.Evaluate;
Var
	lTabularData : TTabularData;
	lX : Integer;
	lCurrent : TXMLNode;
Begin
	// Debug  WriteLn('TPrintCommand');
	If Stack.Focused.IsCapable('TPrintCommand') Then
	Begin
		lTabularData := TTabularData.Create;
		Try
			lTabularData.Clear;
			Stack.Focused.First;
			While Not Stack.Focused.IsAfterLast Do
			Begin
				lTabularData.NewLine;
				lCurrent := (Stack.Focused.GetCurrent As TXMLNode);
				lTabularData.SetData('No.', IntToStr(Stack.Focused.GetCurrentIndex));
				lTabularData.SetData('Name', lCurrent.Name);
				For lX := Low(lCurrent.Properties.Pairs) To High(lCurrent.Properties.Pairs) Do
					lTabularData.SetData(lCurrent.Properties.Pairs[lX].Name, lCurrent.Properties.Pairs[lX].Value);
				Stack.Focused.Next;
			End;
			lTabularData.Print;
		Finally
			FreeAndNil(lTabularData);
		End;
	End
	Else
		Stack.Focused.RaiseError('You cannot use that command here.');
End;

// TListCommand

Procedure TListCommand.Evaluate;
Begin
	// Debug  WriteLn('TListCommand');
	If Stack.Focused.IsCapable('TListCommand') Then
	Begin
		Stack.Focused.First;
		While Not Stack.Focused.IsAfterLast Do
		Begin
			WriteLn((Stack.Focused.GetCurrent As TXMLNode).IndexedName);
			Stack.Focused.Next;
		End;
	End
	Else
		Stack.Focused.RaiseError('You cannot use that command here.');
End;

Procedure TAddCommand.Evaluate;
Begin
	// Debug  WriteLn('TListCommand');
	If Stack.Focused.IsCapable('TAddCommand') Then
	Begin
		If Stack.Focused.Factory.IsRegisteredClass(NameOfClass) Then
		Begin
			Stack.Focused := Stack.Focused.AppendChild(NameOfClass) As TXMLNode;
			Stack.Focused.Properties.Merge(Stack.NameValueStack.Top);
		End
		Else
			Stack.Focused.RaiseError('You cannot create here a child of that class.');
	End
	Else
		Stack.Focused.RaiseError('You cannot use that command here.');
End;

Procedure TParseTemplate.Evaluate;
Var
	lTemplate : TXMLTemplateRootNode;
	lFileStream : TFileStream;
	lBuffer : String;
	lStringStream : TStringStream;
Begin
	// Debug WriteLn('TParseTemplate');
	ExpressionList.Evaluate;
	Stack.Enter(2);
	lTemplate := TXMLTemplateRootNode.Create(Nil);
	lFileStream := TFileStream.Create(Stack.Top.Peek(1), fmCreate);
	Try
		lTemplate.LoadFromFile(Stack.Top.Peek(0));
		lTemplate.PropagateStack(Stack);
		lBuffer := lTemplate.Emit;
		lBuffer := StringReplace(lBuffer, ccNewLine, #13, [ rfReplaceAll, rfIgnoreCase ]);
		lBuffer := StringReplace(lBuffer, ccSpace, ' ', [ rfReplaceAll, rfIgnoreCase ]);
		lBuffer := StringReplace(lBuffer, ccTab, #09, [ rfReplaceAll, rfIgnoreCase ]);
		lBuffer := AdjustLineBreaks(lBuffer);
		lBuffer := DelSpace1(lBuffer);
		lStringStream := TStringStream.Create(lBuffer);
		lStringStream.Seek(0, soFromBeginning);
		lFileStream.CopyFrom(lStringStream, lStringStream.Size);
	Finally
		FreeAndNil(lTemplate);
		FreeAndNil(lFileStream);
		FreeAndNil(lStringStream);
	End;
	Stack.Leave(0);
End;

End.
