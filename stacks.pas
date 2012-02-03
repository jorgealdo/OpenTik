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
	Stacks;

Interface

Uses
	Classes,
	SysUtils,
	BaseException,
	Variants,
	Contnrs,
	StringArray,
	NameValue,
	XMLNodes,
	XMLLoader;

Type
	EStackUnderflow = Class(Exception);

	Generic TStack<_T> = Class(TObject)
	Private
		fBuffer : Array Of _T;
	Public
		Procedure Push(Const aValue : _T);
		Function Pop: _T;
		Function Top: _T;
		Function First: _T;
		Function Previous: _T;
		Function Peek(Const aIndex : Integer): _T;
		Procedure Poke(Const aIndex : Integer; Const aValue : _T);
		Function Count : Integer;
		Function AtLeast(Const aCount : Integer): Boolean;
	End;

	TIntegerStack = Specialize TStack<Integer>;

	TVariantStack = Specialize TStack<Variant>;
	TVariantStackStack = Specialize TStack<TVariantStack>;
	TWindownedStack = Class(TVariantStackStack)
	Public
		Constructor Create;
		Destructor Destroy; Override;
		Procedure Enter(Const aCount : Integer);
		Procedure Leave(Const aCount : Integer);
		Procedure Purge;
	End;

	TArithmeticalStack = Class(TWindownedStack)
	Public
		Procedure DoDup;
		Procedure DoDrop;
		Procedure DoRot;
		Procedure DoSwap;
		Procedure DoPushTrue;
		Procedure DoPushFalse;
		Procedure DoAdd;
		Procedure DoSub;
		Procedure DoMul;
		Procedure DoDiv;
		Procedure DoIDiv;
		Procedure DoMod;
		Procedure DoPow;
		Procedure DoNot;
		Procedure DoAnd;
		Procedure DoOr;
		Procedure DoXOr;
		Procedure DoCmpEq;
		Procedure DoCmpSm;
		Procedure DoCmpGt;
		Procedure DoCmpDif;
		Procedure DoCmpEqSm;
		Procedure DoCmpGtSm;
	End;

	TVirtualMachineStack = Class;
	
	TStackFunctorCall = Record
		MsgStr : String[255];
		Data : Pointer;
	End;
	
	EStackFunctor = Class(Exception);
	TStackFunctor = Class 
	Private	
		fStack : TVirtualMachineStack;
	Public
		Function GetCallerStack(Var aMessage): TVirtualMachineStack;
		Function ListFunctions: TStringArray;
		Procedure Call(Const aFunctionName : String; Const aCaller : TVirtualMachineStack);
		Procedure DefaultHandlerStr(Var aMessage); Override;
		Property Stack : TVirtualMachineStack Read fStack Write fStack;
	End;
	
	TTNameValueStack = Specialize TStack<TNameValue>;
	TNameValueStack = Class(TTNameValueStack)
	Public
		Constructor Create;
		Destructor Destroy; Override;
		Procedure Enter;
		Procedure Leave;
		Procedure Purge;
	End;
	
	TTXMLNodeStack = Specialize TStack<TXMLNode>;

	EVirtualMachineStack = Class(Exception);
	TVirtualMachineStack = Class(TArithmeticalStack)
	Private
		fFunctionTable : TFPObjectHashTable;
		fXMLNodeStack : TTXMLNodeStack;
		fNameValueStack : TNameValueStack;
		fFocused : TXMLNode;
		fRunning : Boolean;
	Public
		Constructor Create;
		Destructor Destroy; Override;
		Procedure RegisterFunctor(Const aFunctor : TStackFunctor);
		Procedure Call(Const aFunctionName : String);
		Property XMLNodeStack : TTXMLNodeStack Read fXMLNodeStack Write fXMLNodeStack;
		Property NameValueStack : TNameValueStack Read fNameValueStack Write fNameValueStack;
		Property Focused : TXMLNode Read fFocused Write fFocused;
		Property Running : Boolean Read fRunning Write fRunning;
	End;

Implementation

Type
	TVirtualMachineMessage = Record
		MsgStr : String[255];
		Data : Pointer;
	End;

{ TStack }

Procedure TStack.Push(Const aValue : _T);
Begin
	SetLength(fBuffer, Length(fBuffer) + 1);
	fBuffer[High(fBuffer)] := aValue;
End;

Function TStack.Pop: _T;
Begin
	If AtLeast(1) Then
	Begin
		Result := fBuffer[High(fBuffer)];
		SetLength(fBuffer, Length(fBuffer) - 1);
	End
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.Top: _T;
Begin
	If AtLeast(1) Then
		Result := fBuffer[High(fBuffer)]
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.First: _T;
Begin
	If AtLeast(1) Then
	Begin
		Result := fBuffer[Low(fBuffer)];
	End
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.Previous: _T;
Begin
	If AtLeast(2) Then
	Begin
		Result := fBuffer[High(fBuffer) - 1];
	End
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.Peek(Const aIndex : Integer): _T;
Begin
	If (aIndex >= Low(fBuffer)) And (aIndex <= High(fBuffer)) Then
		Result := fBuffer[aIndex]
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Procedure TStack.Poke(Const aIndex : Integer; Const aValue : _T);
Begin
	If (aIndex >= Low(fBuffer)) And (aIndex <= High(fBuffer)) Then
		fBuffer[aIndex] := aValue
	Else
		Raise EStackUnderflow.Create('Stack underflow.');
End;

Function TStack.Count : Integer;
Begin
	Result := Length(fBuffer);
End;

Function TStack.AtLeast(Const aCount : Integer): Boolean;
Begin
	Result := Count >= aCount;
End;

// TWindownedStack

Constructor TWindownedStack.Create;
Begin
	Inherited Create;
	Push(TVariantStack.Create);
End;

Destructor TWindownedStack.Destroy;
Begin
	While AtLeast(1) Do
		Pop.Free;
	Inherited Destroy;
End;

Procedure TWindownedStack.Enter(Const aCount : Integer);
Var
	lCtrl : Integer;
	lTempStack : TVariantStack;
Begin
	Try
		lTempStack := TVariantStack.Create;
		For lCtrl := 1 To aCount Do
			lTempStack.Push(Top.Pop);
		Push(TVariantStack.Create);
		For lCtrl := 1 To aCount Do
			Top.Push(lTempStack.Pop);
	Finally
		FreeAndNil(lTempStack);
	End;
End;

Procedure TWindownedStack.Leave(Const aCount : Integer);
Var
	lCtrl : Integer;
	lTempStack : TVariantStack;
Begin
	Try
		lTempStack := TVariantStack.Create;
		For lCtrl := 1 To aCount Do
			lTempStack.Push(Top.Pop);
		Top.Free;
		Pop;
		For lCtrl := 1 To aCount Do
			Top.Push(lTempStack.Pop);
	Finally
		FreeAndNil(lTempStack);
	End;
End;

Procedure TWindownedStack.Purge;
Begin
	While AtLeast(1) Do
		Pop.Free;
	Push(TVariantStack.Create);
End;

{ TArithmeticalStack }

Procedure TArithmeticalStack.DoDup;
Var
	lTmp : Variant;
Begin
	lTmp := Top.Pop;
	Top.Push(lTmp);
	Top.Push(lTmp);
End;

Procedure TArithmeticalStack.DoDrop;
Begin
	Top.Pop;
End;

Procedure TArithmeticalStack.DoRot;
Var
	lTmp1,
	lTmp2,
	lTmp3 : Variant;
Begin
	lTmp1 := Top.Pop;
	lTmp2 := Top.Pop;
	lTmp3 := Top.Pop;
	Top.Push(lTmp1);
	Top.Push(lTmp2);
	Top.Push(lTmp3);
End;

Procedure TArithmeticalStack.DoSwap;
Var
	lTmp1,
	lTmp2 : Variant;
Begin
	lTmp1 := Top.Pop;
	lTmp2 := Top.Pop;
	Top.Push(lTmp1);
	Top.Push(lTmp2);
End;

Procedure TArithmeticalStack.DoPushTrue;
Begin
	Top.Push(True);
End;

Procedure TArithmeticalStack.DoPushFalse;
Begin
	Top.Push(False);
End;

Procedure TArithmeticalStack.DoAdd;
Begin
	Top.Push(Top.Pop + Top.Pop);
End;

Procedure TArithmeticalStack.DoSub;
Begin
	Top.Push(Top.Pop - Top.Pop);
End;

Procedure TArithmeticalStack.DoMul;
Begin
	Top.Push(Top.Pop * Top.Pop);
End;

Procedure TArithmeticalStack.DoDiv;
Begin
	Top.Push(Top.Pop / Top.Pop);
End;

Procedure TArithmeticalStack.DoIDiv;
Begin
	Top.Push(Top.Pop Div Top.Pop);
End;

Procedure TArithmeticalStack.DoMod;
Begin
	Top.Push(Top.Pop Mod Top.Pop);
End;

Procedure TArithmeticalStack.DoPow;
Begin
	Top.Push(Top.Pop ** Top.Pop);
End;

Procedure TArithmeticalStack.DoNot;
Begin
	Top.Push(Not(Top.Pop));
End;

Procedure TArithmeticalStack.DoAnd;
Begin
	Top.Push(Top.Pop And Top.Pop);
End;

Procedure TArithmeticalStack.DoOr;
Begin
	Top.Push(Top.Pop Or Top.Pop);
End;

Procedure TArithmeticalStack.DoXOr;
Begin
	Top.Push(Top.Pop XOr Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpEq;
Begin
	Top.Push(Top.Pop = Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpSm;
Begin
	Top.Push(Top.Pop < Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpGt;
Begin
	Top.Push(Top.Pop > Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpDif;
Begin
	Top.Push(Top.Pop <> Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpEqSm;
Begin
	Top.Push(Top.Pop <= Top.Pop);
End;

Procedure TArithmeticalStack.DoCmpGtSm;
Begin
	Top.Push(Top.Pop >= Top.Pop);
End;

// TStackFunctor

Function TStackFunctor.GetCallerStack(Var aMessage): TVirtualMachineStack;
Var
	lMsg : TStackFunctorCall;
Begin
	lMsg := TStackFunctorCall(aMessage);
	Result := TVirtualMachineStack(lMsg.Data);
End;

Function TStackFunctor.ListFunctions: TStringArray;
Var
	lCtrl : Integer;
	lClass : TClass;
Begin
	SetLength(Result, 0);
	lClass := Self.ClassType;
	While Assigned(lClass) Do
	Begin
		If lClass.StringMessageTable <> Nil Then
			If lClass.StringMessageTable^.Count > 0 Then
				For lCtrl := 0 To lClass.StringMessageTable^.Count - 1 Do
				Begin
						SetLength(Result, Length(Result) + 1);
						Result[High(Result)] := LowerCase((lClass.StringMessageTable^.MsgStrTable[lCtrl].Name^));
				End;
		lClass := lClass.ClassParent;
	End;
	Result := Result;
End;

Procedure TStackFunctor.Call(Const aFunctionName : String; Const aCaller : TVirtualMachineStack);
Var
	lMsg : TStackFunctorCall;
Begin
	lMsg.MsgStr := LowerCase(aFunctionName);
	lMsg.Data := Pointer(aCaller);
	DispatchStr(lMsg);
End;

Procedure TStackFunctor.DefaultHandlerStr(Var aMessage);
Begin
	Raise EStackFunctor.Create('Function is not defined : ' + TStackFunctorCall(aMessage).MsgStr + ' (I have : ' + ConcatenateStrings(ListFunctions) + ')');
End;

// TNameValueStack

Constructor TNameValueStack.Create;
Begin
	Inherited Create;
	Enter;
End;

Destructor TNameValueStack.Destroy;
Begin
	Purge;
	Inherited Destroy;
End;

Procedure TNameValueStack.Enter;
Begin
	Push(TNameValue.Create);
End;

Procedure TNameValueStack.Leave;
Begin
	Pop.Free;
End;

Procedure TNameValueStack.Purge;
Begin
	While AtLeast(1) Do
		Leave;
End;

// TVirtualMachineStack

Constructor TVirtualMachineStack.Create;
Begin
	Inherited Create;
	fFunctionTable := TFPObjectHashTable.Create(False);
	fXMLNodeStack := TTXMLNodeStack.Create;
	fNameValueStack := TNameValueStack.Create;
End;

Destructor TVirtualMachineStack.Destroy;
Begin
	FreeAndNil(fXMLNodeStack);
	FreeAndNil(fNameValueStack);
	FreeAndNil(fFunctionTable);
	Inherited Destroy;
End;

Procedure TVirtualMachineStack.RegisterFunctor(Const aFunctor : TStackFunctor);
Var
	lCtrl : Integer;
	lFunctions : TStringArray;
Begin
	lFunctions := aFunctor.ListFunctions;
	For lCtrl := Low(lFunctions) To High(lFunctions) Do
		fFunctionTable.Add(lFunctions[lCtrl], aFunctor)
End;

Procedure TVirtualMachineStack.Call(Const aFunctionName : String);
Var
	lFunctor : TObject;
Begin
	lFunctor := fFunctionTable[LowerCase(aFunctionName)];
	If Assigned(lFunctor) And (lFunctor Is TStackFunctor) Then
		(lFunctor As TStackFunctor).Call(LowerCase(aFunctionName), Self)
	Else
		Raise EVirtualMachineStack.Create('This function is not defined : ' + aFunctionName);
End;

End.