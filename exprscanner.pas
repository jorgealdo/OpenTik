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
	ExprScanner;

Interface

Uses
	Classes,
	SysUtils,
	BaseException,
	StrUtils,
	Stacks;

Const
	ccExprPunctuation : Array[1..25] Of String = (
		'+', '-', '*', '/',
		'div', 'mod', '(', ')', 
		'not', 'and', 'or', 'xor',
		'=', '>', '<','<>',
		'>=', '<=', ',', '^',
		'.', '$', '[', ']', 
		'|'
	);

Type
	TExprSource = Class
	Private
		fOwnsSource : Boolean;
		fSource : TStream;
		fTempRow,
		fTempCol,
		fRow,
		fCol : Integer;
		fNext : Char;
		fEOF : Boolean;
	Public
		Constructor Create(Const aSource : TStream; Const aOwnsSource : Boolean = True); Virtual;
		Destructor Destroy; Override;
		Property Row : Integer Read fRow;
		Property Col : Integer Read fCol;
		Procedure Mark;
		Procedure HardGetNext;
		Procedure SoftGetNext;
		Function IsEOF : Boolean;
		Procedure RaiseError(Const aMsg : String);
		Property Next : Char Read fNext;
	End;

	TExprTokenKind = (
		tkExprWhite,
		tkExprComment,
		tkExprIdentifier,
		tkExprLiteralString,
		tkExprLiteralNumber,
		tkExprLiteralFloat,
		tkExprWord,
		tkExprEOF
	);

	TExprScanner = Class
	Private
		fOwnsSource : Boolean;
		fSource : TExprSource;
		fDestination : TStream;
		Procedure AppendToOutput(Const aLiteral : String; Const aKind : TExprTokenKind);
		Function IsPunct(Const aLiteral : String): Boolean;
		Procedure ScanWhite;
		Procedure ScanComment;
		Procedure ScanIdentifier;
		Procedure ScanString1;
		Procedure ScanString2;
		Procedure ScanNumber;
		Procedure ScanPunct;
	Public
		Constructor Create(Const aSource : TExprSource; Const aDestination : TStream; Const aOwnsSource : Boolean = True); Virtual;
		Destructor Destroy; Override;
		Procedure Scan;
	End;
	
	TExprTokenIterator = Class
	Private
		fOwnsSource : Boolean;
		fStack : TIntegerStack;
		fSource : TStream;
		fKind : TExprTokenKind;
		fLiteral : String;
		fRow,
		fCol : Integer;
	Public
		Constructor Create(Const aSource : TStream; Const aOwnsSource : Boolean = True); Virtual;
		Destructor Destroy; Override;
		Procedure Start;
		Procedure Next;
		Procedure Mark;
		Procedure Recall;
		Function Expected(Const aLiteral : String; Const aHaltIfNotFound : Boolean = False): Boolean; Overload;
		Function Expected(Const aKind : TExprTokenKind; Const aHaltIfNotFound : Boolean = False): Boolean; Overload;
		Function Expected(Const aKinds : Array Of TExprTokenKind; Const aHaltIfNotFound : Boolean = False): Boolean; Overload;
		Function Expected(Const aLiterals : Array Of String; Const aHaltIfNotFound : Boolean = False): Boolean; Overload;
		Function Consume(Const aLiteral : String; Const aHaltIfNotFound : Boolean = True): Boolean; Overload;
		Function Consume(Const aKind : TExprTokenKind; Const aHaltIfNotFound : Boolean = True): Boolean; Overload;
		Function ConsumeElse(Const aLiteral, aAlternative : String): Boolean;
		Function Match(Const aKinds : Array Of TExprTokenKind): Boolean;
		Function Extract(Const aKind : TExprTokenKind): String;
		Function IsEOS: Boolean;
		Procedure RaiseError(Const aMsg : String);
		Property Kind : TExprTokenKind Read fKind;
		Property Literal : String Read fLiteral;
		Property Row : Integer Read fRow;
		Property Col : Integer Read fCol;
	End;

Implementation

Constructor TExprSource.Create(Const aSource : TStream; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fSource := aSource;
	fOwnsSource := aOwnsSource;
	fTempRow := 1;
	fTempCol := 0;
	fRow := 1;
	fCol := 0;
	fNext := #00;
	fEOF := False;
End;

Destructor TExprSource.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	Inherited Destroy;
End;
	
Procedure TExprSource.Mark;
Begin
	fRow := fTempRow;
	fCol := fTempCol;
End;
	
Procedure TExprSource.HardGetNext;
Begin
	Try
		fNext := Char(fSource.ReadByte);
	Except
		On E: Exception Do 
			fNext := #00;
	End;
End;
	
Procedure TExprSource.SoftGetNext;
Begin
	HardGetNext;
	Case fNext Of
		#13 :
			fTempCol := 1;
		#10 :
		Begin
			fTempCol := 1;
			fTempRow := fTempRow + 1;
		End;
	Else
		fTempCol := fTempCol + 1;
	End;
	If fSource.Position > fSource.Size Then
		fEOF := True;
	// Debug Write(fNext, ':');
End;

Function TExprSource.IsEOF: Boolean;
Begin
	Result := fEOF;
End;

Procedure TExprSource.RaiseError(Const aMsg : String);
Var
	lException : TRowColException;
Begin
	lException := TRowColException.Create(aMsg);
	lException.Row := fRow;
	lException.Col := fCol;
	Raise lException;
End;

Procedure TExprScanner.AppendToOutput(Const aLiteral : String; Const aKind : TExprTokenKind);
Begin
	// Debug WriteLn('(', fSource.Row:3, ':', fSource.Col:3, ') "', aLiteral, '" -> ', aKind);
	fDestination.WriteByte(Ord(aKind));
	fDestination.WriteAnsiString(aLiteral);
	fDestination.WriteDWord(fSource.Row);
	fDestination.WriteDWord(fSource.Col);
End;

Function TExprScanner.IsPunct(Const aLiteral : String): Boolean;
Var
	lCtrl : Integer;
Begin
	Result := False;
	For lCtrl := Low(ccExprPunctuation) To High(ccExprPunctuation) Do
		If ccExprPunctuation[lCtrl] = aLiteral Then
		Begin
			Result := True;
			Break;
		End;
End;

Procedure TExprScanner.ScanWhite;
Var
	lLiteral : String;
Begin
	lLiteral := '';
	fSource.Mark;
	While (fSource.Next In [#32, #09, #13, #10]) And Not(fSource.IsEOF) Do
	Begin
		lLiteral := lLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	// AppendToOutput(lLiteral, tkExprWhite);
End;

Procedure TExprScanner.ScanComment;
Var
	lLiteral : String;
Begin
	lLiteral := '';
	fSource.Mark;
	fSource.SoftGetNext;
	While (fSource.Next <> '}') And Not(fSource.IsEOF) Do
	Begin
		lLiteral := lLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	If fSource.Next = '}' Then
	Begin
		AppendToOutput(lLiteral, tkExprComment);
		fSource.SoftGetNext;
	End
	Else
		fSource.RaiseError('Unexpected end of file.');
End;

Procedure TExprScanner.ScanIdentifier;
Var
	lLiteral : String;
Begin
	lLiteral := '';
	fSource.Mark;
	While (fSource.Next In ['a'..'z', 'A'..'Z', '0'..'9']) And Not(fSource.IsEOF) Do
	Begin
		lLiteral := lLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	If IsPunct(lLiteral) Then
		AppendToOutput(LowerCase(lLiteral), tkExprWord)
	Else
		AppendToOutput(LowerCase(lLiteral), tkExprIdentifier);
End;

Procedure TExprScanner.ScanString1;
Var
	lLiteral : String;
Begin
	lLiteral := '';
	fSource.Mark;
	fSource.SoftGetNext;
	While (fSource.Next <> '''') And Not(fSource.IsEOF) And Not(fSource.Next In [#13, #10]) Do
	Begin
		lLiteral := lLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	If fSource.Next = '''' Then
	Begin
		AppendToOutput(lLiteral, tkExprLiteralString);
		fSource.SoftGetNext;
	End
	Else
		If fSource.Next In [#13, #10] Then
			fSource.RaiseError('String exceeds line.')
		Else
			fSource.RaiseError('Unexpected end of file.');
End;


Procedure TExprScanner.ScanString2;
Var
	lLiteral : String;
Begin
	lLiteral := '';
	fSource.Mark;
	fSource.SoftGetNext;
	While (fSource.Next <> '"') And Not(fSource.IsEOF) And Not(fSource.Next In [#13, #10]) Do
	Begin
		lLiteral := lLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	If fSource.Next = '"' Then
	Begin
		AppendToOutput(lLiteral, tkExprLiteralString);
		fSource.SoftGetNext;
	End
	Else
		If fSource.Next In [#13, #10] Then
			fSource.RaiseError('String exceeds line.')
		Else
			fSource.RaiseError('Unexpected end of file.');
End;

Procedure TExprScanner.ScanNumber;
Var
	lLiteral : String;
Begin
	lLiteral := '';
	fSource.Mark;
	While (fSource.Next In ['0'..'9']) And Not(fSource.IsEOF) Do
	Begin
		lLiteral := lLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	If fSource.Next = '.' Then
	Begin
		lLiteral := lLiteral + '.';
		fSource.SoftGetNext;
		If (fSource.Next In ['0'..'9']) And Not(fSource.IsEOF) Then
		Begin
			lLiteral := lLiteral + fSource.Next;
			fSource.SoftGetNext;
		End
		Else
			fSource.RaiseError('Expected number after "."');
		While (fSource.Next In ['0'..'9']) And Not(fSource.IsEOF) Do
		Begin
			lLiteral := lLiteral + fSource.Next;
			fSource.SoftGetNext;
		End;
		AppendToOutput(lLiteral, tkExprLiteralFloat);
	End
	Else
		AppendToOutput(lLiteral, tkExprLiteralNumber);
End;

Procedure TExprScanner.ScanPunct;
Var
	lLiteral : String;
Begin
	lLiteral := '';
	fSource.Mark;
	While IsPunct(lLiteral + fSource.Next) And Not(fSource.IsEOF) Do
	Begin
		lLiteral := lLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	AppendToOutput(lLiteral, tkExprWord);
End;

Constructor TExprScanner.Create(Const aSource : TExprSource; Const aDestination : TStream; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fSource := aSource;
	fDestination := aDestination;
	fOwnsSource := aOwnsSource;
End;

Destructor TExprScanner.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	Inherited Destroy;
End;

Procedure TExprScanner.Scan;
Begin
	fSource.SoftGetNext;
	fSource.Mark;
	While Not(fSource.IsEOF) Do
		If fSource.Next In [#32, #09, #13, #10] Then
			ScanWhite
		Else If fSource.Next = '{' Then
			ScanComment
		Else If fSource.Next In ['a'..'z', 'A'..'Z'] Then
			ScanIdentifier
		Else If fSource.Next = '''' Then
			ScanString1
		Else If fSource.Next = '"' Then
			ScanString2
		Else If fSource.Next In ['0'..'9'] Then
			ScanNumber
		Else If IsPunct(fSource.Next) Then
			ScanPunct
		Else If fSource.Next = #00 Then
			Break
		Else
			fSource.RaiseError('Unknown char "' + fSource.Next + '" (' + IntToStr(Byte(fSource.Next)) + ') at input.');
	AppendToOutput('', tkExprEOF);
End;

Constructor TExprTokenIterator.Create(Const aSource : TStream; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fStack := TIntegerStack.Create;
	fSource := aSource;
	fOwnsSource := aOwnsSource;
	fLiteral := '';
	fRow := 0;
	fCol := 0;
	Start;
End;

Destructor TExprTokenIterator.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	fStack.Free;
	Inherited Destroy;
End;

Procedure TExprTokenIterator.Start;
Begin
	fSource.Seek(0, soFromBeginning);
	Next;
End;

Procedure TExprTokenIterator.Next;
Begin
	If fSource.Position < fSource.Size Then
	Begin
		fKind := TExprTokenKind(fSource.ReadByte);
		fLiteral := fSource.ReadAnsiString;
		fRow := fSource.ReadDWord;
		fCol := fSource.ReadDWord;
	End
	Else
	Begin
		fKind := tkExprEOF;
		fLiteral := '';
	End;
	// Debug  WriteLn('Position : ', fSource.Position, ' Size : ', fSource.Size);
	// Debug  WriteLn('Literal : ', fLiteral, ' Kind : ', fKind);
End;

Procedure TExprTokenIterator.Mark;
Begin
	fStack.Push(fSource.Position);
End;

Procedure TExprTokenIterator.Recall;
Begin
	fSource.Seek(fStack.Pop, soFromBeginning);
	Next;
End;

Function TExprTokenIterator.Expected(Const aLiteral : String; Const aHaltIfNotFound : Boolean = False): Boolean;
Begin
	Result := fLiteral = aLiteral;
	If aHaltIfNotFound And Not(Result) Then
		RaiseError('Expected ' + aLiteral + '.')
End;

Function TExprTokenIterator.Expected(Const aKind : TExprTokenKind; Const aHaltIfNotFound : Boolean = False): Boolean;
Begin
	Result := fKind = aKind;
	If aHaltIfNotFound And Not(Result) Then
		Case aKind Of 
			tkExprWhite         : RaiseError('Expected whitespace.');
			tkExprComment       : RaiseError('Expected commentary.');
			tkExprIdentifier    : RaiseError('Expected identifier.');
			tkExprLiteralString : RaiseError('Expected string.');
			tkExprLiteralNumber : RaiseError('Expected number.');
			tkExprWord          : RaiseError('Expected punctuation.');
			tkExprEOF           : RaiseError('Expected end of source.');
		End;
End;

Function TExprTokenIterator.Expected(Const aKinds : Array Of TExprTokenKind; Const aHaltIfNotFound : Boolean = False): Boolean;
Var
	lCtrl : Integer;

	Function ExpectedNames: String;
	Var
		lCtrl2 : Integer;
	Begin
		Result := 'Expected ';
		If Length(aKinds) = 1 Then
			Case aKinds[Low(aKinds)] Of 
				tkExprWhite         : Result := Result + 'whitespace.';
				tkExprComment       : Result := Result + 'commentary.';
				tkExprIdentifier    : Result := Result + 'identifier.';
				tkExprLiteralString : Result := Result + 'string.';
				tkExprLiteralNumber : Result := Result + 'number.';
				tkExprWord          : Result := Result + 'punctuation.';
				tkExprEOF           : Result := Result + 'end of source.';
			End
		Else If Length(aKinds) = 2 Then
		Begin
			Case aKinds[Low(aKinds)] Of 
				tkExprWhite         : Result := Result + 'whitespace or ';
				tkExprComment       : Result := Result + 'commentary or ';
				tkExprIdentifier    : Result := Result + 'identifier or ';
				tkExprLiteralString : Result := Result + 'string or ';
				tkExprLiteralNumber : Result := Result + 'number or ';
				tkExprWord          : Result := Result + 'punctuation or ';
				tkExprEOF           : Result := Result + 'end of source or ';
			End;
			Case aKinds[Low(aKinds)] Of 
				tkExprWhite         : Result := Result + 'whitespace.';
				tkExprComment       : Result := Result + 'commentary.';
				tkExprIdentifier    : Result := Result + 'identifier.';
				tkExprLiteralString : Result := Result + 'string.';
				tkExprLiteralNumber : Result := Result + 'number.';
				tkExprWord          : Result := Result + 'punctuation';
				tkExprEOF           : Result := Result + 'end of source';
			End
		End
		Else
			For lCtrl2 := Low(aKinds) To High(aKinds) Do
			Begin
				Case aKinds[Low(aKinds)] Of 
					tkExprWhite         : Result := Result + 'whitespace';
					tkExprComment       : Result := Result + 'commentary';
					tkExprIdentifier    : Result := Result + 'identifier';
					tkExprLiteralString : Result := Result + 'string';
					tkExprLiteralNumber : Result := Result + 'number';
					tkExprWord          : Result := Result + 'punctuation';
					tkExprEOF           : Result := Result + 'end of source';
				End;
				If lCtrl2 < (High(aKinds) - 1) Then
					Result := Result + ', '
				Else
					If lCtrl2 = (High(aKinds) - 1) Then
						Result := Result + ' or '
					Else
						If lCtrl2 = High(aKinds) Then
							Result := Result + '.';
			End;
	End;

Begin
	Result := False;
	For lCtrl := Low(aKinds) To High(aKinds) Do
		Result := Result Or Expected(aKinds[lCtrl]);
	If aHaltIfNotFound And Not(Result) Then
			RaiseError(ExpectedNames);
End;

Function TExprTokenIterator.Expected(Const aLiterals : Array Of String; Const aHaltIfNotFound : Boolean = False): Boolean;
Var
	lCtrl : Integer;

	Function ExpectedNames: String;
	Var
		lCtrl2 : Integer;
	Begin
		Result := 'Expected ';
		If Length(aLiterals) = 1 Then
			Result := Result + '"' + aLiterals[Low(aLiterals)] + '"'
		Else If Length(aLiterals) = 2 Then
			Result := Result + '"' + aLiterals[Low(aLiterals)] + '" or "' + aLiterals[High(aLiterals)] + '"'
		Else
			For lCtrl2 := Low(aLiterals) To High(aLiterals) Do
			Begin
				Result := Result + aLiterals[lCtrl2];
				If lCtrl2 < (High(aLiterals) - 1) Then
					Result := Result + ', '
				Else
					If lCtrl2 = (High(aLiterals) - 1) Then
						Result := Result + ' or '
					Else
						If lCtrl2 = High(aLiterals) Then
							Result := Result + '.';
			End;
	End;

Begin
	Result := False;
	For lCtrl := Low(aLiterals) To High(aLiterals) Do
		Result := Result Or Expected(aLiterals[lCtrl]);
	If aHaltIfNotFound And Not(Result) Then
			RaiseError(ExpectedNames);
End;

Function TExprTokenIterator.Consume(Const aLiteral : String; Const aHaltIfNotFound : Boolean = True): Boolean;
Begin
	// Debug WriteLn('Consuming ', aLiteral);
	Result := Expected(aLiteral, aHaltIfNotFound);
	If Result Then
		Next;
End;

Function TExprTokenIterator.Consume(Const aKind : TExprTokenKind; Const aHaltIfNotFound : Boolean = True): Boolean;
Begin
	// Debug WriteLn('Consuming ', aKind);
	Result := Expected(aKind, aHaltIfNotFound);
	If Result Then
		Next;
End;

Function TExprTokenIterator.ConsumeElse(Const aLiteral, aAlternative : String): Boolean;
Begin
	Result := Consume(aLiteral, False);
	If Not(Result) Then
		Consume(aAlternative, True);
End;

Function TExprTokenIterator.Match(Const aKinds : Array Of TExprTokenKind): Boolean;
Var
	lCtrl : Integer;
Begin
	// Debug  WriteLn('Matching :');
	Mark;
	Result := True;
	For lCtrl := Low(aKinds) To High(aKinds) Do
	Begin
		// Debug  WriteLn(fKind, '->', aKinds[lCtrl]);
		Result := Result And (fKind = aKinds[lCtrl]);
		Next;
	End;
	Recall;
End;

Function TExprTokenIterator.Extract(Const aKind : TExprTokenKind): String;
Begin
	// Debug WriteLn('Extracting ', aKind, ' got ', fKind, ' string "', fLiteral, '".');
	Result := fLiteral;
	Consume(aKind);
End;

Function TExprTokenIterator.IsEOS: Boolean;
Begin
	Result := fSource.Position >= fSource.Size;
End;

Procedure TExprTokenIterator.RaiseError(Const aMsg : String);
Var
	lException : TRowColException;
Begin
	lException := TRowColException.Create(aMsg);
	lException.Row := fRow;
	lException.Col := fCol;
	Raise lException;
End;

End.
