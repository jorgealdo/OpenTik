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
	XMLScanner;

Interface

Uses
	Classes,
	SysUtils,
	BaseException,
	StrUtils;

Type
	TXMLSource = Class
	Private
		fOwnsSource : Boolean;
		fSource : TStream;
		fTempRow,
		fTempCol,
		fRow,
		fCol : Integer;
		fNext : Char;
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
	
	TXMLTokenKind = (tkXMLWhite, tkXMLOpenTag, tkXMLCloseTag, tkXMLIdentifier, tkXMLText,
		tkXMLString, tkXMLSlash, tkXMLExclamation, tkXMLQuestion,  tkXMLEqual, tkXMLEOF);

	TXMLScanner = Class
	Private
		fOwnsSource : Boolean;
		fSource : TXMLSource;
		fDestination : TStream;
		Procedure AppendToOutput(Const aLiteral : String; Const aKind : TXMLTokenKind);
		Procedure ScanWhite;
		Procedure ScanIdentifier;
		Procedure ScanString;
		Procedure ScanText;
		Procedure ScanPunct;
		Procedure ScanTag;
	Public
		Constructor Create(Const aSource : TXMLSource; Const aDestination : TStream; Const aOwnsSource : Boolean = True); Virtual;
		Destructor Destroy; Override;
		Procedure Scan;
	End;
	
	TXMLTokenIterator = Class
	Private
		fOwnsSource : Boolean;
		fStack : Array Of Integer;
		fSource : TStream;
		fKind : TXMLTokenKind;
		fLastPosition : Integer;
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
		Function Expected(Const aKind : TXMLTokenKind; Const aHaltIfNotFound : Boolean = False): Boolean; Overload;
		Function Expected(Const aKinds : Array Of TXMLTokenKind; Const aHaltIfNotFound : Boolean = False): Boolean; Overload;
		Function Consume(Const aLiteral : String; Const aHaltIfNotFound : Boolean = True): Boolean; Overload;
		Function Consume(Const aKind : TXMLTokenKind; Const aHaltIfNotFound : Boolean = True): Boolean; Overload;
		Function ConsumeElse(Const aLiteral, aAlternative : String): Boolean;
		Function Match(Const aKinds : Array Of TXMLTokenKind): Boolean;
		Function Extract(Const aKind : TXMLTokenKind): String;
		Function IsEOS: Boolean;
		Procedure RaiseError(Const aMsg : String);
		Property Kind : TXMLTokenKind Read fKind;
		Property Literal : String Read fLiteral;
		Property Row : Integer Read fRow;
		Property Col : Integer Read fCol;
	End;
	
Implementation

Constructor TXMLSource.Create(Const aSource : TStream; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fSource := aSource;
	fOwnsSource := aOwnsSource;
	fTempRow := 1;
	fTempCol := 0;
	fRow := 1;
	fCol := 0;
	fNext := #00;
End;

Destructor TXMLSource.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	Inherited Destroy;
End;
	
Procedure TXMLSource.Mark;
Begin
	fRow := fTempRow;
	fCol := fTempCol;
End;
	
Procedure TXMLSource.HardGetNext;
Begin
	Try
		fNext := Char(fSource.ReadByte);
	Except
		On E: Exception Do 
			fNext := #00;
	End;
End;
	
Procedure TXMLSource.SoftGetNext;
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
End;

Function TXMLSource.IsEOF: Boolean;
Begin
	Result := fSource.Position >= fSource.Size;
End;

Procedure TXMLSource.RaiseError(Const aMsg : String);
Var
	lException : TRowColException;
Begin
	lException := TRowColException.Create(aMsg);
	lException.Row := fRow;
	lException.Col := fCol;
	Raise lException;
End;

Procedure TXMLScanner.AppendToOutput(Const aLiteral : String; Const aKind : TXMLTokenKind);
Begin
	If Not(IsEmptyStr(aLiteral, [#32, #13, #10, #09])) Then
	Begin
		fDestination.WriteByte(Ord(aKind));
		fDestination.WriteAnsiString(DelSpace1(TrimSet(aLiteral, [ #32, #13, #10, #09 ])));
		fDestination.WriteDWord(fSource.Row);
		fDestination.WriteDWord(fSource.Col);
	End;
End;

Procedure TXMLScanner.ScanWhite;
Begin
	fSource.Mark;
	While (fSource.Next In [#32, #09, #13, #10]) And Not(fSource.IsEOF) Do
		fSource.SoftGetNext;
	// AppendToOutput('', tkXMLWhite);
End;

Procedure TXMLScanner.ScanIdentifier;
Var
	fLiteral : String;
Begin
	fLiteral := '';
	fSource.Mark;
	While (fSource.Next In ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) And Not(fSource.IsEOF) Do
	Begin
		fLiteral := fLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	AppendToOutput(fLiteral, tkXMLIdentifier);
End;

Procedure TXMLScanner.ScanString;
Var
	fLiteral : String;
Begin
	fLiteral := '';
	fSource.Mark;
	fSource.SoftGetNext;
	While (fSource.Next <> '"') And Not(fSource.IsEOF) And Not(fSource.Next In [#13, #10]) Do
	Begin
		fLiteral := fLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	If fSource.Next = '"' Then
	Begin
		AppendToOutput(fLiteral, tkXMLString);
		fSource.SoftGetNext;
	End
	Else
		If fSource.Next In [#13, #10] Then
			fSource.RaiseError('String exceeds line.')
		Else
			fSource.RaiseError('Unexpected end of file.');
End;

Procedure TXMLScanner.ScanText;
Var
	fLiteral : String;
Begin
	fLiteral := '';
	fSource.Mark;
	While (fSource.Next <> '<') And Not(fSource.IsEOF) Do
	Begin
		fLiteral := fLiteral + fSource.Next;
		fSource.SoftGetNext;
	End;
	fLiteral := AdjustLineBreaks(DelSpace1(fLiteral));
	AppendToOutput(fLiteral, tkXMLText);
End;

Procedure TXMLScanner.ScanPunct;
Begin
	fSource.Mark;
	Case fSource.Next Of
	'<' :
		Begin
			AppendToOutput('<', tkXMLOpenTag);
			fSource.SoftGetNext;
		End;
	'>' :
		Begin
			AppendToOutput('>', tkXMLCloseTag);
			fSource.SoftGetNext;
		End;
	'!' :
		Begin
			AppendToOutput('!', tkXMLExclamation);
			fSource.SoftGetNext;
		End;
	'?' :
		Begin
			AppendToOutput('?', tkXMLQuestion);
			fSource.SoftGetNext;
		End;
	'/' :
		Begin
			AppendToOutput('/', tkXMLSlash);
			fSource.SoftGetNext;
		End;
	'=' :
		Begin
			AppendToOutput('=', tkXMLEqual);
			fSource.SoftGetNext;
		End;
	End;
End;

Procedure TXMLScanner.ScanTag;
Begin
	If fSource.Next = '<' Then
		ScanPunct;
	While (fSource.Next <> '>') And Not(fSource.IsEOF) Do
	Begin
		If fSource.Next In [#32, #09, #13, #10] Then
			ScanWhite
		Else If fSource.Next In ['a'..'z', 'A'..'Z', '0'..'9', '-', '_'] Then
			ScanIdentifier
		Else If fSource.Next In ['/', '!', '?', '='] Then
			ScanPunct
		Else If fSource.Next = '"' Then
			ScanString
		Else If fSource.Next In [#13, #10] Then
			fSource.RaiseError('Tag exceeds end of line.')
		Else
			fSource.RaiseError('Unknow char at input.');
	End;
	If fSource.Next = '>' Then
		ScanPunct
	Else
		fSource.RaiseError('Tag exceeds end of file.');
End;

Constructor TXMLScanner.Create(Const aSource : TXMLSource; Const aDestination : TStream; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fSource := aSource;
	fDestination := aDestination;
	fOwnsSource := aOwnsSource;
End;

Destructor TXMLScanner.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	Inherited Destroy;
End;

Procedure TXMLScanner.Scan;
Begin
	fSource.SoftGetNext;
	While Not(fSource.IsEOF) Do
		If fSource.Next = '<' Then
			ScanTag
		Else
			ScanText;
	AppendToOutput('', tkXMLEOF);
End;

Constructor TXMLTokenIterator.Create(Const aSource : TStream; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fSource := aSource;
	fOwnsSource := aOwnsSource;
	fLastPosition := 0;
End;

Destructor TXMLTokenIterator.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	Inherited Destroy;
End;

Procedure TXMLTokenIterator.Start;
Begin
	fSource.Seek(0, soFromBeginning);
	Next;
End;

Procedure TXMLTokenIterator.Next;
Begin
 fLastPosition := fSource.Position;
	If fSource.Position < fSource.Size Then
	Begin
		fKind := TXMLTokenKind(fSource.ReadByte);
		fLiteral := fSource.ReadAnsiString;
		fRow := fSource.ReadDWord;
		fCol := fSource.ReadDWord;
	End
	Else
	Begin
		fKind := tkXMLEOF;
		fLiteral := '';
	End;
	// Debug  WriteLn('Position : ', fSource.Position, ' Size : ', fSource.Size);
	// Debug  WriteLn('Literal : ', fLiteral, ' Kind : ', fKind);
End;

Procedure TXMLTokenIterator.Mark;
Begin
	SetLength(fStack, Length(fStack) + 1);
	fStack[High(fStack)] := fLastPosition;
End;

Procedure TXMLTokenIterator.Recall;
Begin
	fSource.Seek(fStack[High(fStack)], soFromBeginning);
	SetLength(fStack, Length(fStack) - 1);
	Next;
End;

Function TXMLTokenIterator.Expected(Const aLiteral : String; Const aHaltIfNotFound : Boolean = False): Boolean;
Begin
	Result := fLiteral = aLiteral;
	If aHaltIfNotFound And Not(Result) Then
		RaiseError('Expected ' + aLiteral + '.')
End;

Function TXMLTokenIterator.Expected(Const aKind : TXMLTokenKind; Const aHaltIfNotFound : Boolean = False): Boolean;
Begin
	Result := fKind = aKind;
	If aHaltIfNotFound And Not(Result) Then
		Case aKind Of 
			tkXMLWhite       : RaiseError('Expected whitespace (?).');
			tkXMLOpenTag     : RaiseError('Expected "<".');
			tkXMLCloseTag    : RaiseError('Expected ">".');
			tkXMLIdentifier  : RaiseError('Expected identifier.');
			tkXMLText        : RaiseError('Expected text.');
			tkXMLString      : RaiseError('Expected quoted string.');
			tkXMLSlash       : RaiseError('Expected "/".');
			tkXMLExclamation : RaiseError('Expected "!".');
			tkXMLQuestion    : RaiseError('Expected "?".');
			tkXMLEqual       : RaiseError('Expected "=".'); 
			tkXMLEOF         : RaiseError('Expected end of file.'); 
		End;
End;

Function TXMLTokenIterator.Expected(Const aKinds : Array Of TXMLTokenKind; Const aHaltIfNotFound : Boolean = False): Boolean;
Var
	lCtrl : Integer;

	Function ExpectedNames: String;
	Var
		lCtrl2 : Integer;
	Begin
		Result := 'Expected ';
		If Length(aKinds) = 1 Then
			Case aKinds[Low(aKinds)] Of 
				tkXMLWhite       : Result := Result + 'whitespace';
				tkXMLOpenTag     : Result := Result + '"<"';
				tkXMLCloseTag    : Result := Result + '">"';
				tkXMLIdentifier  : Result := Result + 'identifier';
				tkXMLText        : Result := Result + 'text';
				tkXMLString      : Result := Result + 'quoted string';
				tkXMLSlash       : Result := Result + '"/"';
				tkXMLExclamation : Result := Result + '"!"';
				tkXMLQuestion    : Result := Result + '"?"';
				tkXMLEqual       : Result := Result + '"="'; 
				tkXMLEOF         : Result := Result + 'end of file'; 
			End
		Else
		If Length(aKinds) = 2 Then
		Begin
			Case aKinds[Low(aKinds)] Of 
				tkXMLWhite       : Result := Result + 'whitespace, ';
				tkXMLOpenTag     : Result := Result + '"<", ';
				tkXMLCloseTag    : Result := Result + '">", ';
				tkXMLIdentifier  : Result := Result + 'identifier, ';
				tkXMLText        : Result := Result + 'text, ';
				tkXMLString      : Result := Result + 'quoted string, ';
				tkXMLSlash       : Result := Result + '"/", ';
				tkXMLExclamation : Result := Result + '"!", ';
				tkXMLQuestion    : Result := Result + '"?", ';
				tkXMLEqual       : Result := Result + '"=", '; 
				tkXMLEOF         : Result := Result + 'end of file, '; 
			End;
			Case aKinds[High(aKinds)] Of 
				tkXMLWhite       : Result := Result + 'whitespace';
				tkXMLOpenTag     : Result := Result + '"<"';
				tkXMLCloseTag    : Result := Result + '">"';
				tkXMLIdentifier  : Result := Result + 'identifier';
				tkXMLText        : Result := Result + 'text';
				tkXMLString      : Result := Result + 'quoted string';
				tkXMLSlash       : Result := Result + '"/"';
				tkXMLExclamation : Result := Result + '"!"';
				tkXMLQuestion    : Result := Result + '"?"';
				tkXMLEqual       : Result := Result + '"="'; 
				tkXMLEOF         : Result := Result + 'end of file'; 
			End
		End
		Else
			For lCtrl2 := Low(aKinds) To High(aKinds) Do
			Begin
				Case aKinds[lCtrl2] Of 
					tkXMLWhite       : Result := Result + 'whitespace';
					tkXMLOpenTag     : Result := Result + '"<"';
					tkXMLCloseTag    : Result := Result + '">"';
					tkXMLIdentifier  : Result := Result + 'identifier';
					tkXMLText        : Result := Result + 'text';
					tkXMLString      : Result := Result + 'quoted string';
					tkXMLSlash       : Result := Result + '"/"';
					tkXMLExclamation : Result := Result + '"!"';
					tkXMLQuestion    : Result := Result + '"?"';
					tkXMLEqual       : Result := Result + '"="'; 
					tkXMLEOF         : Result := Result + 'end of file'; 
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
	For lCtrl := Low(aKinds) To High(aKinds) Do
		Result := Expected(aKinds[lCtrl]);
	If aHaltIfNotFound And Not(Result) Then
			RaiseError(ExpectedNames);
End;

Function TXMLTokenIterator.Consume(Const aLiteral : String; Const aHaltIfNotFound : Boolean = True): Boolean;
Begin
	// Debug WriteLn('Consuming ', aLiteral);
	Result := Expected(aLiteral, aHaltIfNotFound);
	If Result Then
		Next;
End;

Function TXMLTokenIterator.Consume(Const aKind : TXMLTokenKind; Const aHaltIfNotFound : Boolean = True): Boolean;
Begin
	// Debug WriteLn('Consuming ', aKind);
	Result := Expected(aKind, aHaltIfNotFound);
	If Result Then
		Next;
End;

Function TXMLTokenIterator.ConsumeElse(Const aLiteral, aAlternative : String): Boolean;
Begin
	Result := Consume(aLiteral, False);
	If Not(Result) Then
		Consume(aAlternative, True);
End;

Function TXMLTokenIterator.Match(Const aKinds : Array Of TXMLTokenKind): Boolean;
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

Function TXMLTokenIterator.Extract(Const aKind : TXMLTokenKind): String;
Begin
	Result := fLiteral;
	Consume(aKind);
End;

Function TXMLTokenIterator.IsEOS: Boolean;
Begin
	Result := fSource.Position >= fSource.Size;
End;

Procedure TXMLTokenIterator.RaiseError(Const aMsg : String);
Var
	lException : TRowColException;
Begin
	lException := TRowColException.Create(aMsg);
	lException.Row := fRow;
	lException.Col := fCol;
	Raise lException;
End;

End.