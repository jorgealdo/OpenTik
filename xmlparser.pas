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
	XMLParser;
	
Interface

Uses
	Classes,
	SysUtils,
	StrUtils,
	BaseException,
	XMLScanner,
	XMLNodes;

Type
	TXMLParser = Class
	Private
		fOwnsSource : Boolean;
		fSource : TXMLTokenIterator;
		fDestination : TXMLNode;
		Procedure ParseTagList(Const aPrevious : TXMLNode);
		Procedure ParseTag(Const aPrevious : TXMLNode);
		Procedure ParsePropertyList(Const aPrevious : TXMLNode);
		Procedure ParseProperty(Const aPrevious : TXMLNode);
		Procedure ParseText(Const aPrevious : TXMLNode);
		Procedure ParseSpecialTag1(Const aPrevious : TXMLNode);
		Procedure ParseSpecialTag2(Const aPrevious : TXMLNode);
	Public
		Constructor Create(Const aSource : TXMLTokenIterator; Const aDestination : TXMLNode; Const aOwnsSource : Boolean = True); Virtual;
		Destructor Destroy; Override;
		Procedure Parse;
	End;

Implementation

Procedure TXMLParser.ParseTagList(Const aPrevious : TXMLNode);
Begin
	// Debug  WriteLn('Parsing tag list.');
	While Not(fSource.IsEOS Or (fSource.Kind = tkXMLEOF)) Do
		If fSource.Match([tkXMLText]) Then
			ParseText(aPrevious)
		Else If fSource.Match([tkXMLOpenTag, tkXMLIdentifier]) Then
			ParseTag(aPrevious)
		Else If fSource.Match([tkXMLOpenTag, tkXMLQuestion]) Then
			ParseSpecialTag1(aPrevious)
		Else If fSource.Match([tkXMLOpenTag, tkXMLExclamation]) Then
			ParseSpecialTag2(aPrevious)
		Else If fSource.Match([tkXMLOpenTag, tkXMLSlash]) Then
			Exit
		Else
			fSource.RaiseError('Expected tag or text.');
	// Debug  WriteLn('Done.');
End;

Procedure TXMLParser.ParseTag(Const aPrevious : TXMLNode);
Var
	lTag : TXMLNode;
Begin
	// Debug  WriteLn('Parsing tag.');
	fSource.Consume(tkXMLOpenTag);
	lTag := aPrevious.Factory.Build(fSource.Literal, aPrevious) As TXMLNode;
	lTag.Row := fSource.Row;
	lTag.Col := fSource.Col;
	lTag.Name := fSource.Extract(tkXMLIdentifier);
	ParsePropertyList(lTag);
	If fSource.Expected(tkXMLSlash) Then
	Begin
		fSource.Consume(tkXMLSlash);
		fSource.Consume(tkXMLCloseTag);
	End
	Else
	Begin
		fSource.Consume(tkXMLCloseTag);
		ParseTagList(lTag);
		fSource.Consume(tkXMLOpenTag);
		fSource.Consume(tkXMLSlash);
		fSource.Consume(lTag.Name);
		fSource.Consume(tkXMLCloseTag);
	End;
	// Debug  WriteLn('Done.');
End;

Procedure TXMLParser.ParsePropertyList(Const aPrevious : TXMLNode);
Begin
	// Debug  WriteLn('Parsing property list.');
	While Not(fSource.Kind In [tkXMLExclamation, tkXMLQuestion, tkXMLSlash, tkXMLCloseTag]) Do
	Begin
		fSource.Consume(tkXMLWhite, False);
		ParseProperty(aPrevious);
		If fSource.Expected(tkXMLEOF) Or fSource.IsEOS Then
			fSource.RaiseError('Unexpected end of file.');
	End;
	// Debug  WriteLn('Done.');
End;

Procedure TXMLParser.ParseProperty(Const aPrevious : TXMLNode);
Var
	lName,
	lValue : String;
Begin
	// Debug  WriteLn('Parsing property.');
	lName := fSource.Extract(tkXMLIdentifier);
	fSource.Consume(tkXMLEqual);
	lValue := fSource.Extract(tkXMLString);
	aPrevious.Properties.SetValue(lName, lValue);
	// Debug  WriteLn('Done.');
End;

Procedure TXMLParser.ParseText(Const aPrevious : TXMLNode);
Var
	lTag : TXMLTextNode;
	lText : String;
Begin
	// Debug  WriteLn('Parsing text.');
	lText := fSource.Extract(tkXMLText);
	lText := StringReplace(lText, #13, '', [ rfReplaceAll ]);
	lText := StringReplace(lText, #10, '', [ rfReplaceAll ]);
	lText := StringReplace(lText, #09, ' ', [ rfReplaceAll ]);
	lText := DelSpace1(lText);
	If Not IsEmptyStr(lText, [ ' ' ]) Then
	Begin
		lTag := TXMLTextNode.Create(aPrevious);
		lTag.Content := lText;
	End;
	// Debug  WriteLn('Done.');
End;

Procedure TXMLParser.ParseSpecialTag1(Const aPrevious : TXMLNode);
Var
	lTag : TXMLSpecialTag1Node;
Begin
	// Debug  WriteLn('Parsing special tag (?).');
	fSource.Consume(tkXMLOpenTag);
	fSource.Consume(tkXMLQuestion);
	lTag := TXMLSpecialTag1Node.Create(aPrevious);
	lTag.Name := fSource.Extract(tkXMLIdentifier);
	ParsePropertyList(lTag);
	fSource.Consume(tkXMLQuestion);
	fSource.Consume(tkXMLCloseTag);
	// Debug  WriteLn('Done.');
End;

Procedure TXMLParser.ParseSpecialTag2(Const aPrevious : TXMLNode);
Var
	lTag : TXMLSpecialTag2Node;
Begin
	// Debug  WriteLn('Parsing special tag (!).');
	fSource.Consume(tkXMLOpenTag);
	fSource.Consume(tkXMLExclamation);
	lTag := TXMLSpecialTag2Node.Create(aPrevious);
	lTag.Name := fSource.Extract(tkXMLIdentifier);
	ParsePropertyList(lTag);
	fSource.Consume(tkXMLExclamation);
	fSource.Consume(tkXMLCloseTag);
	// Debug  WriteLn('Done.');
End;

Constructor TXMLParser.Create(Const aSource : TXMLTokenIterator; Const aDestination : TXMLNode; Const aOwnsSource : Boolean = True);
Begin
	Inherited Create;
	fOwnsSource := aOwnsSource;
	fSource := aSource;
	fDestination := aDestination;
End;

Destructor TXMLParser.Destroy;
Begin
	If fOwnsSource Then
		fSource.Free;
	Inherited Destroy;
End;

Procedure TXMLParser.Parse;
Begin
	fSource.Start;
	// Debug  WriteLn('Parsing file.');
	ParseTagList(fDestination);
	// Debug  WriteLn('Done.');
	fSource.Consume(tkXMLEOF);
End;

End.