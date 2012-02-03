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
	XMLLoader;

Interface

Uses
	Classes,
	SysUtils,
	BaseException,
	Tree,
	StrUtils,
	XMLNodes,
	XMLParser,
	XMLScanner;

Type
	TXMLRootNode = Class(TXMLNode)
	Public
		Function IndexedName: String; Override;
		Function AsXML: String; Override;
		Function AsClosingXML: String; Override;
		Procedure LoadFromFile(Const aFileName : String);
	End;

Implementation

Function TXMLRootNode.IndexedName: String;
Begin
	Result := '';
End;

Function TXMLRootNode.AsXML: String;
Begin
	Result := '';
End;

Function TXMLRootNode.AsClosingXML: String;
Begin
	Result := '';
End;

Procedure TXMLRootNode.LoadFromFile(Const aFileName : String);
Var
	lFile : TFileStream;
	lSource : TXMLSource;
	lTokens : TMemoryStream;
	lScanner : TXMLScanner;
	lTokenIterator : TXMLTokenIterator;
	lParser : TXMLParser;
Begin
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

End.