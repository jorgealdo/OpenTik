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
	TabularData;

Interface

Uses
	Classes,
	StrUtils,
	StringArray;

Type
	TTabularData = Class
	Private
		fHeaders : TStringArray;
		fLines : Array Of TStringArray;
		fSizes : Array Of Integer;
	Public
		Procedure Clear;
		Procedure AddHeader(Const aHeader : String);
		Procedure AdjustColumns;
		Function FindOrAddHeader(Const aHeader : String): Integer;
		Procedure SetData(Const aHeader, aData : String);
		Procedure NewLine;
		Procedure CalculateSizes;
		Procedure Print(Const aStopSize : Integer = 24);
	End;

Implementation

Procedure TTabularData.Clear;
Begin
	SetLength(fHeaders, 0);
	SetLength(fLines, 0);
	SetLength(fSizes, 0);
End;

Procedure TTabularData.AddHeader(Const aHeader : String);
Var
	lCtrl : Integer;
	lFound : Boolean;
Begin
	// Debug  WriteLn('AddHeader;');
	For lCtrl := Low(fHeaders) To High(fHeaders) Do
		If fHeaders[lCtrl] = aHeader Then
		Begin
			lFound := True;
			Break;
		End;
	If Not lFound Then
	Begin
		SetLength(fHeaders, Length(fHeaders) + 1);
		fHeaders[High(fHeaders)] := aHeader;
	End;
End;

Procedure TTabularData.AdjustColumns;
Var
	lCtrl : Integer;
Begin
	// Debug  WriteLn('AdjustColumns;');
	For lCtrl := Low(fLines) To High(fLines) Do
	Begin
		// Debug  WriteLn('lCtrl ', lCtrl, ' Length ', Length(fLines));
		SetLength(fLines[lCtrl], Length(fHeaders));
		// Debug  WriteLn('SetLength');
	End;
End;

Function TTabularData.FindOrAddHeader(Const aHeader : String): Integer;
Var
	lCtrl : Integer;
	lFound : Boolean;
Begin
	// Debug  WriteLn('FindOrAddHeader;');
	lFound := False;
	Result := -1;
	For lCtrl := Low(fHeaders) To High(fHeaders) Do
		If fHeaders[lCtrl] = aHeader Then
		Begin
			lFound := True;
			Result := lCtrl;
			Break;
		End;
	If Not lFound Then
	Begin
		SetLength(fHeaders, Length(fHeaders) + 1);
		fHeaders[High(fHeaders)] := aHeader;
		Result := High(fHeaders);
	End;
	AdjustColumns;
End;

Procedure TTabularData.SetData(Const aHeader, aData : String);
Begin
	// Debug  WriteLn('SetData;');
	// Debug  WriteLn(FindOrAddHeader(aHeader), ' ', Length(fLines[High(fLines)]));
	FindOrAddHeader(aHeader);
	fLines[High(fLines)][FindOrAddHeader(aHeader)] := aData;
End;

Procedure TTabularData.NewLine;
Begin
	// Debug  WriteLn('NewLine;');
	SetLength(fLines, Length(fLines) + 1);
	SetLength(fLines[High(fLines)], Length(fHeaders));
End;

Procedure TTabularData.CalculateSizes;
Var
	lX, lY : Integer;
Begin
	// Debug  WriteLn('CalculateSizes;');
	SetLength(fSizes, Length(fHeaders));
	For lX := Low(fHeaders) To High(fHeaders) Do
		fSizes[lX] := Length(fHeaders[lX]);
	For lY := Low(fLines) To High(fLines) Do
		For lX := Low(fHeaders) To High(fHeaders) Do
			If Length(fLines[lY][lX]) > fSizes[lX] Then
				fSizes[lX] := Length(fLines[lY][lX]);
End;

Procedure TTabularData.Print(Const aStopSize : Integer = 24);
Var
	lX,
	lY : Integer;
Begin
	// Debug  WriteLn('Print;');
	CalculateSizes;
	// Debug  WriteLn('Length ', Length(fHeaders));
	For lX := Low(fHeaders) To High(fHeaders) Do
		Write('|', UpCase(PadCenter(fHeaders[lX], fSizes[lX])));
	// Debug  WriteLn('Length Lines ', Length(fLines));
	WriteLn;
	For lY := Low(fLines) To High(fLines) Do
	Begin
		For lX := Low(fLines[lY]) To High(fLines[lY]) Do
		Begin
			// Debug  Write(ly, ',', lx);
			Write('|', PadRight(fLines[lY][lX], fSizes[lX]));
		End;
		WriteLn;
	End;
End;

End.