{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 - J. Aldo G. de Freitas Junior

{$mode objfpc}
{$H+}

Unit
	StringArray;

Interface

Type
	TStringArray = Array Of String;

Procedure AppendString(Var aStrings : TStringArray; Const aString : String);
Function ConcatenateStrings(Const aStrings : TStringArray; Const aSeparator : String = ', '; Const aLastSeparator : String = ', '): String;
Function IsMember(Const aStringArray : TStringArray; Const aString : String): Boolean;
Procedure AddNewString(Var aStrings : TStringArray; Const aString : String);

Implementation

Procedure AppendString(Var aStrings : TStringArray; Const aString : String);
Begin
	SetLength(aStrings, Length(aStrings) + 1);
	aStrings[High(aStrings)] := aString;
End;

Function ConcatenateStrings(Const aStrings : TStringArray; Const aSeparator : String = ', '; Const aLastSeparator : String = ', '): String;
Var
	lCtrl : Integer;
Begin
	Result := '';
	For lCtrl := Low(aStrings) To High(aStrings) Do
	If lCtrl < (High(aStrings) - 1) Then
		Result := Result + aStrings[lCtrl]  + aSeparator
	Else If lCtrl < High(aStrings) Then
		Result := Result + aStrings[lCtrl]  + aLastSeparator
	Else
		Result := Result + aStrings[lCtrl];
End;

Function IsMember(Const aStringArray : TStringArray; Const aString : String): Boolean;
Var
	lCtrl : Integer;
Begin
	Result := False;
	For lCtrl := Low(aStringArray) To High(aStringArray) Do
		If aStringArray[lCtrl] = aString Then
		Begin
			Result := True;
			Break;
		End;
End;

Procedure AddNewString(Var aStrings : TStringArray; Const aString : String);
Begin
	If Not IsMember(aStrings, aString) Then
		AppendString(aSTrings, aString);
End;

End.