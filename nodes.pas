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
	Nodes;

Interface

Uses
	Classes,
	Tree,
	XMLNodes;

Type
	TSystemNode = Class(TXMLNode)
	Public
		Constructor Create(Const aOwner : TTreeNode); Override;
	End;

Implementation

Constructor TSystemNode.Create(Const aOwner : TTreeNode);
Begin
	Inherited Create(aOwner);
	Factory.Register('config', TXMLNode);
	SetAsCapable('TListCommand');
	SetAsCapable('TParseTemplate');
	ReadOnly := True;
End;

End.