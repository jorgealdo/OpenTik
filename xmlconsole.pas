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

Program
	XMLConsole;

Uses
	Classes,
	SysUtils,
	BaseException,
	Stacks,
	StringArray,
	StrUtils,
	XMLNodes,
	XMLLoader,
	ExprNodes,
	CmdNodes,
	CmdLoader,
	Commands,
	Functions,
	Nodes;

Const
	ccConfigFile = 'config.xml';

Var
	gConfig : TXMLRootNode;
	gStack  : TVirtualMachineStack;
	gCommandLine : String;
	gCommandLoader : TCmdRootNode;
	gNodeFunctions : TNodeFunctions;

Procedure RunInteractive;
Begin
	Repeat
		Try
			Write(gStack.Focused.IndexedName, '>');
			ReadLn(gCommandLine);
			Try	
				gCommandLoader := TCmdRootNode.Create(Nil);
				gCommandLoader.ClassFactory.Register('print', TPrintCommand);
				gCommandLoader.ClassFactory.Register('list', TListCommand);
				gCommandLoader.ClassFactory.Register('template', TParseTemplate);
				gCommandLoader.ClassFactory.Register('add', TAddCommand);
				gCommandLoader.DoLoadCommand(gCommandLine, gStack);
				gCommandLoader.Evaluate;
			Finally
				FreeAndNil(gCommandLoader);
			End;
		Except
			On E: Exception Do
				WriteLn(E.Message);
		End;
	Until Not gStack.Running;
End;

Begin
	gConfig := TXMLRootNode.Create(Nil);
	gStack := TVirtualMachineStack.Create;
	gNodeFunctions := TNodeFunctions.Create;
	Try
		Try
			gConfig.Factory.Register('system', TSystemNode);
			gConfig.LoadFromFile(ccConfigFile);
			gConfig.SetAsCapable('TListCommand');
			gConfig.SetAsCapable('TParseTemplate');
			gConfig.ReadOnly := True;
			gConfig.AfterLoad;
			gStack.RegisterFunctor(gNodeFunctions);
			gNodeFunctions.Stack := gStack;
			gStack.Focused := gConfig;
			gStack.Running := True;
			If ParamCount = 0 Then
				RunInteractive;
		Finally
			FreeAndNil(gNodeFunctions);
			FreeAndNil(gConfig);
			FreeAndNil(gStack);
		End;
	Except
		On E: Exception Do
			WriteLn(E.Message);
	End;
End.
