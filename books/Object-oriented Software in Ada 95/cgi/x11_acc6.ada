---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:44 PM BST  --
---------------------------------------------------------------------
-- Initialisation of an object item
-- Remember discriminants must have descreit or access types
--
--[class_account.ads] Specification
package Class_Account is

  subtype Money  is Float;
  subtype Pmoney is Float range 0.0 .. Float'Last;
  type Account( Number: Natural:= 0 ) is private;

  procedure Statement( The:in Account );
  procedure Deposit ( The:in out Account; Amount:in Pmoney );
  procedure Withdraw( The:in out Account; Amount:in Pmoney;
    Get:out Pmoney );
  function  Balance ( The:in Account ) return Money;
  procedure New_Number( The: in out Account; N:in Natural );
  function  New_Account( N:in Natural;
    Amount:in Pmoney:=0.0 ) return Account;

private
  type Account( Number: Natural:= 0) is record
    Balance_Of : Float := 0.00;
  end record;
end Class_Account;


--[class_account.adb] Implementation
with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io;
package body Class_Account is

  procedure Statement( The:in Account ) is
  begin
    Put("Mini statement: Account #"); Put( The.Number ); New_Line;
    Put("The amount on deposit is $" );
    Put( The.Balance_Of, Aft=>2, Exp=>0 );
    New_Line(2);
  end Statement;

  procedure Deposit ( The:in out Account; Amount:in Pmoney ) is
  begin
    The.Balance_Of := The.Balance_Of + Amount;
  end Deposit;

  procedure Withdraw( The:in out Account; Amount:in Pmoney;
      Get:out Pmoney ) is
  begin
    if The.Balance_Of >= Amount then
      The.Balance_Of := The.Balance_Of - Amount;
      Get := Amount;
    else
      Get := 0.00;
    end if;
  end Withdraw;

  function  Balance( The:in Account ) return Money is
  begin
    return The.Balance_Of;
  end Balance;

  procedure New_Number( The: in out Account; N:in Natural ) is
  begin
    The := Account'( N, The.Balance_Of );
  end New_Number;

  function  New_Account( N:in Natural;
      Amount:in Pmoney:=0.0 ) return Account is
    An_Account : Account := Account'( N, Amount );
  begin
    return An_Account;
  end New_Account;

end Class_Account;


--[main1.adb] Procedure
with Ada.Text_Io, Class_Account;
use  Ada.Text_Io, Class_Account;
procedure Main1 is
  My_Account: Account(10001);
  Obtain    : Money;
begin
  Statement( My_Account );

  Put("Deposit $100.00 into account"); New_Line;  -- Deposit
  Deposit( My_Account, 100.00 );
  Statement( My_Account );

  Put("Withdraw $80.00 from account"); New_Line;  -- Withdraw
  Withdraw( My_Account, 80.00, Obtain );
  Statement( My_Account );

  Put("Deposit $200.00 into account"); New_Line;  -- Deposit
  Deposit( My_Account, 200.00 );
  Statement( My_Account );

end Main1;


--[main2.adb] Procedure
with Ada.Text_Io, Class_Account;
use  Ada.Text_Io, Class_Account;
procedure Main2 is
  My_Account: Account(10001);
begin
  begin
    Deposit( My_Account, 200.00 );
    Statement( My_Account );
    New_Number( My_Account, 10002 );
    Statement( My_Account );
  exception
    when Constraint_Error =>
      Put("raised CONSTRAINT_ERROR It when wrong"); New_Line;
    when others =>
      Put("Unknow error"); New_Line;
  end;
end Main2;


--[main3.adb] Procedure
with Class_Account;
use  Class_Account;
procedure Main3 is
  My_Account : Account := New_Account( 10001, 20.0 );
begin
  Statement( My_Account );
end Main3;

--[main4.adb] Procedure
procedure Main4 is
begin
  null;
end Main4;

--[main.adb] Procedure
with Ada.Text_Io, Main1, Main2, Main3, Main4;
use  Ada.Text_Io;
procedure Main is
begin
  Put("Example Account 1 "); New_Line; Main1;
  Put("Example Account 2 "); New_Line; Main2;
  Put("Example Account 3 "); New_Line; Main3;
  Put("Example Account 4 "); New_Line; Main4;
end Main;
