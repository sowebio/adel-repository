---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:41 PM BST  --
---------------------------------------------------------------------
--[class_account.ads] Specification
package Class_Account is

  type Account is private;
  subtype Money  is Float;
  subtype Pmoney is Float range 0.0 .. Float'Last;

  procedure Deposit ( The:in out Account; Amount:in Pmoney );
  procedure Withdraw( The:in out Account; Amount:in Pmoney;
                      Get:out Pmoney );
  function  Balance ( The:in Account ) return Money;

private
  type Account is record
    Balance_Of : Money := 0.00;      -- Amount in account
  end record;
end Class_Account;


--[class_account.adb] Implementation
package body Class_Account is


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

end Class_Account;

--[class_account_other.ads] Specification
package Class_Account_Other is
  type Account is private;
  subtype Money  is Float;
  procedure Statement( The:in Account );

private
  type Account is record
    Amount : Money := 0.00;      -- Amount in account
  end record;
end Class_Account_Other;


--[class_account_other.adb] Implementation
with Ada.Text_Io, Ada.Float_Text_Io;
use  Ada.Text_Io, Ada.Float_Text_Io;
package body Class_Account_Other is

  procedure Statement( The:in Account ) is
  begin
    Put("Other account system "); New_Line(2);
  end Statement;

end Class_Account_Other;

--[Statement.adb] Procedure
with Ada.Text_Io, Ada.Float_Text_Io, Class_Account;
use  Ada.Text_Io, Ada.Float_Text_Io, Class_Account;
procedure Statement( An_Account:in Account ) is
begin
  Put("Mini statement: The amount on deposit is $" );
  Put( Balance( An_Account), Aft=>2, Exp=>0 );
  New_Line(2);
end Statement;

--[main1.adb] Procedure
with Ada.Text_Io, Class_Account, Statement;
use  Ada.Text_Io, Class_Account;
procedure Main1 is
  My_Account: Account;
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
with Ada.Text_Io, Class_Account, Statement;
procedure Main2 is
  My_Account: Class_Account.Account;
  Obtain    : Class_Account.Money;
begin
  Statement( My_Account );

  Ada.Text_Io.Put("Deposit $100.00 into account");
  Ada.Text_Io.New_Line;
  Class_Account.Deposit( My_Account, 100.00 );
  Statement( My_Account );

  Ada.Text_Io.Put("Withdraw $80.00 from account");
  Ada.Text_Io.New_Line;
  Class_Account.Withdraw( My_Account, 80.00, Obtain );
  Statement( My_Account );

  Ada.Text_Io.Put("Deposit $200.00 into account");
  Ada.Text_Io.New_Line;
  Class_Account.Deposit( My_Account, 200.00 );
  Statement( My_Account );

end Main2;


--[main3.adb] Procedure
with Class_Account, Class_Account_Other, Statement;
use  Class_Account, Class_Account_Other;
procedure Main3 is
  My_Account    :Class_Account.Account;
  Other_Account :Class_Account_Other.Account;
begin
  Statement( My_Account );    -- statement in Class_account
  Statement( Other_Account ); -- statement in Class_account_other
end Main3;

--[main4.adb] Procedure
with Class_Account;
use  Class_Account;
procedure Main4 is
  My_Account   : Account;
  Other_Account: Account;
  Obtain       : Pmoney;
begin
  Deposit( My_Account, 100.00 );
  Other_Account := My_Account;            -- Copy and
  Withdraw(Other_Account, 100.00, Obtain);-- Withdraw 100.00

  Other_Account := My_Account;            -- Copy again and
  Withdraw(Other_Account, 100.00, Obtain);-- Withdraw 100.00

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
