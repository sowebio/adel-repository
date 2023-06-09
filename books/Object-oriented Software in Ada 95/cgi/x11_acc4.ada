---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:42 PM BST  --
---------------------------------------------------------------------
--[class_abstract_account.ads] Specification
package Class_Abstract_Account is

  type Abstract_Account is abstract tagged null record;
  subtype Money  is Float;
  subtype Pmoney is Float range 0.0 .. Float'Last;

  procedure Deposit  ( The:in out Abstract_Account;
                       Amount:in Pmoney ) is abstract;
  procedure Withdraw ( The:in out Abstract_Account;
                       Amount:in Pmoney;
                       Get:out Pmoney ) is abstract;
  function  Balance  ( The:in Abstract_Account )
                       return Money is abstract;
end Class_Abstract_Account;

-- =======================================================

-- FIX
-- TYPE Abstract_account  IS TAGGED PRIVATE;
--  MUST BE
-- TYPE Abstract_account  IS ABSTRACT TAGGED PRIVATE;

--[class_abstract_account.ads] Specification
package Class_Abstract_Account_Other is

  type Abstract_Account  is abstract tagged private;
  type Abstract_Account2 is abstract tagged private;
  subtype Money  is Float;
  subtype Pmoney is Float range 0.0 .. Float'Last;

  procedure Deposit  ( The:in out Abstract_Account;
                       Amount:in Pmoney ) is abstract;
  procedure Withdraw ( The:in out Abstract_Account;
                       Amount:in Pmoney;
                       Get:out Pmoney ) is abstract;
  function  Balance  ( The:in Abstract_Account )
                       return Money is abstract;
private
  type Abstract_Account  is abstract tagged record null; end record;
  type Abstract_Account2 is abstract tagged null record;
end Class_Abstract_Account_Other;

-- =======================================================

--[class_account.ads] Specification
with Class_Abstract_Account;
use  Class_Abstract_Account;
package Class_Account is

  type Account is new Abstract_Account with private;
  subtype Money  is Class_Abstract_Account.Money;
  subtype Pmoney is Class_Abstract_Account.Pmoney;

  procedure Deposit  ( The:in out Account; Amount:in Pmoney );
  procedure Withdraw ( The:in out Account; Amount:in Pmoney;
                       Get:out Pmoney );
  function  Balance  ( The:in Account ) return Money;
private
  type Account is new Abstract_Account with record
    Balance_Of : Money := 0.00;       -- Amount in account
  end record;
end Class_Account;


--[class_account.adb] Implementation
package body Class_Account is


  procedure Deposit( The:in out Account; Amount:in Pmoney ) is
  begin
    The.Balance_Of := The.Balance_Of + Amount;
  end Deposit;

  procedure Withdraw( The:in out Account;
      Amount:in Pmoney; Get:out Pmoney ) is
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

-- =======================================================

--[class_interest_account.ads] Specification
with Class_Account;
use  Class_Account;
package Class_Interest_Account is

  type Interest_Account is new Account with private;

  procedure Set_Rate( Rate:in Float );
  procedure Calc_Interest( The:in out Interest_Account );
  procedure Add_Interest( The:in out Interest_Account );
private
  Daily_Interest_Rate: constant Float := 0.00026116; -- 10%
  type Interest_Account is new Account with record
    Accumulated_Interest : Money := 0.00;            -- To date
  end record;
  The_Interest_Rate      : Float := Daily_Interest_Rate;
end Class_Interest_Account;


--[class_interest_account.adb] Implementation
package body Class_Interest_Account is

  procedure Set_Rate( Rate:in Float ) is
  begin
    The_Interest_Rate := Rate;
  end Set_Rate;

  procedure Calc_Interest( The:in out Interest_Account ) is
  begin
    The.Accumulated_Interest := The.Accumulated_Interest +
      Balance(The) * The_Interest_Rate;
  end Calc_Interest;

  procedure Add_Interest( The:in out Interest_Account ) is
  begin
    Deposit( The, The.Accumulated_Interest );
    The.Accumulated_Interest := 0.00;
  end Add_Interest;

end Class_Interest_Account;

-- =======================================================
--[class_account_ltd.ads] Specification
with Class_Account;
use  Class_Account;
package Class_Account_Ltd is

  type Account_Ltd is new Account with private;

  procedure Withdraw ( The:in out Account_Ltd;
                       Amount:in Pmoney; Get:out Pmoney );
  procedure Reset( The:in out Account_Ltd );
private
  Withdrawals_In_A_Week : Natural := 3;
  type Account_Ltd is new Account with record
    Withdrawals : Natural := Withdrawals_In_A_Week;
  end record;
end Class_Account_Ltd;


--[class_account_ltd.adb] Implementation
package body Class_Account_Ltd is

  procedure Withdraw ( The:in out Account_Ltd;
      Amount:in Pmoney; Get:out Pmoney ) is
  begin
    if The.Withdrawals > 0 then               -- Not limit
      The.Withdrawals := The.Withdrawals - 1;
      Withdraw( Account(The), Amount, Get );  -- In Account
    else
      Get := 0.00;                             -- Sorry
    end if;
  end Withdraw;

  procedure Reset( The:in out Account_Ltd ) is
  begin
    The.Withdrawals := Withdrawals_In_A_Week;
  end Reset;

end Class_Account_Ltd;

with Ada.Text_Io, Ada.Float_Text_Io, Class_Account;
use  Ada.Text_Io, Ada.Float_Text_Io, Class_Account;
procedure Statement( An_Account:in Account ) is
begin
  Put("Mini statement: The amount on deposit is $" );
  Put( Balance( An_Account), Aft=>2, Exp=>0 );
  New_Line(2);
end Statement;


-- =======================================================
-- Can not have abstract account as now two definitions for PMoney

--[pack_procedures.ads] Specification
with Ada.Text_Io, Ada.Float_Text_Io,
     Class_Account, Class_Interest_Account, Class_Account_Ltd,
     Statement;
use  Ada.Text_Io, Ada.Float_Text_Io,
     Class_Account, Class_Interest_Account, Class_Account_Ltd;
package Pack_Procedures is
  procedure Ex1;
  procedure Ex2;
  procedure Ex3;
  procedure Ex4;
end Pack_Procedures;


--[pack_procedures.adb] Implementation
package body Pack_Procedures is

  procedure Ex1 is
    Mike  : Interest_Account;
    Obtain: Money;
  begin
    Statement( Account(Mike) );
    Withdraw( Mike, 100.00, Obtain );   -- Withdraw some money
    Statement( Account(Mike) );
    Deposit( Account(Mike), 300.00 );   -- In credit
    Statement( Account(Mike) );
  end Ex1;

  procedure Ex2 is
    Mike  : Account_Ltd;
    Obtain: Money;
  begin
    Deposit( Mike, 300.00 );          -- In credit
    Statement( Account(Mike) );
    Withdraw( Mike, 100.00, Obtain ); -- Withdraw some money
    Withdraw( Mike,  10.00, Obtain ); -- Withdraw some money
    Withdraw( Mike,  10.00, Obtain ); -- Withdraw some money
    Withdraw( Mike,  20.00, Obtain ); -- Withdraw some money
    Statement( Account(Mike) );
  end Ex2;

  procedure Ex3 is
    Mike  : Account_Ltd;
    Obtain: Money;
  begin
    Deposit( Mike, 300.00 );          -- In credit
    Statement( Account(Mike) );
    Withdraw( Mike, 100.00, Obtain ); -- Withdraw some money
    Statement( Account(Mike) );
    Withdraw( Mike,  10.00, Obtain ); -- Withdraw some money
    Statement( Account(Mike) );
    Withdraw( Mike,  10.00, Obtain ); -- Withdraw some money
    Statement( Account(Mike) );
    Withdraw( Account(Mike),  20.00, Obtain ); -- Cheat
    Statement( Account(Mike) );
  end Ex3;

  procedure Ex4 is
    Max_Accounts : constant := 5;
    type P_Account is access all Account'Class;

    type    Bank_Index is range 1 .. Max_Accounts;
    type    Bank_Array is array ( Bank_Index ) of P_Account;

    procedure Print_Statement( No:in Bank_Index;
                               An_Account:in Account'Class ) is
    begin
      Put("Mini Statement for account number #" & 
          Integer'Image(Integer(No)) );
      New_Line;
      Put("The amount in deposit is $" );
      Put( Balance( An_Account ), Aft=>2, Exp=>0 );
      New_Line(2);
    end Print_Statement;

    Piggy    : Bank_Array;
    Obtained : Money;

  begin
    Piggy(1) := new Account;
    Piggy(2) := new Account_Ltd;
    Piggy(3) := new Interest_Account;
    Piggy(4) := new Interest_Account;
    Piggy(5) := new Account;


    Deposit( Piggy(1).all, 100.00 );        -- deposit $100 #1
    Deposit( Piggy(3).all, 100.00 );        -- deposit $100 #3

    Withdraw( Piggy(2).all, 50.00, Obtained ); -- withdraw $50 #2
    Withdraw( Piggy(4).all, 50.00, Obtained ); -- withdraw $50 #4

    for I in Bank_Index loop
      Print_Statement( I, Piggy(I).all );
    end loop;
    null;
  end Ex4;

end Pack_Procedures;

--[main.adb] Procedure
with Ada.Text_Io, Pack_Procedures;
use  Ada.Text_Io, Pack_Procedures;
procedure Main is
begin
  Put("Example 1"); New_Line; Ex1;
  Put("Example 2"); New_Line; Ex2;
  Put("Example 3"); New_Line; Ex3;
  Put("Example 4"); New_Line; Ex4;
end Main;
