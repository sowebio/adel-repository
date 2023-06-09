---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:43 PM BST  --
---------------------------------------------------------------------

--[class_abstract_account.ads] Specification
package Class_Abstract_Account is

  type Abstract_Account is abstract tagged null record;
  subtype Money  is Float;
  subtype Pmoney is Float range 0.0 .. Float'Last;

  procedure Statement( The:in Abstract_Account ) is abstract;
  procedure Deposit  ( The:in out Abstract_Account;
                       Amount:in Pmoney ) is abstract;
  procedure Withdraw ( The:in out Abstract_Account;
                       Amount:in Pmoney;
                       Get:out Pmoney ) is abstract;
  function  Balance  ( The:in Abstract_Account )
                       return Money is abstract;
end Class_Abstract_Account;

-- =======================================================

--[class_account.ads] Specification
with Class_Abstract_Account;
use  Class_Abstract_Account;
package Class_Account is

  type Account is new Abstract_Account with private;
  subtype Money  is Class_Abstract_Account.Money;
  subtype Pmoney is Class_Abstract_Account.Pmoney;

  procedure Statement( The:in Account );
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
with Ada.Text_Io, Ada.Float_Text_Io;
use  Ada.Text_Io, Ada.Float_Text_Io;
package body Class_Account is

  procedure Statement( The:in Account ) is
  begin
    Put("Mini statement: The amount on deposit is $" );
    Put( The.Balance_Of, Aft=>2, Exp=>0 );
    New_Line(2);
  end Statement;

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
--[class_interest_account/inspect_interest.ads] Specification
package Class_Interest_Account.Inspect_Interest is
  function Interest_Is( The:in Interest_Account )
    return Money;
end Class_Interest_Account.Inspect_Interest;

--[class_interest_account/inspect_interest.adb] Implementation
package body Class_Interest_Account.Inspect_Interest is

  function Interest_Is( The:in Interest_Account )
      return Money is
  begin
    return The.Accumulated_Interest;
  end Interest_Is;

end Class_Interest_Account.Inspect_Interest;

--[pack_procedures.ads] Specification
with Ada.Text_Io, Ada.Float_Text_Io, Class_Account, 
     Class_Interest_Account, Class_Interest_Account.Inspect_Interest;
use  Ada.Text_Io, Ada.Float_Text_Io, Class_Account, 
     Class_Interest_Account, Class_Interest_Account.Inspect_Interest; 
package Pack_Procedures is
  procedure Ex1;
  procedure Ex2;
  procedure Ex3;
  procedure Ex4;
end Pack_Procedures;

--[pack_procedures.adb] Implementation
package body Pack_Procedures is

  procedure Ex1 is
    My_Account: Interest_Account;
    Obtained  : Money;
  begin
    Statement( My_Account );
    Put("Deposit 100.00 into account"); New_Line;
    Deposit( My_Account, 100.00 );            -- Day 1
    Calc_Interest( My_Account );              -- End of day 1
    Calc_Interest( My_Account );              -- End of day 2
    Statement( My_Account );                  -- Day 3
    Obtained := Interest_Is( My_Account );    -- How much interest
    Put("Interest accrued so far : $" );
    Put( Obtained, Aft=>2, Exp=>0 ); New_Line;
  end Ex1;

  procedure Ex2 is
  begin
    null;
  end Ex2;

  procedure Ex3 is
  begin
    null;
  end Ex3;

  procedure Ex4 is
  begin
    null;
  end Ex4;

end Pack_Procedures;

--[main.adb] Procedure
with Ada.Text_Io, Ada.Float_Text_Io, Pack_Procedures;
use  Ada.Text_Io, Ada.Float_Text_Io, Pack_Procedures;
procedure Main is
begin
  Put("Example 1"); New_Line; Ex1;
  Put("Example 2"); New_Line; Ex2;
  Put("Example 3"); New_Line; Ex3;
  Put("Example 4"); New_Line; Ex4;
end Main;
