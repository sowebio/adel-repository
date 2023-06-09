---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:44 PM BST  --
---------------------------------------------------------------------
--[class_name_address.ads] Specification
package Class_Name_Address is
  type Name_Address is tagged private;

  procedure Set( The:out Name_Address; Str:in String );
  function Deliver_Line( The:in Name_Address;
                         Line:in Positive ) return String;
  function Lines( The:in Name_Address ) return Positive;
private
  Max_Chs : constant := 200;
  subtype Line_Index   is Natural    range 0 .. Max_Chs;
  subtype Line_Range   is Line_Index range 1 .. Max_Chs;

  type Name_Address is tagged record
    Text   : String( Line_Range );   -- Details
    Length : Line_Index := 0;        -- Length of address
  end record;
end Class_Name_Address;




--[class_name_address.adb] Implementation
package body Class_Name_Address is

  function Spaces( Line:in Positive ) return String;

  procedure Set( The:out Name_Address; Str:in String ) is
  begin
    if Str'Length > Max_Chs then
      Set( The, Str( Str'First .. Str'First+Max_Chs-1 ) );
    else
      The.Text( 1 .. Str'Length ) := Str;
      The.Length := Str'Length;
    end if;
  end Set;

  --  The reason for the line:
  --      FOR i IN 1 .. Line_range(the.length) LOOP
  --  is so the compiler will know that the index i can never go outside
  --  the range of the array so no need to do array bound checking

  function Deliver_Line( The:in Name_Address;
                         Line:in Positive ) return String is
                         Line_On : Positive := 1;
  begin
    for I in 1 .. The.Length loop
      if Line_On = Line then
        for J in I .. The.Length loop
          if The.Text(J) = '/' then
            return Spaces(Line_On) & The.Text(I .. J-1);
          end if;
        end loop;
        return Spaces(Line_On) & The.Text(I..The.Length);
      end if;
      if The.Text(I) = '/' then Line_On := Line_On+1; end if;
    end loop;
    return "";
  end Deliver_Line;

  function Lines( The:in Name_Address ) return Positive is
                  No_Lines : Positive := 1;
  begin
    for I in  1 .. The.Length loop
      if The.Text(I) = '/' then No_Lines := No_Lines + 1; end if;
    end loop;
    return No_Lines;
  end Lines;

  function Spaces( Line:in Positive ) return String is
    Spaces_Are : String( 1 .. Line ) := (others=>' ');
  begin
    return Spaces_Are;
  end Spaces;

end Class_Name_Address;


-- =======================================================





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




-- =======================================================

--[class_named_account.ads] Specification
with Class_Account, Class_Name_Address;
use  Class_Account, Class_Name_Address;
package Class_Named_Account is

  type Named_Account is tagged private;
  subtype Pmoney is Class_Account.Pmoney;
  subtype Money  is Class_Account.Money;

  procedure Set( The:out Named_Account; Str:in String );
  function  Deliver_Line( The:in Named_Account;
                          Line:in Positive ) return String;
  function  Lines( The:in Named_Account ) return Positive;
  procedure Deposit( The:in out Named_Account; Amount:in Pmoney );
  procedure Withdraw( The:in out Named_Account; Amount:in Pmoney;
                      Get:out Pmoney );
  function  Balance( The:in Named_Account ) return Pmoney;
private
  type Named_Account is tagged record
    Acc : Account;           -- An account object
    Naa : Name_Address;      -- A Name and address object
  end record;
end Class_Named_Account;




--[class_named_account.adb] Implementation
package body Class_Named_Account is

  procedure Set( The:out Named_Account; Str:in String ) is
  begin
    Set( The.Naa, Str );
  end Set;

  function Deliver_Line( The:in Named_Account;
      Line:in Positive ) return String is
  begin
    return Deliver_Line( The.Naa, Line );
  end Deliver_Line;

  function Lines( The:in Named_Account ) return Positive is
  begin
    return Lines( The.Naa );
  end Lines;


  procedure Deposit(The:in out Named_Account; Amount:in Pmoney) is
  begin
    Deposit( The.Acc, Amount );
  end Deposit;

  procedure Withdraw( The:in out Named_Account; Amount:in Pmoney;
      Get:out Pmoney ) is
  begin
    Withdraw( The.Acc, Amount, Get );
  end Withdraw;

  function  Balance  ( The:in Named_Account ) return Pmoney is
  begin
    return Balance( The.Acc );
  end Balance;

end Class_Named_Account;


with Ada.Text_Io, Ada.Float_Text_Io, Class_Named_Account;
use  Ada.Text_Io, Ada.Float_Text_Io, Class_Named_Account;
procedure Statement( An_Account:in Named_Account ) is
begin
  Put("Statement for : " );
  Put( Deliver_Line( An_Account, 1 ) ); New_Line;
  Put("Mini statement: The amount on deposit is $" );
  Put( Balance( An_Account), Aft=>2, Exp=>0 );
  New_Line(2);
end Statement;


with Class_Named_Account, Statement;
use  Class_Named_Account;
procedure Main1 is
  Mike : Named_Account;
  Get  : Money;
begin
  Set      ( Mike, "A.N.Other/Brighton/UK" );
  Deposit  ( Mike, 10.00 );
  Statement( Mike );
  Withdraw ( Mike, 5.00, Get );
  Statement( Mike );
end Main1;

with Ada.Text_Io, Ada.Integer_Text_Io, Class_Name_Address;
use  Ada.Text_Io, Ada.Integer_Text_Io, Class_Name_Address;
procedure Main2 is
  Name    : Name_Address;
  Address : String := "A.N.Other/Brighton/East Sussex/UK";
begin
  Set( Name, Address );
  Put( Address ); New_Line; Put("There are ");
  Put( Lines( Name ) ); Put(" lines"); New_Line;
  for I in 1 .. Lines(Name)+1 loop
    Put("Line #"); Put(I); Put("  ");
    Put( Deliver_Line(Name, I) ); New_Line;
  end loop;
end Main2;

procedure Main3 is
begin
  null;
end Main3;

procedure Main4 is
begin
  null;
end Main4;

--[main.adb] Procedure
with Ada.Text_Io, Main1, Main2, Main3, Main4;
use  Ada.Text_Io;
procedure Main is
begin
  Put("Example 1"); New_Line; Main1;
  Put("Example 2"); New_Line; Main2;
  Put("Example 3"); New_Line; Main3;
  Put("Example 4"); New_Line; Main4;
end Main;
