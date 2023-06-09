---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:42 PM BST  --
---------------------------------------------------------------------
--[class_account.ads] Specification
package Class_Account is
  type Account is private;
  subtype Money  is Float;
  subtype Pmoney is Float range 0.0 .. Float'Last;

  procedure Deposit( The:in out Account; Amount:in Pmoney );
  procedure Withdraw( The:in out Account;
                      Amount:in Pmoney; Get:out Pmoney );
  function  Balance( The:in Account ) return Money;
private
  type Account is record
    Balance_Of : Money := 0.00;           -- Amount on deposit
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


--[class_tui.ads] Specification
package Class_Tui is

  type Menu_Item is ( M_1, M_2, M_3, M_4, M_Quit );
  type Tui is private;

  procedure Menu( The:in out Tui; M1,M2,M3,M4:in String );
  function  Event( The:in Tui ) return Menu_Item;
  procedure Message( The:in Tui; Mes:in String );
  procedure Dialog(The:in Tui; Mes:in String; Res:out Float);

private
  type Tui is record
    Selection : Menu_Item := M_Quit;
  end record;
end Class_Tui;

--[class_tui.adb] Implementation
with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io;
package body Class_Tui is
  procedure Menu( The:in out Tui; M1,M2,M3,M4:in String ) is

    Selection      : Character;
    Valid_Response : Boolean := False;

    procedure Set_Response(Choice:in Menu_Item; Mes:in String) is
    begin
      if Mes /= "" then               -- Allowable choice
        The.Selection := Choice; Valid_Response := True;
      end if;
    end Set_Response;

    procedure Display_Menu_Item(Prompt, Name:in String) is
    begin
      if Name/="" then Put(Prompt & Name); New_Line(2); end if;
    end Display_Menu_Item;

  begin
    while not Valid_Response loop
      Display_Menu_Item( "[a]  ", M1 );
      Display_Menu_Item( "[b]  ", M2 );
      Display_Menu_Item( "[c]  ", M3 );
      Display_Menu_Item( "[d]  ", M4 );
      Put( "Input selection: "); Get( Selection ); Skip_Line;
      case Selection is
        when 'a' | 'A' => Set_Response( M_1, M1 );
        when 'b' | 'B' => Set_Response( M_2, M2 );
        when 'c' | 'C' => Set_Response( M_3, M3 );
        when 'd' | 'D' => Set_Response( M_4, M4 );
        when 'e' | 'E' => Set_Response( M_Quit, "Quit" );
        when others    => Valid_Response := False;
      end case;
      if not Valid_Response then
        Message( The, "Invalid response" );
      end if;
    end loop;
  end Menu;

  function  Event( The:in Tui ) return Menu_Item is
  begin
    return The.Selection;
  end;

  procedure Message( The:in Tui; Mes:in String ) is
  begin
    New_Line; Put( Mes ); New_Line;
  end Message;

  procedure Dialog(The:in Tui; Mes:in String; Res:out Float) is
  begin
    New_Line(1); Put( Mes & " : " );
    Get( Res ); Skip_Line;
  end Dialog;

  procedure Dialog(The:in Tui; Mes:in String; Res:out Integer) is
  begin
    New_Line(1); Put( Mes & " : " );
    Get( Res ); Skip_Line;
  end Dialog;

end Class_Tui;

--===============================================================

--[class_container.ads] Specification
package Class_Container is
  procedure Main1;
  procedure Main2;
end Class_Container;

--[class_container.adb] Implementation
with Ada.Text_Io, Ada.Float_Text_Io, Class_Account, Class_Tui;
use  Ada.Text_Io, Ada.Float_Text_Io, Class_Account, Class_Tui;
package body Class_Container is

  procedure Main1 is
    Miles  : Float;
    Screen : Tui;
    function Float_Image( F:in Float ) return String is
      Res : String( 1 .. 10 );        -- String of 10 characters
    begin
      Put( Res, F, Aft=>2, Exp=>0 );  -- 2 digits - NO exp
      return Res;
    end Float_Image;
  begin
    Message( Screen, "Distance converter" );
    Dialog ( Screen, "Enter distance in miles", Miles );
    Message( Screen, "Distance in kilometers is " &
      Float'Image( Miles * 1.6093 )  );
    Message( Screen, "Distance in kilometers is " &
      Float_Image( Miles * 1.6093 )  );
  end Main1;

  procedure Main2 is
    User     : Account;            -- The users account
    Screen   : Tui;                -- The display screen
    Cash     : Money;              --
    Received : Money;              --

    function Float_Image( F:in Float ) return String is
      Res : String( 1 .. 10 );     -- String of 10 characters
    begin
      Put( Res, F, 2, 0 );         -- 2 digits - NO exp
      return Res;
    end Float_Image;

  begin
    loop
      Menu( Screen, "Deposit", "Withdraw", "Balance", "" );
      case Event( Screen ) is
        when M_1 =>                                    -- Deposit
          Dialog( Screen, "Amount to deposit", Cash );
          if Cash <= 0.0 then
            Message( Screen, "Must be >= 0.00" );
          else
            Deposit( User, Cash );
          end if;
        when M_2 =>                                     -- Withdraw
          Dialog( Screen, "Amount to withdraw", Cash );
          if Cash <= 0.0 then
            Message( Screen, "Must be >= 0.00" );
          else
            Withdraw( User, Cash, Received );
            if Received <= 0.0 then
              Message( Screen, "Not enough money" );
            end if;
          end if;
        when M_3 =>                                    -- Balance
          Message( Screen, "Balance is " &
            Float_Image( Balance(User)) );
        when M_Quit =>                                 -- Exit
          return;
        when others =>                                 -- Not used
          Message( Screen, "Program error");         -- oops
      end case;
    end loop;
  end Main2;

end Class_Container;

--[main.adb] Procedure
with Text_Io, Class_Container;
use  Text_Io, Class_Container;
procedure Main is
begin
  Put("Example 1"); New_Line; Main1;
  Put("Example 2"); New_Line; Main2;
end Main;
