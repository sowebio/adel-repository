---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:53 PM BST  --
---------------------------------------------------------------------
with Class_Window;
use  Class_Window;
package Pack_Program is
  procedure Main;
private
  P_Result : P_Window;
end Pack_Program;


with Ada.Text_Io, Ada.Float_Text_Io;
use  Ada.Text_Io, Ada.Float_Text_Io;
with Class_Input_Manager, Class_Window, Class_Dialog;
use  Class_Input_Manager, Class_Window, Class_Dialog;
package body Pack_Program is

  function User_Input( Cb_Mes:in String ) return String is
    Miles  : Float;             -- Miles input by user
    Last   : Positive;          --
    Str_Kms: String( 1 .. 10 ); -- As a string in Kms
    Str_Mls: String( 1 .. 10 ); -- As a string in Miles
  begin
    begin
      Get( Cb_Mes & ".", Miles, Last );
      Put( Str_Kms, Miles * 1.609_344, Aft=>2, Exp=>0 );
      Put( Str_Mls, Miles, Aft=>2, Exp=>0 );
      Put( P_Result.all, "Distance in Miles = " );
      Put( P_Result.all, Str_Mls ); New_Line( P_Result.all );
      Put( P_Result.all, "Distance in Kms   = " );
      Put( P_Result.all, Str_Kms ); New_Line( P_Result.all );
    exception
      when Data_Error =>
        Put( P_Result.all, " Not a valid number" );
        New_Line( P_Result.all );
      when others =>
        Put( P_Result.all, " [Calculation error]" );
        New_Line( P_Result.all );
    end;
    return "";
  end User_Input;
  
  
  procedure Main is
  begin
    Window_Prologue;                -- Setup window system
    declare
      Result : aliased Window;      -- Result window
      Input  : Dialog;              -- Input Window
      Title  : Window;              -- title Window
    begin
      Framework( Title,  20,  1,  36, 5 );   -- Title Window
      Framework( Result, 30, 10,  36, 5 );   -- Result Window
  
      Position( Title, 8, 2 );
      Put( Title, "Miles to kilometres" );
      Framework( Input, 5, 10, 22,            -- Input Window
                  "Miles", User_Input'access );
      P_Result := Result'Unchecked_Access;
  
      Window_Start;             -- Start the user interaction
    end;
    Window_Epilogue;            -- Close window system
  end Main;

end Pack_Program;

with Pack_Program;
procedure Main is
begin
  Pack_Program.Main;
end Main;
