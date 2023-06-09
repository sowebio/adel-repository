pragma Source_Reference (000797, "x80_ed.ada");

--[class_user.adb] Implementation
with Pack_Constants, Pack_Md_Io, Pack_Ansi_Display;
use  Pack_Constants, Pack_Md_Io, Pack_Ansi_Display;

package body Class_User is

  function Get_Command( The:in User ) return Character is
    Ch : Character;
  begin
    Get_Immediate( Ch );
    return Ch;
  end Get_Command;

  function  Dialog( The:in User;
      Mes:in String) return String is
    Ch      : Character;
    Reading : Boolean;
    Str     : String( 1 .. Max_Answer );
    Str_Len : Integer := 1;
  begin
    Cursor_Position( Lines_On_Screen, 1 ); Clear_To_End_Of_Line;
    Put( Mes ); Reading := True; Str(1) := ' ';
    for I in Str'range loop
      Ch := Get_Character( The, No_Echo );
      if Ch = Ascii.Cr or else Ch = Ascii.Lf then
        Reading := False;
        exit;
      else
        Put( Ch ); Str(I) := Ch; Str_Len := I;
      end if;
    end loop;
    return Str( 1 .. Str_Len );
  end Dialog;

  function Get_Character( The:in User; M:in Mode ) return Character is
    Ch : Character;
  begin
    Get_Immediate( Ch );
    if M = Echo then Put( Ch ); end if;
    return Ch;
  end Get_Character;

end Class_User;
