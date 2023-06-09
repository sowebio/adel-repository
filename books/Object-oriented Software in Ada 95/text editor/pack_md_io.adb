pragma Source_Reference (000670, "x80_ed.ada");

--[pack_md_io.adb] Implementation
with Raw_Io, Pack_Constants;
use  Raw_Io, Pack_Constants;
package body Pack_Md_Io is
  procedure Put( Ch:in Character ) is
  begin
    Raw_Io.Put( Ch );
  end Put;

  procedure Put( Str:in String ) is
  begin
    Raw_Io.Put( Str );
  end Put;

  procedure Get_Immediate( Ch:out Character) is
    Esc: constant Character := Character'Val(27);
  begin
    Raw_Io.Get_Immediate( Ch );
    if Ch = Esc then                         -- ESC 
      Raw_Io.Get_Immediate( Ch );            -- [
      if Ch = '[' then
        Raw_Io.Get_Immediate( Ch );
        case Ch is
          when 'A'    => Ch := C_Up;         -- A - Up arrow
          when 'B'    => Ch := C_Down;       -- B - Down arrow
          when 'C'    => Ch := C_Right;      -- C - Right arrow
          when 'D'    => Ch := C_Left;       -- D - Left arrow
          when others => Ch := '?';          -- ? - Unknown
        end case;
      end if;
    end if;
  end Get_Immediate;

end Pack_Md_Io;
