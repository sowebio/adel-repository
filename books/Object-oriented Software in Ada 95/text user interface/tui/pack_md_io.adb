
with Raw_io, Pack_constants; 
use  Raw_io, Pack_constants;
package body Pack_md_io is
  procedure put( ch:in Character ) is
  begin
    Raw_io.put( ch );
  end put;

  procedure put( str:in String ) is
  begin
    Raw_io.put( str );
  end put;

  procedure get_immediate( ch:out Character) is
    ESC: CONSTANT Character := Character'Val(27);
  begin
    Raw_io.get_immediate( ch );
    if ch = ESC then                         -- ESC 
      Raw_io.get_immediate( ch );            -- [
      if ch = '[' then
        Raw_io.get_immediate( ch );
        case ch is
          when 'A'    => ch := C_UP;         -- A - Up arrow
          when 'B'    => ch := C_DOWN;       -- B - Down arrow
          when 'C'    => ch := C_RIGHT;      -- C - Right arrow
          when 'D'    => ch := C_LEFT;       -- D - Left arrow
          when others => ch := '?';          -- ? - Unknown
        end case;
      end if;
    end if;
  end get_immediate;

end Pack_md_io;
