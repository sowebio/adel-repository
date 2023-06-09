pragma Source_Reference (000705, "x80_ed.ada");

--====================================================================

--[pack_ansi_display.ads] Specification
package Pack_Ansi_Display is
  procedure Clear;                        -- Clear screen
  procedure Down ( N:in Natural );        -- Cursor Down
  procedure Up   ( N:in Natural );        -- Cursor Up
  procedure Left ( N:in Natural );        -- Cursor Left
  procedure Right( N:in Natural );        -- Cursor Right
  procedure Insert_Line( N:in Natural );  -- Insert Line(s)
  procedure Delete_Line( N:in Natural );  -- Delete Line(s)
  procedure Clear_To_End_Of_Line;         -- Clear to end of line
  procedure Cursor_Position (Row:in Natural; Column:in Natural);
  procedure Put  ( N:in Natural );        -- Write decimal number
private
end Pack_Ansi_Display;
