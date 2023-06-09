
-------------------------------------------------------------

with Pack_constants, Pack_md_io, Class_screen,
     Class_window_control, Class_root_window;
use  Pack_constants, Pack_md_io, Class_screen,
     Class_window_control, Class_root_window;
package body Class_input_manager is

  procedure window_prologue is
  begin
    clear_screen;
  end window_prologue;

  procedure window_start is
    p_w : P_Root_window;                   -- A window
    ch  : Character;                       -- Current Char
  begin
    loop
      get_immediate( ch );                 -- From Keyboard
      exit when ch = C_exit;
      find( p_w, ch );                     -- Active window
      if p_w /= null then                  -- Window activation
        switch_away_from_top;              --  No longer active
        top( p_w );                        --  Make p_w top
        switch_to_top;                     --  & make active
        send_to_top( C_WHERE );            -- In selected window
      else                                 --
        send_to_top( ch );                 -- Give to top window
      end if;
    end loop;
    Pack_md_io.put( Character'Val(0) );    -- Capture output
  end window_start;

  procedure window_epilogue is
  begin
    null;
  end window_epilogue;

end Class_input_manager;
