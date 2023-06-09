
package body Class_dialog is

  procedure framework( the:in out Dialog;
                       abs_x, abs_y:in Positive;
                       max_x:in Positive;
                       name:in String; cb:in P_cbf ) is
    dashes : String( 1 .. max_x ) := (others=>'-');
  begin
    create( the, abs_x, abs_y, max_x, 5 );
    the.dialog_len := max_x-2;                  -- User input
    the.dialog_pos := 1;                        -- In Dialog
    set_call_back( the, cb );                   -- Call back fun
    put( the, "Dialog| " ); put( the, name );   -- Dialog title
    position( the, 1, 2 ); put( the, dashes );  -- Line
    position( the, 1, 3 ); put( the, C_CURSor );-- Cursor
    make_window( the, VisIBLE );
    register( the'Unchecked_Access, C_SWITCH ); -- Activation Chr
  end framework;

  procedure send_to( the:in out Dialog; ch:in Character ) is
    spaces : String(1 .. about(Window(the),TOP)) := (others => ' ');
    res    : String(1..0);
  begin
    case ch is
      when C_WHERE =>
        put( the, "" );
      when C_ACTION =>
        res := call_call_back( the, 
                 the.dialog_mes(1..the.dialog_pos-1) )(1..0);
        the.dialog_pos := 1;
        the.dialog_mes := ( others => ' ' );
        position( the, 1, 3 );                   -- Start
        put( the, C_CURSor & spaces );           -- Clear
        position( the, 2, 3 );                   -- Cursor
        put( the, "" );                          -- Cursor
      when C_DEL =>
        if the.dialog_pos > 1 then               -- Can delete
          the.dialog_pos := the.dialog_pos - 1;  -- Make avail.
          the.dialog_mes(the.dialog_pos):= ' ';  -- Remove
          position( the, the.dialog_pos, 3 );
          put( the, C_CURSor & " " );            -- Overwrite
          position( the, the.dialog_pos, 3 );
          put( the, "" );                        -- Cursor
        end if;
      when others =>
        if the.dialog_pos <= the.dialog_len then
          if ch in ' ' .. '~' then               -- Add to
            the.dialog_mes( the.dialog_pos ) := ch; -- Save ch
            position( the, the.dialog_pos, 3 );
            put( the, the.dialog_mes(the.dialog_pos) );
            put( the, C_CURSor );
            the.dialog_pos := the.dialog_pos + 1;
          end if;
        end if;
    end case;
  end send_to;
end Class_dialog;
