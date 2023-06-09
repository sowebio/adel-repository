
package body Class_window is

  procedure put( the:in out Window;
                 x,y:in Positive; mes:in String );

  procedure initialize( the:in out Window ) is
  begin
    null;
  end initialize;

  procedure finalize( the:in out Window ) is
  begin
    make_window( the, INVisIBLE );
    de_register( the'Unchecked_Access );
  end finalize;

  procedure create( the:in out Window;
                    abs_x_crd, abs_y_crd: Positive;
                    max_x_crd, max_y_crd: Positive ) is
  begin
    if max_x_crd < 3 or else max_x_crd > WINDOW_MAX_X or else
       max_y_crd < 3 or else max_y_crd > WINDOW_MAX_Y or else
       abs_x_crd + max_x_crd - 1 > VDT_MAX_X or else
       abs_y_crd + max_y_crd - 1 > VDT_MAX_Y then
       window_fatal("Creation window parameter error");
    end if;
    declare
      top_bottom: String(1..max_x_crd)     := (others => '-');
      spaces    : String(2 .. max_x_crd-1) := (others => ' ');
    begin
      top_bottom(1) := '+'; top_bottom(max_x_crd) := '+';
      the.max_x := max_x_crd - 2;        -- For border
      the.max_y := max_y_crd - 2;        -- For border
      the.abs_y := abs_y_crd;            -- Abs position screen
      the.abs_x := abs_x_crd;            --
      the.pane(1)(1..max_x_crd) := top_bottom;  -- Clear / set up
      for y in 2 .. max_y_crd-1 loop
        the.pane(y)(1..max_x_crd):= '|'&spaces&'|';
      end loop;
      the.pane(max_y_crd)(1..max_x_crd) := top_bottom;
      position( the, 1, 1 );             -- Top left hand corner
    end;
  end create;

-- The window co-ordinates of 1 .. n , 1 .. m are
--  stored into an array in position 2 .. n+1, 2 .. m+1
--  this allows the border to be stored

  procedure framework( the:in out Window;
                       abs_x_crd, abs_y_crd: Positive;
                       max_x_crd, max_y_crd: Positive;
                       cb:in P_cbf := null ) is
  begin
    create( the, abs_x_crd, abs_y_crd, max_x_crd, max_y_crd );
    make_window( the, VisIBLE );
    if cb /= null then
      set_call_back( the, cb );
      register( the'Unchecked_Access, C_SWITCH );
    else
      register( the'Unchecked_Access, C_NO_CHAR );
    end if;
  end framework;

  procedure set_call_back( the:in out Window; cb:in P_cbf ) is
  begin
     the.call_back := cb;
  end set_call_back;

  function call_call_back( the:in Window; 
                           str:in String ) return String is
  begin
    if the.call_back /= null then
      return the.call_back(str);
    end if;
    return "No call back function";
  end;

  procedure put( the:in out Window; mes:in String ) is
    add : Natural;
  begin
    add := mes'Length;                   -- Length
    if add + the.c_x > the.max_x then    -- Actual characters
      add := the.max_x - the.c_x + 1;    --  to add
    end if;
    if add >= 1 then                     -- There are some
      the.pane(the.c_y+1)(the.c_x+1 .. the.c_x+add)
          := mes( 1 .. add );
      if the.mode_of = VisIBLE then      -- Add to screen
        put(the, the.c_x+1, the.c_y+1, mes( 1 .. add) );
      end if;
      the.c_x := the.c_x + add;
    else
      put(the, the.c_x+1, the.c_y+1, "" );
    end if;
  end put;

  procedure put( the:in out Window; ch:in Character ) is
  begin
    put( the, "" & ch );           -- Convert to string
  end put;

  procedure put( the:in out Window; n:in Integer ) is
  begin
    put( the, Integer'Image(n) );  -- Convert to string
  end put;

  procedure position( the:in out Window; x,y:in Positive ) is
  begin
    if x <= the.max_x and y <= the.max_y then
      the.c_x := x; the.c_y := y;
    end if;
  end position;

  procedure clear( the:in out Window ) is
    empty : String( 1 .. the.max_x ) := (others => ' ');
  begin
    position(the, 1, 1);            -- Top right hand corner
    for y in 1 .. the.max_y loop    -- Clear text
      put( the, empty ); new_line(the);
    end loop;
  end clear;

  procedure new_line( the:in out Window ) is
  begin
    if the.c_y >= the.max_y then         -- Scroll text
      for y in 2 .. the.max_y loop       --  Copy up
        the.pane(y) := the.pane(y+1);
      end loop;
      the.pane(the.max_y+1)(2..the.max_x+1):= (others=>' ');
      refresh(the);                      --  refresh
    else
      the.c_y := the.c_y + 1;            -- Next line
    end if;
    the.c_x := 1;                        -- At start
  end new_line;

  procedure refresh( the:in out Window ) is
  begin
    if the.mode_of = VisIBLE then             -- Visible
      for y in 1 .. the.max_y+2 loop          -- Text
        put( the, 1, y,
             the.pane(y)(1 .. the.max_x+2) ); -- include border
      end loop;
      put( the, "" );                         -- Cursor
    end if;
  end refresh;

  procedure make_window( the:in out Window; mo:in Mode ) is
  begin
    if the.mode_of /= mo then              -- Change so
      the.mode_of := mo;                   -- Set new mode_of
      case mo is
        when INVisIBLE =>                  -- Clear from screen
          hide_win( the'Unchecked_Access );-- Hide window
        when VisIBLE =>                    -- Redraw on screen
          refresh( the );
      end case;
    end if;
  end make_window;

  procedure mark_border( the:in out Window;
                          a_border:in Attribute;
                          pos:in Positive; ch:in Character ) is
    a_y, a_x : Positive;
  begin
    case a_border is
      when TOP    => a_x := pos; a_y := 1;
      when BOTTOM => a_x := pos; a_y := the.max_y+2;
      when LEFT   => a_x := 1; a_y := pos;
      when RIGHT  => a_x := the.max_x+2; a_y := pos;
      when others => null;
    end case;
    if a_x <= the.max_x+2 and then a_y <= the.max_y+2 then
      the.pane(a_y)(a_x) := ch;       -- Store
      if the.mode_of = VisIBLE then   -- Update on screen
        put( the, a_x, a_y, ch & "" );
        put( the, "" );
      end if;
    end if;
  end mark_border;

  function about(the:in Window; b:in Attribute) return Natural is
  begin
    case b is
      when TOP  | BOTTOM => return the.max_x+2;
      when LEFT | RIGHT  => return the.max_y+2;
      when abs_X         => return the.abs_x;
      when abs_Y         => return the.abs_y;
      when others        => return 0;
    end case;
  end;

  procedure switch_away( the:in out Window ) is
  begin
    mark_border( the, TOP, 1, C_WIN_PAS );
  end switch_away;

  procedure switch_to( the:in out Window ) is
  begin
    mark_border( the, TOP, 1, C_WIN_A );
  end switch_to;

  procedure send_to( the:in out Window; ch:in Character ) is
  begin
    null;
  end send_to;

  procedure register( p_w:in P_Root_window;
                      ch:in Character ) is
  begin
    switch_away_from_top;           -- Register window focus
    add_to_list( p_w, ch );         -- Register window
    switch_to_top;                  -- Make focus
  end register;

  procedure de_register( p_w:in P_Root_window ) is
  begin
    top( p_w );                     -- Make top
    switch_away_from_top;           --  prepare for demise
    remove_from_list( p_w );        -- De register window
    switch_to_top;                  -- Make focus
  end de_register;

 -- Write to Physical Screen

  procedure put( the:in out Window;
                 x,y:in Positive; mes:in String ) is
  begin
    write_to( the'Unchecked_Access, x, y, mes );
  end put;

end Class_window;
