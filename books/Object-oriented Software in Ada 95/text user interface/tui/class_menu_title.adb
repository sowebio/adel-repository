
with Pack_constants;
use  Pack_constants;
package body Class_menu_title is

  procedure set_up( the:in out Menu_title; active:in Positive ) is
    me: Menu_item;
  begin
    create( the, 1, 1, (1+MAX_MENU)*active+1, 3 ); -- Fixed size
    make_window( the, VisIBLE );
    the.act_menu( 1 ) := Menu(the)'Unchecked_Access;-- Title menu
    the.menu_index := 1;
    for i in 1 .. active loop                      -- Get menu
      get_menu_name( the, i, me );                 --  name
      put( the, me ); put( the, "|" );             --  write
    end loop;
    register( the'Unchecked_Access, C_MENU );      -- Register
    menu_spot( the, C_CURSor );                    -- Cursor on
  end set_up;

  procedure send_to( the:in out Menu_title; ch:in Character ) is
    current, next : P_Menu;
    proc          : P_cbf;
    res           : String( 1..0 );
  begin
    current := the.act_menu( the.menu_index );  -- Active menu
    get_cur_selected_details( current, next, proc );
    case ch is
      when C_WHERE =>
        put( current.all, "" );
      when C_ACTION =>
        if next /= null and the.menu_index < MAX_ACT_MENU then
          make_window( current.all, INVisIBLE );     -- Hide cur.
          the.menu_index := the.menu_index + 1;      --
          the.act_menu( the.menu_index ) := next;    -- New menu
          make_window( next.all, VisIBLE );          -- Reveal
        else
          if proc /= null then                       -- Call
             res := proc("Action")(1 .. 0 );
          end if;
        end if;
      when others =>
        send_to( current.all , ch );  -- Treat as normal menu
    end case;
  end send_to;

  procedure switch_away( the:in out Menu_title ) is
  begin
    mark_border( the, TOP, 1, C_WIN_PAS ); -- Now inactive
    if the.menu_index > 1 then          -- Not top level menu
      make_window( the.act_menu(the.menu_index).all, INVisIBLE );
      the.menu_index := 1;
      make_window( the.act_menu( 1 ).all, VisIBLE ); -- Top level
    end if;
  end switch_away;

end Class_menu_title;
