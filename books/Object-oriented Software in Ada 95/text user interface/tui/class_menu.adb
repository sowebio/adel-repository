
with Pack_constants;
use  Pack_constants;
package body Class_menu is

   -- The type is Menu'Class so a run time dispatch will
   -- take place when set_up is called

  procedure framework( the:in out Menu'Class;
    m1:in String:=""; w1:in P_Menu:=null; cb1:in P_cbf:=null;
    m2:in String:=""; w2:in P_Menu:=null; cb2:in P_cbf:=null;
    m3:in String:=""; w3:in P_Menu:=null; cb3:in P_cbf:=null;
    m4:in String:=""; w4:in P_Menu:=null; cb4:in P_cbf:=null;
    m5:in String:=""; w5:in P_Menu:=null; cb5:in P_cbf:=null;
    m6:in String:=""; w6:in P_Menu:=null; cb6:in P_cbf:=null
    ) is
    spaces : Menu_item := ( others => ' ' );
    active : Menus_index := 1;
    procedure set_up( mi:in String; wi:in P_Menu;
                      cb:in P_cbf; n:in Menus_index ) is
    begin
      if mi /= "" then active := n; end if;   -- A menu item
      the.menu_set( n ) :=
        (" "&mi&spaces(1 .. MAX_MENU-1-mi'Length), wi, cb);
    end set_up;
  begin
    set_up( m1, w1, cb1, 1 ); set_up( m2, w2, cb2, 2 );
    set_up( m3, w3, cb3, 3 ); set_up( m4, w4, cb4, 4 );
    set_up( m5, w5, cb5, 5 ); set_up( m6, w6, cb6, 6 );
    the.number := active;
    set_up( the, Positive(active) );
  end framework;

  procedure set_up( the:in out Menu;
                    active:in Positive ) is
    me: Menu_item;
  begin
    create( the, 1, 1, (1+MAX_MENU)*active+1, 3 );
    for I in 1 .. active loop            -- Display menu names
      get_menu_name( the, i, me );
      put( the, me ); put( the, "|" );
      null;
    end loop;
    menu_spot( the, C_CURSor );          -- Mark current
  end set_up;

  procedure menu_spot( the:in out Menu; ch:in Character ) is
  begin
    position( the, (MAX_MENU+1)*(Positive(the.cur_men)-1)+1, 1 );
    put( the, ch );
  end menu_spot;

  procedure send_to( the:in out Menu; ch:in Character ) is
  begin
    menu_spot( the, C_BLANK );
    case ch is
      when C_RIGHT => next( the, D_forWARD );
      when C_LEFT  => next( the, D_reverse );
      when others  => null;
    end case;
    menu_spot( the, C_CURSor );
  end send_to;

  procedure next( the:in out Menu; dir:in Direction ) is
  begin
    case dir is
      when D_forWARD =>
        the.cur_men := the.cur_men rem the.number + 1;
      when D_reverse =>
        if the.cur_men = 1
          then the.cur_men := the.number;
          else the.cur_men := the.cur_men-1;
        end if;
    end case;
  end next;

  procedure get_menu_name( the:in Menu; i:in Positive;
                           n:out Menu_item ) is
  begin
    n  := the.menu_set( Menus_index(i) ).name;
  end get_menu_name;

  procedure get_cur_selected_details( the:in P_Menu;
                            w:out P_Menu; cb:out P_cbf ) is
  begin
    w  := the.menu_set( the.cur_men ).p_m;
    cb := the.menu_set( the.cur_men ).fun;
  end get_cur_selected_details;

end Class_menu;
