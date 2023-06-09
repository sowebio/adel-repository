
--------------------------------------------------------------------

with Class_root_window, Class_window;
use  Class_root_window, Class_window;
package Class_menu is
  type Menu is new Window with private;
  type P_Menu is access all Menu;

  procedure framework( the:in out Menu'Class;
     m1:in String:=""; w1:in P_Menu:=null; cb1:in P_cbf:=null;
     m2:in String:=""; w2:in P_Menu:=null; cb2:in P_cbf:=null;
     m3:in String:=""; w3:in P_Menu:=null; cb3:in P_cbf:=null;
     m4:in String:=""; w4:in P_Menu:=null; cb4:in P_cbf:=null;
     m5:in String:=""; w5:in P_Menu:=null; cb5:in P_cbf:=null;
     m6:in String:=""; w6:in P_Menu:=null; cb6:in P_cbf:=null );

  procedure set_up( the:in out Menu; active:in Positive);
  procedure menu_spot( the:in out Menu; ch:in Character );
  procedure send_to( the:in out Menu; ch:in Character );

  MAX_MENU : CONSTANT Positive := 10;
  subtype Menu_item is String( 1 .. MAX_MENU );

  procedure get_menu_name( the:in Menu; i:in Positive;
                           n:out Menu_item );
  procedure get_cur_selected_details( the:in P_Menu;
                            w:out P_Menu; cb:out P_cbf );
private
  type    Direction is (D_reverse, D_forWARD);
  procedure next( the:in out Menu; dir:in Direction );

  type Menu_desc is record  -- A menu is:
    name: Menu_item;        -- Name of menu item
    p_m : P_Menu;           -- Menu window
    fun : P_cbf;            -- Call back function
  end record;

  MAX_MENU_ITEMS : CONSTANT := 6;    -- Maximum menu items

  type    Menus_index is range 0 .. MAX_MENU_ITEMS;
  subtype Menus_range is Menus_index range 1 .. MAX_MENU_ITEMS;
  type    Menus       is array ( Menus_range ) of Menu_desc;

  type Menu is new Window with record
    number   : Menus_index := 0;   -- Number of menu items
    cur_men  : Menus_index := 1;   -- Currently selected item
    menu_set : Menus;              -- Components of a menu
  end record;
end Class_menu;
