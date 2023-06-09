
--------------------------------------------------------------------

with Class_root_window, Class_window, Class_menu;
use  Class_root_window, Class_window, Class_menu;
package Class_menu_title is
  type Menu_title is new Menu with private;
  type P_Menu_title is access all Menu_title;

  procedure set_up( the:in out Menu_title; active:in Positive );
  procedure send_to( the:in out Menu_title; ch:in Character );
  procedure switch_away( the:in out Menu_title );
private

  MAX_ACT_MENU : CONSTANT := 6;    -- Maximum depth of menus
  type    Act_index is range 0 .. MAX_ACT_MENU;
  subtype Act_range is Act_index range 1 .. MAX_ACT_MENU;
  type    Act_menus is array ( Act_range ) of P_Menu;

  type Menu_title is new Menu with record
    act_menu  : Act_menus;        -- Stack of displayed menus
    menu_index: Act_index := 0;   -- Top of menu stack
  end record;
end Class_menu_title;
