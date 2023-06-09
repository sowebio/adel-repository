
-------------------------------------------------------------------

with Pack_constants, Ada.Finalization;
use  Pack_constants, Ada.Finalization;
package Class_root_window is
  type Root_window   is abstract tagged limited private;
  type P_Root_window is access all Root_window'Class;
  type Attribute is ( TOP, BOTTOM, LEFT, RIGHT, abs_X, abs_Y );

  procedure send_to( the:in out Root_window;
                     ch:in Character) is abstract;
  procedure switch_to( the:in out Root_window ) is abstract;
  procedure switch_away( the:in out Root_window ) is abstract;
  function  about( the:in Root_window;
                   b:in Attribute) return Natural is abstract;
private
  type Root_window is
    abstract new Limited_controlled with null record;

end Class_root_window;
