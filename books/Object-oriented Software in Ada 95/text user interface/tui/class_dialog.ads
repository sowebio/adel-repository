

--------------------------------------------------------------------

with Pack_constants, Class_root_window, Class_window;
use  Pack_constants, Class_root_window, Class_window;
package Class_dialog is
  type Dialog is new Window with private;

  procedure framework ( the:in out Dialog;
                        abs_x, abs_y:in Positive;
                        max_x: in Positive;
                        name:in String; cb:in P_cbf );

  procedure send_to( the:in out Dialog; ch:in Character );
private
  subtype Message is String( 1 ..  WINDOW_MAX_X );
  type Dialog is new Window with record
    dialog_pos: Positive := 1;  -- Position in input message
    dialog_len: Positive := 1;  -- Length of dialogue message
    dialog_mes: Message := ( others => ' '); -- Input message
  end record;
end Class_dialog;
