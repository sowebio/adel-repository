
-------------------------------------------------------------

with Pack_constants, Class_root_window,
     Class_input_manager, Class_window_control;
use  Pack_constants, Class_root_window,
     Class_input_manager, Class_window_control;
package Class_window is
  type Window   is new Root_window with private;
  type P_Window is access all Window;

  type Mode    is ( VisIBLE, INVisIBLE );
  type P_cbf   is access function(str:in String) return String;

  procedure initialize( the:in out Window );
  procedure finalize( the:in out Window );

  -- Basic construction

  procedure framework( the:in out Window;
                       abs_x_crd, abs_y_crd: Positive;
                       max_x_crd, max_y_crd: Positive;
                       cb:in P_cbf := null );
  procedure create   ( the:in out Window;
                       abs_x_crd, abs_y_crd: Positive;
                       max_x_crd, max_y_crd: Positive );

  -- Call back function processing

  procedure set_call_back( the:in out Window; cb:in P_cbf );
  function call_call_back( the:in Window; 
                           str:in String ) return String;

  -- I/O to a window

  procedure put( the:in out Window; mes:in String );
  procedure put( the:in out Window; ch:in Character );
  procedure put( the:in out Window; n:in Integer );

  procedure position( the:in out Window; x,y:in Positive );
  procedure clear( the:in out Window );
  procedure new_line( the:in out Window );
  procedure refresh( the:in out Window );

  -- Look and Feel

  procedure make_window( the:in out Window; mo:in Mode );
  procedure mark_border( the:in out Window;
                          a_border:in Attribute;
                          pos:in Positive; ch:in Character );
  function about(the:in Window; b:in Attribute) return Natural;

  -- When window selected do

  procedure switch_away( the:in out Window );
  procedure switch_to( the:in out Window );
  procedure send_to( the:in out Window; ch:in Character );

  -- Register window with poling system

  procedure register( p_w:in P_Root_window; ch:in Character );
  procedure de_register( p_w:in P_Root_window );
private
  subtype Y_Cord is Positive range 1 .. VDT_MAX_Y;
  subtype X_Cord is Positive range 1 .. VDT_MAX_X;

  subtype Line_index  is X_Cord range 1 .. WINDOW_MAX_X;
  subtype Line_range  is Line_index;
  subtype Line        is String( Line_range );

  subtype Pane_index  is Y_Cord range 1 .. WINDOW_MAX_Y;
  subtype Pane_range  is Pane_index;
  type    Pane_array  is array ( Pane_range ) of Line;

  type Window is new Root_window with record
    abs_x    : X_Cord := 1;    -- The position on the vdt
    abs_y    : Y_Cord := 1;    -- The position on the vdt
    c_x      : X_Cord := 1;    -- Current position in window
    c_y      : Y_Cord := 1;    -- Current position in window
    max_x    : X_Cord := 5;    -- X size of window (+Border)
    max_y    : Y_Cord := 5;    -- Y size of window (+Border)
    pane     : Pane_array;     -- Copy of window in memory
    mode_of  : Mode := INVisIBLE;-- Invisible window by default
    call_back: P_cbf := null;  -- Call back function
  end record;
end Class_window;
