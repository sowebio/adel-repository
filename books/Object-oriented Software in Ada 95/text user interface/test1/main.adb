

with Class_input_manager, Class_window,
     Class_dialog, Pack_globals, user_input;
use  Class_input_manager, Class_window,
     Class_dialog, Pack_globals;
procedure main is
begin
  window_prologue;                -- Setup window system
  declare
    result : aliased Window;      -- Result window
    input  : Dialog;              -- Input Window
    title  : Window;              -- title Window
  begin
    framework( title,  20,  1,  36, 5 );   -- Title Window
    framework( result, 30, 10,  36, 5 );   -- Result Window
  
    position( title, 8, 2 );
    put( title, "Miles to kilometres" );
    framework( input, 5, 10, 22,            -- Input Window
               "Miles", user_input'Access );
    p_result := result'Unchecked_Access;
  
    window_start;             -- Start the user interaction
  end;
  window_epilogue;            -- Close window system
end main;
