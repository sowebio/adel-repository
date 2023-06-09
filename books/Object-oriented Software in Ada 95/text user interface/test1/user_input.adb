

with Simple_io, Class_window, Class_dialog, Pack_globals;
use  Simple_io, Class_window, Class_dialog, Pack_globals;
function user_input( cb_mes:in String ) return String is
  miles  : Float;             -- Miles input by user
  last   : Positive;          --
  str_kms: String( 1 .. 10 ); -- As a string in Kms
  str_mls: String( 1 .. 10 ); -- As a string in Miles
begin
  begin
    get( cb_mes & ".", miles, last );
    put( str_kms, miles * 1.609_344, aft=>2, exp=>0 );
    put( str_mls, miles, aft=>2, exp=>0 );
    put( p_result.all, "Distance in Miles = " );
    put( p_result.all, str_mls ); new_line( p_result.all );
    put( p_result.all, "Distance in Kms   = " );
    put( p_result.all, str_kms ); new_line( p_result.all );
  exception
    when Data_Error =>
      put( p_result.all, " Not a valid number" );
      new_line( p_result.all );
    when others =>
      put( p_result.all, " [Calculation error]" );
      new_line( p_result.all );
  end;
  return "";
end user_input;
