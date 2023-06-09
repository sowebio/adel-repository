--::::::::::
--testing.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with program_log, io_exceptions;
use  io_exceptions;
package body testing is

    function again return boolean is

	package log is new program_log (program_name, "try");
	use log;

	function report (ename : string) return boolean is
	begin
	    error   ("alarm: " & ename);
	    warning ("*** test failed ***");
	    return false;
	end report;

    begin

      assert (program, "false: " & description,
		       " true: " & description);
      return true;

    exception

      when assertion_failed => return report ("assertion alarm");

      when constraint_error => return report ("Constraint Error");
      when program_error =>    return report ("Program    Error");
      when storage_error =>    return report ("Storage    Error");
      when tasking_error =>    return report ("Tasking    Error");

      when status_error  =>    return report ("Status     Error");
      when mode_error    =>    return report ("Mode       Error");
      when name_error    =>    return report ("Name       Error");
      when use_error     =>    return report ("Use        Error");
      when device_error  =>    return report ("Device     Error");
      when end_error     =>    return report ("End        Error");
      when data_error    =>    return report ("Data       Error");

--      when numeric_error =>  return report ("Numeric    Error");
      when others =>         error   ("alarm: <NAME UNKNOWN>");
			     warning ("*** test failed ***");
			     raise;
   end again;

begin
   if again then null; end if;
end testing;
