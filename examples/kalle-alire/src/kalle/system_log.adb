--::::::::::
--systelog.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.

-- with extended_calendar;
-- use  extended_calendar;
with calendar;
use  calendar;

package body system_log is

   sep: constant string := ": ";

   procedure put_line (s: string) is separate;

   function image (moment: time) return string is
   begin
	-- unimplemented in this release
	return ""; -- timeimage (moment, " [%40:%50:%60]");
   end;

   procedure error (msg: string;
		 moment: time   := start_time;
		  owner: string := owner_name) is
   begin
       if moment /= start_time then
	   put_line ("*** " & owner & image(moment) & sep & msg);
       else
	   put_line ("*** " & owner & sep & msg);
       end if;
   end;

   procedure warning (msg: string;
		   moment: time   := start_time;
		    owner: string := owner_name) is
   begin
       if moment /= start_time then
	   put_line ("+++ " & owner & image(moment) & sep & msg);
       else
	   put_line ("+++ " & owner & sep & msg);
       end if;
   end;

   procedure message (msg: string;
		   moment: time   := start_time;
		    owner: string := owner_name) is
   begin
       if moment /= start_time then
	   put_line ("--- " & owner & image(moment) & sep & msg);
       else
	   put_line ("--- " & owner & sep & msg);
       end if;
   end;

   procedure assert (hypothesis: boolean;
                          false_msg: string := "";
                           true_msg: string := "";
			     moment: time   := start_time;
			      owner: string := owner_name) is
   begin
       if not hypothesis
       then
           if false_msg'length > 0
           then
                 error (false_msg, owner => owner);
           end if;
           raise assertion_failed;
       else
           if true_msg'length > 0
           then
                 message (true_msg, owner => owner);
           end if;
       end if;
   end;

end system_log;
