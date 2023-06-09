--::::::::::
--formstri.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
function format_string (fmt: string) return string is
   arg: natural;
   separator: constant character := '%';
begin
   if fmt'length > 1
   then
      if fmt (fmt'first) /= separator
      then
	 for n in fmt'first + 1 .. fmt'last
	 loop
	     if fmt (n) = separator
	     then
	       return fmt (fmt'first..n - 1) &
		      format_string ( fmt (n..fmt'last));
	     end if;
	 end loop;
      else
	 arg := 0;
	 for n in fmt'first + 1 .. fmt'last
	 loop -- read arg
	    if fmt (n) in '0'..'9'
	    then
	    arg := 10 * arg + character'pos (fmt (n))
			    - character'pos ('0');
	    else
		return argument (arg) & format_string (fmt (n..fmt'last));
	    end if;
	 end loop;
	 return argument (arg);
      end if;
   end if;
   return fmt;
end;
