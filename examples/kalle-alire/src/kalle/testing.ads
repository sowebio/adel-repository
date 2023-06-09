--::::::::::
--testing.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
generic
    with function program return boolean;
    program_name  : string := " (no name)";
    description   : string := " (no description)";
package testing is

    function again return boolean;

end;
