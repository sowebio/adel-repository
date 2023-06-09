--::::::::::
--filename.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
package filenames is

    function fullname   (name: in string) return string; -- "l:/d/f.e"
    function volumename (name: in string) return string; -- "l:"
    function pathname   (name: in string) return string; -- "/d/"
    function filename   (name: in string) return string; -- "f"
    function typename   (name: in string) return string; -- ".e"
    function existent   (name: in string) return boolean;

end;
