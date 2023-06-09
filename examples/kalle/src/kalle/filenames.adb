--::::::::::
--filename.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with text_io;
package body filenames is

  type septype is (volume, pathlist, typemark, version, namechar);

  function sep (c: character) return septype is
  begin
     case c is
	when '.'       => return typemark;
	when '\' | '/' |
	     '[' | ']' => return pathlist;
	when ':'       => return volume;
	when ';'       => return version;
	when others    => return namechar;
     end case;
  end;

  ------------------------------------------------------------------------

  function volumename (name: in string) return string is
  begin
     for n in name'range
     loop
	 case sep(name (n)) is
	     when volume   => return name (name'first..n);
	     when namechar => null;
	     when others   => exit;
	 end case;
     end loop;
     return "";
  end;

  ------------------------------------------------------------------------

  function pathname   (name: in string) return string is
     p_beg: positive := name'first + volumename (name)'length;
     p_end: natural  := 0;
  begin
     for n in  p_beg..name'last
     loop
	 case sep(name (n)) is
	     when pathlist  => p_end := n;
	     when others    => null;
	 end case;
     end loop;

     if p_beg <= p_end
     then
	 return name (p_beg..p_end);
     end if;
     return "";
  end;

  ------------------------------------------------------------------------

  function filename   (name: in string) return string is
   f_end: natural := name'last;
   f_beg: natural := name'first;
  begin
     for n in reverse name'range
     loop
	 case sep(name (n)) is
	     when typemark |
		  version  => f_end := n - 1;
	     when namechar => null;
	     when others   => f_beg := n + 1;
			      exit;
	 end case;
     end loop;
     return name (f_beg..f_end);
  end;

  ------------------------------------------------------------------------

  function typename   (name: in string) return string is
   t_end: natural  := name'last;
  begin
     for n in reverse name'range
     loop
	 case sep(name (n)) is
	     when typemark => return name (n..t_end);
	     when version  => t_end := n - 1;
	     when namechar => null;
	     when others   => exit;
	 end case;
     end loop;
     return "";
  end;

  ------------------------------------------------------------------------

  function fullname   (name: in string) return string is
    use text_io;
    file: file_type;

    function checked (s: in string) return string is
      tmp: string (name'range);
    begin
      for n in tmp'range
      loop
	  case s(n) is
	    when '\'    => tmp (n) := '/';
	    when others => tmp (n) := s(n);
	  end case;
      end loop;
      return tmp;
    end;

    function name_then_close  (n: string) return string is
    begin
	 close (file); return (n);
    end;

  begin
    if name'length > 0
    then
	 begin
	      open (file, name => checked (name), mode => in_file);
	      return name_then_close (text_io.name (file));
	 exception
	      when others => null;
	 end;

    end if;
    return name;
  end;

  function existent   (name: in string) return boolean is
      use text_io;
      file: file_type;

  begin
    if name'length > 0
    then begin
	  open (file, name => name, mode => in_file);
	  close (file); return true;
      exception
	  when others => null;
      end;

      begin
	  open (file, name => name, mode => out_file);
	  close (file); return true;
      exception
	  when others => null;
      end;
    end if;
    return false;
  end;

end;
