--::::::::::
--stritool.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
package body string_tools is

     function build_same_case return translation;

     the_same_case: constant translation := build_same_case;

     procedure format (fmt: string; buf: in out string_object) is
	  use string_handler;
	  mrk, pos, arg: natural;

     begin
          mrk := fmt'first;
          loop
               if mrk > fmt'last
               then
                    return;
               end if;
               pos := mrk;
               loop
		   exit when fmt (pos) = separator;
                   if pos = fmt'last
                   then
                        append (fmt (mrk..pos), buf);
                        return;
                   end if;
                   pos := pos + 1;
               end loop;
	       if fmt(pos) = separator
               then
                    if pos = fmt'last
                    then
                         append (fmt (mrk..pos - 1), buf);
                         return;
                    end if;
                    pos := pos + 1;
                    case fmt (pos) is
                         when '0'..'9' =>
                              append (fmt (mrk..pos - 2), buf);
                              arg := 0;
                              loop
				  arg := 10 * arg + character'pos (fmt (pos))
						  - character'pos ('0');
                                  if pos = fmt'last
                                  then
                                       append (argument (arg), buf);
                                       return;
                                  end if;
                                  pos := pos + 1;
                                  exit when not (fmt (pos) in '0'..'9');
                              end loop;
                              append (argument (arg), buf);
                              mrk := pos;
			 when '%' => -- ??? separator  =>
                              append (fmt (mrk..pos - 1), buf);
                              if pos = fmt'last
                              then
                                   return;
                              end if;
                              mrk := pos + 1;
                         when others   =>
                              append (fmt (mrk..pos - 2), buf);
                              mrk := pos;
                    end case;
               end if;
          end loop;
     end format;

    -----------------------------------------------------------------------

    function locate (frag:     charset;
		      within:  string_object;
		      from:    positive := 1;
		      to:      positive := positive'last)
		      return natural is

	 use string_handler;

	 function pos (s: string) return natural;
	 function findit is new translating_mono_in (natural, pos);

	 function pos (s: string) return natural is
	 begin
	     if from in s'range then
		     for i in s'range
		     loop
		       if frag (s(i)) then
			  return i;
		       end if;
		     end loop;
	     end if;
	     return 0;
	 end;

    begin
	 return findit (within, from, to);
    end;

    -----------------------------------------------------------------------

    function spaces   return charset is
    begin
       return value (ascii.vt & ascii.ht & character'val(32));
    end;

    -----------------------------------------------------------------------

--    function value (t: translation) return charset is
--      tmp: charset := (others => false);
--    begin
--      for n in t'range
--      loop
--         tmp (n) := true;
--      end loop;
--      return tmp;
--    end;

    -----------------------------------------------------------------------

    function numbers  return charset is
    begin
      return ('0'..'9' => true, others => false);
    end;

    -----------------------------------------------------------------------

    function lowers   return charset is
    begin
      return ('a'..'z' => true, others => false);
    end;

    -----------------------------------------------------------------------

    function uppers   return charset is
    begin
      return ('A'..'Z' => true, others => false);
    end;

    -----------------------------------------------------------------------

    function controls return charset is
    begin
       return (character'val(0)..character'val(31) => true,
	       others => false);
    end;

    -----------------------------------------------------------------------

    function specials return charset is
    begin
       return value (",:;.!?`'<>{}[]()\/@#$%^&*|~+-");
    end;

    -----------------------------------------------------------------------

    procedure translate (item: in out string; tab: translation) is
    begin

	 for i in item'range
	 loop
	     item (i) := tab (item (i));
	 end loop;

    end;

    -----------------------------------------------------------------------

    function  translate (item: string; tab: translation) return string is
	 tmp : string (item'range);
    begin

	 tmp := item;
	 translate (tmp, tab);
	 return tmp;

    end;

    -----------------------------------------------------------------------

    function upper_case return translation is
      t: translation := the_same_case;
    begin
      t ('a'..'z') := the_same_case ('A'..'Z');
      return t;
    end;

    -----------------------------------------------------------------------

    function lower_case return translation is
      t: translation := the_same_case;
    begin
      t ('A'..'Z') := the_same_case ('a'..'z');
      return t;
    end;

    -----------------------------------------------------------------------

    function flip_case return translation is
      t: translation := the_same_case;
    begin
      t ('a'..'z') := the_same_case ('A'..'Z');
      t ('A'..'Z') := the_same_case ('a'..'z');
      return t;
    end;

    -----------------------------------------------------------------------

    function same_case return translation is
    begin
      return the_same_case;
    end;

    -----------------------------------------------------------------------

    function build_same_case return translation is
      t: translation;
    begin
      for n in t'range
      loop
	 t (n) := n;
      end loop;
      return t;
    end;

end;
