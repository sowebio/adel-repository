--::::::::::
--stritool.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with  array_handler;
with  boolean_set;
package string_tools is

     package string_handler is new array_handler (character, string, 256, "txt");
     subtype string_object  is string_handler.object;

     generic
	  with function argument (n: natural) return string;
	  separator: in character := '%';
     procedure format (fmt: string; buf: in out string_object);

     package charsets is new boolean_set (character);
     use charsets;

     subtype charset is charsets.boolset;

     function value is new charsets.array_import (positive, string);
     package string_charsets is new charsets.import_set (string);

     function spaces   return charset;
     function numbers  return charset;
     function lowers   return charset;
     function uppers   return charset;
     function controls return charset;
     function specials return charset;

     function locate (frag:     charset;
		      within:   string_object;
		      from:     positive := 1;
		      to:       positive := positive'last) return natural;

     type translation is array (character) of character;

     function upper_case return translation;
     function lower_case return translation;
     function flip_case return translation;
     function same_case return translation;

     function  translate (item: string; tab: translation) return string;
     procedure translate (item: in out string; tab: translation);

     function  translate is new
     string_handler.translating_poly_in (translation, string, translate);

     procedure translate is new
     string_handler.transforming_poly_in_out (translation, translate);

end;
