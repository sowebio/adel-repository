--::::::::::
--test_ah.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.

with array_handler, testing, text_io;
procedure test_ah is

 package text_handler is new array_handler (character, string, 80, "string", false);
 subtype text_type    is text_handler.object;

 procedure put is new text_handler.transfering_mono_in (text_io.put_line);
 procedure get is new text_handler.transfering_mono_out_changes (text_io.get_line);

   function t12 return boolean is
      use text_handler;
      a, b: text_type(50);
   begin
      set    (a, "wasq123ras");
      set    (b, "wasq1qqq23ras");
      return suffix(a, b) = 5;
   end;
   package p12 is new testing (t12, "suffix","ordinary");

   function t13 return boolean is
      use text_handler;
      t: text_type(50);
   begin
      set    (t, "wasq123ras");
      insert (t, '4', 5);
      return value(t) = "wasq4123ras";
   end;
   package p13 is new testing (t13, "insert","with expand for 1 elem");

   function t14 return boolean is
      use text_handler;
      t: text_type(50);
   begin
      set    (t, "wasq123ras");
      insert (t, '4', 5, 1);
      return value(t) = "wasq423ras";
   end;
   package p14 is new testing (t14, "insert","with amend for 1 elem");

   function t15 return boolean is
      use text_handler;
      t: text_type(50);
   begin
      set    (t, "wasq123ras");
      insert (t, '4', 5, 3);
      return value(t) = "wasq4ras";
   end;
   package p15 is new testing (t15, "insert","with delete for 1 elem");

   function t16 return boolean is
      use text_handler;
      t: text_type(50);
   begin
      set    (t, "wasq123ras");
      insert (t, "45", 5, 3);
      return value(t) = "wasq45ras";
   end;
   package p16 is new testing (t16, "insert", "with delete");

   function t17 return boolean is
      use text_handler;
      t: text_type(50);
   begin
      set    (t, "wasq123ras");
      insert (t, "456789", 5, 3);
      return value(t) = "wasq456789ras";
   end;
   package p17 is new testing (t17, "insert", "with expand");

   function t19 return boolean is
      use text_handler;
      t, u: text_type(50);
    begin
      set (t,      "wasq123ras");
      expand (t, 5, 7);
      return value(t) = "wasq123123ras";
    end;
   package p19 is new testing (t19, "expand", "ordinary");

   function t20 return boolean is
      use text_handler;
      t, u: text_type(10);
   begin
      set (t, "wasq123ras");
      delete (t, 5, 7);
      return value(t) = "wasqras";
   end;
   package p20 is new testing (t20, "delete", "ordinary");

   function t21 return boolean is
      use text_handler;

      t: text_type(150);
   begin
      set (t, "123a123b123c123d123eabcabcabcxqqqqq");
      return      locate ("123a", t)     =  1
         and then locate ("a1", t)       =  4
         and then locate ("123b", t)     =  5
         and then locate ("123e", t)     = 17
         and then locate ("" , t)        =  0
         and then locate ("456" , t)     =  0
         and then locate ("bcx", t)      = 28
         and then locate ("abcx", t)     = 27
         and then locate ("cabcx", t)    = 26
         and then locate ("bcabcx", t)   = 25;
   end;
   package p21 is new testing (t21, "locate", "full subpattern matching");

begin
   null;
end;
