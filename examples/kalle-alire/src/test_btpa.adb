--::::::::::
--testbtpa.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.

with text_io, b_tree_file, testing, program_log;

procedure test_btpaged is

   test_count: constant := 300;

   subtype item_type is string (1.. 5); -- long_integer;
   subtype data_type is string (1..10); -- long_integer;

   package b_tree is new b_tree_file (item_type, data_type);

   package bt_log is new program_log ("btree");
   package int_io is new text_io.integer_io (integer);

   use b_tree,
       int_io,
       bt_log;

   tree: tree_type;
   item: item_type;
   itm1: item_type;
   data: data_type;
   okay: boolean;

   name: constant string := "bt_file.ndx";

   procedure construct (tree: in out tree_type) is
   begin
      create (tree, name => name);
   end;

   procedure prepare (tree: in out tree_type) is
   begin
      open (tree, inout_tree, name);
   end;

   procedure release (tree: in out tree_type) is
   begin
      close (tree);
   end;

   procedure info (msg: string;
		     n: integer;
		  item: item_type;
		  data: data_type) is
   begin
      error (msg & " : n = " & integer'image(n) &
			 ";" & item & "," & data);
   end;

   function true_value (n: integer; item: item_type) return boolean is
      test: integer;
      last: integer;

   begin
      get (item, test, last);
      return test = n;
   end;


   function insertion return boolean is
   begin

      construct (tree);

      for n in reverse 1..test_count
      loop
	 put    (item, n * 10);
	 put    (data, n * 10);
	 insert (tree, item, data);
      end loop;
      release (tree);
      return true;
   end;

   function ge_upper_once return boolean is
   begin

      prepare (tree);

      put    (item, test_count * 10 + 1);

      get_ge (tree, item, data, okay);
      if okay
      then
	 info ("result is true", test_count * 10 + 1, item, data);
	 release (tree);
	 return false;
      end if;

      release (tree);
      return true;
   end;

   function ge_lower_all return boolean is
   begin

      prepare (tree);

      for n in 1..test_count
      loop
	 put    (item, n * 10 - 1);
	 get_ge (tree, item, data, okay);

	 if not okay
	 then
	    info ("result is false", n * 10 - 1, item, data);
	    release (tree);
	    return false;
	 end if;

	 if not true_value (n * 10, item)
	 then
	    info ("value is false", n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

      end loop;

      release (tree);
      return true;
   end;

   function ge_equal_all return boolean is
   begin

      prepare (tree);

      for n in 1..test_count
      loop
	 put    (item, n * 10);
	 get_ge (tree, item, data, okay);

	 if not okay
	 then
	    info ("result is false",  n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

	 if not true_value (n * 10, item)
	 then
	    info ("value is false",  n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

      end loop;

      release (tree);
      return true;
   end;

   function le_lower_once return boolean is
   begin

      prepare (tree);

      put    (item, 1 * 10 - 1);

      get_le (tree, item, data, okay);
      if okay
      then
	 info ("result is true", 9, item, data);
	 release (tree);
	 return false;
      end if;

      release (tree);
      return true;
   end;

   function le_equal_all return boolean is
   begin

      prepare (tree);

      for n in 1..test_count
      loop
	 put    (item, n * 10    );
	 get_le (tree, item, data, okay);

	 if not okay
	 then
	    info ("result is false ", n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

	 if not true_value (n * 10, item)
	 then
	    info ("value is false", n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

      end loop;

      release (tree);
      return true;
   end;

   function le_upper_all return boolean is
   begin

      prepare (tree);

      for n in 1..test_count
      loop
	 put    (item, n * 10 + 1);
	 get_le (tree, item, data, okay);

	 if not okay
	 then
	    info ("result is false ", n * 10 + 1, item, data);
	    release (tree);
	    return false;
	 end if;

	 if not true_value (n * 10, item)
	 then
	    info ("value is false", n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

      end loop;

      release (tree);
      return true;
   end;

   function lt_lower_once return boolean is
   begin

      prepare (tree);

      put    (item, 1 * 10 - 1);

      get_lt (tree, item, data, okay);
      if okay
      then
	 info ("result is true", 9, item, data);
	 release (tree);
	 return false;
      end if;

      release (tree);
      return true;
   end;

   function lt_upper_all return boolean is
   begin

      prepare (tree);

      for n in 1..test_count
      loop
	 put    (item, n * 10 + 1);
	 get_lt (tree, item, data, okay);

	 if not okay
	 then
	    info ("result is false", n * 10 + 1, item, data);
	    release (tree);
	    return false;
	 end if;

	 if not true_value (n * 10, item)
	 then
	    info ("value is false", n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

      end loop;

      release (tree);
      return true;
   end;

   function lt_equal_all return boolean is
   begin

      prepare (tree);

      for n in 1..test_count
      loop
	 put    (item, n * 10 + 10);
	 get_lt (tree, item, data, okay);

	 if not okay
	 then
	    info ("result is false ", n * 10 + 10, item, data);
	    release (tree);
	    return false;
	 end if;

	 if not true_value (n * 10, item)
	 then
	    info ("value is false", n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

      end loop;

      release (tree);
      return true;
   end;

   function gt_upper_once return boolean is
   begin

      prepare (tree);

      put    (item, test_count  * 10 + 1);

      get_gt (tree, item, data, okay);
      if okay
      then
	 info ("result is true", test_count * 10 + 1, item, data);
	 release (tree);
	 return false;
      end if;

      release (tree);
      return true;
   end;

   function gt_lower_all return boolean is
   begin

      prepare (tree);

      for n in 1..test_count
      loop
	 put    (item, n * 10 - 1);
	 get_gt (tree, item, data, okay);

	 if not okay
	 then
	    info ("result is false ", n * 10 - 1, item, data);
	    release (tree);
	    return false;
	 end if;

	 if not true_value (n * 10, item)
	 then
	    info ("test get_gt for ", n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

      end loop;

      release (tree);
      return true;
   end;

   function gt_equal_all return boolean is
   begin

      prepare (tree);

      for n in 1..test_count
      loop
	 put    (item, n * 10 - 10);
	 get_gt (tree, item, data, okay);

	 if not okay
	 then
	    info ("result is false ", n * 10 - 10, item, data);
	    release (tree);
	    return false;
	 end if;

	 if not true_value (n * 10, item)
	 then
	    info ("test get_gt for ", n * 10, item, data);
	    release (tree);
	    return false;
	 end if;

      end loop;

      release (tree);
      return true;
   end;

   function del_first return boolean is
   begin
      prepare (tree);

      for n in reverse 10..150
      loop
	put     (item, n * 10);
	delete  (tree, item, data, okay);

	if not okay
	then
	    info ("result is false", n * 10, item, data);
	    release (tree);
	    return false;
	end if;

	if not true_value (n * 10, item)
	then
	    info ("test deleted item value,   ", n * 10, item, data);
	    release (tree);
	    return false;
	end if;

      end loop;

      for n in         10..150
      loop
	put     (item, n * 10);

	delete  (tree, item, data, okay);
	if okay
	then
	    info ("result is true", n * 10, item, data);
	    release (tree);
	    return false;
	end if;

      end loop;

      release (tree);
      return true;
   end;

   function del_second return boolean is
   begin
      prepare (tree);

      for n in        151..300
      loop
	put     (item, n * 10);
	delete  (tree, item, data, okay);

	if not okay
	then
	    info ("result is false", n * 10, item, data);
	    release (tree);
	    return false;
	end if;

	if not true_value (n * 10, item)
	then
	    info ("value is false", n * 10, item, data);
	    release (tree);
	    return false;
	end if;

      end loop;

      for n in reverse 150..300
      loop
	put     (item, n * 10);

	delete  (tree, item, data, okay);
	if okay
	then
	    info ("result is true", n * 10, item, data);
	    release (tree);
	    return false;
	end if;

      end loop;

      release (tree);
      return true;
   end;

   package test_insertion     is new testing ( insertion,
	    "insert", "inserting values in empty tree");

   package test_ge_upper_once is new testing ( ge_upper_once,
	   "get_ge", "try to get value over the upper bound");

   package test_ge_lower_all  is new testing ( ge_lower_all,
	   "get_ge", "find values by less argument");

   package test_ge_equal_all  is new testing ( ge_equal_all,
	   "get_ge", "find values by equal argument");

   package test_le_lower_once is new testing ( le_lower_once,
	   "get_le", "try to get value over the lower bound");

   package test_le_equal_all  is new testing ( le_equal_all,
	   "get_le", "find values by equal argument");

   package test_le_upper_all  is new testing ( le_upper_all,
	    "get_le", "find values by greater argument");

   package test_lt_lower_once is new testing ( lt_lower_once,
	   "get_lt",  "try to get value over the lower bound");

   package test_lt_upper_all  is new testing ( lt_upper_all,
	   "get_lt", "find values by greater argument");

   package test_lt_equal_all  is new testing ( lt_equal_all,
	   "get_lt", "find values by equal argument");

   package test_gt_upper_once is new testing ( gt_upper_once,
	   "get_gt", "try to get value over the upper bound");

   package test_gt_lower_all  is new testing ( gt_lower_all,
	   "get_gt", "find values by less argument");

   package test_gt_equal_all  is new testing ( gt_equal_all,
	   "get_gt", "find values by equal argument");

   package test_del_first     is new testing ( del_first,
	   "delete", "1st part");
   package test_del_second    is new testing ( del_second,
	   "delete", "2nd part");

begin
   null;
end test_btpaged;
