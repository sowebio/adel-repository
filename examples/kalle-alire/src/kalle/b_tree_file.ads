--::::::::::
--btrefile.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
generic

   type item_type is private;
   type data_type is private;

   with function "<" (left, right: item_type) return boolean is <>;

   degree: in positive := 16;

package b_tree_file is

  type tree_type is limited private;
  type tree_mode is (in_tree, inout_tree, out_tree);

  procedure open   (file : in out tree_type;
		    mode : in     tree_mode;
		    name : in     string := "";
		    form : in     string := "");

  procedure create (file : in out tree_type;
		    mode : in     tree_mode := inout_tree;
		    name : in     string := "";
		    form : in     string := "");

  procedure delete (file : in out tree_type);
  procedure close  (file : in out tree_type);
  procedure reset  (file : in out tree_type);
  procedure reset  (file : in out tree_type;
		    mode : in     tree_mode);

  function is_open (file : in     tree_type) return boolean;
  function mode    (file : in     tree_type) return tree_mode;
  function name    (file : in     tree_type) return string;
  function form    (file : in     tree_type) return string;

  procedure insert (file : in out tree_type;
		    item : in     item_type;
		    data : in     data_type);

  procedure modify (file : in out tree_type;
		    item : in     item_type;
		    data : in     data_type;
		    ok   :    out boolean);

  procedure delete (file : in out tree_type;
		    item : in     item_type;
		    data :    out data_type;
		    ok   :    out boolean);

  procedure get_ge (file : in     tree_type;
		    item : in out item_type;
		    data :    out data_type;
		    ok   :    out boolean);

  procedure get_gt (file : in     tree_type;
		    item : in out item_type;
		    data :    out data_type;
		    ok   :    out boolean);

  procedure get_le (file : in     tree_type;
		    item : in out item_type;
		    data :    out data_type;
		    ok   :    out boolean);

  procedure get_lt (file : in     tree_type;
		    item : in out item_type;
		    data :    out data_type;
		    ok   :    out boolean);

  procedure get_first (file : in     tree_type;
		       item :    out item_type;
		       data :    out data_type;
		       ok   :    out boolean);

  procedure get_last  (file : in     tree_type;
		       item :    out item_type;
		       data :    out data_type;
		       ok   :    out boolean);

private

  type tree_object;
  type tree_type is access tree_object;
end b_tree_file;
