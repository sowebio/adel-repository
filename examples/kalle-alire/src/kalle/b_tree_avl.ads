--::::::::::
--btreeavl.ads
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

package b_tree_avl is

   type tree_type is limited private;

   procedure delete (tree : in out tree_type);

   procedure insert (tree : in out tree_type;
		     item :        item_type;
		     data :        data_type);

   procedure modify (tree : in out tree_type;
		     item :        item_type;
		     data :        data_type;
		       ok :    out boolean);

   procedure delete (tree : in out tree_type;
		     item :        item_type;
		     data :    out data_type;
		       ok :    out boolean);

   procedure get_first (tree: in     tree_type;
			item:    out item_type;
			data:    out data_type;
			  ok:    out boolean);

   procedure get_last  (tree: in     tree_type;
			item:    out item_type;
			data:    out data_type;
			  ok:    out boolean);

   procedure get_ge (tree:        tree_type;
		     item: in out item_type;
		     data:    out data_type;
		     ok  :    out boolean);

   procedure get_gt (tree:        tree_type;
		     item: in out item_type;
		     data:    out data_type;
		     ok  :    out boolean);

   procedure get_le (tree:        tree_type;
		     item: in out item_type;
		     data:    out data_type;
		     ok  :    out boolean);

   procedure get_lt (tree:        tree_type;
		     item: in out item_type;
		     data:    out data_type;
		     ok  :    out boolean);
private

       type tree_data;
       type tree_type is access tree_data;
end;
