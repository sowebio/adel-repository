--::::::::::
--btrefile.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.

with unchecked_deallocation,
     direct_io;

package body b_tree_file is

  seek_softly: constant boolean  := true;
  comp_softly: constant boolean  := true;

  tree_degree: constant positive := degree;
  node_length: constant positive := 2 * tree_degree;

  --------------------------------------------------------------------------

  subtype node_position is long_integer range 0..long_integer'last;

  --------------------------------------------------------------------------

  type element is
  record
     node : node_position := 0;
     item : item_type;
     data : data_type;
  end record;

  type    elem_count is new integer range  0..node_length;
  subtype elem_index is elem_count  range  1..elem_count'last;

  --------------------------------------------------------------------------

  type elements      is array (elem_index range <>) of element;
  type elem_sequence is
  record
    len: elem_count := 0;
    seq: elements (1..elem_count'last);
  end record;

  --------------------------------------------------------------------------

  type elem_location is
  record
     node: node_position := 0;
     slot: elem_index    := 1;
     less: boolean       := false;
  end record;

  --------------------------------------------------------------------------

  type step_array    is array (1..node_position'size) of elem_location;
  type step_sequence is
  record
     len: natural     := 0;
     seq: step_array;
  end record;

  --------------------------------------------------------------------------

  type node_record is
  record
     left: node_position := 0;
     data: elem_sequence;
  end record;

  --------------------------------------------------------------------------

  type node_access  is access node_record;
  procedure release is new unchecked_deallocation (node_record, node_access);

  --------------------------------------------------------------------------

  package node_io is new direct_io (node_record); use node_io;
  subtype io_pos  is node_io.positive_count;

  --------------------------------------------------------------------------

  type tree_object is
  record
    file: node_io.file_type;
    path: step_sequence;
    curr: elem_location;
    node: node_access := null;
    mode: tree_mode;
  end record;

  --------------------------------------------------------------------------

  io_mode: constant array (tree_mode) of node_io.file_mode :=
					(in_tree    => node_io.in_file    ,
					 inout_tree => node_io.inout_file ,
					 out_tree   => node_io.inout_file );

  --------------------------------------------------------------------------

  function length (item: step_sequence) return natural is
  begin
      return item.len;
  end;

  --------------------------------------------------------------------------

  procedure clear  (what: in out step_sequence) is
  begin
     what.len := 0;
  end;

  --------------------------------------------------------------------------

  procedure append (tail: in elem_location; to: in out step_sequence) is
  begin
     to.len := to.len + 1;
     to.seq (to.len) := tail;
  end;

  --------------------------------------------------------------------------

  procedure discard (from: in out step_sequence;
		     tail:    out elem_location) is
  begin
     tail     := from.seq (from.len);
     from.len := from.len - 1;
  end;

  --------------------------------------------------------------------------

  function min (a, b: in elem_count) return elem_count is
  begin
    if a < b then return a;
	     else return b;
    end if;
  end;

  --------------------------------------------------------------------------

  procedure set_length (item: in out elem_sequence; len: in elem_count) is
  begin
     item.len := min (len, item.seq'last);
  end;

  --------------------------------------------------------------------------

  function find_ge (items: in elem_sequence;
		     item: in item_type) return elem_index is
    left, right, mid: elem_index;
  begin
    left  := 1;
    right := items.len;
    loop
       exit when left >= right;
       mid := elem_index'val((elem_index'pos(left) + elem_index'pos(right)) / 2);
       if not (item < items.seq (mid).item)
       then
	  left  := elem_index'succ (mid);
       else
	  right := mid;
       end if;
    end loop;
    return left;
  end;

  --------------------------------------------------------------------------

  function find_le (items: in elem_sequence;
		    item:  in item_type) return elem_index is
     left, right, mid: elem_index;
  begin
    left  := 1;
    right := items.len;
    loop
       exit when left >= right;
       mid := elem_index'val((elem_index'pos(left) + elem_index'pos(right)) / 2);
       if items.seq (mid).item < item
       then
	  left  := elem_index'succ(mid);
       else
	  right := mid;
       end if;
    end loop;
    return left;
  end;

  --------------------------------------------------------------------------

  function length (item: elem_sequence) return elem_count is
  begin
      return item.len;
  end;

  --------------------------------------------------------------------------

  procedure append (tail: in elements; to: in out elem_sequence) is
     len: elem_count;
  begin
     if tail'length > 0
     then
	len := min(tail'length, to.seq'last - to.len);
	to.seq (to.len + 1 .. to.len + len) :=
	tail(tail'first .. tail'first + len - 1);
	to.len := to.len + len;
     end if;
  end;

  --------------------------------------------------------------------------

  procedure append (tail: in element; to: in out elem_sequence) is
  begin
     if  to.seq'last > to.len then
	 to.len := to.len + 1;
	 to.seq(to.len) := tail;
     end if;
  end;

  --------------------------------------------------------------------------

  procedure extend (item: in out elem_sequence;
		    frag: in     elem_count;
		    from: in     elem_index) is

  begin
     for n in reverse from..item.len
     loop
	 item.seq (n + frag) := item.seq (n);
     end loop;
     item.len := item.len + frag;
  end;

  --------------------------------------------------------------------------

  procedure amend (item: in out elem_sequence;
		   frag: in     elements;
		   from: in     elem_index) is
  begin
     item.seq (from..from + frag'length - 1) := frag;
  end;

  --------------------------------------------------------------------------

  procedure insert (item: in out elem_sequence;
		    frag: in     elements;
		    from: in     elem_index) is

  begin
      if frag'length > 0
      then
	 extend (item, frag'length, from);
	 amend  (item, frag,        from);
      end if;
  end;

  --------------------------------------------------------------------------

  procedure insert (item: in out elem_sequence;
		    frag: in     element;
		    from: in     elem_index) is

  begin
      extend (item, 1, from);
      item.seq (from) := frag;
  end;

  --------------------------------------------------------------------------

  procedure delete (item: in out elem_sequence;
		    from: in     elem_index;
		    num:  in     elem_index := 1) is


  begin
      for n in from + num..item.len
      loop
	  item.seq (n - num) := item.seq (n);
      end loop;
      item.len := item.len - num;
  end;

  --------------------------------------------------------------------------

  procedure set    (item: in out elem_sequence; value: in elements) is
  begin
     item.len := value'length;
     if item.len > 0
     then
	item.seq (1..item.len) := value;
     end if;
  end;

  --------------------------------------------------------------------------

  procedure set    (item: in out elem_sequence; value: in element) is
  begin
     item.seq (1) := value;
     item.len     := 1;
  end;

  --------------------------------------------------------------------------

  procedure clear (item: in out elem_sequence; from: in elem_index := 1) is
  begin
     set_length (item, from - 1);
  end;

  --------------------------------------------------------------------------

  procedure ensure_status (file: in tree_type) is
  begin
      if not is_open (file)
      then
	 raise status_error;
      end if;
  end;

  --------------------------------------------------------------------------

  procedure except_mode (file: in tree_type; mode: tree_mode) is
  begin
      if file.mode = mode
      then
	 raise mode_error;
      end if;
  end;

  --------------------------------------------------------------------------

  procedure put_node (file: in     tree_type;
		      tonp: in     node_position;
		      node: in     node_access) is
  begin

     -- WARNING !!!
     -- When size(file) = index(file) = tonp
     -- meridian v4.11 writes data record to tonp + 1,
     -- reset (file) temporary avoids this strange reaction.

     reset (file.file);
     write (file.file, node.all, io_pos (tonp));
  end;

  procedure get_node (file: in     tree_type;
		      tonp: in     node_position;
		      node: in     node_access) is
  begin
     read  (file.file, node.all, io_pos (tonp));
  end;

  procedure put_node (file: in     tree_type;
		      tonp: in     node_position) is
  begin
     put_node (file, tonp, file.node);
  end;

  procedure get_node (file: in     tree_type;
		      tonp: in     node_position) is
  begin
     get_node (file, tonp, file.node);
  end;

  --------------------------------------------------------------------------

  function is_equal (file: in tree_type; item: item_type) return boolean is
    curr: elem_location renames file.curr;
    data: elem_sequence renames file.node.data;
  begin
    if comp_softly
    then
       return not (data.seq (curr.slot).item < item xor curr.less);
    else
       return item = data.seq (curr.slot).item;
    end if;
  end;

  --------------------------------------------------------------------------

  procedure seek_root (file: in tree_type) is
  begin
    if file.curr.node /= 1
    then
       get_node (file, 1);
       file.curr.node  := 1;
       clear (file.path);
    end if;
  end;

  --------------------------------------------------------------------------

  procedure seek_right (file: in tree_type) is
    next: node_position;
    curr: elem_location renames file.curr;
    data: elem_sequence renames file.node.data;
  begin

    curr.less := false;
    loop

      curr.slot := data.len;
      next      := data.seq (curr.slot).node;

      exit when next = 0;

      append (curr, file.path);
      get_node (file, next);

      curr.node := next;

    end loop;
  end;

  --------------------------------------------------------------------------

  procedure seek_left (file: in tree_type) is
    next: node_position;
    curr: elem_location renames file.curr;
    data: elem_sequence renames file.node.data;
  begin

    curr.less := true;
    loop

      curr.slot := 1;
      next      := file.node.left;

      exit when next = 0;

      append   (curr, file.path);
      get_node (file, next);

      curr.node := next;

    end loop;
  end;

  --------------------------------------------------------------------------

  procedure seek_leaf (file: in tree_type; item: in item_type) is
    item_inside: boolean;
    next: node_position;
    curr: elem_location renames file.curr;
  begin

    if seek_softly
    then

       seek_up: loop

	  curr.slot  := find_le (file.node.data, item);
	  curr.less  := item < file.node.data.seq (curr.slot).item;

	  if    curr.slot = length (file.node.data)
	  then
	     item_inside :=     curr.less;
	  elsif curr.slot = 1
	  then
	     item_inside := not curr.less;
	  else
	     item_inside := true;
	  end if;

	  exit when item_inside;
	  exit when length (file.path) = 0;

	  discard (file.path, curr);

	  get_node (file, curr.node);

       end loop seek_up;

    else
	  seek_root (file);

	  curr.slot := find_le (file.node.data, item);
	  curr.less := item < file.node.data.seq (curr.slot).item;

    end if;

    seek_down: loop
       if curr.less
       then
	 if curr.slot = 1
	 then
	    next := file.node.left;
	 else
	    next := file.node.data.seq (curr.slot - 1).node;
	 end if;
       else
	 next  := file.node.data.seq (curr.slot).node;
       end if;

       exit when next = 0;

       append (curr, file.path);

       get_node (file, next);

       curr.node  := next;
       curr.slot  := find_le (file.node.data, item);
       curr.less  := item < file.node.data.seq (curr.slot).item;
    end loop seek_down;

  end seek_leaf;

  --------------------------------------------------------------------------

  procedure seek_pred (file: in tree_type; okay: in out boolean) is

    curr: elem_location renames file.curr;
    data: elem_sequence renames file.node.data;
    next: node_position;
    last: node_position;

    function next_node (elem: in elem_index) return node_position is
    begin
       if elem = 1
       then
	  return file.node.left;
       else
	  return data.seq (elem - 1).node;
       end if;
    end;

  begin
    okay := false;
    if data.len > 0
    then

       next := next_node (curr.slot);

       if next /= 0
       then

	 curr.less := true;

	 append     (curr, file.path);
	 get_node   (file, next);
	 seek_right (file);
	 okay      := true;

       else
	  last := curr.node;
	  loop

	    if curr.slot > 1
	    then
	       curr.slot := curr.slot - 1;
	       okay      := true;
	       exit;
	    end if;

	    exit when length (file.path) = 0;

	    discard (file.path, curr);

	    if not curr.less
	    then
	       okay      := true;
	       exit;
	    end if;
	  end loop;

	  if last /= curr.node
	  then
	     get_node (file, curr.node);
	  end if;
       end if;
    end if;
  end seek_pred;

  --------------------------------------------------------------------------

  procedure seek_succ (file: in tree_type; okay: out boolean) is

    curr: elem_location renames file.curr;
    data: elem_sequence renames file.node.data;
    next: node_position;

    function next_node (elem: in elem_index) return node_position is
    begin
       return data.seq (elem).node;
    end;

  begin
    okay := false;
    if data.len > 0
    then
       next := next_node (curr.slot);

       if next /= 0
       then

	 curr.less := false;

	 append     (curr, file.path);
	 get_node   (file, next);
	 seek_left  (file);
	 okay      := true;

       else

	  loop

	    if curr.slot < length (data)
	    then
	       curr.slot := curr.slot + 1;
	       okay      := true;
	       exit;
	    end if;

	    exit when length (file.path) = 0;

	    discard  (file.path, curr);
	    get_node (file, curr.node);

	    if curr.less
	    then
	       okay      := true;
	       exit;
	    end if;
	  end loop;

       end if;
    end if;
  end seek_succ;

  --------------------------------------------------------------------------

  procedure seek_item (file: in tree_type;
		       item: in item_type;
		       okay: in out boolean) is

    curr: elem_location renames file.curr;
    data: elem_sequence renames file.node.data;

  begin

    seek_leaf (file, item);

    okay := false;

    if data.len > 0
    then
       if is_equal (file, item)
       then
	  okay := true;
       else
	  seek_pred (file, okay);

	  if okay
	  then
	    curr.less := item < data.seq (curr.slot).item;
	    okay      := is_equal (file, item);
	  end if;
       end if;
    end if;
  end seek_item;

  --------------------------------------------------------------------------

  procedure destroy (file: in out tree_type) is
     procedure release is new unchecked_deallocation (tree_object, tree_type);
  begin
    if file /= null
    then
      if is_open (file.file)
      then
	 close (file.file);
      end if;
      if file.node /= null
      then
	 release (file.node);
      end if;
      release (file);
    end if;
  end;

  --------------------------------------------------------------------------

  procedure create(file : in out tree_type;
		   mode : in tree_mode := inout_tree;
		   name : in string := "";
		   form : in string := "") is
  begin
     file      := new tree_object;
     file.node := new node_record;

     file.mode := mode;
     create (file.file, io_mode (mode), name, form);

     file.node.left  := 0;

     clear  (file.node.data);
     clear  (file.path);

     file.curr.less  := false;
     file.curr.node  := 1;
     file.curr.slot  := 1;

     put_node (file, file.curr.node);

  exception
     when others => destroy (file);
		    raise;
  end;

  --------------------------------------------------------------------------

  procedure open(file : in out tree_type;
		 mode : in tree_mode;
		 name : in string := "";
		 form : in string := "") is
  begin
     file      := new tree_object;
     file.node := new node_record;
     file.mode := mode;

     open  (file.file, io_mode (mode), name, form);
     file.curr.node := 0;
     seek_root(file);
  exception
     when others => destroy (file); raise;
  end;

  --------------------------------------------------------------------------

  procedure close (file : in out tree_type) is
  begin
     ensure_status (file);
     close (file.file);
     destroy (file);
  end;

  --------------------------------------------------------------------------

  procedure delete (file : in out tree_type) is
  begin
     ensure_status (file);
     delete  (file.file);
     destroy (file);
  end;

  --------------------------------------------------------------------------

  function is_open (file : in tree_type) return boolean is
  begin
     if file /= null
     then
	return is_open (file.file);
     end if;
     return false;
  end;

  --------------------------------------------------------------------------

  function name (file : in tree_type) return string is
  begin
     ensure_status (file);
     return name (file.file);
  end;

  --------------------------------------------------------------------------

  function form (file : in tree_type) return string is
  begin
     ensure_status (file);
     return form (file.file);
  end;

  --------------------------------------------------------------------------

  function mode (file : in tree_type) return tree_mode is
  begin
     ensure_status (file);
     return file.mode;
  end;

  --------------------------------------------------------------------------

  procedure reset  (file : in out tree_type) is
  begin
     ensure_status (file);
     reset (file.file);
     seek_root (file);
  end;

  --------------------------------------------------------------------------

  procedure reset  (file : in out tree_type;
		    mode : in     tree_mode) is
  begin
     ensure_status (file);
     reset (file.file, io_mode (mode));
     seek_root (file);
  end;

  --------------------------------------------------------------------------

  procedure condalloc (node: in out node_access) is
  begin
     if node = null
     then
	node := new node_record;
     end if;
  end;

  --------------------------------------------------------------------------

  procedure insert (file : in out tree_type;
		    item : in item_type;
		    data : in data_type) is

    upd : boolean := false;
    temp: node_access := null;
    npos: node_position;
    elem: element;
    left: node_position;  -- left node of element

    procedure split_node is
    begin

       condalloc (temp);

       if file.curr.slot <= elem_count (tree_degree + 1)
       then

	  set (temp.data, file.node.data.seq (elem_count (tree_degree) + 1..
					      elem_count (tree_degree) * 2));

	  set_length (file.node.data, elem_count (tree_degree));

	  if file.curr.slot <= elem_count (tree_degree)
	  then
	     insert (file.node.data, elem, file.curr.slot);
	     elem := file.node.data.seq (elem_count (tree_degree) + 1);
	  end if;
       else
	  if file.curr.slot = elem_count (tree_degree) + 2
	  then
	     set    (temp.data, elem);
	     append (file.node.data.seq (elem_count (tree_degree) + 2..
					 elem_count (node_length)), temp.data);
	  else

	     set (temp.data, file.node.data.seq (elem_count (tree_degree) + 2..
						 elem_count (node_length)));

	     if file.curr.slot = elem_count (node_length) and then
		file.curr.less = false
	     then
		append (elem, temp.data);
	     else
		insert (temp.data, elem, file.curr.slot);
	     end if;

	  end if;
	  elem := file.node.data.seq (elem_count (tree_degree) + 1);
       end if;
       set_length (file.node.data, elem_count (tree_degree));

       temp.left := elem.node;

       npos := node_position (size (file.file) + 1);

       temp.left     := elem.node;
       elem.node     := npos;

       put_node (file, npos, temp); -- temp is always right

    end split_node;

  begin

    ensure_status (file);
    except_mode   (file, in_tree);

    elem.item  := item;
    elem.data  := data;
    elem.node  := 0;

    if length (file.node.data) < 1
    then
       append (elem, file.node.data);
    else

       seek_leaf (file, item);

       inserting: loop

	  if length (file.node.data) < elem_count (node_length)
	  then
	     if file.curr.slot < length (file.node.data)
	     then
	       insert (file.node.data, elem, file.curr.slot);
	     else
	       if file.curr.less
	       then
		  insert (file.node.data, elem, file.curr.slot);
	       else
		  append (elem, file.node.data);
	       end if;
	     end if;

	     exit inserting;

	  else

	     split_node;

	     if length (file.path) = 0 -- root node here
	     then
		-- append old root to file
		npos := node_position (size (file.file) + 1);

		put_node (file, npos);

		set (file.node.data, elem);

		file.node.left := npos;
		file.curr.less := false;
		file.curr.slot := 1;

		exit inserting;

	     else
		put_node (file, file.curr.node);
	     end if;

	     discard (file.path, file.curr);

	     get_node (file, file.curr.node);

	  end if;

       end loop inserting;
    end if;

    -- update changes of curr node
    put_node (file, file.curr.node);

    if temp /= null
    then
       release (temp);
    end if;

  exception
      when others => release (temp);
		     raise;
  end insert;

  --------------------------------------------------------------------------

  procedure modify (file : in out tree_type;
		    item : in item_type;
		    data : in data_type;
		    ok   : out boolean) is

  begin

    ok := false;

    ensure_status (file);
    except_mode   (file, in_tree);

    declare
       elms: elem_sequence renames file.node.data;
       curr: elem_location renames file.curr;
       okay: boolean;
    begin
       seek_item     (file, item, okay);

       if okay
       then

	  elms.seq (curr.slot).data := data;
	  put_node (file, curr.node);

	  ok   := true;
       end if;
    end;
  end modify;

  --------------------------------------------------------------------------

  procedure junk    (elem:  in     element;
		     left:  in     node_access;
		     right: in     node_access) is
  begin

      append (elem, left.data);
      left.data.seq (left.data.len).node := right.left;
      append (right.data.seq (1..right.data.len), left.data);

  end;

  --------------------------------------------------------------------------

  procedure balance (elem:  in out element;
		     left:  in     node_access;
		     right: in     node_access) is

     num: elem_count := elem_count (abs (integer(length (left.data)) -
					 integer(length (right.data))) / 2);

  begin

     if num >= 1
     then
	if length (left.data) < length (right.data)
	then

	   append (elem, left.data);
	   left.data.seq (left.data.len).node := right.left;

	   append (right.data.seq (1..num - 1), left.data);

	   declare
	      last: element renames right.data.seq (num);
	   begin
	      right.left := last.node;
	      elem.data  := last.data;
	      elem.item  := last.item;
	   end;

	   delete (right.data, 1, num);

	else

	   extend (right.data, num, 1);

	   right.data.seq (num).data := elem.data;
	   right.data.seq (num).item := elem.item;
	   right.data.seq (num).node := right.left;

	   declare
	      spos: elem_index := (left.data.len - num + 1);
	      last: element renames left.data.seq (spos);
	   begin

	      amend (right.data, left.data.seq (spos + 1..left.data.len), 1);

	      right.left := last.node;
	      elem.data  := last.data;
	      elem.item  := last.item;

	      delete (left.data, spos, num);

	   end;

	end if;
     end if;
  end;

  --------------------------------------------------------------------------

  procedure delete (file : in out tree_type;
		    item : in item_type;
		    data : out data_type;
		    ok   : out boolean) is

    tmpr: node_access := null;
    tmpl: node_access := null;

    next: node_position;
    fore: elem_location;
    elem: element;
    okay: boolean;

    function left (elem: in elem_index) return node_position is
    begin
       if elem = 1
       then
	  return file.node.left;
       else
	  return file.node.data.seq (elem - 1).node;
       end if;
    end;

    procedure swap (a, b: in out node_access) is
       t: node_access;
    begin
       t := a; a := b; b := t;
    end;

  begin

    ok := false;

    ensure_status (file);
    except_mode   (file,  in_tree);
    except_mode   (file, out_tree);

    declare
       curr: elem_location renames file.curr;
       fino: node_access   renames file.node;
    begin

       seek_item (file, item, okay);

       if okay
       then

	  data := fino.data.seq (curr.slot).data;
	  next := left (curr.slot);

	  if next /= 0
	  then

	     tmpl      := fino;
	     fino      := new node_record;

	     fore      := curr;
	     curr.less := true;

	     append     (curr, file.path);
	     get_node   (file, next, fino);

	     curr.node := next;

	     seek_right (file);

	     tmpl.data.seq (fore.slot).data := fino.data.seq (curr.slot).data;
	     tmpl.data.seq (fore.slot).item := fino.data.seq (curr.slot).item;

	     put_node (file, fore.node, tmpl);

	  end if;

	  delete (fino.data, curr.slot);

	  loop

	     exit when length (file.path)  = 0;
	     exit when length (fino.data) >= elem_count (tree_degree);

	     condalloc (tmpl);
	     condalloc (tmpr);

	     discard (file.path, curr); -- prepare to reading root

	     -- save old root

	     if curr.less
	     then
		swap (tmpl, fino);
		get_node (file, curr.node, fino);
		get_node (file, fino.data.seq (curr.slot).node, tmpr);

	     else
		swap (tmpr, fino);
		get_node (file, curr.node       , fino);
		get_node (file, left (curr.slot), tmpl);

	     end if;

	     if  integer (length (tmpl.data)) +
		 integer (length (tmpr.data)) < node_length
	     then
		 junk (fino.data.seq (curr.slot), tmpl, tmpr);

		 next := left (curr.slot);
		 delete (fino.data, curr.slot);

		 if length (fino.data) = 0
		 then
		    swap (tmpl, fino);
		 else
		    put_node (file, next, tmpl);
		 end if;

	     else
		 balance  (fino.data.seq (curr.slot), tmpl, tmpr);
		 put_node (file, fino.data.seq (curr.slot).node, tmpr);
		 put_node (file, left (curr.slot), tmpl);

	     end if;

	  end loop;

	  put_node (file, curr.node, fino);

	  ok   := true;

	  release (tmpl);
	  release (tmpr);

       end if;

    end;

  exception
    when others => release (tmpl);
		   release (tmpr);
		   raise;
  end delete;

  --------------------------------------------------------------------------

  procedure get_first(file : in tree_type;
		      item : out item_type;
		      data : out data_type;
                      ok   : out boolean) is 


  begin
    ok := false;

    ensure_status (file);
    except_mode   (file, out_tree);

    declare
       elms: elem_sequence renames file.node.data;
       curr: elem_location renames file.curr;
    begin
       if elms.len > 0
       then
	 seek_root  (file);
	 seek_left  (file);

	 item := elms.seq (curr.slot).item;
	 data := elms.seq (curr.slot).data;

	 ok := true;
       end if;
    end;
  end get_first; 

  --------------------------------------------------------------------------

  procedure get_last(file : in tree_type;
		     item : out item_type;
		     data : out data_type;
		     ok   : out boolean) is

  begin
    ok := false;

    ensure_status (file);
    except_mode   (file, out_tree);

    declare
       elms: elem_sequence renames file.node.data;
       curr: elem_location renames file.curr;
    begin
       if elms.len > 0
       then
	 seek_root  (file);
	 seek_right (file);

	 item := elms.seq (curr.slot).item;
	 data := elms.seq (curr.slot).data;

	 ok   := true;
       end if;
    end;
  end get_last; 

  --------------------------------------------------------------------------

  procedure get_ge(file : in tree_type;
		   item : in out item_type;
		   data : out data_type;
                   ok   : out boolean) is 

  begin

    ok := false;

    ensure_status (file);
    except_mode   (file, out_tree);

    declare
       elms: elem_sequence renames file.node.data;
       curr: elem_location renames file.curr;
       elem: element;
       okay: boolean := false;
    begin

       if elms.len > 0
       then

	  seek_leaf (file, item);

	  if is_equal (file, item)
	  then
	     okay := true;
	  else
	     if curr.less
	     then

		elem.item := elms.seq (curr.slot).item;
		elem.data := elms.seq (curr.slot).data;

		seek_pred (file, okay);

		if okay
		then
		   curr.less := item < elms.seq (curr.slot).item;
		   okay      := is_equal (file, item);
		end if;

		if not okay
		then
		   item := elem.item;
		   data := elem.data;
		   ok   := true;
		end if;
	     else
		seek_succ (file, okay);
	     end if;
	  end if;

	  if okay
	  then
	    item := elms.seq (curr.slot).item;
	    data := elms.seq (curr.slot).data;
	    ok   := okay;
	  end if;

       end if;
    end;
  end get_ge;

  --------------------------------------------------------------------------

  procedure get_le(file : in tree_type;
		   item : in out item_type;
		   data : out data_type;
                   ok   : out boolean) is 

  begin

    ok := false;

    ensure_status (file);
    except_mode   (file, out_tree);

    declare
       elms: elem_sequence renames file.node.data;
       curr: elem_location renames file.curr;
       elem: element;
       okay: boolean := false;
    begin

       if elms.len > 0
       then
	  seek_leaf (file, item);

	  if is_equal (file, item)
	  then
	     okay := true;
	  else
	     if not curr.less
	     then

		elem.item := elms.seq (curr.slot).item;
		elem.data := elms.seq (curr.slot).data;

		seek_succ (file, okay);

		if okay
		then
		   curr.less := item < elms.seq (curr.slot).item;
		   okay      := is_equal (file, item);
		end if;

		if not okay
		then
		   item := elem.item;
		   data := elem.data;
		   ok   := true;
		end if;
	     else
		seek_pred (file, okay);
	     end if;

	  end if;

	  if okay
	  then
	    item := elms.seq (curr.slot).item;
	    data := elms.seq (curr.slot).data;
	    ok   := okay;
	  end if;

       end if;
    end;

  end get_le;

  --------------------------------------------------------------------------

  procedure get_lt(file : in tree_type;
		   item : in out item_type;
		   data : out data_type;
                   ok   : out boolean) is 

  begin

    ok := false;

    ensure_status (file);
    except_mode   (file, out_tree);

    declare
       elms: elem_sequence renames file.node.data;
       curr: elem_location renames file.curr;
       elem: element;
       okay: boolean := false;
    begin

       if elms.len > 0
       then

	  seek_leaf (file, item);

	  if curr.less
	  then
	     seek_pred (file, okay);
	     if okay
	     then
		curr.less := item < elms.seq (curr.slot).item;
	     end if;
	  else
	     okay := true;
	  end if;

	  if okay
	  then
	     if is_equal (file, item)
	     then
		seek_pred (file, okay);
	     end if;
	  end if;

	  if okay
	  then
	    item := elms.seq (curr.slot).item;
	    data := elms.seq (curr.slot).data;
	    ok   := okay;
	  end if;

       end if;
    end;

  end get_lt;

  --------------------------------------------------------------------------

  procedure get_gt(file : in tree_type;
		   item : in out item_type;
		   data : out data_type;
		   ok   : out boolean) is

  begin

    ok := false;

    ensure_status (file);
    except_mode   (file, out_tree);

    declare
       elms: elem_sequence renames file.node.data;
       curr: elem_location renames file.curr;
       elem: element;
       okay: boolean := false;
    begin

       if elms.len > 0
       then

	  seek_leaf (file, item);

	  if not curr.less
	  then
	     seek_succ (file, okay);
	     if okay
	     then
		curr.less := item < elms.seq (curr.slot).item;
	     end if;
	  else
	     okay := true;
	  end if;

	  if okay
	  then
	     if is_equal (file, item)
	     then
		seek_succ (file, okay);
	     end if;
	  end if;

	  if okay
	  then
	    item := elms.seq (curr.slot).item;
	    data := elms.seq (curr.slot).data;
	    ok   := okay;
	  end if;

       end if;
    end;

  end get_gt;

begin
   if tree_degree < 2
   then
      raise program_error;
   end if;
end b_tree_file;
