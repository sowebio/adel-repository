--::::::::::
--btreeavl.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with unchecked_deallocation;
package body b_tree_avl is

  type balance is (l_disb, no_disb, r_disb);

  type tree_data is record
       item: item_type;
       data: data_type;
       bal:  balance          := no_disb;
       left, right: tree_type := null;
  end record;

  procedure free is new unchecked_deallocation(tree_data, tree_type);

  --------------------------------------------------------------------

  procedure insert (tree: in out tree_type;
		    item:        item_type;
		    data:        data_type) is

       upd : boolean := false;
       hght: boolean;

       procedure tryput (node: in out tree_type; h: in out boolean) is
	    pp1, pp2: tree_type;
       begin
	    if  node = null then
		 node       := new tree_data;
		 node.item  := item;
		 node.data  := data;
		 h         := true;
	    elsif item < node.item then
		 tryput (node.left, h);
		 if h then
		      case node.bal is
			   when  r_disb => node.bal :=  no_disb; h:= false;
			   when  no_disb => node.bal := l_disb;
			   when l_disb =>
				pp1 := node.left;
				if pp1.bal = l_disb then
				     node.left := pp1.right;
				     pp1.right := node;
				     node.bal := no_disb; node := pp1;
				else
				     pp2 := pp1.right;
				     pp1.right := pp2.left;
				     pp2.left := pp1;
				     node.left := pp2.right;
				     pp2.right := node;

				     if pp2.bal = l_disb then
					  node.bal := r_disb;
				     else
					  node.bal := no_disb;
				     end if;

				     if pp2.bal =  r_disb then
					  pp1.bal := l_disb;
				     else
					  pp1.bal := no_disb;
				     end if;
				     node := pp2;
				end if;
				node.bal := no_disb; h:= false;
		      end case;
		 end if;
	    elsif node.item < item then
		 tryput (node.right, h);
		 if h then
		      case node.bal is
			   when l_disb => node.bal :=  no_disb; h:= false;
			   when  no_disb => node.bal :=  r_disb;
			   when  r_disb =>
				pp1 := node.right;
				if pp1.bal = r_disb then
				     node.right := pp1.left;
				     pp1.left := node;
				     node.bal := no_disb; node := pp1;
				else
				     pp2 := pp1.left;
				     pp1.left := pp2.right;
				     pp2.right := pp1;
				     node.right := pp2.left;
				     pp2.left := node;

				     if pp2.bal =  r_disb then
					  node.bal := l_disb;
				     else
					  node.bal := no_disb;
				     end if;

				     if pp2.bal =  l_disb then
					  pp1.bal := r_disb;
				     else
					  pp1.bal := no_disb;
				     end if;
				     node := pp2;
				end if;
				node.bal := no_disb; h:= false;
		      end case;
		 end if;
	    else -- dup
		 if upd then
		    node.data  := data;
		 end if;
	    end if;
       end tryput;
  begin
       tryput (tree, hght);
  end insert;

  --------------------------------------------------------------------

  procedure free_nodes (node: in out tree_type) is
  begin
       if node /= null then
	    free_nodes (node.left);
	    free_nodes (node.right);
	    free (node);
       end if;
  end;

  --------------------------------------------------------------------

  procedure delete (tree : in out tree_type) is
  begin
       free_nodes (tree);
  end;

  --------------------------------------------------------------------

  procedure b_left (pp: in out tree_type; h: in out boolean) is
       pp1, pp2: tree_type;
       bb1, bb2: balance;
  begin
       case pp.bal is
	    when l_disb => pp.bal := no_disb;
	    when  no_disb => pp.bal := r_disb; h := false;
	    when  r_disb => pp1 := pp.right; bb1 := pp1.bal;
		 if bb1 >= no_disb then
		      pp.right := pp1.left; pp1.left := pp;
		      if bb1 = no_disb then
			   pp.bal := r_disb; pp1.bal := l_disb; h:= false;
		      else
			   pp.bal := no_disb; pp1.bal := no_disb;
		      end if;
		      pp := pp1;
		 else
		      pp2 := pp1.left; bb2 := pp2.bal;
		      pp1.left := pp2.right; pp2.right := pp1;
		      pp.right := pp2.left; pp2.left := pp;
		      if bb2 = r_disb then pp.bal := l_disb; else pp.bal := no_disb; end if;
		      if bb2 = l_disb then pp1.bal := r_disb; else pp1.bal := no_disb; end if;
		      pp := pp2; pp2.bal := no_disb;
		 end if;
       end case;
  end b_left;

  --------------------------------------------------------------------

  procedure b_right (pp: in out tree_type; h: in out boolean) is
       pp1, pp2: tree_type;
       bb1, bb2: balance;
  begin
       case pp.bal is
	    when  r_disb => pp.bal :=  no_disb;
	    when  no_disb => pp.bal := l_disb; h := false;
	    when  l_disb => pp1 := pp.left; bb1 := pp1.bal;
		 if bb1 <= no_disb then
		      pp.left := pp1.right; pp1.right := pp;
		      if bb1 = no_disb then
			   pp.bal := l_disb; pp1.bal := r_disb; h:= false;
		      else
			   pp.bal := no_disb; pp1.bal := no_disb;
		      end if;
		      pp := pp1;
		 else
		      pp2 := pp1.right; bb2 := pp2.bal;
		      pp1.right := pp2.left; pp2.left := pp1;
		      pp.left := pp2.right; pp2.right := pp;
		      if bb2 = l_disb then pp.bal := r_disb; else pp.bal := no_disb; end if;
		      if bb2 = r_disb then pp1.bal := l_disb; else pp1.bal := no_disb; end if;
		      pp := pp2; pp2.bal := no_disb;
		 end if;
       end case;
  end b_right;

  --------------------------------------------------------------------

  procedure delete (tree     : in out tree_type;
		    item     :        item_type;
		    data     :    out data_type;
		     ok     :    out boolean) is

       hei: boolean;

       procedure recur_del  (p: in out tree_type; h: in out boolean) is
	    q: tree_type;
	    procedure x_del (node: in out tree_type; h: in out boolean) is
	    begin
		 if node.right /= null then
		      x_del (node.right, h);
		      if h then b_right (node, h); end if;
		 else
		      q.item := node.item;
		      q.data := node.data; q := node;
		      node := node.left; h := true;
		 end if;
	    end x_del;

       begin
	    if p = null then return;
	    elsif item < p.item then
		 recur_del (p.left, h);
		 if h then b_left (p, h); end if;
	    elsif p.item = item then
		 q := p;
		 if    q.right = null then
		      p := q.left; h:= true;
		 elsif q.left  = null then
		      p := q.right; h := true;
		 else
		      x_del (q.left, h);
		      if h then b_left (p, h); end if;
		 end if;
		 data := q.data;
		 ok  := true;
		 free (q);
	    else -- not "<"
		 recur_del (p.right, h);
		 if h then b_right (p, h); end if;
	    end if;
       end recur_del;
  begin
       ok := false;
       recur_del (tree, hei);
  end delete;

  --------------------------------------------------------------------

  procedure get_first (tree  : in     tree_type;
		       item  :    out item_type;
		       data  :    out data_type;
		       ok    :    out boolean) is

       x: tree_type := tree;
  begin
       ok  := false;
       if x = null then return; end if;

       loop
	     exit when x.left = null;
	     x := x.left;
       end loop;
       item := x.item;
       data := x.data;
       ok  := true;
  end;

  --------------------------------------------------------------------

  procedure get_last  (tree  : in     tree_type;
		       item  :    out item_type;
		       data  :    out data_type;
		       ok    :    out boolean) is

       x: tree_type := tree;
  begin
       ok  := false;
       if x = null then return; end if;

       loop
	     exit when x.right = null;
	     x := x.right;
       end loop;
       item := x.item;
       data := x.data;
       ok  := true;
  end;

  --------------------------------------------------------------------

  procedure modify (tree     : in out tree_type;
		    item     :        item_type;
		    data     :        data_type;
		      ok     :    out boolean) is

       x: tree_type := tree;
  begin
       ok := false;
       loop
	    exit when x = null;
	    exit when x.item = item;
	    if   item < x.item then
		 x := x.left;
	    else    x := x.right;
	    end if;
       end loop;
       if x /= null then
	  x.data := data;
	  ok    := true;
       end if;
  end;

  --------------------------------------------------------------------

  procedure get_ge (tree     :        tree_type;
		    item     : in out item_type;
		    data     :    out data_type;
		      ok     :    out boolean) is

       x: tree_type := tree;
       g: tree_type := null;
  begin
       ok  := false;
       loop
	    exit when x = null;
	    exit when x.item = item;
	    if   item < x.item then
		 g := x;
		 x := x.left;
	    else x := x.right;
	    end if;
       end loop;

       if x = null then
	  x := g;
       end if;

       if x /= null then
	    item := x.item;
	    data := x.data;
	    ok  := true;
       end if;
  end;

  --------------------------------------------------------------------

  procedure get_gt (tree     :        tree_type;
		    item     : in out item_type;
		    data     :    out data_type;
		      ok     :    out boolean) is

       x: tree_type := tree;
       g: tree_type;
  begin
       ok  := false;
       loop
	    exit when x = null;
	    if   item < x.item then
		 g := x;
		 x := x.left;
	    else
		 x := x.right;
	    end if;
       end loop;
       if g /= null then
	    item := g.item;
	    data := g.data;
	    ok  := true;
       end if;
  end;

  --------------------------------------------------------------------

  procedure get_le (tree     :        tree_type;
		    item     : in out item_type;
		    data     :    out data_type;
		      ok     :    out boolean) is

       x: tree_type := tree;
       l: tree_type := null;
  begin
       ok  := false;
       loop
	    exit when x = null;
	    exit when x.item = item;
	    if   x.item < item then
		 l := x;
		 x := x.right;
	    else x := x.left;
	    end if;
       end loop;

       if x = null then
	  x := l;
       end if;

       if x /= null then
	    item := x.item;
	    data := x.data;
	    ok  := true;
       end if;
  end;

  --------------------------------------------------------------------

  procedure get_lt (tree    :        tree_type;
		    item    : in out item_type;
		    data    :    out data_type;
		      ok    :    out boolean) is

       x: tree_type := tree;
       l: tree_type;
  begin
       ok  := false;
       loop
	    exit when x = null;
	    if   x.item < item then
		 l := x;
		 x := x.right;
	    else x := x.left;
	    end if;
       end loop;
       if l /= null then
	    item := l.item;
	    data := l.data;
	    ok  := true;
       end if;
  end;

end;
