--::::::::::
--dequtype.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
package deque_types is

  type edge_type is (left_edge, right_edge);

  type edges_rule is
  record
    source, target : edge_type;
  end record;

  lifo_right: constant edges_rule := (source |
				      target => right_edge);
  fifo_right: constant edges_rule := (source => left_edge,
				      target => right_edge);
  lifo_left : constant edges_rule := (source |
				      target => left_edge);
  fifo_left : constant edges_rule := (source => right_edge,
				      target => left_edge);

  opposite  : constant array(edge_type)
			of edge_type := (right_edge => left_edge,
					 left_edge  => right_edge);

  deque_overflow, deque_underflow : exception;

  generic

    type element is private;
    default_length : in positive;
    default_state  : edges_rule := fifo_right;

  package deques is

    type object (width  : positive := default_length) is private;
		     -- source : edge_type := default_state.source;
		     -- target : edge_type := default_state.target;

    function length (deque : in object) return natural;
    function value  (deque : in object;
		     source: in edge_type) return element;
    function value  (deque : in object) return element;


    procedure update (deque  : in out object;
		      item   : in element;
		      target : in edge_type);

    procedure update (deque : in out object;
		      item  : in element);

    pragma inline (length, value, update);

    procedure discard (deque: in out object; item: out element);
    procedure append  (deque: in out object; item: in element);

    pragma inline (discard, append);

    procedure discard (deque : in out object; item: out element;
		       source: in edge_type);
    procedure append (deque : in out object; item: in element;
		      target: in edge_type);

    function current (deque : in object) return edges_rule;
    function source  (deque : in object) return edge_type;
    function target  (deque : in object) return edge_type;

    procedure reset_rule (deque: in out object);
    procedure flip_edges (deque: in out object);
    procedure set_target (deque: in out object; target: in edge_type);
    procedure set_source (deque: in out object; source: in edge_type);
    procedure set_rule   (deque: in out object; rule: in edges_rule);

    procedure clear (deque: in out object);

    pragma inline (set_target, set_source, reset_rule, set_rule,
		  flip_edges, source, target, current, clear);
    generic
       type index is (<>);
       type sequence is array (index range <>) of element;
    package array_support is

--  function value  (deque : in object) return sequence;
--  function "&" (left: object; right: object) return sequence;
--  function "&" (left: sequence  ; right: object) return sequence;
--  function "&" (left: object; right: sequence  ) return sequence;
--  procedure set (deque : in out object; seq: in  sequence);
--  procedure get (deque : in out object; seq: out sequence);

    end array_support;

  private

    type elem_arry  is array (positive range <>) of element;
    type edge_index is array (edge_type) of integer;

    type object (width  : positive := default_length) is
		 --  source : edge_type := default_state.source;
		 --  target : edge_type := default_state.target
      record
	current: edges_rule := (source => default_state.source,
				target => default_state.target);
	count  : natural := 0;
	index  : edge_index := (1, width);
	data   : elem_arry (1 .. width);
      end record;

  end deques;

end deque_types;
