--::::::::::
--dequtype.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
package body deque_types is

  package body deques is

    dqsgn: array(edge_type) of integer := (-1, 1);

    function valid (i, w : in integer) return positive is
    begin
      return ((i + w - 1) mod w) + 1;
    end valid;

    pragma inline(valid);

    procedure set_target (deque  : in out object;
			  target : in edge_type) is
    begin
      deque.current.target := target;
    end set_target;

    procedure set_source (deque  : in out object;
			  source : in edge_type) is
    begin
      deque.current.source := source;
    end set_source;

    procedure set_rule (deque : in out object;
			rule  : in edges_rule) is
    begin
      deque.current := rule;
    end set_rule;

    function source (deque : in object) return edge_type is
    begin
      return deque.current.source;
    end source;

    function target (deque : in object) return edge_type is
    begin
      return deque.current.target;
    end target;

    function current (deque : in object) return edges_rule is
    begin
      return deque.current;
    end current;

    procedure reset_rule (deque : in out object) is
    begin
      set_rule(deque, (source => default_state.source,
		       target => default_state.target));
    end reset_rule;

    procedure flip_edges (deque : in out object) is
    begin
      set_rule(deque, (source => opposite (source(deque)),
		       target => opposite (target(deque))));
    end flip_edges;

    procedure discard (deque : in out object;
		       item  : out element) is
    begin
      discard(deque, item, deque.current.source);
    end discard;

    procedure append (deque : in out object;
		      item  : in element) is
    begin
      append(deque, item, deque.current.target);
    end append;

    procedure discard (deque  : in out object;
		       item   : out element;
		       source : in edge_type) is

      dqei : integer renames deque.index(source);

    begin
      if deque.count < 1 then
	raise deque_underflow;
      end if;
      item := deque.data (dqei);
      dqei := valid(dqei - dqsgn(source), deque.width);
      deque.count := deque.count - 1;
    end discard;

    procedure append (deque  : in out object;
		      item   : in element;
		      target : in edge_type) is
      dqei : integer renames deque.index(target);
    begin
      if deque.count >= deque.width then
	raise deque_overflow;
      end if;
      dqei := valid(dqei + dqsgn(target), deque.width);
      deque.data (dqei) := item;
      deque.count := deque.count + 1;
    end append;

    procedure clear (deque : in out object) is
    begin
      deque.count := 0;
      deque.index := (1, deque.width);
    end clear;

    function value (deque  : in object;
		    source : in edge_type) return element is
    begin
      return deque.data (deque.index(source));
    end value;

    function value (deque : in object) return element is
    begin
      return deque.data (deque.index(deque.current.source));
    end value;

    function length (deque : in object) return natural is
    begin
      return deque.count;
    end length;

    procedure update (deque  : in out object;
		      item   : in element;
		      target : in edge_type) is
    begin
      deque.data (deque.index(target)) := item;
    end update;

    procedure update (deque : in out object;
		      item  : in element) is
    begin
      deque.data (deque.index(deque.current.target)) := item;
    end update;

    package body array_support is

--  function value  (deque: in object) return sequence is
--  procedure get (deque : in out object; seq: out sequence);
--  function "&" (left: object; right: object) return sequence;
--  function "&" (left: sequence  ; right: object) return sequence;
--  function "&" (left: object; right: sequence  ) return sequence;
--  procedure set (deque : in out object; seq: in  sequence) is

    end array_support;

  end deques;

end deque_types;
