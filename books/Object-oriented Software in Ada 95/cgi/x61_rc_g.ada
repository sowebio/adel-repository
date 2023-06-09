---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:51 PM BST  --
---------------------------------------------------------------------

--[class_account.ads] Specification
package Class_Account is

  type Account is private;
  subtype Money  is Float;
  subtype Pmoney is Float range 0.0 .. Float'Last;

  procedure Deposit ( The:in out Account; Amount:in Pmoney );
  procedure Withdraw( The:in out Account; Amount:in Pmoney;
    Get:out Pmoney );
  function  Balance ( The:in Account ) return Money;

private
  type Account is record
    Balance_Of : Money := 0.00;      -- Amount in account
  end record;
end Class_Account;


--[class_account.adb] Implementation
package body Class_Account is


  procedure Deposit ( The:in out Account; Amount:in Pmoney ) is
  begin
    The.Balance_Of := The.Balance_Of + Amount;
  end Deposit;

  procedure Withdraw( The:in out Account; Amount:in Pmoney;
      Get:out Pmoney ) is
  begin
    if The.Balance_Of >= Amount then
      The.Balance_Of := The.Balance_Of - Amount;
      Get := Amount;
    else
      Get := 0.00;
    end if;
  end Withdraw;

  function  Balance( The:in Account ) return Money is
  begin
    return The.Balance_Of;
  end Balance;

end Class_Account;


-- Generic IN        constant
--         IN OUT    Renaming




--[class_object_rc.ads] Specification
with Ada.Finalization; use Ada.Finalization;
generic
  type T is private;                    -- The type
  Null_Value:in T;                      -- Identity element
package Class_Object_Rc is
  type Object is new Controlled with private;
  type P_T is access all T;

  procedure Initialize( The:in out Object );
  procedure Initialize( The:in out Object; Data:in T );
  procedure Finalize( The:in out Object );
  procedure Adjust( The:in out Object );
  function  Deliver( The:in Object) return T;
  function  Deliver_Ref( The:in Object) return P_T;
  procedure Unique( The:in out Object);
private
  procedure Build_Storage ( The:in out Object; Value:in T );
  procedure Release_Storage( The:in out Object );

  type Descriptor;
  type P_Descriptor is access all Descriptor;

  type Descriptor is record
    Refs   : Natural;       -- References to this data
    Object : aliased T;     -- The physical data
  end record;

  type Object is new Controlled with record
    P_Desc : P_Descriptor:= null; -- Descriptor for a number
  end record;

end Class_Object_Rc;




--[class_object_rc.adb] Implementation
with Unchecked_Deallocation;
package body Class_Object_Rc is

  procedure Initialize( The:in out Object ) is
  begin
    Build_Storage( The, Null_Value );
  end Initialize;

  procedure Initialize( The:in out Object; Data:in T ) is
  begin
    Build_Storage( The, Data );
  end Initialize;

  procedure Build_Storage ( The:in out Object; Value:in T ) is
  begin
    The.P_Desc := new Descriptor'(1,Value);
  end Build_Storage;

  procedure Finalize( The:in out Object ) is
  begin
    if The.P_Desc /= null then
      Release_Storage( The );
      The.P_Desc := null;
    end if;
  end Finalize;

  procedure Dispose is
    new Unchecked_Deallocation( Descriptor, P_Descriptor );

  procedure Release_Storage( The:in out Object ) is
  begin
    The.P_Desc.Refs := The.P_Desc.Refs-1;
    if The.P_Desc.Refs = 0 then
      Dispose( The.P_Desc );
    else
      null;
    end if;
  end Release_Storage;

  procedure Adjust( The:in out Object ) is
  begin
    The.P_Desc.Refs := The.P_Desc.Refs+1;
  end Adjust;

  function  Deliver( The:in Object) return T is
  begin
    return The.P_Desc.Object;
  end Deliver;

  function  Deliver_Ref( The:in Object) return P_T is
  begin
    return The.P_Desc.Object'access;
  end Deliver_Ref;

  procedure Unique( The:in out Object) is
    Tmp : P_Descriptor;
  begin
    if The.P_Desc.Refs > 1 then
      The.P_Desc.Refs := The.P_Desc.Refs-1;
      Tmp := new Descriptor'(1,The.P_Desc.Object);
      The.P_Desc := Tmp;
    end if;
  end Unique;
end Class_Object_Rc;




--[class_object_rc_int.ADS] Specification Instantiation
with Class_Object_Rc;
  pragma Elaborate_All( Class_Object_Rc );
  package Class_Rc_Integer is
           new Class_Object_Rc(Integer, 0);

--[class_ref_integer.ads] Specification
with Class_Rc_Integer; use Class_Rc_Integer;
package Class_Ref_Integer is
  type Ref_Integer is new Object with null record;
  function  Number_Const( Value:in Integer ) return Ref_Integer;
  function "+" ( L,R:in Ref_Integer ) return Ref_Integer;
  function "=" ( L,R:in Ref_Integer ) return Boolean;
end Class_Ref_Integer;

--[class_ref_integer.adb] Implementation
package body Class_Ref_Integer is
  function  Number_Const( Value:in Integer ) return Ref_Integer is
    Res: Ref_Integer;
  begin
    Initialize( Res, Value );
    return Res;
  end Number_Const;

  function "+" ( L,R:in Ref_Integer ) return Ref_Integer is
    Res : Ref_Integer;
  begin
    Initialize( Res, Deliver(L) + Deliver(R) );
    return Res;
  end "+";

  function "=" ( L,R:in Ref_Integer ) return Boolean is
  begin
    return Deliver(L) = Deliver(R);
  end "=";
end Class_Ref_Integer;


--[pack_procedures.adb] Implementation
with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Class_Rc_Integer;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Class_Rc_Integer;
procedure Main1 is
  A,B,C : Class_Rc_Integer.Object;
begin
  Initialize( B, 10 );
  Initialize( C, 20 );

  Put("a := b; "); New_Line;
  A := B;

  Put("b := c; "); New_Line;
  B := C;

  Put("c := a; "); New_Line;
  C := A;

  Put("a = "); Put( Deliver( A ) ); New_Line;
  Put("b = "); Put( Deliver( B ) ); New_Line;
  Put("c = "); Put( Deliver( C ) ); New_Line;

end Main1;

package Pack_Types is
  type P_Integer is access all Integer;
end Pack_Types;

with Pack_Types, Class_Object_Rc;
package Class_Object_Rc_P_Int is
  new Class_Object_Rc(Pack_Types.P_Integer, null);

with Ada.Text_Io, Ada.Integer_Text_Io, Class_Object_Rc_P_Int;
use  Ada.Text_Io, Ada.Integer_Text_Io, Class_Object_Rc_P_Int;
procedure Main2 is
  A,B,C  : Object;
begin
  Initialize( A, new Integer'(10) );
  Initialize( B, new Integer'(20) );

  Put("a := b; "); New_Line;
  A := B;
  Put("a = "); Put( Deliver( A ).all ); New_Line;
  Put("b = "); Put( Deliver( B ).all ); New_Line;
end Main2;

procedure Main3 is
begin
  null;
end Main3;

with Class_Account;
package Pack_Consts is
  Null_Account: Class_Account.Account;
end Pack_Consts;

with Pack_Consts, Class_Object_Rc, Class_Account;
package Class_Account_Rc is
  new Class_Object_Rc(Class_Account.Account, Pack_Consts.Null_Account);


with Ada.Text_Io, Ada.Float_Text_Io, Class_Account;
use  Ada.Text_Io, Ada.Float_Text_Io, Class_Account;
procedure Statement( An_Account:in Account ) is
begin
  Put("Mini statement: The amount on deposit is $" );
  Put( Balance( An_Account), Aft=>2, Exp=>0 );
  New_Line(2);
end Statement;

with Ada.Text_Io, Ada.Float_Text_Io, 
     Class_Account, Class_Account_Rc, Statement;
use  Ada.Text_Io, Ada.Float_Text_Io, 
     Class_Account, Class_Account_Rc; 
procedure Main4 is
  Original,Copy : Class_Account_Rc.Object;
begin
  Deposit( Deliver_Ref(Original).all, 100.00 );
  Put("copy := original; (Shallow copy)"); New_Line;
  Copy := Original;                           -- Shallow copy
  Statement( Deliver_Ref(Original).all );     -- The same object
  Statement( Deliver_Ref(Copy).all );         --  "  "
  Put("Make copy unique (Deep copy if necessary)"); New_Line;
  Unique( Copy );                             -- Deep copy
  Deposit( Deliver_Ref(Copy).all, 20.00 );    -- copy only
  Statement( Deliver_Ref(Original).all );     -- Unique object
  Statement( Deliver_Ref(Copy).all );         --   "   "
end Main4;

with Ada.Text_Io, Ada.Integer_Text_Io, Class_Rc_Integer;
use  Ada.Text_Io, Ada.Integer_Text_Io, Class_Rc_Integer;
procedure Main5 is
  A : Class_Rc_Integer.Object;
begin
  Initialize( A, 20 );
  Put("a = "); Put( Deliver( A ) ); New_Line;

  Put("a := a; "); New_Line;
  A := A;

  Put("a = "); Put( Deliver( A ) ); New_Line;

end Main5;

--[main.adb] Procedure
with Ada.Text_Io, Main1, Main2, Main3, Main4, Main5;
use  Ada.Text_Io;
procedure Main is
begin
  Put("Example 1"); New_Line; Main1;
  Put("Example 2"); New_Line; Main2;
  Put("Example 3"); New_Line; Main3;
  Put("Example 4"); New_Line; Main4;
  Put("Example 5"); New_Line; Main5;
end Main;
