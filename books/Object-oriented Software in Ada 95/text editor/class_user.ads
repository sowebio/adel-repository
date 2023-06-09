pragma Source_Reference (000781, "x80_ed.ada");

--====================================================================

--[class_user.ads] Specification
package Class_User is
  type User is private;
  type Mode is ( No_Echo, Echo );

  function Get_Command( The:in User ) return Character;
  function Dialog(The:in User; Mes:in String) return String;
  function Get_Character(The:in User; M:in Mode)return Character;

private
  type User is record null; end record;

end Class_User;
