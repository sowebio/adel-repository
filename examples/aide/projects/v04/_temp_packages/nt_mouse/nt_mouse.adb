-- in modification, HM

pragma C_Pass_By_Copy (128);
--with Text_Io;

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

with Unchecked_Conversion;
package body Nt_Mouse is

-- pragma Linker_Options ("-luser32");   -- for GNAT only

  ---------------------
  -- WIN32 INTERFACE --
  ---------------------


  -- basic types
  subtype Dword  	is Unsigned_32;
  subtype Handle 	is Unsigned_32;
  subtype Word   	is Unsigned_16;
  subtype Short  	is Short_Integer;
  subtype Bool   	is Interfaces.C.Int;
  subtype Char   	is Interfaces.C.Char;
  subtype Uint   	is Interfaces.C.Unsigned;
  subtype Cstring	is Interfaces.C.Char_Array;
  type 		Bit 		is mod 2;
  type Click_Type is (Mouseclick, Anyclick, Keyclick);

  --Constants
  Win32_Error          : constant Dword  	:= 0;
  Invalid_Handle_Value : constant Handle 	:= -1;
  Std_Input_Handle     : constant Dword  	:= -10;
  Std_Output_Handle    : constant Dword  	:= -11;

  False: 				constant Bool		:=0;
  Mouse_Moved: 	constant Dword 	:=1;
  Key_Event: 		constant Word  :=    1;
  Mouse_Event: 	constant Word  :=    2;
  Clnrecords: 	constant Dword :=100;
--  Latin_1: 			constant Uint := 1252;


  -- records
  type Coord is record
    X : Short;
    Y : Short;
  end record;
  pragma Convention (C, Coord);


  type Key_Event_Record is record
    Bkeydown: Bool;
    Wrepeatcount: Word;
    Wvirtualkeycode: Word;
    Wvirtualscancode: Word;
    Asciichar:	Char;
    Dwcontrolkeystate: Dword;
  end record;
  for Key_Event_Record use   record
    Bkeydown 		at 0 range 0..31;
    Wrepeatcount 	at 4 range 0..15;
    Wvirtualkeycode 	at 6 range 0..15;
    Wvirtualscancode	at 8 range 0..15;
    Asciichar	   	at 10 range 0..7;
    Dwcontrolkeystate at 12 range 0..31;
  end record;
  for Key_Event_Record'Size use 16*8;


  type Mouse_Event_Record is record
    Dwmouseposition: Coord;
    Dwbuttonstate: Dword;
    Dwcontrolkeystate: Dword;
    Dweventflags: 	Dword;
  end record;
  for Mouse_Event_Record use record
    Dwmouseposition  at 0 range 0..31;
    Dwbuttonstate	at 4 range 0..31;
    Dwcontrolkeystate at 8 range 0..31;
    Dweventflags		at 12 range 0..31;
  end record;
  for Mouse_Event_Record'Size use 16*8;

  type Input_Record (Eventtype: Word:=Mouse_Event) is record
    case Eventtype is
      when Key_Event =>
        Ker: Key_Event_Record;
      when Mouse_Event =>
        Mer: Mouse_Event_Record;
      when others =>
        null;
    end case;
  end record;
  for Input_Record'Size use 20*8;
  for Input_Record use record
    Eventtype		at 0 range 0..31;
    Mer					at 4 range 0..16*8-1;
    Ker					at 4 range 0..16*8-1;
  end record;
  pragma Convention (C, Input_Record);



  type Lparam_Type is record
    Dont_Care_Flag: Bit;
    Ext_Key_Flag: 	Bit;
    Scan_Code:			Char;
  end record;
  for Lparam_Type'Size use 32;
  for Lparam_Type use record
    Dont_Care_Flag 	at 0 range 25..25;
    Ext_Key_Flag 		at 0 range 24..24;
    Scan_Code 			at 0 range 16..23;
  end record;



  -- Access Types
  type Lpdword is access all Dword;
  pragma Convention (C, Lpdword);
  subtype Zstring is Cstring(0..80);
  type Lpzstring is access all Zstring;
  pragma Convention (C,Lpzstring);
  type Input_Record_Array is array (1..Clnrecords) of Input_Record;
  type Pinput_Record_Array is access all Input_Record_Array;
  pragma Convention (C, Pinput_Record_Array);





  -----------------------
  -- PACKAGE VARIABLES --
  -----------------------
  Irbuffer: aliased Input_Record_Array;
  Pirbuffer: Pinput_Record_Array:= Irbuffer'access;
  Hconsoleinput    : Handle;
  Num_Read        : aliased Dword;
  Lpcread: Lpdword:= Num_Read'access;
  Mouseloc: Coord:=(0,0);
  Mouse_But: Dword:=0;
  Old_Mouse_But: Dword:=Mouse_But;
  Lparam: Lparam_Type:=(0,0,Char'Val(78));
  Nint: Int;
  Z_String: aliased Zstring;
  P_Z: Lpzstring := Z_String'access;

  -------------------------
  -- SUPPORTING SERVICES --
  -------------------------
  function Long_Param is new Unchecked_Conversion (Source=> Lparam_Type, Target=> Long);

  function Getstdhandle (Value : Dword) return Handle;
  pragma Import (Stdcall, Getstdhandle, "GetStdHandle");

  function Getkeynametext (Lparam: Long; Lpstring: Lpzstring; Nsize: Int) return Int;
  pragma Import(Stdcall, Getkeynametext,"GetKeyNameTextA");

  function Setconsolecp (Cp: Uint) return Bool;
  pragma Import (Stdcall, Setconsolecp, "SetConsoleCP");
  function Getconsolecp  return Uint;
  pragma Import (Stdcall, Getconsolecp, "GetConsoleCP");

  function Setconsoleoutputcp (Cp: Uint) return Bool;
  pragma Import (Stdcall, Setconsoleoutputcp, "SetConsoleOutputCP");

  function Readconsoleinputa (
    Hconsoleinput:		Handle;
    Pirbuffer: 				Pinput_Record_Array;
    Clnrecords: 			Dword;
    Lpcread: 					Lpdword)
    return Bool;
    pragma Import(Stdcall, Readconsoleinputa, "ReadConsoleInputA");


    procedure Wait_For(Wait_For_This: Click_Type; X,Y: out Integer; Buttonstate: out Unsigned;
      Asciichar: 				out Character;
      Virtualkeycode: 	out Integer;
      Virtualscancode: 	out Integer;
      Controlkeystate: 	out Unsigned) is

    begin
      Outer: loop
        if     Readconsoleinputa ( Hconsoleinput, Pirbuffer,Clnrecords,Lpcread) = False
          then raise Readconsoleinput_Error;
        end if;
        for I in 1..Num_Read loop
          case Irbuffer(I).Eventtype is
            when Mouse_Event =>
              if  Wait_For_This /= Keyclick
                then
                  Mouseloc:= Irbuffer(I).Mer.Dwmouseposition;
                  Old_Mouse_But:=Mouse_But;
                  Mouse_But:= Irbuffer(I).Mer.Dwbuttonstate;
                  if (Old_Mouse_But and Mouse_But) = Mouse_But
                    then null;  -- an UN_CLICK OR mouse_move (not a new click)
                  else          -- a new mouse_click
                    X:=Integer (Mouseloc.X);
                    Y:=Integer (Mouseloc.Y);
                    Buttonstate:=Unsigned (Mouse_But);
                    Asciichar:=Character'Val(0);
                    Virtualkeycode:=0;  Virtualscancode:=0;
                    Controlkeystate:=Unsigned(Irbuffer(I).Mer.Dwcontrolkeystate);
                    exit Outer;
                  end if;
              end if;
              when Key_Event =>
                if Wait_For_This /= Mouseclick
                  then
                    if Irbuffer(I).Ker.Bkeydown=1
                      and  then  Irbuffer(I).Ker.Wvirtualscancode > 0
                        then									                        		-- check for Dead Keys:
                          if  Irbuffer(I).Ker.Wvirtualkeycode = 16        -- caps_lock
                          or else Irbuffer(I).Ker.Wvirtualkeycode = 17   	-- shift
                          or else Irbuffer(I).Ker.Wvirtualkeycode = 18   	-- ctrl
                          or else Irbuffer(I).Ker.Wvirtualkeycode = 20    -- Alt
                          or else Irbuffer(I).Ker.Wvirtualkeycode = 145   -- Alt (right)
                          or else Irbuffer(I).Ker.Wvirtualkeycode = 144   -- Ctrl (right)
                          or else Irbuffer(I).Ker.Wvirtualkeycode = 186  	-- ^¨~ are dead
                          or else (
                            Irbuffer(I).Ker.Wvirtualkeycode = 219 				-- `´  are dead
                              and then                             				-- but | is OK
                                0 = (Irbuffer(I).Ker.Dwcontrolkeystate and Unsigned_32(Right_Alt_Pressed))
                                )			-- do return |
                                then   null;   -- no action on the dead keys
                          else
                            X:=Integer (Mouseloc.X);
                            Y:=Integer (Mouseloc.Y);
                            Buttonstate:=Unsigned (Mouse_But);
                            Asciichar:=Character(Irbuffer(I).Ker.Asciichar);
                            Virtualkeycode:=Integer(Irbuffer(I).Ker.Wvirtualkeycode);
                            Virtualscancode:=Integer(Irbuffer(I).Ker.Wvirtualscancode);
                            Controlkeystate:=Unsigned(Irbuffer(I).Ker.Dwcontrolkeystate);
                            exit Outer;
                          end if; 		-- check for dead key
                    end if; 					-- check for Key_down and positive scancode
                end if; 							-- no mouse click
                when others =>
                  null;
          end case;
        end loop;
      end loop Outer;
  end Wait_For;

  procedure Get_Mouseclick(X,Y: out Integer; Buttonstate: out Unsigned)is
    Asciichar: Character;
    Virtualkeycode:  Integer;
    Virtualscancode: Integer;
    Controlkeystate: Unsigned;
  begin


    Wait_For(Mouseclick,X,Y,Buttonstate,
      Asciichar,
      Virtualkeycode,
      Virtualscancode,
      Controlkeystate);
  end Get_Mouseclick;

  procedure Get_Anything(X,Y: out Integer; Buttonstate: out Unsigned;
    Asciichar: out Character;
    Virtualscancode: out Integer;
    Controlkeystate: out Unsigned) is
      Virtualkeycode:  Integer;
  begin
    Wait_For(Anyclick,X,Y,Buttonstate,
      Asciichar,
      Virtualkeycode,
      Virtualscancode,
      Controlkeystate);
    end Get_Anything;

    procedure Get_Anykey(
      Asciichar: out Character;
      Virtualscancode: out Integer;
      Controlkeystate: out Unsigned) is
        X,Y:  Integer; Buttonstate:  Unsigned;
        Virtualkeycode: Integer;
    begin
      Wait_For(Keyclick,X,Y,Buttonstate,
        Asciichar,
        Virtualkeycode,
        Virtualscancode,
        Controlkeystate);
        end Get_Anykey;


        function Key_Name_Text(
          Virtualscancode: Integer;
          Controlkeystate: Unsigned )
          return String is
            subtype Num_Key_Range is Integer range 71..83;

        begin
          Lparam.Dont_Care_Flag:= 0;
          Lparam.Ext_Key_Flag:=0;
          if (Controlkeystate and Enhanced_Key) = Enhanced_Key then
            Lparam.Ext_Key_Flag:=1;
          end if;
          if Virtualscancode in Num_Key_Range and then
            (Controlkeystate and Numlock_On) = 0 then
              Lparam.Ext_Key_Flag:=1;
          end if;
          Lparam.Scan_Code:= Char'Val(Virtualscancode);
          Nint := Getkeynametext(Long_Param(Lparam),P_Z,80);
          if Nint > 0 then
            return To_Ada(Z_String);
          else
            return "";
          end if;
                        end Key_Name_Text;

                        procedure Set_Codepage (Codepage: Integer:=Latin_1)is
                          Cp: Uint:=Uint(Codepage);
                        begin
                          if Setconsolecp(Cp) = False then
                            raise Codepage_Error;
                          end if;
                          if Setconsoleoutputcp(Cp) = False
                            then
                              raise Codepage_Error;
                          end if;
                        end Set_Codepage;

                        function Get_Codepage return Integer is
                          Cp: Uint:=Getconsolecp;
                        begin
                           return Integer(Cp);
                        end;


                        --------------------------
                        -- WIN32 INITIALIZATION --
                        --------------------------
                            begin
                              Hconsoleinput := Getstdhandle (Std_Input_Handle);
                              if Hconsoleinput = Invalid_Handle_Value then
                                raise Invalid_Handle_Error;
                              end if;
                            end Nt_Mouse;


