package Nt_Mouse is
  
  type Unsigned is mod 2**32;
  
  procedure Get_Mouseclick(X,Y: out Integer; Buttonstate: out Unsigned);
  
  -- Buttonstate Constants 
  From_Left_1st_Button_Pressed:		constant Unsigned := 16#01#;
  Rightmost_Button_Pressed:				constant Unsigned := 16#02#;
  From_Left_2nd_Button_Pressed:		constant Unsigned := 16#04#;
  From_Left_3rd_Button_Pressed:		constant Unsigned := 16#08#;
  From_Left_4th_Button_Pressed:		constant Unsigned := 16#10#;
  
  
  
  procedure Get_Anything(X,Y: out Integer; Buttonstate: out Unsigned; 
    Asciichar: 			out Character; 
    Virtualscancode: 	out Integer; 
    Controlkeystate: 		out Unsigned); 
    
  procedure Get_Anykey( 
      Asciichar: 			out Character; 
      Virtualscancode: 	out Integer; 
      Controlkeystate: 		out Unsigned); 
      
  -- Controlkeystate Constants
  Right_Alt_Pressed: 		constant Unsigned :=16#01#;
  Left_Alt_Pressed: 		constant Unsigned :=16#02#;
  Right_Ctrl_Pressed: 	constant Unsigned :=16#04#;
  Left_Ctrl_Pressed: 		constant Unsigned :=16#08#;
  Shift_Pressed: 				constant Unsigned :=16#10#;
  Numlock_On: 					constant Unsigned :=16#20#;
  Scrolllock_On: 				constant Unsigned :=16#40#;
  Capslock_On: 					constant Unsigned :=16#80#;
  Enhanced_Key: 				constant Unsigned :=16#100#;
      
  function Key_Name_Text(
        Virtualscancode: Integer;
        Controlkeystate: Unsigned ) return String;
        
  Latin_1: constant Integer:= 1252;
  procedure Set_Codepage (Codepage: Integer:= Latin_1);  
  function Get_Codepage return Integer;
        
        -- exceptions
  Invalid_Handle_Error   : exception;
  Readconsoleinput_Error : exception;
  Codepage_Error			   : exception;
        
        
end Nt_Mouse;

