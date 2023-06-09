with Nt_Mouse; use Nt_Mouse;
with Text_Io; 
use  Text_Io; 
with Ada.Characters.Handling;
procedure Testmouse is
  package Iio is new Integer_Io(Integer);
  use Iio;
  X,Y: Integer;
  Buttonstate,Controlkeystate: Nt_Mouse.Unsigned;
  Ch: Character;
  Scancode: Integer;
  N: Integer:=0;
  
  procedure Cput(Var,Mask: Unsigned; St1: String; St2: String :="") is
  begin
    if (Var and Mask) /=0 then 
      Put(St1);
    else 
      Put (St2);
    end if;
  end Cput;

begin
  begin
    Put_Line("Initial (OEM)  CodePage:"&Integer'Image(Nt_Mouse.Get_Codepage));
    Nt_Mouse.Set_Codepage;
    Put_Line("Current (ANSI) CodePage:"&Integer'Image(Nt_Mouse.Get_Codepage));
    Put_Line("Use LucidaConsole Font for true rendering of entire Latin_1 characterset"); 
    Put_Line("(Rightclick Title Bar, select Properties, Font, Lucidaconsole, OK)");
  exception
    when Nt_Mouse.Codepage_Error=>
      Put_Line("Codepage_Error ignored (Set_codepage works only for Windows NT)");
  end;
    New_Line;
    Put_Line("Click Mouse, any Key, or any Extended Key:");
  loop
    Nt_Mouse.Get_Anything(X,Y,Buttonstate,Ch,Scancode,Controlkeystate);
    if Buttonstate /= 0 then
      Put("Mouseclick: (");
      Cput (Buttonstate,From_Left_1st_Button_Pressed,"L"," ");
      Cput (Buttonstate,From_Left_2nd_Button_Pressed,"M"," ");
      Cput (Buttonstate,Rightmost_Button_Pressed,"R"," ");
      Put(") "); 
    else 
      if Ch /= Character'Val(0) then
        Put("Normal Key:"&Integer'Image(Character'Pos(Ch))&"='"&Ch&"'");
      else 
      Cput(Controlkeystate,Enhanced_Key,"Enhanced Key:","Extended Key:");
      end if;
      New_Line;
      Put("Scancode:"&Integer'Image(Scancode));    
      Put(" Ctrl_keys:(");
      Cput(Controlkeystate,Right_Alt_Pressed,"Alt_R ");
      Cput(Controlkeystate,Left_Alt_Pressed,"Alt_L ");
      Cput(Controlkeystate,Right_Ctrl_Pressed,"Ct_R ");
      Cput(Controlkeystate,Left_Ctrl_Pressed,"Ct_L ");
      Cput(Controlkeystate,Capslock_On,"C_lock ");
      Cput(Controlkeystate,Enhanced_Key,"Enhanced ");
      Cput(Controlkeystate,Numlock_On,"N_lock ");
      Cput(Controlkeystate,Scrolllock_On,"S_lock ");
      Cput(Controlkeystate,Shift_Pressed,"Shift ");
      Put(")");
      Put(" KeyName:"""&
        Key_Name_Text(Scancode,Controlkeystate)&"""");
        
    end if;
    
    Put(" CursorPos:(");Put(X,3); Put(Y,3);Put(")");
    New_Line(2);
  end loop;
end;

