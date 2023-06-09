
with Class_root_window, Class_input_manager, Class_window,
     Class_dialog, Class_menu, Class_menu_title, Pack_globals,
     execute_call_back, write_name, no_help, echo_mess;
use  Class_root_window, Class_input_manager, Class_window,
     Class_dialog, Class_menu, Class_menu_title, Pack_globals;
procedure main is
begin
  window_prologue;
declare
  e,f            : Dialog;
  w1,w2          : aliased Window;
  m1             : aliased Menu_title;
  m2,m3,m4,m5    : aliased Menu;


  begin
    framework( w1,  1,  5, 30, 10 );
    framework( w2, 70, 20, 10, 6 );

    framework(m2, "Open",   null,       null,
                  "Close",  null,       null,
                  "Window", null,       execute_call_back'Access,
                  "help",   null,       no_help'Access );

    framework(m3, "up",     null,       null,
                  "down",   null,       null,
                  "left",   null,       null,
                  "right",  null,       null,
                  "loop",   m4'Unchecked_Access,  null,
                  "About",  null,       write_name'Access);

    framework(m4, "Loop",   m3'Unchecked_Access,   null,
                  "Help",   null,        no_help'Access,
                  "About",  null,        write_name'Access,
                  "About",  null,        write_name'Access );

    framework(m5, "Mike",   null,        write_name'Access,
                  "A",      null,        write_name'Access,
                  "Smith",  null,        write_name'Access,
                  "Next",   m4'Unchecked_Access,   null );

    framework(m1, "PC",    m2'Unchecked_Access,    null,
                  "File",  m3'Unchecked_Access,    null,
                  "Trans", m5'Unchecked_Access,    null );

    for i in 1 .. 7 loop
      put(w1,"Mike Smith"); new_line( w1 );
    end loop;

    for i in 1 .. 20 loop
      for b in Attribute loop
        mark_border( w2, b, i, ':' );
      end loop;
    end loop;

    framework(e, 40, 6 , 20, "Diag 1", echo_mess'Access );
    framework(f, 40, 18, 22, "Diag 2", execute_call_back'Access );

    p_w1 := w1'Unchecked_Access;
    p_w2 := w2'Unchecked_Access;

    window_start;
  end;
  window_epilogue;
end main;
