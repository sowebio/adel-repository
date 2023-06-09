
with Class_input_manager, Class_board, Class_window,
     Class_dialog, Class_menu, Class_menu_title,
     Pack_globals, reset_x, reset_o, about, user_input;
use  Class_input_manager, Class_board, Class_window,
     Class_dialog, Class_menu, Class_menu_title,
     Pack_globals;
procedure play is
begin
  window_prologue;             -- Setup window system
  declare
    win_brd  : aliased Window; -- Board Window
    win_r    : aliased Window; -- Result Window
    win_bnr  : aliased Window; -- title Window
    win_usr  : aliased Dialog; -- Input Window
    ttt_reset: aliased Menu;   -- Reset menu
    ttt_menu : Menu_title;     -- Title menu


  begin
    framework( win_bnr,  1,  4, 52, 3 );    -- Banner
    framework( win_brd, 32,  8, 13, 9 );    -- OXO board
    framework( win_r,    9, 14, 22, 3 );    -- Results

    framework( ttt_reset,
               "X start",  null,  reset_x'Access,
               "O start",  null,  reset_o'Access  );

    framework( ttt_menu,
               "About",    null,  about'Access,
               "Reset",    ttt_reset'Unchecked_Access,  null );

    position( win_bnr, 17, 1 );
    put( win_bnr, "Noughts and crosses" );

    framework( win_usr,  9, 8, 22,
               "Move (1-9)", user_input'Access );

    player := 'X';                          -- Set player
    p_win_brd := win_brd'Unchecked_Access;  -- OXO Board
    p_win_bnr := win_bnr'Unchecked_Access;  -- Banner
    p_win_r   := win_r'Unchecked_Access;    -- Commentary

    display_board( game, p_win_brd );       -- Empty board
    new_line( win_r );                      -- Clear
    put( win_r, " Player " & player );      -- Players turn is

    put( win_usr, "" );       -- Cursor
    window_start;             -- Start the user interaction
  end;
  window_epilogue;            -- Close window system
end play;
