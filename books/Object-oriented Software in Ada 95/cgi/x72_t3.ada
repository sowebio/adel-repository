---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:54 PM BST  --
---------------------------------------------------------------------
package Class_Board is

  type Board      is private;
  type Game_State is ( Win, Playable, Draw );

  procedure Add( The:in out Board; Pos:in Integer;
                 Piece:in Character );
  function  Valid( The:in Board; Pos:in Integer ) return Boolean;
  function  State( The:in Board ) return Game_State;
  function  Cell( The:in Board; Pos:in Integer ) return Character;
  procedure Reset( The:in out Board );

private
  subtype Board_Index is Integer range 1 .. 9;
  type    Board_Array is array( Board_Index ) of Character;
  type Board is record
    Sqrs  : Board_Array := ( others => ' ');   -- Initialize
    Moves : Natural     := 0;
  end record;
end Class_Board;

package body Class_Board is

  procedure Add( The:in out Board; Pos:in Integer;
                 Piece:in Character ) is
  begin
    The.Sqrs( Pos ) := Piece;
  end Add;

  function Valid(The:in Board; Pos:in Integer) return Boolean is
  begin
    return Pos in Board_Array'Range and then The.Sqrs( Pos ) = ' ';
  end Valid;

  function  State( The:in Board ) return Game_State is
    subtype Position   is Integer range 1 .. 9;
    type Win_Line      is array( 1 .. 3 ) of Position;
    type All_Win_Lines is range 1 .. 8;
    Cells: constant array ( All_Win_Lines ) of Win_Line :=
      ( (1,2,3), (4,5,6), (7,8,9), (1,4,7),
        (2,5,8), (3,6,9), (1,5,9), (3,5,7) ); -- All win lines
    First : Character;
  begin
    for Pwl in All_Win_Lines loop         -- All Pos Win Lines
      First := The.Sqrs( Cells(Pwl)(1) ); -- First cell in line
      if First /= ' ' then                --  Looks promising
        if First = The.Sqrs(Cells(Pwl)(2)) and then
            First = The.Sqrs(Cells(Pwl)(3)) then return Win;
        end if;
      end if;
    end loop;
    if The.Moves >= 9 then                -- Check for draw
      return Draw;                        --  Board full
    else
      return Playable;                    --  Still playable
    end if;
  end State;

  function Cell( The:in Board; Pos:in Integer ) return Character is
  begin
    return The.Sqrs( Pos );
  end Cell;

  procedure Reset( The:in out Board ) is
  begin
    The.sqrs  := ( others => ' ');   -- All spaces
    The.moves := 0;                  -- No of moves
  end reset;

end Class_Board;

with Class_Board, Class_Window;
use  Class_Board, Class_Window;
package Pack_Program is
  procedure Play;
private
  Game      : Board;       -- The board
  P_Win_Brd : P_Window;    -- Window to display OXO board in
  P_Win_Bnr : P_Window;    -- Window to display Banner in
  P_Win_R   : P_Window;    -- Window to display commentary in
  Player    : Character;   -- Either 'X' or 'O'
end Pack_Program;


with Ada.Integer_Text_Io, 
     Class_Dialog, Class_Menu, Class_Input_Manager, Class_Menu_Title;
use  Ada.Integer_Text_Io, 
     Class_Dialog, Class_Menu, Class_Input_Manager, Class_Menu_Title;
package body Pack_Program is

  procedure Display_Board( Win:in P_Window ) is
  begin
    Position( Win.all, 1, 2 );
    Put(Win.all, " 7 | 8 | 9" ); New_Line( Win.all );
    Put(Win.all, " ---------" ); New_Line( Win.all );
    Put(Win.all, " 4 | 5 | 6" ); New_Line( Win.all );
    Put(Win.all, " ---------" ); New_Line( Win.all );
    Put(Win.all, " 1 | 2 | 3" ); New_Line( Win.all );
  end Display_Board;
  
  -- Note mapping for squares
  
  procedure Update( Move:in Integer; Win:in P_Window ) is
    type Co_Ordinate is ( X , Y );
    type Cell_Pos is array ( Co_Ordinate ) of Positive;
    type Board    is array ( 1 .. 9 ) of Cell_Pos;
    Pos: constant Board :=    ( (2,6), (6,6), (10,6),
                                (2,4), (6,4), (10,4),
                                (2,2), (6,2), (10,2) );
  begin
    Position( Win.all, Pos(Move)(X), Pos(Move)(Y) );
    Put( Win.all, Cell( Game, Move ) );       -- Display counter;
  end Update;
  
  function User_Input( Cb_Mes:in String ) return String is
    Move: Integer; Last: Positive;
  begin
    Clear( P_Win_R.all );                    -- Clear
    Get( Cb_Mes, Move, Last );               -- to int
    if Valid( Game, Move ) then              -- Valid
      Add( Game, Move, Player );             -- to board
      Update( Move, P_Win_Brd );
      case State( Game ) is                  -- Game is
        when Win       =>
          Put(P_Win_R.all, " " & Player & " wins");
        when Playable  =>
          case Player is                     -- Next player
            when 'X'    => Player := 'O';    --  'X' => 'O'
            when 'O'    => Player := 'X';    --  'O' => 'X'
            when others => null;             --
          end case;
          Put( P_Win_R.all, " Player " & Player );
        when Draw      =>
          Put( P_Win_R.all, " It's a draw ");
      end case;
    else
      Put(P_Win_R.all, " " & Player & " Square invalid");
    end if;
    return "";
  exception
    when others =>
      Put(P_Win_R.all, " " & Player & " re-enter move");
      return "";
  end User_Input;
  
  procedure Re_Start( First_Player:in Character ) is
  begin
    Player := First_Player;                  -- Start with
    Reset( Game );                           -- Reset Board
    Display_Board( P_Win_Brd );              -- Display
    Clear( P_Win_R.all );                    -- Status info
    Put( P_Win_R.all, " Player " & Player ); -- Player name
  end Re_Start;
  
  function Reset_X( Cb_Mes:in String ) return String is
  begin
    Re_Start('X'); return "";
  end Reset_X;
  
  function Reset_O( Cb_Mes:in String ) return String is
  begin
    Re_Start('O'); return "";
  end Reset_O;
  
  function About( Cb_Mes:in String ) return String is
  begin
    Clear( P_Win_Bnr.all ); Position( P_Win_Bnr.all, 17, 1 );
    Put( P_Win_Bnr.all, "Written in Ada 95");
    return "";
  end About;
  
  --[play.adb] Procedure
  procedure Play is
  begin
    Window_Prologue;             -- Setup window system
    declare
      Win_Brd  : aliased Window; -- Board Window
      Win_R    : aliased Window; -- Result Window
      Win_Bnr  : aliased Window; -- title Window
      Win_Usr  : aliased Dialog; -- Input Window
      Ttt_Reset: aliased Menu;   -- Reset menu
      Ttt_Menu : Menu_Title;     -- Title menu
  
  
    begin
      Framework( Win_Bnr,  1,  4, 52, 3 );    -- Banner
      Framework( Win_Brd, 32,  8, 13, 9 );    -- OXO board
      Framework( Win_R,    9, 14, 22, 3 );    -- Results
  
      Framework( Ttt_Reset,
        "X start",  null,  Reset_X'access,
        "O start",  null,  Reset_O'access  );
  
      Framework( Ttt_Menu,
        "About",    null,  About'access,
        "Reset",    Ttt_Reset'Unchecked_Access,  null );
  
      Position( Win_Bnr, 17, 1 );
      Put( Win_Bnr, "Noughts and crosses" );
  
      Framework( Win_Usr,  9, 8, 22,
                 "Move (1-9)", User_Input'access );
  
      Player := 'X';                          -- Set player
      P_Win_Brd := Win_Brd'Unchecked_Access;  -- OXO Board
      P_Win_Bnr := Win_Bnr'Unchecked_Access;  -- Banner
      P_Win_R   := Win_R'Unchecked_Access;    -- Commentary
  
      Display_Board( P_Win_Brd );             -- Empty board
      New_Line( Win_R );                      -- Clear
      Put( Win_R, " Player " & Player );      -- Players turn is
  
      Put( Win_Usr, "" );       -- Cursor
      Window_Start;             -- Start the user interaction
    end;
    Window_Epilogue;            -- Close window system
  end Play;

end Pack_Program;


with Pack_Program;
procedure Main is
begin
  Pack_program.Play;
end Main;
