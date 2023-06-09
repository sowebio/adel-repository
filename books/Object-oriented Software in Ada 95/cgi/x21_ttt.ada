---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:46 PM BST  --
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
        if First = The.Sqrs( Cells(Pwl)(2) ) and then
            First = The.Sqrs( Cells(Pwl)(3) ) then return Win;
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



with Class_Board, Ada.Text_Io;
use  Class_Board, Ada.Text_Io;
procedure Display( The:in Board ) is
begin
  for I in 1 .. 9 loop
    Put( Cell( The, I ) );
    case I is                        -- after printing counter
      when 3 | 6  =>                 --  print Row Separator
        New_Line; Put("---------");  --
        New_Line;
      when 9      =>                 --  print new line
        New_Line;
      when 1 | 2 | 4 | 5 | 7 | 8 =>  --  print Col separator
        Put(" | ");
    end case;
  end loop;
end Display;

with Class_Board, Ada.Text_Io, Ada.Integer_Text_Io, Display;
use  Class_Board, Ada.Text_Io, Ada.Integer_Text_Io;
procedure Main is
  Player : Character;          -- Either 'X' or 'O'
  Game   : Board;              -- An instance of Class Board
  Move   : Integer;            -- Move from user
begin
  Player := 'X';                           -- Set player

  while State( Game ) = Playable loop      -- While playable
    Put( Player & " enter move (1-9) : "); --   move
    Get( Move ); Skip_Line;                --  Get move
    if Valid( Game, Move ) then            -- Valid
      Add( Game, Move, Player );           --  Add to board
      Display( Game );                     --  Display board
      case State( Game ) is                -- Game is
        when Win       =>
          Put( Player & " wins");
        when Playable  =>
          case Player is                   -- Next player
            when 'X'    => Player := 'O';  --  'X' => 'O'
            when 'O'    => Player := 'X';  --  'O' => 'X'
            when others => null;           --
          end case;
        when Draw      =>
          Put( "It's a draw ");
      end case;
      New_Line;
    else
      Put("Move invalid"); New_Line;       -- for board
    end if;
  end loop;
  New_Line(2);
end Main;
