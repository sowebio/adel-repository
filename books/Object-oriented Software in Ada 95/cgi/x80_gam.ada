---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:51:00 PM BST  --
---------------------------------------------------------------------
--[pack_screen.ads] Specification
package Pack_Screen is
  procedure Screen_Clear;                -- Home clear screen
  procedure Screen_Home;                 -- Home no clear screen
private
  Esc: constant Character := Character'Val(27);
end Pack_Screen;

--[pack_screen.adb] Implementation
with Text_Io; use Text_Io;
package body Pack_Screen is             -- Terminal dependent I/O
  procedure Screen_Clear is             -- Clear screen
  begin
    Put( Esc & "[2J" );                 -- Escape sequence
  end Screen_Clear;
  procedure Screen_Home is              -- Home
  begin
    Put( Esc & "[0;0H");                -- Escape sequence
  end Screen_Home;
end Pack_Screen;

--====================================================================

--[class_counter.ads] Specification
package Class_Counter is
  type Counter        is private;
  type Counter_Colour is ( Black, White );
  procedure Set( The:in out Counter; Rep:in Counter_Colour );
  procedure Display( The:in Counter );
  procedure Display_None( The:in Counter );
  procedure Flip( The:in out Counter );
  function  Rep( The:in Counter ) return Counter_Colour;
private
  type Counter is record
    Colour: Counter_Colour;         -- Colour of counter
  end record;
end Class_Counter;

--[class_counter.adb] Implementation
with Ada.Text_Io;
use  Ada.Text_Io;
package body Class_Counter is
  procedure Set( The:in out Counter; Rep:in Counter_Colour ) is
  begin
    The.Colour := Rep;
  end Set;

  procedure Display( The:in Counter ) is
  begin
    case The.Colour is
      when Black  => Put('X'); -- Representation of a black piece
      when White  => Put('O'); -- Representation of a white piece
    end case;
  end Display;

  procedure Display_None( The:in Counter ) is
  begin
    Put(' ');                   -- Representation of NO piece
  end Display_None;

  procedure Flip( The:in out Counter ) is
  begin
    case The.Colour is
      when Black  => The.Colour := White;     -- Flip to White
      when White  => The.Colour := Black;     -- Flip to Black
    end case;
  end Flip;

  function  Rep( The:in Counter ) return Counter_Colour is
  begin
    return The.Colour;  -- Representation of the counter colour
  end Rep;
end Class_Counter;

--====================================================================

--[class_cell.ads] Specification
with Class_Counter;
use  Class_Counter;
package Class_Cell is
  type Cell is private;
  type Cell_Holds is ( C_White, C_Black, Empty );

  procedure Initialize( The:in out Cell );
  function  Holds( The:in Cell ) return Cell_Holds;
  procedure Add( The:in out Cell; Players_Counter:in Counter );
  procedure Display( The:in Cell );
  procedure Flip( The:in out Cell );
  function To_Colour( C:in Cell_Holds ) return Counter_Colour;
private
  type Cell_Is is ( Empty_Cell, Not_Empty_Cell );
  type Cell is record
    Contents: Cell_Is := Empty_Cell;
    Item    : Counter;                 -- The counter
  end record;
end Class_Cell;

--[class_cell.adb] Implementation
package body Class_Cell is
  procedure Initialize( The:in out Cell ) is
  begin
    The.Contents := Empty_Cell;   -- Initialize cell to empty
  end Initialize;

  function  Holds( The:in Cell ) return Cell_Holds is
  begin
    case The.Contents is
      when Empty_Cell     =>             -- Empty
        return Empty;                    --  No counter
      when Not_Empty_Cell =>             -- Counter
        case Rep( The.Item ) is
          when White => return C_White;  --  white counter
          when Black => return C_Black;  --  black counter
        end case;
    end case;
  end Holds;

  procedure Add(The:in out Cell; Players_Counter:in Counter) is
  begin
    The := (Not_Empty_Cell,Players_Counter);
  end Add;

  procedure Display( The:in Cell ) is
  begin
    if The.Contents = Not_Empty_Cell then
      Display( The.Item );           -- Display the counter
    else
      Display_None( The.Item );      -- No counter
    end if;
  end Display;

  procedure Flip( The:in out Cell ) is
  begin
    Flip( The.Item );                -- Flip counter
  end Flip;

  function To_Colour(C:in Cell_Holds) return Counter_Colour is
  begin
    case C is                        -- Conversion of enum.
      when C_White => return White;
      when C_Black => return Black;
      when others  => raise Constraint_Error;
    end case;
  end To_Colour;

end Class_Cell;

--====================================================================

--[class_board.ads] Specification
with Class_Counter, Class_Cell;
use  Class_Counter, Class_Cell;
package Class_Board is

  type Board         is private;
  type State_Of_Game is ( Play, Win, Draw, Lose );
  type Move_Status   is ( Ok, Invalid, Pass );

  procedure Set_Up( The:in out Board );
  procedure Add( The:in out Board; X,Y:in Integer;
                 Move_Is:in Move_Status );
  procedure Now_Playing( The:in out Board; C:in Counter_Colour );
  procedure Display( The:in Board );
  function  Check_Move( The:in Board; X,Y:in Integer )
                        return Move_Status;
  function  Status( The:in Board ) return State_Of_Game;
  function  Contents( The:in Board; X,Y:in Integer )
                      return Cell_Holds;

private
  Size: constant := 8;                            -- 8 * 8 Board
  subtype Board_Index is Integer range 1 .. Size; --

  type Board_Array is array (Board_Index, Board_Index) of Cell;
  type Score_Array is array (Counter_Colour) of Natural;
  type Move_Array  is array (Counter_Colour) of Move_Status;

  type Board is record
    Sqrs     : Board_Array;               -- Game board
    Player   : Counter_Colour;            -- Current Player
    Opponent : Counter_Colour;            -- Opponent
    Score    : Score_Array;               -- Running score
    Last_Move: Move_Array;                -- Last move is
  end record;
end Class_Board;

--[class_board.adb] Implementation
with Ada.Text_Io, Ada.Integer_Text_Io, Pack_Screen;
use  Ada.Text_Io, Ada.Integer_Text_Io, Pack_Screen;
package body Class_Board is

  procedure Next( The:in Board; X_Co,Y_Co:in out Board_Index;
                  Dir:in Natural; Res:out Boolean);
  function  Find_Turned( The:in Board; X,Y: in Board_Index )
                        return Natural;
  procedure Turn_Counters(The: in out Board; X,Y: in Board_Index;
                          Total: out Natural );
  function  No_Turned(The:in Board; O_X,O_Y:in Board_Index;
                      Dir:in Natural;
                      N:in Natural := 0 ) return Natural;
  procedure Capture(The:in out Board; X_Co, Y_Co:in Board_Index;
                    Dir:in Natural );

  procedure Set_Up( The:in out Board ) is
    Black_Counter: Counter;               -- A black counter
    White_Counter: Counter;               -- A white counter
  begin
    Set( Black_Counter, Black );          -- Set black
    Set( White_Counter, White );          -- Set white
    for X in The.Sqrs'range(1) loop
      for Y in The.Sqrs'range(2) loop
        Initialize( The.Sqrs(X,Y) );      -- To empty
      end loop;
    end loop;
    Add( The.Sqrs( Size/2,   Size/2 ),   Black_Counter );
    Add( The.Sqrs( Size/2,   Size/2+1 ), White_Counter );
    Add( The.Sqrs( Size/2+1, Size/2 ),   White_Counter );
    Add( The.Sqrs( Size/2+1, Size/2+1 ), Black_Counter );
    The.Score( Black ) := 2; The.Score( White ) := 2;
  end Set_Up;

  procedure Add( The:in out Board; X,Y:in Integer;
                 Move_Is:in Move_Status ) is
    Plays_With: Counter;          -- Current player's counter
    Turned    : Natural;          -- Number counters turned
  begin
    Set( Plays_With, The.Player );          -- Set current colour
    The.Last_Move( The.Player ) := Move_Is; -- Last move is
    if Move_Is = Ok then                    -- Not Pass
      Turn_Counters(The, X,Y, Turned);      -- and flip
      Add( The.Sqrs( X, Y ), Plays_With );  -- to board
      The.Score( The.Player ) :=
        The.Score( The.Player ) + Turned + 1;
      The.Score( The.Opponent ):=
        The.Score( The.Opponent ) - Turned;
    end if;
  end Add;

  procedure Now_Playing(The:in out Board; C:in Counter_Colour) is
  begin
    The.Player   := C;                       -- Player
    case C is                                -- Opponent
      when White => The.Opponent := Black;
      when Black => The.Opponent := White;
    end case;
  end Now_Playing;

  procedure Display( The:in Board ) is
    Dashes: String( 1 .. The.Sqrs'Length*4+1 ) := (others=>'-');
  begin
    Screen_Clear;                            -- Clear screen
    Put( Dashes ); New_Line;                 -- Top
    for X in The.Sqrs'range(1) loop
      Put("|");                              -- Cells on line
      for Y in The.Sqrs'range(2) loop
        Put(" "); Display( The.Sqrs(X,Y) ); Put(" |");
      end loop;
      New_Line; Put( Dashes ); New_Line;     -- Bottom lines
    end loop;
    New_Line;
    Put( "Player X has " );
    Put( Integer(The.Score(Black)), Width=>2 );
    Put( " counters" ); New_Line;
    Put( "Player O has " );
    Put( Integer(The.Score(White)), Width=>2 );
    Put( " counters" ); New_Line;
  end Display;

  function Check_Move( The:in Board; X,Y:in Integer )
      return Move_Status is
  begin
    if X = 0 and then Y = 0 then
      return Pass;
    elsif X in Board_Index and then Y in Board_Index then
      if Holds( The.Sqrs( X, Y ) ) = Empty then
        if Find_Turned(The, X, Y) > 0 then
          return Ok;
        end if;
      end if;
    end if;
    return Invalid;
  end Check_Move;

  function Find_Turned( The:in Board; X,Y: in Board_Index )
      return Natural is
    Sum     : Natural := 0;       -- Total stones turned
  begin
    if Holds( The.Sqrs( X, Y ) ) = Empty then
      for Dir in 1 .. 8 loop      -- The  8 possible directions
        Sum := Sum + No_Turned( The, X, Y, Dir );
      end loop;
    end if;
    return Sum;                   -- return total
  end Find_Turned;

  procedure Turn_Counters(The: in out Board; X,Y: in Board_Index;
      Total: out Natural ) is
    Num_Cap  : Natural := 0;
    Captured : Natural;
  begin
    if Holds( The.Sqrs( X, Y ) ) = Empty then
      for Dir in 1 .. 8 loop
        Captured := No_Turned( The, X, Y, Dir );
        if Captured > 0 then
          Capture( The, X, Y, Dir );
          Num_Cap := Num_Cap + Captured;
        end if;
      end loop;
    end if;
    Total := Num_Cap;
  end Turn_Counters;

  function  No_Turned(The:in Board; O_X,O_Y:in Board_Index;
      Dir:in Natural;
      N:in Natural := 0 ) return Natural is
    Ok : Boolean;                           -- Result from next
    Nxt: Cell_Holds;                        -- Next in line is
    Col: Counter_Colour;                    -- Counter colour
    X  : Board_Index := O_X;                -- Local copy
    Y  : Board_Index := O_Y;                -- Local copy
  begin
    Next( The, X,Y, Dir, Ok );              -- Next cell
    if Ok then                              -- On the board
      Nxt := Holds( The.Sqrs(X,Y) );        -- Contents are
      if Nxt = Empty then                   -- End of line
        return 0;
      else
        Col := To_Colour( Nxt );            -- Colour
        if Col = The.Opponent then          -- Opponents counter
          return No_Turned(The, X,Y, Dir, N+1); -- Try next cell
        elsif Col = The.Player then         -- End of counters
          return N;                         -- Counters turned
        end if;
      end if;
    else
      return 0;                             -- No line
    end if;
  end No_Turned;

  procedure Next( The:in Board; X_Co,Y_Co:in out Board_Index;
      Dir:in Natural; Res:out Boolean) is
    X, Y   : Natural;
  begin
    X := X_Co; Y := Y_Co;         -- May go outside Board_range
    case Dir is
      when 1 =>         Y:=Y+1;   --  Direction to move
      when 2 => X:=X+1; Y:=Y+1;   --      8   1   2
      when 3 => X:=X+1;           --
      when 4 => X:=X+1; Y:=Y-1;   --      7   *   3
      when 5 =>         Y:=Y-1;   --
      when 6 => X:=X-1; Y:=Y-1;   --      6   5   4
      when 7 => X:=X-1;           --
      when 8 => X:=X-1; Y:=Y+1;   --
      when others => raise Constraint_Error;
    end case;
    if X in Board_Index and then Y in Board_Index then
      X_Co :=  X; Y_Co :=  Y;     --
      Res := True;                -- Found a next cell
    else
      Res := False;               -- No next cell
    end if;
  end Next;

  procedure Capture(The:in out Board; X_Co, Y_Co:in Board_Index;
                    Dir:in Natural ) is
    Ok   : Boolean;                   -- There is a next cell
    X, Y : Board_Index;               -- Coordinates of cell
    Nxt  : Cell_Holds;                -- Next in line is
  begin
    X := X_Co; Y := Y_Co;
    Next( The, X, Y, Dir, Ok );       -- Calculate pos next cell
    if Ok then                        -- Cell exists (Must)
      Nxt := Holds( The.Sqrs(X,Y) );
      if To_Colour( Nxt ) = The.Opponent then
        Flip( The.Sqrs(X, Y) );       -- Capture
        Capture(The, X, Y, Dir );     -- Implement capture
      else
        return;                       -- End of line
      end if;
    else
      raise Constraint_Error;         -- Will never occur
    end if;
  end Capture;

  function  Status ( The:in Board ) return State_Of_Game is
  begin
    if The.Score( The.Opponent ) = 0 then
      return Win;
    end if;
    if (The.Sqrs'Length(1) * The.Sqrs'Length(2) =
        The.Score(The.Opponent)+The.Score(The.Player)) or
        (The.Last_Move(Black)=Pass and The.Last_Move(White)=Pass)
        then
      if The.Score(The.Opponent) = The.Score(The.Player)
          then return Draw;
      end if;
      if The.Score(The.Opponent) < The.Score(The.Player)
          then return Win;
      else
        return Lose;
      end if;
    end if;
    return Play;
  end;

  function Contents( The:in Board; X,Y:in Integer )
                     return Cell_Holds is
  begin
    return Holds( The.Sqrs( X, Y ) );
  end Contents;

end Class_Board;

--====================================================================

--[class_player.ads] Specification
with Class_Counter, Class_Board;
use  Class_Counter, Class_Board;
package Class_Player is
  type Player is private;

  procedure Set( The:in out Player; C:in Counter_Colour );
  procedure Get_Move(The:in Player; Row,Column:out Integer);
  function  My_Counter( The:in Player ) return Counter;
  procedure Announce( The:in Player; What:in State_Of_Game );
private
  type Player is record
    Plays_With : Counter;        -- Player's counter
  end record;
end Class_Player;

--[class_player.adb] Implementation
with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io;
package body Class_Player is
  procedure Set(The:in out Player; C:in Counter_Colour ) is
    A_Counter : Counter;
  begin
    Set( A_Counter, C );          -- Set colour
    The.Plays_With := A_Counter;  -- Player is playing with
  end Set;

  procedure Get_Move(The:in Player; Row,Column:out Integer) is
    Valid_Move : Boolean := False;
  begin
    while not Valid_Move loop
      begin
        Put("Please enter move "); Display( The.Plays_With );
        Put(" row column : "); Get( Row ); Get( Column );
        Valid_Move := True;
      exception
        when Data_Error =>
          Row := -1; Column := -1; Skip_Line;
        when End_Error =>
          Row := 0; Column := 0;
          return;
      end;
    end loop;
  end Get_Move;

  function  My_Counter( The:in Player ) return Counter is
  begin
    return The.Plays_With;
  end My_Counter;

  procedure Announce(The:in Player; What:in State_Of_Game) is
  begin
    case What is
      when Win    =>
        Put("Player "); Display( The.Plays_With );
        Put(" has won");
      when Lose   =>
        Put("Player "); Display( The.Plays_With );
        Put(" has lost");
      when Draw   =>
        Put("It's a draw");
      when others =>
        raise Constraint_Error;
    end case;
    New_Line;
  end Announce;

end Class_Player;

--====================================================================

--[play.adb] Procedure
with Class_Board, Class_Player, Class_Counter;
use  Class_Board, Class_Player, Class_Counter;
package Class_Game is
  type Game is private;
  procedure play( The:in out Game );
private
  type Player_Array is array(Counter_Colour) of Player;
  type Game is record
    Reversi    : Board;                     -- The playing board
    Contestant : Player_Array;
  end record;
end Class_Game;


package body Class_Game is

procedure Play( The:in out Game ) is         -- Play reversi
  Current_State : State_Of_Game;             -- State of game
  Person        : Counter_Colour;            -- Current player
  X, Y          : Integer;                   -- Move
  Move_Is       : Move_Status;               -- Last move is
begin
  Set_Up( The.Reversi );                     -- Set up board
  Set( The.Contestant(Black), Black );       -- Set player black
  Set( The.Contestant(White), White );       -- Set player white

  Current_State := Play;  Person := Black;   -- Black starts

  Display( The.Reversi );                    -- Initial board

  while Current_State = Play loop            -- Playable game
    Now_Playing( The.Reversi, Person );      -- set player

    loop                                     -- Get move 
      Get_Move(The.Contestant(Person), X, Y);
      Move_Is:=Check_Move(The.Reversi, X, Y);-- Validate
      exit when Move_Is=Ok or Move_Is=Pass;  -- OK
    end loop;

    Add( The.Reversi, X, Y, Move_Is );       -- Add move to board

    Display( The.Reversi );                  -- Display new board
    Current_State := Status( The.Reversi );  -- State of play is

    if Current_State = Play then             -- Is still playable
      case Person is                         -- next player
        when Black  => Person := White;
        when White  => Person := Black;
      end case;
    end if;

  end loop;                                  -- Next move

  Announce( The.Contestant(Person), Current_State );  -- Result

end Play;

--====================================================================

end Class_Game;

with Class_Game;
use  Class_Game;
procedure Main is
  A_Game : Game;
begin
  Play( A_Game );
end Main;
