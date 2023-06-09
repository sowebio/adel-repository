

-------------------------------------------------------------

with Ada.Finalization;
use  Ada.Finalization;
package Class_input_manager is
  type Input_manager is abstract tagged limited private;
  procedure window_prologue;       -- Initialize window system
  procedure window_start;          -- Start taking user input
  procedure window_epilogue;       -- Clean up
private
  type Input_manager is
    abstract new Limited_controlled with null record;
end Class_input_manager;
