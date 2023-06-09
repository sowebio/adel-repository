---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:52 PM BST  --
---------------------------------------------------------------------
--[pack_factorial.ads] Specification
package Pack_Factorial is
  task type Task_Factorial is                -- Specification
    entry Start( F:in Positive );            -- Rendezvous
    entry Finish( Result:out Positive );     -- Rendezvous
  end Task_Factorial;
end Pack_Factorial;

--[pack_factorial.adb] Implementation
package body Pack_Factorial is
  task body Task_Factorial is                -- Implementation
    Factorial : Positive;
    Answer    : Positive := 1;
  begin
    accept Start( F:in Positive ) do         -- Factorial
      Factorial := F;
    end Start;
    for I in 2 .. Factorial loop             -- Calculate
      Answer := Answer * I;
    end loop;
    accept Finish( Result:out Positive ) do -- Return answer
      Result := Answer;
    end Finish;
  end Task_Factorial;
end Pack_Factorial;

--[pack_is_a_prime.ads] Specification
package Pack_Is_A_Prime is
  task type Task_Is_Prime is                 -- Specification
    entry Start( P:in Positive );            -- Rendezvous
    entry Finish( Result:out Boolean );      -- Rendezvous
  end Task_Is_Prime;
end Pack_Is_A_Prime;

--[pack_is_a_prime.adb] Implementation
package body Pack_Is_A_Prime is
  task body Task_Is_Prime is                 -- Implementation
    Prime : Positive;
    Answer: Boolean := True;
  begin
    accept Start( P:in Positive ) do         -- Factorial
      Prime := P;
    end Start;
    for I in 2 .. Prime-1 loop               -- Calculate
      if Prime rem I = 0 then
        Answer := False; exit;
      end if;
    end loop;
    accept Finish( Result:out Boolean ) do  -- Return answer
      Result := Answer;
    end Finish;
  end Task_Is_Prime;
end Pack_Is_A_Prime;

--[execute_threads.adb] Procedure
with Ada.Text_Io, Ada.Integer_Text_Io, Pack_Factorial, Pack_Is_A_Prime;
use  Ada.Text_Io, Ada.Integer_Text_Io, Pack_Factorial, Pack_Is_A_Prime;
procedure Main is
  Thread_1 : Task_Factorial;
  Thread_2 : Task_Factorial;
  Thread_3 : Task_Is_Prime;
  Factorial: Positive;
  Prime    : Boolean;

begin
  Thread_1.Start(5);            -- Start factorial calculation
  Thread_2.Start(7);            -- Start factorial calculation
  Thread_3.Start(97);           -- Start is_prime  calculation

  Put("Factorial  5 is ");
  Thread_1.Finish( Factorial ); -- Obtain result
  Put( Factorial ); New_Line;

  Put("Factorial  8 is ");
  Thread_2.Finish( Factorial ); -- Obtain result
  Put( Factorial ); New_Line;

  Put("97 is a prime is ");
  Thread_3.Finish( Prime );     -- Obtain result
  if Prime then                 --
     Put("True");               --   and print
  else 
     Put("False"); 
  end if; 
  New_Line;
end Main;
