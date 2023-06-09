-- conversion.adb

with ADA.TEXT_IO, ADA.FLOAT_TEXT_IO;
use ADA.TEXT_IO, ADA.FLOAT_TEXT_IO;

procedure conversion is

taux : constant float := 0.45;

begin

  for i in 1..10 loop

      put (integer'image(i) & " livre = ");
      put (Float(i)* taux, fore => 2, aft => 2, exp =>0);
      put (" Kg");

      new_line;

  end loop;

end conversion;