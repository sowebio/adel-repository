
-- car_32_126.adb

with ADA.TEXT_IO, ADA.INTEGER_TEXT_IO;
use ADA.TEXT_IO, ADA.INTEGER_TEXT_IO;

procedure car_32_126 is

begin

  for n in 32..126 loop
      put ("Le caractere dont le code ASCII est ");
      put (n, width => 3);
      put_line (" est " & character'val (n));  

  end loop;

end car_32_126;