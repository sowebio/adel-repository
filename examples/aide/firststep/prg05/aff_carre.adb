
-- aff_carre.adb

with ADA.TEXT_IO;
use ADA.TEXT_IO;

procedure aff_carre is

begin

  for i in 1..9 loop

      put(integer'image(i));

      if (i mod 3) = 0 then 
         new_line;
      end if;

  end loop;

end aff_carre;