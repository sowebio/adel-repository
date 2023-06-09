-- mult_8.adb

with ADA.TEXT_IO, ADA.INTEGER_TEXT_IO;
use ADA.TEXT_IO, ADA.INTEGER_TEXT_IO;

procedure mult_8 is

mult : constant integer := 8;

begin

  put ("Voici la table de multiplication de "); 
  put(mult); 
  new_line;

  for i in 1..9 loop
      put (mult); put (" x "); put(i); put ( " = " );
      put (mult * i);

      new_line;

  end loop;

end mult_8;