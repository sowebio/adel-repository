-- bonj.adb

with ADA.TEXT_IO;  -- necessaire pour faire put (affichage) et get (saisie) ..
                   -- des chaines de caract�res
procedure bonj is

begin

  ADA.TEXT_IO.put("Bonjour");
  ADA.TEXT_IO.new_line; -- retour � la ligne

end bonj;