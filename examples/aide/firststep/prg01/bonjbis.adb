-- bonjbis.adb

with ADA.TEXT_IO;  -- necessaire pour faire put (affichage) et get (saisie) ...
                   -- des chaines de caract�res
use ADA.TEXT_IO;   -- evite de mettre ADA.TEXT_IO. avant put, get, ...

procedure bonjbis is

begin

  put("Bonjour");
  new_line; -- retour � la ligne

end bonjbis;