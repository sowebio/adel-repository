-- bonjter.adb

with ADA.TEXT_IO;  -- necessaire pour faire put (affichage) et get (saisie) ...
                   -- des chaines de caractères

procedure bonjter is

package tio renames ADA.TEXT_IO;  -- permet d'ameliorer la lisibilite
                                  -- en gardant le nom du package lie
                                  -- aux procedures ou au fonctions 
begin

  tio.put("Bonjour");
  tio.new_line; -- retour à la ligne

end bonjter;
