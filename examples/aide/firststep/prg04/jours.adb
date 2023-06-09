-- jours.adb

with ADA.TEXT_IO; use ADA.TEXT_IO;

procedure jours is

type tjour is (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

-- pour faire des put et des get sur le type tjour, voici ce qu'il faut faire

package tjour_io is new ADA.TEXT_IO.ENUMERATION_IO(tjour); use tjour_io;

jour : tjour;

begin

  put("entrez un jour : ");   -- put qui vient de ADA.TEXT_IO
  get(jour);                  -- get qui vient de tjour_io

  -- on affiche ici le jour rentre par l'utilisateur
  put(jour);                  -- put qui vient de tjour_io
  new_line;

  -- et ici le lendemain
  put("Le lendemain est : ");

  if jour = Dimanche then 
     put(Lundi);

  else 
     put(tjour'succ(jour));

  end if;

  new_line;

end jours;
