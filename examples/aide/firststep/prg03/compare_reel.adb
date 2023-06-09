-- compare_reel.adb

with ADA.TEXT_IO; use ADA.TEXT_IO;
with ADA.FLOAT_TEXT_IO; use ADA.FLOAT_TEXT_IO; -- pour saisie et affichage de reels

procedure compare_reel is

a, b : float; -- declaration des variables

begin

  put_line("Bonjour, je peux vous calculer le plus grand de deux nombres ");
  put("Entrez le premier nombre reel : ");
  get(a);

  put("Entrez le second nombre reel : ");
  get(b);
  
  put("Le plus grand est ");

  if (a >= b) then 
     put(a, fore => 3, aft => 1, exp => 0);

  else 
     put(b, fore => 3, aft => 1, exp => 0);

  end if;
     
  new_line;

end compare_reel;
