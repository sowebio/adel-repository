with ADA.TEXT_IO; use ADA.TEXT_IO;
with ADA.INTEGER_TEXT_IO; use ADA.INTEGER_TEXT_IO;

procedure compare is

a, b : integer; -- declaration des variables

begin

  put_line("Bonjour, je peux vous calculer le plus grand de deux nombres ");
  put("Entrez le premier nombre : ");
  get(a);

  put("Entrez le second nombre : ");
  get(b);
  
  put("Le plus grand est ");

  if (a >= b) then 
     put(a);

  else
     put(b);

  end if;

  new_line;

end compare;