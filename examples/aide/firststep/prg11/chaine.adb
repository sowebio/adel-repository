-- chaine.adb

with ADA.TEXT_IO; use ADA.TEXT_IO;

procedure chaine is

taille : constant integer := 40;
ch : string(1..taille);

long : integer; --longueur de chaine

x : character;

begin
  
  put("Donnez la chaine : ");
  get_line (ch, long);   -- après l'appel, long contient le nombre de
                         -- caractères effectivement saisis 

  put("Voici votre chaine : ");
  put_line (ch(1..long));  -- uniquement les caractères utiles

  -- on inverse la chaine

  for i in 1 .. (1+long)/ 2 loop

      -- on echange les contenus des cases i et long+1-i
      x := ch(i);
      ch(i) := ch(long+1-i);
      ch(long+1-i):= x;

  end loop;

  put("Voici la chaine miroir : ");
  put_line (ch(1..long));  

end chaine;