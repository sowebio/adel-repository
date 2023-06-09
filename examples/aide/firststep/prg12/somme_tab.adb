
-- somme_tab.adb

with ADA.TEXT_IO; use ADA.TEXT_IO;
with ADA.INTEGER_TEXT_IO; use ADA.INTEGER_TEXT_IO;

procedure somme_tab is

-- declaration des variables
tab : array (1..5) of integer := (10,20,30,40,50);
S : integer := 0;

begin
  for i in tab'range loop
      S := S + tab(i);
  end loop;

  put (" La somme des elements du tableau est : ");
  put (S);
  new_line;  

end somme_tab;