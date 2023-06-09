-- car_tri.adb

with ADA.TEXT_IO, ADA.INTEGER_TEXT_IO;
use ADA.TEXT_IO, ADA.INTEGER_TEXT_IO;

procedure car_tri is

c : character;

begin

 loop

    put ("Entrez un caractere (* pour finir) : ");
    get (c);

    exit when c='*';

    put ("Ce caractere est " );

    case c is
         when 'a'..'z'|'A'..'Z'        => 
              put_line ("une lettre");

         when '1'..'9'                 => 
              put_line ("un chiffre");

         when '.'|';'|','|'?'|'!'|':'  => 
              put_line ("un carac. de ponctuation");

         when others                   => 
              put_line ("un carac. non classe");

    end case;

    new_line;   

  end loop;

end car_tri;