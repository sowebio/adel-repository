with Ada.Numerics.Discrete_Random;
with Ada.Exceptions;             use Ada.Exceptions;
with gnat.directory_operations;  use gnat.directory_operations;
with Ada.Text_Io;                use Ada.Text_Io;
with Ada.Integer_Text_Io;        use Ada.Integer_Text_Io;
with Ada.Float_Text_Io;          use Ada.Float_Text_Io;
with Ada.Calendar;               use Ada.Calendar;
with Gnat.Calendar;              use Gnat.Calendar;
with V04.Crt;                    use V04;
with Visual;

procedure MX is

   Long     : Integer;
   Nom      : String(1..20);
   Boutton  : String (1..4) := "null";

   Quit     : String (1..15) := "quitter => null";
   Quitter  : Boolean := False;
   L        : Integer := 19;

procedure Calculatrice is

   A, C, D : Float;
   B       : Character;

   procedure Afficher_Cal is

      procedure Affiche_Boitier is

         Y1       : Integer := 3;
         Compteur : Integer := 1;

      begin

         loop

            Crt.Put_Lc(Y1,9,"                 ",Crt.Gray, Crt.Gray);

            Y1 := Y1 + 1;

            Compteur := Compteur + 1;

            if Compteur >= 14 then
               exit;
            end if;

         end loop;

      end Affiche_Boitier;


      procedure Affiche_Ecran is

      begin

         Crt.Put_Lc(4,11,"             ",Crt.Light_Green, Crt.black);

      end Affiche_Ecran;


      procedure Affiche_Touches is

         X1 : Integer := 10;
         Y2 : Integer := 6;
         Compteur : Integer := 1;

      begin

         loop

            Crt.Put_Lc(Y2,X1,"   ",Crt.Light_Blue, Crt.Light_Blue);

            Y2 := Y2 + 2;

            Compteur := Compteur + 1;

            if Compteur = 6 then
               X1 := X1 + 4;
               Y2 := 6;
            elsif Compteur = 11 then
               X1 := X1 + 4;
               Y2 := 6;
            elsif Compteur = 16 then
               X1 := X1 + 4;
               Y2 := 6;
            elsif Compteur = 21 then
               X1 := X1 + 5;
               Y2 := 6;
               exit;
            else
               null;
            end if;

         end loop;

      end Affiche_Touches;


      procedure Affiche_Symboles is

      begin

         Crt.Put_Lc(6,23,"C",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(8,11,"7",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(8,15,"8",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(8,19,"9",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(8,23,"/",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(10,11,"4",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(10,15,"5",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(10,19,"6",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(10,23,"X",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(12,11,"1",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(12,15,"2",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(12,19,"3",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(12,23,"-",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(14,11,"0",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(14,15,".",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(14,19,"=",Crt.Yellow,Crt.Light_blue);
         Crt.Put_Lc(14,23,"+",Crt.Yellow,Crt.Light_blue);

      end Affiche_Symboles;

   begin

      Affiche_Boitier;
      Affiche_Ecran;
      Affiche_Touches;
      Affiche_Symboles;

   end Afficher_Cal;


   procedure calcul is

      Key : Integer;

   begin

      Get (A);
      Get (B);
      Get (C);

      if    B = '+' then
         D := A + C;

      elsif B = '-' then
         D := A - C;

      elsif B = '*' then
         D := A * C;

      elsif B = '/' then
         D := A / C;

      end if;

      Afficher_Cal;

      Crt.Cursor_Move(4,11);

      Put(D, Exp => 0);

      loop

         Key := Crt.Key_Read;

         if Character'Val (Key) = 'Q'
           or Character'Val (Key) = 'q'
           or Key = Crt.K_escape then

            Quitter := True;
            exit;
         else
            exit;
         end if;

      end loop;


   end calcul;


begin

   loop


      Afficher_Cal;
      Crt.Cursor_Move(4,11);
      Calcul;

      exit when Quitter;

   end loop;

   Crt.Clear;
   Quitter := False;

end Calculatrice;



   procedure lister is

      X       : Integer := 1;
      Y       : Integer := 1;

      Couleur : Integer;

      Fichier : File_Type;

      Error   : Integer;

   begin

      begin

         if Nom (1..Long) = "ecran" then
            Open ( File => Fichier, Mode => In_File, Name => "fond_ecran.vsl" );
         elsif Nom (1..Long) = "barre" then
            Open ( File => Fichier, Mode => In_File, Name => "barre.vsl" );
         elsif Nom (1..Long) = "menu_general" then
            Open ( File => Fichier, Mode => In_File, Name => "menu_general.vsl" );
         elsif Nom (1..Long) = "correctif" then
            Open ( File => Fichier, Mode => In_File, Name => "correctif.vsl" );
         end if;

      exception

         when Name_Error => Crt.Put_Lc (4,7, "ERREUR! FICHIER INEXISTANT!!!", Crt.Light_Red, Crt.Light_Blue);
            delay 1.0;
            Error := 1;

      end;

      if Error /= 1 then

         for Y in 1..80 loop

            for X in 1..24 loop

               if ( End_Of_Page(Fichier) ) then
                  Skip_Page(Fichier);
               elsif ( End_Of_Line(Fichier) ) then
                  Skip_Line(Fichier);
               else
                  Get ( Fichier, Couleur );
               end if;

               if    Couleur = 001 then
                  null;
               elsif Couleur = 002 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Blue, Crt.Blue );
               elsif Couleur = 003 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Green, Crt.Green );
               elsif Couleur = 004 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Cyan, Crt.Cyan );
               elsif Couleur = 005 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Red, Crt.Red );
               elsif Couleur = 006 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Magenta, Crt.Magenta );
               elsif Couleur = 007 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Brown, Crt.Brown );
               elsif Couleur = 008 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Gray, Crt.Gray );
               elsif Couleur = 009 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Yellow, Crt.Yellow );
               elsif Couleur = 010 then
                  Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
               elsif Couleur = 100 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Light_Blue, Crt.Light_Blue );
               elsif Couleur = 200 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Light_Green, Crt.Light_Green );
               elsif Couleur = 300 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Light_Cyan, Crt.Light_Cyan );
               elsif Couleur = 400 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Light_Red, Crt.Light_Red );
               elsif Couleur = 500 then
                  Crt.Put_Lc ( X, Y, " ", Crt.Light_Magenta, Crt.Light_Magenta );
               end if;

            end loop;

         end loop;

      end if;

      Close ( Fichier );

   end lister;

   procedure Afficher_Fond_ecran is

   begin

      Long := 5;
      Nom (1..Long) := "ecran";
      Lister;

   end Afficher_Fond_ecran;

   procedure Afficher_Barre is

   begin

      Long := 5;
      Nom (1..Long) := "barre";
      Lister;

      Crt.Put_Lc(22,4,"" & Character'Val(218), Crt.White, Crt.Gray);
      Crt.Put_Lc(22,5,"" & Character'Val(196), Crt.White, Crt.Gray);
      Crt.Put_Lc(22,5,"" & Character'Val(196), Crt.White, Crt.Gray);
      Crt.Put_Lc(22,6,"" & Character'Val(196), Crt.White, Crt.Gray);
      Crt.Put_Lc(22,7,"" & Character'Val(191), Crt.White, Crt.Gray);
      Crt.Put_Lc(23,4,"" & Character'Val(179), Crt.White, Crt.Gray);
      Crt.Put_Lc(23,7,"" & Character'Val(179), Crt.red, Crt.Gray);
      Crt.Put_Lc(24,4,"" & Character'Val(192), Crt.red, Crt.Gray);
      Crt.Put_Lc(24,5,"" & Character'Val(196), Crt.red, Crt.Gray);
      Crt.Put_Lc(24,6,"" & Character'Val(196), Crt.red, Crt.Gray);
      Crt.Put_Lc(24,7,"" & Character'Val(217), Crt.red, Crt.Gray);

      Crt.Put_Lc(23,5,"MX", Crt.Light_Blue, Crt.Gray);

      Crt.Put_Lc(23,1,"" & Character'Val (16), Crt.Light_blue, Crt.Light_red);
      Crt.Put_Lc(23,80,"" & Character'Val (17), Crt.Light_blue, Crt.Light_red);

      --heure

      Crt.Put_Lc(22,67,"" & character'Val(218), Crt.White, Crt.Gray);
      Crt.Put_Lc(24,67,"" & character'Val(192), Crt.White, Crt.Gray);
      Crt.Put_Lc(22,78,"" & character'Val(191), Crt.white, Crt.Gray);
      Crt.Put_Lc(24,78,"" & character'Val(217), Crt.red, Crt.gray);

      for I in 68..77 loop

         Crt.Put_Lc(22,I,"" & character'Val(196), Crt.White, Crt.Gray);
         Crt.Put_Lc(24,I,"" & character'Val (196), Crt.red, Crt.gray);

      end loop;

      Crt.Put_Lc(23,67,"" & character'Val(179), Crt.White, Crt.Gray);
      Crt.Put_Lc(23,78,"" & character'Val(179), Crt.red, Crt.gray);

   end Afficher_Barre;

   procedure Afficher_heure is

      Temps   : Ada.Calendar.Time;

      Minute     : Gnat.Calendar.Minute_Number;
      Seconde    : Gnat.Calendar.Second_Number;
      Heure      : Gnat.Calendar.Hour_Number;
      Annee      : Ada.Calendar.Year_Number;
      Mois       : Ada.Calendar.Month_Number;
      Jour       : Ada.Calendar.Day_Number;
      Seconde_R  : Gnat.Calendar.Second_Duration;

   begin

      Temps := Ada.Calendar.Clock;
      Gnat.Calendar.Split ( Temps, Annee, Mois, Jour, Heure, Minute, Seconde, Seconde_R );

      Crt.Cursor_Move (23,68);
      Crt.Put(Integer'image(heure), Crt.Light_Blue, Crt.gray);
      Crt.Put(" :", Crt.Light_Blue, Crt.gray);
      Crt.Put(Integer'image(minute), Crt.Light_Blue, Crt.gray);

   end Afficher_heure;

   procedure Afficher_Correctif is

   begin

      Long := 9;
      Nom (1..Long) := "correctif";
      Lister;

   end Afficher_Correctif;

   procedure Afficher_Boutton is

   begin

      if boutton = "null" then

         Crt.Put_Lc(22,4,"" & Character'Val(218), Crt.White, Crt.Gray);
         Crt.Put_Lc(22,5,"" & Character'Val(196), Crt.White, Crt.Gray);
         Crt.Put_Lc(22,5,"" & Character'Val(196), Crt.White, Crt.Gray);
         Crt.Put_Lc(22,6,"" & Character'Val(196), Crt.White, Crt.Gray);
         Crt.Put_Lc(22,7,"" & Character'Val(191), Crt.White, Crt.Gray);
         Crt.Put_Lc(23,4,"" & Character'Val(179), Crt.White, Crt.Gray);
         Crt.Put_Lc(23,7,"" & Character'Val(179), Crt.red, Crt.Gray);
         Crt.Put_Lc(24,4,"" & Character'Val(192), Crt.red, Crt.Gray);
         Crt.Put_Lc(24,5,"" & Character'Val(196), Crt.red, Crt.Gray);
         Crt.Put_Lc(24,6,"" & Character'Val(196), Crt.red, Crt.Gray);
         Crt.Put_Lc(24,7,"" & Character'Val(217), Crt.red, Crt.Gray);

      elsif boutton = "push" then

         Crt.Put_Lc(22,4,"" & Character'Val(218), Crt.red, Crt.Gray);
         Crt.Put_Lc(22,5,"" & Character'Val(196), Crt.red, Crt.Gray);
         Crt.Put_Lc(22,5,"" & Character'Val(196), Crt.Red, Crt.Gray);
         Crt.Put_Lc(22,6,"" & Character'Val(196), Crt.Red, Crt.Gray);
         Crt.Put_Lc(22,7,"" & Character'Val(191), Crt.Red, Crt.Gray);
         Crt.Put_Lc(23,4,"" & Character'Val(179), Crt.Red, Crt.Gray);
         Crt.Put_Lc(23,7,"" & Character'Val(179), Crt.white, Crt.Gray);
         Crt.Put_Lc(24,4,"" & Character'Val(192), Crt.white, Crt.Gray);
         Crt.Put_Lc(24,5,"" & Character'Val(196), Crt.white, Crt.Gray);
         Crt.Put_Lc(24,6,"" & Character'Val(196), Crt.white, Crt.Gray);
         Crt.Put_Lc(24,7,"" & Character'Val(217), Crt.white, Crt.Gray);

      end if;

   end Afficher_Boutton;

   procedure Message_Fermeture_Mx is

      Key   : Integer;
      Choix : Integer := 2;

   begin

      Crt.Put_Lc(8,30, "                                  ", Crt.Yellow, Crt.Gray);
      Crt.Put_Lc(9,30, "  Voulez-vous vraiment quitter ?  ", Crt.Light_blue, Crt.Gray);
      Crt.Put_Lc(10,30,"     OUI               NON        ", Crt.yellow, Crt.Gray);
      Crt.Put_Lc(11,30,"                                  ", Crt.Yellow, Crt.Gray);
      Crt.Put_Lc(12,31,"                                  ", Crt.Black, Crt.red);

      Crt.Put_Lc(9,64, " ", Crt.Yellow, Crt.red);
      Crt.Put_Lc(10,64," ", Crt.yellow, Crt.red);
      Crt.Put_Lc(11,64," ", Crt.yellow, Crt.red);
      Crt.Put_Lc(12,64," ", Crt.Yellow, Crt.red);

      Crt.Put_Lc(10,51,"> NON <", Crt.yellow, Crt.Light_Blue);

      loop

         Key := Crt.Key_Read;

         if Key = Crt.K_Right then

            Crt.Put_Lc(10,33,"> OUI <", Crt.gray, crt.gray);
            Crt.Put_Lc(10,33,"  OUI  ", Crt.yellow, crt.gray);
            Crt.Put_Lc(10,51,"> NON <", Crt.yellow, Crt.Light_Blue);
            Choix := 2;

         elsif Key = Crt.K_Left then

            Crt.Put_Lc(10,51,"> NON <", Crt.gray, Crt.gray);
            Crt.Put_Lc(10,51,"  NON  ", Crt.yellow, crt.gray);
            Crt.Put_Lc(10,33,"> OUI <", Crt.yellow, Crt.Light_Blue);
            Choix := 1;

         elsif Key = Crt.K_Escape then

            exit;

         elsif Key = Crt.K_Return then

            if Choix = 1 then
               Quit := "quitter => tout";
            end if;
            exit;

         end if;

      end loop;

      Crt.Put_Lc(8,30, "                                  ", Crt.Yellow, Crt.black);
      Crt.Put_Lc(9,30, "                                  ", Crt.yellow, Crt.black);
      Crt.Put_Lc(10,30,"                                  ", Crt.yellow, Crt.black);
      Crt.Put_Lc(11,30,"                                  ", Crt.Yellow, Crt.black);
      Crt.Put_Lc(12,31,"                                  ", Crt.Black, Crt.black);

      Crt.Put_Lc(9,64, " ", Crt.Yellow, Crt.black);
      Crt.Put_Lc(10,64," ", Crt.yellow, Crt.black);
      Crt.Put_Lc(11,64," ", Crt.yellow, Crt.black);
      Crt.Put_Lc(12,64," ", Crt.Yellow, Crt.black);

      Afficher_Fond_Ecran;

   end Message_Fermeture_Mx;

   procedure Fermeture_Menu is

   begin

      Boutton := "null";
      Afficher_Boutton;

      for Clear in 10..21 loop

         Crt.Put_Lc(Clear,4, "                                    ", Crt.Black, Crt.Black);

      end loop;

      Afficher_Fond_ecran;

   end Fermeture_Menu;

   procedure Liste_jeux is

      Key : Integer;

   begin

      Crt.Put_Lc(17,21," Xspace       ", Crt.Yellow, Crt.Blue);

      loop

         exit when Quit = "quitter => menu";
         exit when Quit = "quitter => tout";

         Crt.Put_Lc(17,21," Xspace       ", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(18,21," Snake        ", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(19,21," Xetris       ", Crt.Yellow, Crt.gray);

         if L = 17 then
            Crt.Put_Lc(17,21," Xspace       ", Crt.Yellow, Crt.Blue);
         elsif L = 18 then
            Crt.Put_Lc(18,21," Snake        ", Crt.Yellow, Crt.blue);
         elsif L = 19 then
            Crt.Put_Lc(19,21," Xetris       ", Crt.Yellow, Crt.blue);
         end if;

         Key := Crt.Key_Read;

         if Key = Crt.K_Up and L /= 17 then
            L := L - 1;
         elsif Key = Crt.K_Down and L /= 19 then
            L := L + 1;
         elsif Key = Crt.K_Pageup then
            L := 17;
         elsif Key = Crt.K_Pagedown then
            L := 19;
         elsif Key = Crt.K_Return then

            if L = 17 then
               null;
            elsif L = 18 then
               null;
            elsif L = 19 then
               null;
            end if;

         elsif Key = Crt.K_Escape or Key = Crt.K_Left then

            Crt.Put_Lc(17,21,"               ", Crt.Yellow, Crt.black);
            Crt.Put_Lc(18,21,"               ", Crt.Yellow, Crt.black);
            Crt.Put_Lc(19,21,"               ", Crt.Yellow, Crt.black);
            Afficher_Correctif;

            exit;

         end if;

      end loop;

      L := 17;

   end Liste_jeux;

   procedure Liste_editeurs is

      Key : Integer;

   begin

      Crt.Put_Lc(16,21," Medit        ", Crt.Yellow, Crt.blue);

      loop

         exit when Quit = "quitter => menu";
         exit when Quit = "quitter => tout";

         Crt.Put_Lc(16,21," Medit        ", Crt.Yellow, Crt.gray);

         if L = 16 then
            Crt.Put_Lc(16,21," Medit        ", Crt.Yellow, Crt.blue);
         end if;

         Key := Crt.Key_Read;

         if Key = Crt.K_Up and L /= 16 then
            L := L - 1;
         elsif Key = Crt.K_Down and L /= 16 then
            L := L + 1;
         elsif Key = Crt.K_Pageup then
            L := 16;
         elsif Key = Crt.K_Pagedown then
            L := 16;
         elsif Key = Crt.K_Return then

            if L = 16 then
               --              Medit;
               --             Fermeture_Menu;
               --             Quit := "quitter => menu";
               --             exit;
               null;
            end if;

         elsif Key = Crt.K_Escape or Key = Crt.K_Left then

            Crt.Put_Lc(16,21,"               ", Crt.Yellow, Crt.black);
            Afficher_Correctif;

            exit;

         end if;

      end loop;

      L := 16;

   end Liste_editeurs;

   procedure Liste_dessin is

      Key : Integer;

   begin

      Crt.Put_Lc(15,21," Visual       ", Crt.Yellow, Crt.blue);

      loop

         exit when Quit = "quitter => menu";
         exit when Quit = "quitter => tout";

         Crt.Put_Lc(15,21," Visual       ", Crt.Yellow, Crt.gray);

         if L = 15 then
            Crt.Put_Lc(15,21," Visual       ", Crt.Yellow, Crt.blue);
         end if;

         Key := Crt.Key_Read;

         if Key = Crt.K_Up and L /= 15 then
            L := L - 1;
         elsif Key = Crt.K_Down and L /= 15 then
            L := L + 1;
         elsif Key = Crt.K_Pageup then
            L := 15;
         elsif Key = Crt.K_Pagedown then
            L := 15;
         elsif Key = Crt.K_Return then

            if L = 15 then
               Visual.traitement;
               Fermeture_Menu;
               Quit := "quitter => menu";
               exit;
            end if;

         elsif Key = Crt.K_Escape or Key = Crt.K_Left then

            Crt.Put_Lc(15,21,"               ", Crt.Yellow, Crt.black);
            Afficher_Correctif;

            exit;

         end if;

      end loop;

      L := 15;

   end Liste_dessin;

   procedure Liste_accessoires is

      Key : Integer;

   begin

      Crt.Put_Lc(14,21," Calculatrice ", Crt.Yellow, Crt.blue);

      loop

         exit when Quit = "quitter => menu";
         exit when Quit = "quitter => tout";

         Crt.Put_Lc(14,21," Calculatrice ", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(15,21," Xphone       ", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(16,21," Password     ", Crt.Yellow, Crt.gray);

         if L = 14 then
            Crt.Put_Lc(14,21," Calculatrice ", Crt.Yellow, Crt.blue);
         elsif L = 15 then
            Crt.Put_Lc(15,21," Xphone       ", Crt.Yellow, Crt.blue);
         elsif L = 16 then
            Crt.Put_Lc(16,21," Password     ", Crt.Yellow, Crt.blue);
         end if;

         Key := Crt.Key_Read;

         if Key = Crt.K_Up and L /= 14 then
            L := L - 1;
         elsif Key = Crt.K_Down and L /= 16 then
            L := L + 1;
         elsif Key = Crt.K_Pageup then
            L := 14;
         elsif Key = Crt.K_Pagedown then
            L := 16;
         elsif Key = Crt.K_Return then

            if L = 14 then
               Fermeture_Menu;
               Quit := "quitter => menu";
               Quitter := False;
               Calculatrice;
               Quitter := False;
               exit;
            elsif L = 15 then
               null;
            elsif L = 16 then
               null;
            end if;

         elsif Key = Crt.K_Escape or Key = Crt.K_Left then

            Crt.Put_Lc(14,21,"              ", Crt.Yellow, Crt.black);
            Crt.Put_Lc(15,21,"              ", Crt.Yellow, Crt.black);
            Crt.Put_Lc(16,21,"              ", Crt.Yellow, Crt.black);
            Afficher_Correctif;

            exit;

         end if;

      end loop;

      L := 14;

   end Liste_accessoires;

   procedure Liste_Generale is

      Key : Integer;

   begin

      Crt.Put_Lc(19,7," Quitter      ", Crt.Yellow, Crt.blue);

      loop

         exit when Quit = "quitter => menu";
         exit when Quit = "quitter => tout";

         Crt.Put_Lc(14,7," Accessoires >", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(15,7," Dessin      >", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(16,7," Editeurs    >", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(17,7," Jeux        >", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(18,7," MC           ", Crt.Yellow, Crt.gray);
         Crt.Put_Lc(19,7," Quitter      ", Crt.Yellow, Crt.gray);

         if L = 14 then
            Crt.Put_Lc(14,7," Accessoires >", Crt.Yellow, Crt.blue);
         elsif L = 15 then
            Crt.Put_Lc(15,7," Dessin      >", Crt.Yellow, Crt.blue);
         elsif L = 16 then
            Crt.Put_Lc(16,7," Editeurs    >", Crt.Yellow, Crt.blue);
         elsif L = 17 then
            Crt.Put_Lc(17,7," Jeux        >", Crt.Yellow, Crt.blue);
         elsif L = 18 then
            Crt.Put_Lc(18,7," MC           ", Crt.Yellow, Crt.blue);
         elsif L = 19 then
            Crt.Put_Lc(19,7," Quitter      ", Crt.Yellow, Crt.blue);
         end if;

         Key := Crt.Key_Read;

         if Key = Crt.K_Up and L /= 14 then
            L := L - 1;
         elsif Key = Crt.K_Down and L /= 19 then
            L := L + 1;
         elsif Key = Crt.K_Pageup then
            L := 14;
         elsif Key = Crt.K_Pagedown then
            L := 19;
         elsif Key = Crt.K_Return then

            if L = 14 then
               Liste_Accessoires;
            elsif L = 15 then
               Liste_Dessin;
            elsif L = 16 then
               Liste_Editeurs;
            elsif L = 17 then
               Liste_Jeux;
            elsif L = 18 then
               null;
            elsif L = 19 then
               Fermeture_Menu;
               Message_Fermeture_Mx;
               exit;
            end if;

         elsif Key = Crt.K_Right then

            if L = 14 then
               Liste_Accessoires;
            elsif L = 15 then
               Liste_Dessin;
            elsif L = 16 then
               Liste_Editeurs;
            elsif L = 17 then
               Liste_Jeux;
            end if;

         elsif Key = Crt.K_Escape then

            Fermeture_Menu;
            exit;

         end if;

      end loop;

      L := 19;

   end Liste_Generale;

   procedure Ouverture_Menu is

   begin

      Boutton := "push";
      Afficher_Boutton;

      Long := 12;
      Nom (1..Long) := "menu_general";
      Lister;

      Liste_Generale;

   end Ouverture_Menu;

   procedure Attente is

      Key : Integer;

   begin

      loop

         Afficher_Heure;

         exit when Quit = "quitter => menu";
         exit when Quit = "quitter => tout";

         Key := Crt.Key_Read;

         if Key = Crt.K_Return then

            Ouverture_Menu;

         elsif Key = Crt.K_Escape then

            Message_Fermeture_Mx;
            exit when Quit = "quitter => tout";

         end if;

      end loop;

   end Attente;

begin

   Crt.Clear;
   Afficher_Fond_Ecran;
   Afficher_Barre;
   Afficher_Heure;

   loop

      Crt.Clear;
      Afficher_Fond_Ecran;
      Afficher_Barre;
      Afficher_Heure;

      Quit := "quitter => null";
      Attente;
      exit when Quit = "quitter => tout";

   end loop;


end MX;
