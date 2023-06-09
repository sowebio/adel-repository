-- +-------------------------------------------------------------+
-- |                                                             |
-- | Fichier:     visual.adb                                     |
-- | Paquets:     v04                                            |
-- | Description: programme de dessin en mode texte              |
-- | Rev:         1.10                                           |
-- | Date:        02-07-04                                       |
-- |                                                             |
-- +-------------------------------------------------------------+

---------------------------------------------------------------
--                                                           --
--                      Les couleurs                         --
-- BLACK    : F1                                             --
-- BLUE     : F2                                             --
-- GREEN    : F3                 C-F2 : LIGHT_BLUE           --
-- CYAN     : F4                 C-F3 : LIGHT_GREEN          --
-- RED      : F5                 C-F4 : LIGHT_CYAN           --
-- MAGENTA  : F6                 C-F5 : LIGHT_RED            --
-- BROWN    : F7                 C-F6 : LIGHT_MAGENTA        --
-- GRAY     : F8                                             --
-- YELLOW   : F9                                             --
-- WHITE    : F10                                            --
--                                                           --
---------------------------------------------------------------
-- Escape ou Alt-F4 pour quitter                             --
---------------------------------------------------------------
-- C-s : sauvegarde; C-o : ouverture; C-n : nouveau fichier; --
---------------------------------------------------------------

with Text_Io;             use Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with V04.Crt;             use V04;
with Ada.Exceptions;      use Ada.Exceptions;

package body Visual is


   Couleur : Crt.Color_Type;

   Tableau : array ( 1..25, 1..80 ) of Crt.Color_Type;  -- Tableau des valeurs
   X       : Integer := 1;      -- Abscisse
   Y       : Integer := 1;      -- Ordonnee




   --
   -- Sous-programme d'affichage selon la couleur
   --

   procedure Affichage is
   begin

      Crt.Put_Lc ( X, Y, " ", Couleur, Couleur );

      Tableau ( X, Y ) := Couleur; -- Telle case du tableau est de telle couleur (qui correspond a une variable)

   end Affichage;




   --
   -- Le fichier en memoire est rempli de noir et l'ecran est propre
   --

   procedure Initialise is
   begin

      for I in 1..25 loop
         for J in 1..80 loop

            Tableau ( I, J ) := Crt.Black;

         end loop;
      end loop;

      X := 1;
      Y := 1;
      Crt.Clear;
      Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
      Crt.Color_Set ( Crt.Black, Crt.Black );
      Couleur := Crt.Black;

--      Crt.Beep;
      Crt.Put_Lc ( 25, 3,  "-- Visual --", Crt.Yellow );
      Crt.Put_Lc ( 25, 40, "Noir          ", Crt.Light_Green );
      Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

   end Initialise;




   --
   -- La cretaion du fichier et l'ecriture a l'interieur
   --

   procedure Creation_Fichier is

      Fichier    : File_Type;       -- Fichier de sauvegarde
      Nom        : String(1..50);   -- Nom du fichier
      Taille_Nom : Integer;         -- Taille du nom de fichier

      Couleur_Valeur : Integer;

   begin

      -- Choix du nom du fichier (qui se terminera par ".vsl", avec vsl pour Visual )
      Crt.Put_Lc ( 2, 5, "Sous quel nom voulez vous sauvegarder votre dessin ? ", Crt.Light_Blue );

      Get_Line ( Nom, Taille_Nom );

      begin

         -- Creation du fichier
         Create ( File => Fichier, Mode => Out_File, Name => Nom(1..Taille_Nom) & ".vsl" );

      exception
         when Status_Error =>
            Open ( File => Fichier, Mode => Out_File, Name => Nom(1..Taille_Nom) & ".vsl" );
            Delete ( Fichier );
            Create ( File => Fichier, Mode => Out_File, Name => Nom(1..Taille_Nom) & ".vsl" );
            delay 1.0;

      end;

      -- Ecriture des couleurs par case
      for Colonne in 1..80 loop
         for Ligne in 1..24 loop

            case Tableau ( Ligne, Colonne ) is
               when Crt.Black         => Couleur_Valeur := 1;
               when Crt.Blue          => Couleur_Valeur := 2;
               when Crt.Green         => Couleur_Valeur := 3;
               when Crt.Cyan          => Couleur_Valeur := 4;
               when Crt.Red           => Couleur_Valeur := 5;
               when Crt.Magenta       => Couleur_Valeur := 6;
               when Crt.Brown         => Couleur_Valeur := 7;
               when Crt.Gray          => Couleur_Valeur := 8;
               when Crt.Yellow        => Couleur_Valeur := 9;
               when Crt.White         => Couleur_Valeur := 10;
               when Crt.Light_Blue    => Couleur_Valeur := 100;
               when Crt.Light_Green   => Couleur_Valeur := 200;
               when Crt.Light_Cyan    => Couleur_Valeur := 300;
               when Crt.Light_Red     => Couleur_Valeur := 400;
               when Crt.Light_Magenta => Couleur_Valeur := 500;
               when others => raise Data_Error;
            end case;

            Put ( Fichier, Integer'Image ( Couleur_Valeur ) );

         end loop;
      end loop;

      -- Fermeture du fichier
      Close ( Fichier );
      Crt.Put_Lc ( 4, 7, "Le fichier est cree.", Crt.Light_Blue );
      delay 1.0;

   end Creation_Fichier;




   --
   -- Ouvre puis lit un fichier
   --

   procedure Ouverture is

      Fichier    : File_Type;       -- Fichier a ouvrir
      Nom        : String(1..50);   -- Nom du fichier
      Taille_Nom : Integer;         -- Taille du nom de fichier

      Couleur_Valeur : Integer;

   begin

      -- Choix du nom du fichier à ouvir (qui se termine normalement par ".vsl")
      Crt.Put_Lc ( 2, 5, "Quel fichier voulez vous ouvrir ? ", Crt.Light_Green );

      Get_Line ( Nom, Taille_Nom );

      begin

      -- Ouverture du fichier
      Open ( File => Fichier, Mode => In_File, Name => Nom(1..Taille_Nom) & ".vsl" );

      exception
         when Name_Error =>
            Crt.Put_Lc ( 4, 7, "Le fichier n'existe pas !", Crt.Light_Red );
            delay 1.0;
            return;

      end;

      Crt.Put_Lc ( 4, 7, "Le fichier est ouvert.", Crt.Light_Green );
      delay 1.0;

      -- Ecriture des couleurs par case
      for Colonne in 1..80 loop
         for Ligne in 1..24 loop
            if    ( End_Of_File(Fichier) ) and then Colonne < 80 and then Ligne < 24 then
               Close ( Fichier );
               Crt.Put_Lc ( 4, 7, "Erreur dans la lecture du fichier !", Crt.Light_Red );
               delay 1.0;
               return;

            elsif ( End_Of_Page(Fichier) ) then
               Skip_Page (Fichier);

            elsif ( End_Of_Line(Fichier) ) then
               Skip_Line (Fichier);

            else
               begin

                  Get ( Fichier, Couleur_Valeur );

               exception
                  when others =>
                     Crt.Put_Lc ( 4, 7, "Erreur dans la lecture du fichier !", Crt.Light_Red );
                     delay 1.0;
                     Close ( Fichier );
                     return;

               end;
            end if;

            case Couleur_Valeur is
               when 1   => Couleur := Crt.Black;
               when 2   => Couleur := Crt.Blue;
               when 3   => Couleur := Crt.Green;
               when 4   => Couleur := Crt.Cyan;
               when 5   => Couleur := Crt.Red;
               when 6   => Couleur := Crt.Magenta;
               when 7   => Couleur := Crt.Brown;
               when 8   => Couleur := Crt.Gray;
               when 9   => Couleur := Crt.Yellow;
               when 10  => Couleur := Crt.White;
               when 100 => Couleur := Crt.Light_Blue;
               when 200 => Couleur := Crt.Light_Green;
               when 300 => Couleur := Crt.Light_Cyan;
               when 400 => Couleur := Crt.Light_Red;
               when 500 => Couleur := Crt.Light_Magenta;
               when others =>
                  Crt.Put_Lc ( 4, 7, "Erreur dans la lecture du fichier !", Crt.Light_Red );
                  delay 1.0;
                  Close ( Fichier );
                  return;
            end case;

            Tableau ( Ligne, Colonne ) := Couleur;

         end loop;
      end loop;


      Close ( Fichier );

   end Ouverture;





   --
   -- Ouvre puis lit un fichier
   --

   procedure Ouvrir ( Nom        : in String;   -- Nom du fichier
                      Taille_Nom : in Integer   -- Taille du nom de fichier
                    ) is

      Fichier    : File_Type;       -- Fichier a ouvrir

      Couleur_Valeur : Integer;

   begin

      begin

         -- Ouverture du fichier
         Open ( File => Fichier, Mode => In_File, Name => Nom(1..Taille_Nom) & ".vsl" );

      exception
         when Name_Error =>
            raise Name_Error;

      end;

      -- Lecture des couleurs par case
      for Colonne in 1..80 loop
         for Ligne in 1..24 loop
            if    ( End_Of_File(Fichier) ) then
               Close (Fichier);
               raise End_Error;

            elsif ( End_Of_Page(Fichier) ) then
               Skip_Page (Fichier);

            elsif ( End_Of_Line(Fichier) ) then
               Skip_Line (Fichier);

            else
               begin

                  Get ( Fichier, Couleur_Valeur );

               exception
                  when Data_Error =>
                     Close ( Fichier );
                     raise Data_Error;

               end;
            end if;

            case Couleur_Valeur is
               when 1   => Couleur := Crt.Black;
               when 2   => Couleur := Crt.Blue;
               when 3   => Couleur := Crt.Green;
               when 4   => Couleur := Crt.Cyan;
               when 5   => Couleur := Crt.Red;
               when 6   => Couleur := Crt.Magenta;
               when 7   => Couleur := Crt.Brown;
               when 8   => Couleur := Crt.Gray;
               when 9   => Couleur := Crt.Yellow;
               when 10  => Couleur := Crt.White;
               when 100 => Couleur := Crt.Light_Blue;
               when 200 => Couleur := Crt.Light_Green;
               when 300 => Couleur := Crt.Light_Cyan;
               when 400 => Couleur := Crt.Light_Red;
               when 500 => Couleur := Crt.Light_Magenta;
               when others =>
                  Close ( Fichier );
                  raise Data_Error;
            end case;

            Tableau ( Ligne, Colonne ) := Couleur;

         end loop;
      end loop;

      Close ( Fichier );

   end Ouvrir;





   --
   -- Reecrit le dessin sur l'ecran (apres un affichage de demande de nom pour une ouverture, par exemple)
   --

   procedure Clean_Ecran ( Min_A : in Integer := 1;
                           Min_O : in Integer := 1;
                           Max_A : in Integer := 80;
                           Max_O : in Integer := 24
                         ) is

      Curseur_X      : Integer := 1;        -- Abcisse apres un clean
      Curseur_Y      : Integer := 1;        -- Ordonee apres un clean

   begin

      if Min_A > Max_A or else Min_O > Max_O then
         raise Constraint_Error;
      end if;

      Curseur_X := X;
      Curseur_Y := Y;

      for I in Min_O..Max_O loop
         for J in Min_A..Max_A loop

            X := I;
            Y := J;
            Couleur := Tableau ( X, Y );
            Affichage;

         end loop;
      end loop;

      X := Curseur_X;
      Y := Curseur_Y;

   end Clean_Ecran;




   Touche : Integer;       -- Touche de saisie

   procedure Traitement is

   begin

      -- Rendre l'ecran vierge
      Initialise;

      -- Saisie des touches et et reaction a celles-ci
      loop

         Touche := Crt.Key_Read;

         if    Touche = Crt.K_Up and then X > 1 then
            Affichage;
            X := X - 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Down and then X < 24 then
            Affichage;
            X := X + 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Left and then Y > 1 then
            Affichage;
            Y := Y - 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Right and then Y < 80 then
            Affichage;
            Y := Y + 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_Up and then X > 1 then
            Crt.Put_Lc ( X, Y, " ", Tableau ( X, Y ),  Tableau ( X, Y ) );
            X := X - 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_Down and then X < 24 then
            Crt.Put_Lc ( X, Y, " ", Tableau ( X, Y ),  Tableau ( X, Y ) );
            X := X + 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_Left and Y > 1 then
            Crt.Put_Lc ( X, Y, " ", Tableau ( X, Y ),  Tableau ( X, Y ) );
            Y := Y - 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_Right and Y < 80 then
            Crt.Put_Lc ( X, Y, " ", Tableau ( X, Y ),  Tableau ( X, Y ) );
            Y := Y + 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Pageup then
            Crt.Put_Lc ( X, Y, " ", Tableau ( X, Y ),  Tableau ( X, Y ) );
            X := 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Pagedown then
            Crt.Put_Lc ( X, Y, " ", Tableau ( X, Y ),  Tableau ( X, Y ) );
            X := 24;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Home then
            Crt.Put_Lc ( X, Y, " ", Tableau ( X, Y ),  Tableau ( X, Y ) );
            Y := 1;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_End then
            Crt.Put_Lc ( X, Y, " ", Tableau ( X, Y ),  Tableau ( X, Y ) );
            Y := 80;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_F1 then
            Couleur := Crt.Black;
            Crt.Put_Lc ( 25, 40, "Noir         ", Crt.Light_Green );

         elsif Touche = Crt.K_F2 then
            Couleur := Crt.Blue;
            Crt.Put_Lc ( 25, 40, "Bleu         ", Crt.Light_Green );

         elsif Touche = Crt.K_F3 then
            Couleur := Crt.Green;
            Crt.Put_Lc ( 25, 40, "Vert         ", Crt.Light_Green );

         elsif Touche = Crt.K_F4 then
            Couleur := Crt.Cyan;
            Crt.Put_Lc ( 25, 40, "Cyan         ", Crt.Light_Green );

         elsif Touche = Crt.K_F5 then
            Couleur := Crt.Red;
            Crt.Put_Lc ( 25, 40, "Rouge        ", Crt.Light_Green );

         elsif Touche = Crt.K_F6 then
            Couleur := Crt.Magenta;
            Crt.Put_Lc ( 25, 40, "Magenta      ", Crt.Light_Green );

         elsif Touche = Crt.K_F7 then
            Couleur := Crt.Brown;
            Crt.Put_Lc ( 25, 40, "Brun         ", Crt.Light_Green );

         elsif Touche = Crt.K_F8 then
            Couleur := Crt.Gray;
            Crt.Put_Lc ( 25, 40, "Gris         ", Crt.Light_Green );

         elsif Touche = Crt.K_F9 then
            Couleur := Crt.Yellow;
            Crt.Put_Lc ( 25, 40, "Jaune        ", Crt.Light_Green );

         elsif Touche = Crt.K_F10 then
            Couleur := Crt.White;
            Crt.Put_Lc ( 25, 40, "Blanc        ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_F2 then
            Couleur := Crt.Light_Blue;
            Crt.Put_Lc ( 25, 40, "Bleu clair   ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_F3 then
            Couleur := Crt.Light_Green;
            Crt.Put_Lc ( 25, 40, "Vert clair   ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_F4 then
            Couleur := Crt.Light_Cyan;
            Crt.Put_Lc ( 25, 40, "Cyan clair   ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_F5 then
            Couleur := Crt.Light_Red;
            Crt.Put_Lc ( 25, 40, "Rouge clair  ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_F6 then
            Couleur := Crt.Light_Magenta;
            Crt.Put_Lc ( 25, 40, "Magenta clair", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_S then
            Creation_Fichier;
            Clean_Ecran;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_O then
            Ouverture;
            Clean_Ecran;
            Crt.Put_Lc ( X, Y, " ", Crt.White, Crt.White );
            Crt.Put_Lc ( 25, 30, Integer'Image(X) & Integer'Image(Y) & "  ", Crt.Light_Green );

         elsif Touche = Crt.K_Ctrl_N or else Touche = Crt.K_Delete then
            Initialise;

         elsif Touche = Crt.K_Escape or else Touche = Crt.K_Alt_F4 then
            exit;

         end if;

      end loop;

      -- Details de fin

      Crt.Clear;
--      Crt.Beep;
      delay 0.25;
--      Crt.Beep;

   end Traitement;

end;
