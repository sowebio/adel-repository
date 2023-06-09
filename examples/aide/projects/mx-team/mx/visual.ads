-- +-------------------------------------------------------------+
-- |                                                             |
-- | Fichier:     visual.ads                                     |
-- | Paquets:     v04                                            |
-- | Description: programme de dessin en mode texte              |
-- | Rev:         1.10                                           |
-- | Date:        01-07-04                                       |
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
with V04.Crt; use V04;

package Visual is

   procedure Affichage;

   procedure Initialise;

   procedure Creation_Fichier;

   procedure Ouverture;

   procedure Ouvrir ( Nom        : in String;   -- Nom du fichier
                      Taille_Nom : in Integer   -- Taille du nom de fichier
                    );

   procedure Clean_Ecran ( Min_A : in Integer := 1;
                           Min_O : in Integer := 1;
                           Max_A : in Integer := 80;
                           Max_O : in Integer := 24
                         );

   procedure Traitement;

end;
