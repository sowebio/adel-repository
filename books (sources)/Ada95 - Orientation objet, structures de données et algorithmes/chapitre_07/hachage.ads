--          Copyright © 1998 Philippe J. Gabrini
PACKAGE Hachage IS
--******************************************
-- Mettez votre documentation ici.
--******************************************
 
 
NoeudsMax: CONSTANT Natural := 8191;
LongueurIdent: CONSTANT Natural := 8;
TYPE Modulaire IS MOD NoeudsMax+1;
      
TYPE TableHachage IS PRIVATE;
SUBTYPE Élément IS String(1..LongueurIdent);
TYPE TypeNoeud IS RECORD
                    Identificateur : Élément;
                    Suivant       : Natural; -- champ lien pour chaînage fusionné
                  END RECORD;

PROCEDURE EngendrerNoeud(Noeud: IN OUT TypeNoeud);
-- Cette procédure crée un Noeud.  Identificateur est défini comme une chaîne
-- de longueur 1 à 8 commençant par une lettre.  Suivant est mis à 0, 
-- et est seulement utilisé pour le chaînage fusionné.
 
PROCEDURE HacherLinéaire(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                         Essais: IN OUT Natural);
-- Cette procédure insère un Noeud dans une Table de hachage avec essais linéaires
 
PROCEDURE HacherDécalé(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                        Essais: IN OUT Natural; Magique: IN Modulaire);
-- Cette procédure insère un Noeud dans une Table de hachage avec essais aléatoires 
 
PROCEDURE HacherFusionné(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                          Essais: IN OUT Natural);
-- Cette procédure insère un Noeud dans une Table de hachage avec chaînage fusionné 

PROCEDURE InitialiserTable(Table: IN OUT TableHachage);
-- Cette procédure prépare une table de hachage pour utilisation
    
FUNCTION Magique(Taille_Table: Positive) RETURN Modulaire;
-- Retourne le plus petit nombre "Magique" correspondant à Taille_Table
-- selon la méthode pseudo-aléatoire.

  PRIVATE
TYPE TableH IS ARRAY (1..NoeudsMax) OF TypeNoeud;
TYPE TableHachage IS RECORD
                       Éléments: TableH;
                       Taille: Natural;
                       Libre: Natural;
                     END RECORD;
 
END Hachage;
