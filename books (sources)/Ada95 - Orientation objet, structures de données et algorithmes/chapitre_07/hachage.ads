--          Copyright � 1998 Philippe J. Gabrini
PACKAGE Hachage IS
--******************************************
-- Mettez votre documentation ici.
--******************************************
 
 
NoeudsMax: CONSTANT Natural := 8191;
LongueurIdent: CONSTANT Natural := 8;
TYPE Modulaire IS MOD NoeudsMax+1;
      
TYPE TableHachage IS PRIVATE;
SUBTYPE �l�ment IS String(1..LongueurIdent);
TYPE TypeNoeud IS RECORD
                    Identificateur : �l�ment;
                    Suivant       : Natural; -- champ lien pour cha�nage fusionn�
                  END RECORD;

PROCEDURE EngendrerNoeud(Noeud: IN OUT TypeNoeud);
-- Cette proc�dure cr�e un Noeud.  Identificateur est d�fini comme une cha�ne
-- de longueur 1 � 8 commen�ant par une lettre.  Suivant est mis � 0, 
-- et est seulement utilis� pour le cha�nage fusionn�.
 
PROCEDURE HacherLin�aire(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                         Essais: IN OUT Natural);
-- Cette proc�dure ins�re un Noeud dans une Table de hachage avec essais lin�aires
 
PROCEDURE HacherD�cal�(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                        Essais: IN OUT Natural; Magique: IN Modulaire);
-- Cette proc�dure ins�re un Noeud dans une Table de hachage avec essais al�atoires 
 
PROCEDURE HacherFusionn�(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                          Essais: IN OUT Natural);
-- Cette proc�dure ins�re un Noeud dans une Table de hachage avec cha�nage fusionn� 

PROCEDURE InitialiserTable(Table: IN OUT TableHachage);
-- Cette proc�dure pr�pare une table de hachage pour utilisation
    
FUNCTION Magique(Taille_Table: Positive) RETURN Modulaire;
-- Retourne le plus petit nombre "Magique" correspondant � Taille_Table
-- selon la m�thode pseudo-al�atoire.

  PRIVATE
TYPE TableH IS ARRAY (1..NoeudsMax) OF TypeNoeud;
TYPE TableHachage IS RECORD
                       �l�ments: TableH;
                       Taille: Natural;
                       Libre: Natural;
                     END RECORD;
 
END Hachage;
