--          Copyright � 1998 Philippe J. Gabrini
-- Arbres binaires de recherche �quilibr�s selon Adelson-Velskii et Landis.
--     Philippe Gabrini     Juin 1998
WITH Ada.Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ArbresBinRecherche.ArbresAVL IS

PROCEDURE Lib�rer IS NEW Ada.Unchecked_Deallocation(NoeudArbre'CLASS, ArbreAVL);

PROCEDURE PivoterDroite(Racine: IN OUT ArbreAVL) IS
Nouveau: ArbreAVL;
BEGIN
  Nouveau := Racine.Gauche;
  Racine.Gauche := Nouveau.Droite;
  Nouveau.Droite := Racine;
  TypeNoeud(Nouveau.Droite.ALL).�quil := 0; -- notez la conversion n�cessaire
  Racine := Nouveau;
END PivoterDroite;

PROCEDURE PivoterGauche(Racine: IN OUT ArbreAVL) IS
Nouveau: ArbreAVL;
BEGIN
  Nouveau := Racine.Droite;
  Racine.Droite := Nouveau.Gauche;
  Nouveau.Gauche := Racine;
  TypeNoeud(Nouveau.Gauche.ALL).�quil := 0; -- notez la conversion n�cessaire
  Racine := Nouveau;
END PivoterGauche;

PROCEDURE PivoterGaucheDroite(Racine: IN OUT ArbreAVL) IS  -- rotation double
Nouveau, Gauche: ArbreAVL;
BEGIN
  Gauche := Racine.Gauche;
  Nouveau := Gauche.Droite;
  Gauche.Droite := Nouveau.Gauche;
  Racine.Gauche := Nouveau.Droite;
  Nouveau.Gauche := Gauche;
  Nouveau.Droite := Racine;
  IF TypeNoeud(Nouveau.ALL).�quil = -1 THEN -- notez les conversions n�cessaires
    TypeNoeud(Racine.ALL).�quil := +1;
  ELSE
    TypeNoeud(Racine.ALL).�quil := 0;
  END IF;
  IF TypeNoeud(Nouveau.ALL).�quil = +1 THEN
    TypeNoeud(Gauche.ALL).�quil := -1;
  ELSE
    TypeNoeud(Gauche.ALL).�quil := 0;
  END IF;
  TypeNoeud(Nouveau.ALL).�quil := 0;
  Racine := Nouveau;
END PivoterGaucheDroite;

PROCEDURE PivoterDroiteGauche(Racine: IN OUT ArbreAVL) IS  -- rotation double
Nouveau, Droite: ArbreAVL;
BEGIN
  Droite := Racine.Droite;
  Nouveau := Droite.Gauche;
  Droite.Gauche := Nouveau.Droite;
  Racine.Droite := Nouveau.Gauche;
  Nouveau.Droite := Droite;
  Nouveau.Gauche := Racine;
  IF TypeNoeud(Nouveau.ALL).�quil = +1 THEN -- notez les conversions n�cessaires
    TypeNoeud(Racine.ALL).�quil := -1;
  ELSE
    TypeNoeud(Racine.ALL).�quil := 0;
  END IF;
  IF TypeNoeud(Nouveau.ALL).�quil = -1 THEN
    TypeNoeud(Droite.ALL).�quil := +1;
  ELSE
    TypeNoeud(Droite.ALL).�quil := 0;
  END IF;
  TypeNoeud(Nouveau.ALL).�quil := 0;
  Racine := Nouveau;
END PivoterDroiteGauche;


PROCEDURE Ins�rerNoeud(Arbre: IN OUT ArbreAVL;
                       �l�ment: IN Type�l�ment) IS
-- Ins�rer �l�ment dans Arbre. La clef se trouve dans �l�ment, lequel est
-- utilis� de fa�on globale dans la proc�dure et ses proc�dures imbriqu�es.

  PROCEDURE Cr�erNoeud(Nouveau: IN OUT ArbreAVL) IS
  BEGIN
    Nouveau := NEW TypeNoeud;
    Nouveau.�l�ment := �l�ment;
    Nouveau.Gauche := NULL;
    Nouveau.Droite := NULL;
    TypeNoeud(Nouveau.ALL).�quil := 0; -- notez la conversion n�cessaire
  END Cr�erNoeud;
  
  PROCEDURE Insertion(Noeud: IN OUT ArbreAVL; HauteurAugment�e: IN OUT Boolean) IS
  Compar�: Integer;
  BEGIN
    IF Noeud = NULL THEN
      Cr�erNoeud(Noeud);
      HauteurAugment�e := True;
    ELSE
      Compar� := Comparaison(�l�ment, Noeud.�l�ment);
      IF Compar� = -1 THEN
        Insertion(Noeud.Gauche, HauteurAugment�e);
        IF HauteurAugment�e THEN   -- hauteur sous-arbre gauche augment�e
	      IF TypeNoeud(Noeud.ALL).�quil = 1 THEN
	        TypeNoeud(Noeud.ALL).�quil :=  0;
	        HauteurAugment�e := False;        -- r��quilibr�
	      ELSIF TypeNoeud(Noeud.ALL).�quil = 0 THEN
	        TypeNoeud(Noeud.ALL).�quil := -1;
	      ELSE -- �quil = -1
	        IF TypeNoeud(Noeud.Gauche.ALL).�quil = -1 THEN
	          PivoterDroite(Noeud);           -- rotation simple droite
	        ELSE PivoterGaucheDroite(Noeud);	-- rotation double droite
	        END IF;
	        TypeNoeud(Noeud.ALL).�quil := 0;  -- Noeud = nouvelle racine
	        HauteurAugment�e := False;
	      END IF;
       	END IF;
      ELSIF Compar� = 1 THEN
        Insertion(Noeud.Droite, HauteurAugment�e);
        IF HauteurAugment�e THEN   -- hauteur sous-arbre droit augment�e
	      IF TypeNoeud(Noeud.ALL).�quil = -1 THEN
	        TypeNoeud(Noeud.ALL).�quil :=  0;
	        HauteurAugment�e := False;        -- r��quilibr�
	      ELSIF TypeNoeud(Noeud.ALL).�quil =  0 THEN
	        TypeNoeud(Noeud.ALL).�quil:= +1;
	      ELSE -- �quil = +1
	        IF TypeNoeud(Noeud.Droite.ALL).�quil = 1 THEN
	          PivoterGauche(Noeud);           -- rotation simple gauche
            ELSE
              PivoterDroiteGauche(Noeud);   -- rotation double gauche
	        END IF;
	        TypeNoeud(Noeud.ALL).�quil := 0;  -- Noeud = nouvelle racine
	        HauteurAugment�e := False;
	      END IF;
      	END IF;
      ELSE -- Compar� = 0, existe d�j�
        HauteurAugment�e := False;
      END IF;
    END IF;
  END Insertion;
  
Changement: Boolean := False;

BEGIN
  Insertion(Arbre, Changement);
END Ins�rerNoeud;


PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreAVL; �l�ment: IN Type�l�ment) IS
-- Trouver l'�l�ment de l'Arbre qui poss�de la clef d'�l�ment et le supprimer 
-- de l'Arbre. �l�ment est utilis� de fa�on globale dans la proc�dure et les
-- proc�dures qui lui sont imbriqu�es.

  PROCEDURE ChercherEtD�truire(Noeud: IN OUT ArbreAVL;
                               HauteurR�duite: IN OUT Boolean) IS
  Compar�: Integer;
  �Supprimer: ArbreAVL;

    PROCEDURE �quilibrerDroite(Noeud: IN OUT ArbreAVL;
                               HauteurR�duite: IN OUT Boolean) IS
    Nouvel�quil: Integer;
    BEGIN
      IF TypeNoeud(Noeud.ALL).�quil = 1 THEN
        TypeNoeud(Noeud.ALL).�quil := 0;
      ELSIF TypeNoeud(Noeud.ALL).�quil = 0 THEN
        TypeNoeud(Noeud.ALL).�quil := -1;
        HauteurR�duite := False;
      ELSE
        Nouvel�quil := TypeNoeud(Noeud.Gauche.ALL).�quil;
	      IF Nouvel�quil <= 0 THEN
	        PivoterDroite(Noeud);
	        IF Nouvel�quil = 0 THEN
	          TypeNoeud(Noeud.ALL).�quil := +1;
	          TypeNoeud(Noeud.Droite.ALL).�quil := -1;
	          HauteurR�duite := False;
	        ELSE
	          TypeNoeud(Noeud.ALL).�quil :=  0;
	          TypeNoeud(Noeud.Droite.ALL).�quil := 0;
	        END IF;
        ELSE -- Nouvel�quil=1
	        PivoterGaucheDroite(Noeud);
	      END IF;
      END IF;
    END �quilibrerDroite;
    
    PROCEDURE �quilibrerGauche(Noeud: IN OUT ArbreAVL; HauteurR�duite: IN OUT Boolean) IS
    Nouvel�quil: Integer;
    BEGIN
      IF TypeNoeud(Noeud.ALL).�quil = -1 THEN
        TypeNoeud(Noeud.ALL).�quil := 0;
      ELSIF TypeNoeud(Noeud.ALL).�quil= 0 THEN
        TypeNoeud(Noeud.ALL).�quil := +1;
        HauteurR�duite := False;
      ELSE
        Nouvel�quil := TypeNoeud(Noeud.Droite.ALL).�quil;
	      IF Nouvel�quil>=0 THEN
	        PivoterGauche(Noeud);
	        IF Nouvel�quil = 0 THEN
	          TypeNoeud(Noeud.Gauche.ALL).�quil := +1;
	          TypeNoeud(Noeud.ALL).�quil := -1;
	          HauteurR�duite := False;
	        ELSE
	          TypeNoeud(Noeud.ALL).�quil := 0;
	          TypeNoeud(Noeud.Gauche.ALL).�quil := 0;
	        END IF;
        ELSE -- Nouvel�quil= -1
	        PivoterDroiteGauche(Noeud);
	      END IF;
      END IF;
    END �quilibrerGauche;

    PROCEDURE �changerEtSupprimer(Remplace: IN OUT ArbreAVL;
                                  HauteurR�duite: IN OUT Boolean) IS
    -- Remplacer par le noeud le plus � droite du sous-arbre gauche.
    BEGIN
      IF Remplace.Droite /= NULL THEN
        �changerEtSupprimer(Remplace.Droite, HauteurR�duite);
       	IF HauteurR�duite THEN
       	  �quilibrerDroite(Remplace, HauteurR�duite);
       	END IF;
      ELSE
        �Supprimer.�l�ment := Remplace.�l�ment;
       	�Supprimer := Remplace;
	      Remplace := Remplace.Gauche;
       	HauteurR�duite := True;
      END IF;
    END �changerEtSupprimer;
    
  BEGIN  -- ChercherEtD�truire
    IF Noeud = NULL THEN
      HauteurR�duite:=False;
      RETURN;
    END IF;           -- clef non trouv�e
    Compar� := Comparaison(�l�ment, Noeud.�l�ment);
    IF Compar� = -1 THEN
      ChercherEtD�truire(Noeud.Gauche, HauteurR�duite);
      IF HauteurR�duite THEN
        �quilibrerGauche(Noeud,HauteurR�duite);
      END IF;
    ELSIF Compar� = 1 THEN
      ChercherEtD�truire(Noeud.Droite, HauteurR�duite);
      IF HauteurR�duite THEN
        �quilibrerDroite(Noeud,HauteurR�duite);
      END IF;
    ELSE              -- clef trouv�e
      �Supprimer := Noeud;
      IF Noeud.Gauche = NULL THEN
        Noeud := Noeud.Droite;
	    HauteurR�duite := True;
      ELSIF Noeud.Droite = NULL THEN
        Noeud := Noeud.Gauche;
	    HauteurR�duite:=True;
      ELSE -- Droite et Gauche ont des sous-arbres non vides
        �changerEtSupprimer(�Supprimer.Gauche, HauteurR�duite);
        IF HauteurR�duite THEN
          �quilibrerGauche(Noeud, HauteurR�duite);
        END IF;
      END IF;
      Lib�rer(�Supprimer);
    END IF;
  END ChercherEtD�truire;

Changement: Boolean := False;

BEGIN
  ChercherEtD�truire(Arbre, Changement);
END SupprimerNoeud;

-- Les op�rations TraverserArbre, Rechercher, AfficherArbre n'ont pas � �tre
-- red�finies puisqu'elles sont identiques � celles qui sont d�finies pour
-- les arbres binaires de recherche. Cependant, D�truireArbre porte un nom
-- diff�rent et est r�duite � un appel simple de la proc�dure du paquetage parent.

PROCEDURE D�truireArbre(Arbre: IN OUT ArbreAVL) IS
-- Toute l'information sur l'Arbre et les donn�es qu'il contient est d�truite
BEGIN
  SupprimerArbre(Arbre);
END D�truireArbre;

END ArbresBinRecherche.ArbresAVL;

