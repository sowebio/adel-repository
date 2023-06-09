--          Copyright © 1998 Philippe J. Gabrini
-- Arbres binaires de recherche équilibrés selon Adelson-Velskii et Landis.
--     Philippe Gabrini     Juin 1998
WITH Ada.Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ArbresBinRecherche.ArbresAVL IS

PROCEDURE Libérer IS NEW Ada.Unchecked_Deallocation(NoeudArbre'CLASS, ArbreAVL);

PROCEDURE PivoterDroite(Racine: IN OUT ArbreAVL) IS
Nouveau: ArbreAVL;
BEGIN
  Nouveau := Racine.Gauche;
  Racine.Gauche := Nouveau.Droite;
  Nouveau.Droite := Racine;
  TypeNoeud(Nouveau.Droite.ALL).Équil := 0; -- notez la conversion nécessaire
  Racine := Nouveau;
END PivoterDroite;

PROCEDURE PivoterGauche(Racine: IN OUT ArbreAVL) IS
Nouveau: ArbreAVL;
BEGIN
  Nouveau := Racine.Droite;
  Racine.Droite := Nouveau.Gauche;
  Nouveau.Gauche := Racine;
  TypeNoeud(Nouveau.Gauche.ALL).Équil := 0; -- notez la conversion nécessaire
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
  IF TypeNoeud(Nouveau.ALL).Équil = -1 THEN -- notez les conversions nécessaires
    TypeNoeud(Racine.ALL).Équil := +1;
  ELSE
    TypeNoeud(Racine.ALL).Équil := 0;
  END IF;
  IF TypeNoeud(Nouveau.ALL).Équil = +1 THEN
    TypeNoeud(Gauche.ALL).Équil := -1;
  ELSE
    TypeNoeud(Gauche.ALL).Équil := 0;
  END IF;
  TypeNoeud(Nouveau.ALL).Équil := 0;
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
  IF TypeNoeud(Nouveau.ALL).Équil = +1 THEN -- notez les conversions nécessaires
    TypeNoeud(Racine.ALL).Équil := -1;
  ELSE
    TypeNoeud(Racine.ALL).Équil := 0;
  END IF;
  IF TypeNoeud(Nouveau.ALL).Équil = -1 THEN
    TypeNoeud(Droite.ALL).Équil := +1;
  ELSE
    TypeNoeud(Droite.ALL).Équil := 0;
  END IF;
  TypeNoeud(Nouveau.ALL).Équil := 0;
  Racine := Nouveau;
END PivoterDroiteGauche;


PROCEDURE InsérerNoeud(Arbre: IN OUT ArbreAVL;
                       Élément: IN TypeÉlément) IS
-- Insérer Élément dans Arbre. La clef se trouve dans Élément, lequel est
-- utilisé de façon globale dans la procédure et ses procédures imbriquées.

  PROCEDURE CréerNoeud(Nouveau: IN OUT ArbreAVL) IS
  BEGIN
    Nouveau := NEW TypeNoeud;
    Nouveau.Élément := Élément;
    Nouveau.Gauche := NULL;
    Nouveau.Droite := NULL;
    TypeNoeud(Nouveau.ALL).Équil := 0; -- notez la conversion nécessaire
  END CréerNoeud;
  
  PROCEDURE Insertion(Noeud: IN OUT ArbreAVL; HauteurAugmentée: IN OUT Boolean) IS
  Comparé: Integer;
  BEGIN
    IF Noeud = NULL THEN
      CréerNoeud(Noeud);
      HauteurAugmentée := True;
    ELSE
      Comparé := Comparaison(Élément, Noeud.Élément);
      IF Comparé = -1 THEN
        Insertion(Noeud.Gauche, HauteurAugmentée);
        IF HauteurAugmentée THEN   -- hauteur sous-arbre gauche augmentée
	      IF TypeNoeud(Noeud.ALL).Équil = 1 THEN
	        TypeNoeud(Noeud.ALL).Équil :=  0;
	        HauteurAugmentée := False;        -- rééquilibré
	      ELSIF TypeNoeud(Noeud.ALL).Équil = 0 THEN
	        TypeNoeud(Noeud.ALL).Équil := -1;
	      ELSE -- Équil = -1
	        IF TypeNoeud(Noeud.Gauche.ALL).Équil = -1 THEN
	          PivoterDroite(Noeud);           -- rotation simple droite
	        ELSE PivoterGaucheDroite(Noeud);	-- rotation double droite
	        END IF;
	        TypeNoeud(Noeud.ALL).Équil := 0;  -- Noeud = nouvelle racine
	        HauteurAugmentée := False;
	      END IF;
       	END IF;
      ELSIF Comparé = 1 THEN
        Insertion(Noeud.Droite, HauteurAugmentée);
        IF HauteurAugmentée THEN   -- hauteur sous-arbre droit augmentée
	      IF TypeNoeud(Noeud.ALL).Équil = -1 THEN
	        TypeNoeud(Noeud.ALL).Équil :=  0;
	        HauteurAugmentée := False;        -- rééquilibré
	      ELSIF TypeNoeud(Noeud.ALL).Équil =  0 THEN
	        TypeNoeud(Noeud.ALL).Équil:= +1;
	      ELSE -- Équil = +1
	        IF TypeNoeud(Noeud.Droite.ALL).Équil = 1 THEN
	          PivoterGauche(Noeud);           -- rotation simple gauche
            ELSE
              PivoterDroiteGauche(Noeud);   -- rotation double gauche
	        END IF;
	        TypeNoeud(Noeud.ALL).Équil := 0;  -- Noeud = nouvelle racine
	        HauteurAugmentée := False;
	      END IF;
      	END IF;
      ELSE -- Comparé = 0, existe déjà
        HauteurAugmentée := False;
      END IF;
    END IF;
  END Insertion;
  
Changement: Boolean := False;

BEGIN
  Insertion(Arbre, Changement);
END InsérerNoeud;


PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreAVL; Élément: IN TypeÉlément) IS
-- Trouver l'élément de l'Arbre qui possède la clef d'Élément et le supprimer 
-- de l'Arbre. Élément est utilisé de façon globale dans la procédure et les
-- procédures qui lui sont imbriquées.

  PROCEDURE ChercherEtDétruire(Noeud: IN OUT ArbreAVL;
                               HauteurRéduite: IN OUT Boolean) IS
  Comparé: Integer;
  ÀSupprimer: ArbreAVL;

    PROCEDURE ÉquilibrerDroite(Noeud: IN OUT ArbreAVL;
                               HauteurRéduite: IN OUT Boolean) IS
    NouvelÉquil: Integer;
    BEGIN
      IF TypeNoeud(Noeud.ALL).Équil = 1 THEN
        TypeNoeud(Noeud.ALL).Équil := 0;
      ELSIF TypeNoeud(Noeud.ALL).Équil = 0 THEN
        TypeNoeud(Noeud.ALL).Équil := -1;
        HauteurRéduite := False;
      ELSE
        NouvelÉquil := TypeNoeud(Noeud.Gauche.ALL).Équil;
	      IF NouvelÉquil <= 0 THEN
	        PivoterDroite(Noeud);
	        IF NouvelÉquil = 0 THEN
	          TypeNoeud(Noeud.ALL).Équil := +1;
	          TypeNoeud(Noeud.Droite.ALL).Équil := -1;
	          HauteurRéduite := False;
	        ELSE
	          TypeNoeud(Noeud.ALL).Équil :=  0;
	          TypeNoeud(Noeud.Droite.ALL).Équil := 0;
	        END IF;
        ELSE -- NouvelÉquil=1
	        PivoterGaucheDroite(Noeud);
	      END IF;
      END IF;
    END ÉquilibrerDroite;
    
    PROCEDURE ÉquilibrerGauche(Noeud: IN OUT ArbreAVL; HauteurRéduite: IN OUT Boolean) IS
    NouvelÉquil: Integer;
    BEGIN
      IF TypeNoeud(Noeud.ALL).Équil = -1 THEN
        TypeNoeud(Noeud.ALL).Équil := 0;
      ELSIF TypeNoeud(Noeud.ALL).Équil= 0 THEN
        TypeNoeud(Noeud.ALL).Équil := +1;
        HauteurRéduite := False;
      ELSE
        NouvelÉquil := TypeNoeud(Noeud.Droite.ALL).Équil;
	      IF NouvelÉquil>=0 THEN
	        PivoterGauche(Noeud);
	        IF NouvelÉquil = 0 THEN
	          TypeNoeud(Noeud.Gauche.ALL).Équil := +1;
	          TypeNoeud(Noeud.ALL).Équil := -1;
	          HauteurRéduite := False;
	        ELSE
	          TypeNoeud(Noeud.ALL).Équil := 0;
	          TypeNoeud(Noeud.Gauche.ALL).Équil := 0;
	        END IF;
        ELSE -- NouvelÉquil= -1
	        PivoterDroiteGauche(Noeud);
	      END IF;
      END IF;
    END ÉquilibrerGauche;

    PROCEDURE ÉchangerEtSupprimer(Remplace: IN OUT ArbreAVL;
                                  HauteurRéduite: IN OUT Boolean) IS
    -- Remplacer par le noeud le plus à droite du sous-arbre gauche.
    BEGIN
      IF Remplace.Droite /= NULL THEN
        ÉchangerEtSupprimer(Remplace.Droite, HauteurRéduite);
       	IF HauteurRéduite THEN
       	  ÉquilibrerDroite(Remplace, HauteurRéduite);
       	END IF;
      ELSE
        ÀSupprimer.Élément := Remplace.Élément;
       	ÀSupprimer := Remplace;
	      Remplace := Remplace.Gauche;
       	HauteurRéduite := True;
      END IF;
    END ÉchangerEtSupprimer;
    
  BEGIN  -- ChercherEtDétruire
    IF Noeud = NULL THEN
      HauteurRéduite:=False;
      RETURN;
    END IF;           -- clef non trouvée
    Comparé := Comparaison(Élément, Noeud.Élément);
    IF Comparé = -1 THEN
      ChercherEtDétruire(Noeud.Gauche, HauteurRéduite);
      IF HauteurRéduite THEN
        ÉquilibrerGauche(Noeud,HauteurRéduite);
      END IF;
    ELSIF Comparé = 1 THEN
      ChercherEtDétruire(Noeud.Droite, HauteurRéduite);
      IF HauteurRéduite THEN
        ÉquilibrerDroite(Noeud,HauteurRéduite);
      END IF;
    ELSE              -- clef trouvée
      ÀSupprimer := Noeud;
      IF Noeud.Gauche = NULL THEN
        Noeud := Noeud.Droite;
	    HauteurRéduite := True;
      ELSIF Noeud.Droite = NULL THEN
        Noeud := Noeud.Gauche;
	    HauteurRéduite:=True;
      ELSE -- Droite et Gauche ont des sous-arbres non vides
        ÉchangerEtSupprimer(ÀSupprimer.Gauche, HauteurRéduite);
        IF HauteurRéduite THEN
          ÉquilibrerGauche(Noeud, HauteurRéduite);
        END IF;
      END IF;
      Libérer(ÀSupprimer);
    END IF;
  END ChercherEtDétruire;

Changement: Boolean := False;

BEGIN
  ChercherEtDétruire(Arbre, Changement);
END SupprimerNoeud;

-- Les opérations TraverserArbre, Rechercher, AfficherArbre n'ont pas à être
-- redéfinies puisqu'elles sont identiques à celles qui sont définies pour
-- les arbres binaires de recherche. Cependant, DétruireArbre porte un nom
-- différent et est réduite à un appel simple de la procédure du paquetage parent.

PROCEDURE DétruireArbre(Arbre: IN OUT ArbreAVL) IS
-- Toute l'information sur l'Arbre et les données qu'il contient est détruite
BEGIN
  SupprimerArbre(Arbre);
END DétruireArbre;

END ArbresBinRecherche.ArbresAVL;

