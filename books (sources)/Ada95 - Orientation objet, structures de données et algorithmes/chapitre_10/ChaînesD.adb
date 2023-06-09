--          Copyright © 1998 Philippe J. Gabrini
WITH Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ChaînesD IS
-- Réalisation des chaînes dynamiques.
PROCEDURE Libérer IS NEW Unchecked_Deallocation(Bloc, PointeurBloc);

FUNCTION Longueur(Chaîne: TypChaîne) RETURN Natural IS
BEGIN
  RETURN Chaîne.Longueur;
END Longueur;

FUNCTION ChaîneVide RETURN TypChaîne IS
Vide: TypChaîne;
BEGIN
  RETURN Vide;
END ChaîneVide;

FUNCTION Tranche(Source: TypChaîne; Bas: Positive;
                 Haut: Natural) RETURN TypChaîne IS
-- Retourne une partie de la chaîne Source.
Intermédiaire: TypChaîne := Source;       -- faire une copie
BEGIN
 IF Haut <= Longueur(Intermédiaire) THEN	-- sous-chaîne valide
   Supprimer(Intermédiaire, Haut+1, 
				       Longueur(Intermédiaire));  -- fin
   Supprimer(Intermédiaire, 1, Bas-1);		-- caractères précédents
   RETURN Intermédiaire;
 ELSE
   RAISE Erreur_Indice;
 END IF;
END Tranche;

PROCEDURE Supprimer(Source: IN OUT TypChaîne; De, À: IN Natural) IS
-- Supprimer une partie d'une chaîne Source.
LongueurSource, Index, IndexFin: Natural;
Départ, Fin, Dernier: PointeurBloc;
BEGIN
  IF Source.Début /= NULL THEN
    Index := De;
    LongueurSource := Source.Longueur;
    IndexFin := À + 1;
    IF (IndexFin <= LongueurSource+1) AND (À > 0) AND (Index > 0) THEN
	    Départ := Source.Début;
	    WHILE Index > LongueurBloc LOOP 
        -- rechercher bloc départ de la sous-chaîne
	      Départ := Départ.Suivant;
	      Index := Index - LongueurBloc;
	    END LOOP;
	    IF IndexFin <= LongueurSource THEN 
        -- copier fin de chaîne par dessus partie supprimée
	      Fin := Source.Début;
	      WHILE IndexFin > LongueurBloc LOOP -- trouver bloc fin de sous-chaîne
	        Fin := Fin.Suivant;
	        IndexFin := IndexFin - LongueurBloc;
	      END LOOP;
	      FOR Courant IN 1..LongueurSource-À LOOP
	        -- copier fin chaîne par dessus sous-chaîne
	        Départ.SuiteCar(Index) := Fin.SuiteCar(IndexFin);
	        Index := Index + 1;
	        IF Index > LongueurBloc THEN    -- bloc suivant
		        Dernier := Départ;
		        Départ := Départ.Suivant;
		        Index := 1;
	        END IF;
	        IndexFin := IndexFin + 1;
	        IF IndexFin > LongueurBloc THEN -- bloc suivant
		        Fin := Fin.Suivant;
		        IndexFin := 1;
	        END IF;
	      END LOOP;
	    END IF;
	    FOR Courant IN Index..LongueurBloc LOOP -- remplir bloc de zéros
	      Départ.SuiteCar(Courant) := FinDeChaîne;
	    END LOOP;
	    Fin := Départ.Suivant;   -- premier bloc à libérer
	    Départ.Suivant := NULL;
	    WHILE Fin /= NULL LOOP   -- libérer tous les autres blocs
	      Départ := Fin.Suivant;
	      Libérer(Fin);
	      Fin := Départ;
      END LOOP;
      Source.Longueur := Source.Longueur - (À - De + 1);
    END IF;
  END IF;
END Supprimer;

PROCEDURE Insérer(Source: IN OUT TypChaîne; Avant: IN Positive;
                  Nouvelle: IN TypChaîne) IS
-- Insérer Nouvelle dans Source avant Avant.
IndexCourant, Libre, LongueurNouvelle, Blocs,
DernierIndex, NouvelIndex, Index: Natural;
Dernier, Nouveau, Courant, Arrière, ÉltNouveau: PointeurBloc;
BEGIN
  LongueurNouvelle := Nouvelle.Longueur;
  IF LongueurNouvelle > 0 THEN   -- insertion
    Index := Avant;
    IndexCourant := Longueur(Source);
    IF Index <= IndexCourant+1 THEN	-- insertion possible
	    IF Source.Début = NULL THEN	  -- Source vide: allouer premier bloc
	      Source.Début := NEW Bloc;
	      Source.Début.Suivant := NULL;
	      Source.Début.SuiteCar(1) := FinDeChaîne;
	    END IF;
	    Dernier := Source.Début;
	    DernierIndex := IndexCourant; -- fin de Source
	    WHILE (DernierIndex > LongueurBloc) AND (Dernier.Suivant /= NULL) LOOP
	      -- trouver le dernier bloc de Source
	      Dernier := Dernier.Suivant;
	      DernierIndex := DernierIndex - LongueurBloc;
	    END LOOP;
	    IF IndexCourant = 0 THEN
	      Libre := LongueurBloc;      -- seul bloc totalement libre
	    ELSE
	      Libre := LongueurBloc - DernierIndex; 
        -- dernier bloc a Libre positions libres
	    END IF;
	    Nouveau := Dernier;
	    IF LongueurNouvelle > Libre THEN
	      Blocs := (LongueurNouvelle - Libre) / LongueurBloc; -- nombre de blocs à ajouter
	      IF (LongueurNouvelle-Libre) MOD LongueurBloc /= 0 THEN
		      Blocs := Blocs + 1;
	      END IF;
	      FOR Nombre IN 1..Blocs LOOP  -- ajouter de nouveaux bloics à Source
		      Nouveau.Suivant := NEW Bloc;
		      Nouveau := Nouveau.Suivant;
	      END LOOP;
	    END IF;
	    NouvelIndex := (IndexCourant + LongueurNouvelle) MOD LongueurBloc + 1;
	    IF NouvelIndex /= 1 THEN
	      -- dernier bloc non plein: marquer fin de chaîne
	      Nouveau.SuiteCar(NouvelIndex) := FinDeChaîne;
		    NouvelIndex := NouvelIndex - 1;
	    END IF;
	    FOR Nombre IN 1..IndexCourant-Index+1 LOOP
	      -- déplacer fin de chaîne Source pour faire de la place pour insertion
	      Nouveau.SuiteCar(NouvelIndex) := Dernier.SuiteCar(DernierIndex);
	      IF NouvelIndex = 1 THEN      -- bloc précédent
		      Arrière := Source.Début;
		      WHILE Arrière.Suivant /= Nouveau LOOP  -- trouver bloc précédent
		        Arrière := Arrière.Suivant;
		      END LOOP;
		      Nouveau := Arrière;
		      NouvelIndex := LongueurBloc;
	      ELSE
		      NouvelIndex := NouvelIndex - 1;         -- reculer
	      END IF;
	      IF DernierIndex = 1 THEN                  -- bloc précédent
		      Arrière := Source.Début;
		      WHILE (Arrière /= NULL) AND THEN (Arrière.Suivant /= Dernier) LOOP
		        Arrière := Arrière.Suivant;
		      END LOOP;
		      Dernier := Arrière;
		      DernierIndex := LongueurBloc;
	      ELSE
		      DernierIndex := DernierIndex - 1;       -- reculer
	      END IF;
	    END LOOP;
	    Courant := Source.Début;
	    WHILE Index > LongueurBloc LOOP  -- trouver bloc où insérer
	      Courant := Courant.Suivant;
	      Index := Index - LongueurBloc;
	    END LOOP;
	    Libre := 1;
	    ÉltNouveau := Nouvelle.Début;
	    FOR Nombre IN 1..LongueurNouvelle LOOP	
        -- insérer LongueurNouvelle caractères de nouveau
	      Courant.SuiteCar(Index) := ÉltNouveau.SuiteCar(Libre);
	      Index := Index + 1;
	      IF Index > LongueurBloc THEN    -- bloc suivant
		      Index := 1;
		      Courant := Courant.Suivant;
	      END IF;
	      Libre := Libre + 1;
	      IF Libre > LongueurBloc THEN    -- bloc suivant
		      Libre := 1;
		      ÉltNouveau := ÉltNouveau.Suivant;
	      END IF;
      END LOOP;
      Source.Longueur := Source.Longueur + LongueurNouvelle;
    END IF;
  END IF;
END Insérer;

FUNCTION Position(Source, Patron: TypChaîne) RETURN Natural IS
-- Recherche sous-chaîne Patron dans chaîne Source.
Index, Courant, Libre, MarqueSource: Natural;
LongueurPatron, LongueurSource, Résultat, Ancien: Natural;
SousChaîne, ÉltSource, AncienBloc: PointeurBloc;
BEGIN
  LongueurPatron := Patron.Longueur;
  LongueurSource := Source.Longueur;
  Résultat := 0;     -- résultat nul si pas trouvé
  IF LongueurPatron <= LongueurSource THEN
    SousChaîne := Patron.Début;
	  ÉltSource := Source.Début;
    Libre := 1;         -- indice général sous-chaîne 
	  Index := 1;         -- indice bloc sous-chaîne
    MarqueSource := 1;  -- indice général source
	  Courant := 1;       -- indice bloc source
    WHILE (Libre <= LongueurPatron) AND (MarqueSource <= LongueurSource) LOOP
      -- comparer caractères
	    IF SousChaîne.SuiteCar(Index) = ÉltSource.SuiteCar(Courant) THEN
	      Libre := Libre + 1; Index := Index + 1;
	      IF Index > LongueurBloc THEN   -- prochain bloc sous-chaîne
		      SousChaîne := SousChaîne.Suivant;
		      Index := 1;
	      END IF;
	      IF Résultat = 0 THEN           -- résultat possible
		      Résultat := MarqueSource;
		      Ancien := Courant;
          AncienBloc := ÉltSource;
	      END IF;
	    ELSIF Résultat /= 0 THEN         -- correspondance incomplète
	      MarqueSource := Résultat;      -- reculer indice
	      Courant := Ancien;
        ÉltSource := AncienBloc;       -- reculer bloc
	      Libre := 1; Index := 1;
        SousChaîne := Patron.Début;    -- premier bloc patron
	      Résultat := 0;
	    END IF;
	    MarqueSource := MarqueSource + 1;
      Courant := Courant + 1;
	    IF Courant > LongueurBloc THEN   -- prochain bloc Source
	      ÉltSource := ÉltSource.Suivant;
	      Courant := 1;
	    END IF;
    END LOOP;
    IF Libre <= LongueurPatron THEN    -- fin source mais pas fin patron
      Résultat := 0;
    END IF;
  END IF;
  RETURN Résultat;
END Position;

FUNCTION "&" (Gauche: TypChaîne; Droite: TypChaîne) RETURN TypChaîne IS
-- Concaténer chaînes Gauche et Droite. 
LongueurGauche, LongueurDroite, Libre, MarqueSource: Natural;
Courant, ÉltDroite: PointeurBloc;
Source: TypChaîne;
BEGIN
  LongueurGauche := Longueur(Gauche);
  LongueurDroite := Longueur(Droite);
  IF LongueurGauche > 0 THEN    -- première chaîne non vide
    Source := Gauche;           -- copier Gauche
    IF LongueurDroite > 0 THEN  -- deuxième chaîne non vide
	    Courant := Source.Début;
	    ÉltDroite := Droite.Début;
	    WHILE Courant.Suivant /= NULL LOOP   -- trouver fin de Gauche
        LongueurGauche := LongueurGauche - LongueurBloc;
	      Courant := Courant.Suivant;
	    END LOOP;
	    Libre := 1; MarqueSource := 1;
      LongueurGauche := LongueurGauche + 1;   -- copier après gauche
	    WHILE Libre <= LongueurDroite LOOP      -- copier caractères Droite à la fin de Gauche
	      IF LongueurGauche > LongueurBloc THEN -- ajouter nouveau bloc
		      LongueurGauche := 1;
		      Courant.Suivant := NEW Bloc;
		      Courant := Courant.Suivant;
	      END IF;
	      Courant.SuiteCar(LongueurGauche) := ÉltDroite.SuiteCar(MarqueSource);
	      Libre := Libre + 1;
        MarqueSource := MarqueSource + 1;
        LongueurGauche := LongueurGauche + 1;
	      IF MarqueSource > LongueurBloc THEN -- avancer au prochain bloc de Droite
		      ÉltDroite := ÉltDroite.Suivant;
		      MarqueSource := 1;
	      END IF;
	    END LOOP;
	    IF LongueurGauche <= LongueurBloc THEN
	      Courant.SuiteCar(LongueurGauche) := FinDeChaîne; -- marquer fin de chaîne
      END IF;
      Source.Longueur := Source.Longueur + LongueurDroite;
    END IF;
  ELSE
    Source := Droite;   -- comme Gauche est vide, copier Droite
  END IF;
  RETURN Source;
END "&";

FUNCTION "&" (Gauche: TypChaîne; Droite: Character) RETURN TypChaîne IS
-- Concaténer chaîne Gauche et caractère Droite.
LongueurGauche: Natural;
Courant: PointeurBloc;
Source: TypChaîne;
BEGIN
  LongueurGauche := Longueur(Gauche);
  IF LongueurGauche > 0 THEN    -- chaîne non vide
    Source := Gauche;           -- copier Gauche
	  Courant := Source.Début;
	  WHILE Courant.Suivant /= NULL LOOP      -- trouver fin de Gauche
      LongueurGauche := LongueurGauche - LongueurBloc;
	    Courant := Courant.Suivant;
	  END LOOP;
    LongueurGauche := LongueurGauche + 1;   -- copier après gauche
	  IF LongueurGauche > LongueurBloc THEN   -- ajouter nouveau bloc
	    LongueurGauche := 1;
	    Courant.Suivant := NEW Bloc;
	    Courant := Courant.Suivant;
	  END IF;
	  Courant.SuiteCar(LongueurGauche) := Droite;
    LongueurGauche := LongueurGauche +1;
	  IF LongueurGauche <= LongueurBloc THEN
	    Courant.SuiteCar(LongueurGauche) := FinDeChaîne; -- marquer fin de chaîne
    END IF;
    Source.Longueur := Source.Longueur + 1;
  ELSE
    Source.Début := NEW Bloc;            -- Gauche est vide, nouveau bloc 
    Source.Début.SuiteCar(1) := Droite;  -- copier Droite
    Source.Longueur := 1;
  END IF;
  RETURN Source;
END "&";

FUNCTION "&" (Gauche: Character; Droite: TypChaîne) RETURN TypChaîne IS
-- Concaténer caractère Gauche et chaîne Droite.
LongueurGauche, LongueurDroite, Libre, MarqueSource: Natural;
Courant, ÉltDroite: PointeurBloc;
Source: TypChaîne;
BEGIN
  LongueurDroite := Longueur(Droite);
  Source.Début := NEW Bloc;   -- copier Gauche
  Source.Début.SuiteCar(1) := Gauche;
  Source.Longueur := LongueurDroite + 1;
  LongueurGauche := 1;
  IF LongueurDroite > 0 THEN  -- deuxième chaîne non vide
	  Courant := Source.Début;
	  ÉltDroite := Droite.Début;
	  Libre := 1; MarqueSource := 1;
    LongueurGauche := LongueurGauche + 1;   -- copier après gauche
	  WHILE Libre <= LongueurDroite LOOP      -- copier caractères Droite à la fin de Gauche
	    IF LongueurGauche > LongueurBloc THEN -- ajouter nouveau bloc
		    LongueurGauche := 1;
		    Courant.Suivant := NEW Bloc;
		    Courant := Courant.Suivant;
	    END IF;
	    Courant.SuiteCar(LongueurGauche) := ÉltDroite.SuiteCar(MarqueSource);
	    Libre := Libre + 1;
      MarqueSource := MarqueSource + 1;
      LongueurGauche := LongueurGauche + 1;
	    IF MarqueSource > LongueurBloc THEN -- avancer au prochain bloc de Droite
		    ÉltDroite := ÉltDroite.Suivant;
		    MarqueSource := 1;
	    END IF;
	  END LOOP;
	  IF LongueurGauche <= LongueurBloc THEN
	    Courant.SuiteCar(LongueurGauche) := FinDeChaîne; -- marquer fin de chaîne
    END IF;
  END IF;
  RETURN Source;
END "&";

FUNCTION "&" (Gauche: TypChaîne; Droite: String) RETURN TypChaîne IS
-- Concaténer chaîne Gauche et cahîne statique Droite.
LongueurGauche, LongueurDroite, Libre: Natural;
Courant: PointeurBloc;
Source: TypChaîne;
BEGIN
  LongueurGauche := Longueur(Gauche);
  LongueurDroite := Droite'Last;
  Source := Gauche;               -- copier Gauche
  IF LongueurDroite > 0 THEN
    IF LongueurGauche > 0 THEN    -- première chaîne non vide
	    Courant := Source.Début;
	    WHILE Courant.Suivant /= NULL LOOP   -- trouver fin de Gauche
        LongueurGauche := LongueurGauche - LongueurBloc;
	      Courant := Courant.Suivant;
      END LOOP;
    ELSE
      Source.Début := NEW Bloc;
      Courant := Source.Début;
    END IF;
	  Libre := 1;
    LongueurGauche := LongueurGauche + 1;   -- copier après gauche
	  WHILE Libre <= LongueurDroite LOOP      -- copier caractères Droite à la fin de Gauche
	    IF LongueurGauche > LongueurBloc THEN -- ajouter nouveau bloc
		    LongueurGauche := 1;
		    Courant.Suivant := NEW Bloc;
		    Courant := Courant.Suivant;
	    END IF;
	    Courant.SuiteCar(LongueurGauche) := Droite(Libre);
	    Libre := Libre + 1;
      LongueurGauche := LongueurGauche + 1;
	  END LOOP;
	  IF LongueurGauche <= LongueurBloc THEN
	    Courant.SuiteCar(LongueurGauche) := FinDeChaîne; -- marquer fin de chaîne
    END IF;
    Source.Longueur := Source.Longueur + LongueurDroite;
  END IF;
  RETURN Source;
END "&";

FUNCTION "&" (Gauche: String; Droite: TypChaîne) RETURN TypChaîne IS
-- Concaténer chaîne statique Gauche et chaîne Droite.
LongueurSource, LongueurGauche, LongueurDroite, Libre, MarqueDroite: Natural;
Courant, ÉltDroite: PointeurBloc;
Source: TypChaîne;
BEGIN
  LongueurGauche := Gauche'Last;
  LongueurDroite := Longueur(Droite);
  IF LongueurGauche > 0 THEN    -- première chaîne non vide
    Source.Début := NEW Bloc;   -- copier Gauche
	  Courant := Source.Début;
  	Libre := 1;
    LongueurSource := 1;                    -- premier carcatère
	  WHILE Libre <= LongueurGauche LOOP      -- copier caractères Gauche
	    IF LongueurSource > LongueurBloc THEN -- ajouter nouveau bloc
		    LongueurSource := 1;
		    Courant.Suivant := NEW Bloc;
		    Courant := Courant.Suivant;
	    END IF;
	    Courant.SuiteCar(LongueurSource) := Gauche(Libre);
	    Libre := Libre + 1;
      LongueurSource := LongueurSource + 1;
	  END LOOP;
    IF LongueurDroite > 0 THEN  -- deuxième chaîne non vide
	    ÉltDroite := Droite.Début;
	    Libre := 1; MarqueDroite := 1;
	    WHILE Libre <= LongueurDroite LOOP      -- copier caractères Droite à la fin de Gauche
	      IF LongueurSource > LongueurBloc THEN -- ajouter nouveau bloc
		      LongueurSource := 1;
		      Courant.Suivant := NEW Bloc;
		      Courant := Courant.Suivant;
	      END IF;
	      Courant.SuiteCar(LongueurSource) := ÉltDroite.SuiteCar(MarqueDroite);
	      Libre := Libre + 1;
        MarqueDroite := MarqueDroite + 1;
        LongueurSource := LongueurSource + 1;
	      IF MarqueDroite > LongueurBloc THEN -- avancer au prochain bloc de Droite
		      ÉltDroite := ÉltDroite.Suivant;
		      MarqueDroite := 1;
	      END IF;
	    END LOOP;
	    IF LongueurSource <= LongueurBloc THEN
	      Courant.SuiteCar(LongueurSource) := FinDeChaîne; -- marquer fin de chaîne
      END IF;
    END IF;
    Source.Longueur := LongueurGauche + LongueurDroite;
  ELSE
    Source := Droite;   -- comme Gauche est vide, copier Droite
  END IF;
  RETURN Source;
END "&";

FUNCTION ">"(Gauche, Droite: TypChaîne) RETURN Boolean IS
-- Comparaison lexicale selon l'ensemble de caractères.
Index, Courant, Libre, MarqueSource: Natural;
ÉltGauche, ÉltDroite: PointeurBloc;
BEGIN
  Index := Gauche.Longueur;
  Courant := Droite.Longueur;
  IF (Index > 0) AND (Courant > 0) THEN
    ÉltGauche := Gauche.Début;
	  ÉltDroite := Droite.Début;
    Libre := 1;         -- indice général
    MarqueSource := 1;  -- indice bloc
    WHILE (Libre <= Index) AND THEN (Libre <= Courant) -- deux chaînes non vides
	   AND THEN (ÉltGauche.SuiteCar(MarqueSource) 
	             = ÉltDroite.SuiteCar(MarqueSource)) LOOP
	    Libre := Libre + 1; MarqueSource := MarqueSource + 1;
	    IF (Libre <= Index) AND (Libre <= Courant) AND 
	       (MarqueSource > LongueurBloc) THEN  -- prochain bloc
	      ÉltGauche := ÉltGauche.Suivant;
	      ÉltDroite := ÉltDroite.Suivant;
	      MarqueSource := 1;
	    END IF;
    END LOOP;
    IF (Libre = Index) AND (Libre = Courant) THEN       -- égal
	    RETURN False;
    ELSIF (Libre = Index) AND (Index < Courant) THEN    -- inférieur
	    RETURN False;
    ELSIF (Libre = Courant) AND (Courant < Index) THEN  -- supérieur
	    RETURN True;
    ELSIF (Libre < Index) AND (Libre < Courant) 
	   AND (ÉltGauche.SuiteCar(MarqueSource) < 
	        ÉltDroite.SuiteCar(MarqueSource)) THEN
	    RETURN False;                                     -- inférieur
    ELSE
	    RETURN True;                                      -- supérieur
    END IF;
  ELSIF (Index = 0) AND (Courant = 0) THEN              -- égal
    RETURN False;
  ELSIF Index = 0 THEN -- première chaîne vide: inférieur
    RETURN False;
  ELSE                 -- seconde chaîne vide: supérieur
    RETURN True;
  END IF;
END ">";

FUNCTION "="(Gauche, Droite: TypChaîne) RETURN Boolean IS
-- Comparaison de l'égalité de deux chaînes.
Index, Courant, Libre, MarqueSource: Natural;
ÉltGauche, ÉltDroite: PointeurBloc;
BEGIN
  Index := Longueur(Gauche);
  Courant := Longueur(Droite);
  IF (Index > 0) AND (Courant > 0) THEN
    ÉltGauche := Gauche.Début;
	  ÉltDroite := Droite.Début;
    Libre := 1;         -- indice général
    MarqueSource := 1;  -- indice bloc
    WHILE (Libre <= Index) AND THEN (Libre <= Courant) -- deux chaînes non vides
	   AND THEN (ÉltGauche.SuiteCar(MarqueSource) = 
	             ÉltDroite.SuiteCar(MarqueSource)) LOOP
	    Libre := Libre + 1; MarqueSource := MarqueSource + 1;
	    IF (Libre <= Index) AND (Libre <= Courant) AND 
	       (MarqueSource > LongueurBloc) THEN  -- prochain bloc
	      ÉltGauche := ÉltGauche.Suivant;
	      ÉltDroite := ÉltDroite.Suivant;
	      MarqueSource := 1;
	    END IF;
    END LOOP;
    IF (Libre = Index+1) AND (Libre = Courant+1) THEN  -- égal
	    RETURN True;
    ELSE
	    RETURN False;                                    -- différent
    END IF;
  ELSIF (Index = 0) AND (Courant = 0) THEN             -- égal
    RETURN True;
  ELSE                                                 -- différent
    RETURN False;
  END IF;
END "=";

FUNCTION Élément(Source: TypChaîne; Index: Natural) RETURN Character IS
-- Extrait un caractère d'une chaîne.
Indice: Natural;
ÉltNouveau: PointeurBloc;
BEGIN
  IF (Index > 0) AND (Index <= Longueur(Source)) THEN
    Indice := Index;
	  ÉltNouveau := Source.Début;
    WHILE Indice > LongueurBloc LOOP     -- trouver bloc
	    ÉltNouveau := ÉltNouveau.Suivant;
	    Indice := Indice - LongueurBloc;
    END LOOP;
    RETURN ÉltNouveau.SuiteCar(Indice);
  ELSE
    RAISE Erreur_Indice;                 -- en dehors
  END IF;
END Élément;

PROCEDURE RemplacerÉlément(Car: IN Character; 
                           Source: IN OUT TypChaîne; Index: IN Natural) IS
-- Range un caractère dans une chaîne.
Courant, Nouvelle, Dernier: PointeurBloc;
Indice: Natural;
BEGIN
  Indice := Index;
  IF (Indice > 0) AND (Indice <= Longueur(Source)) THEN  -- changer
    Courant := Source.Début;
    WHILE Indice > LongueurBloc LOOP     -- trouver bloc
	    Courant := Courant.Suivant;
	    Indice := Indice - LongueurBloc;
    END LOOP;
    Courant.SuiteCar(Indice) := Car;
  ELSIF Indice = Longueur(Source)+1 THEN -- ajouter un caractère à la fin
    Courant := Source.Début;
    WHILE Indice > LongueurBloc LOOP     -- trouver dernier bloc
	    Dernier := Courant;
	    Courant := Courant.Suivant;
	    Indice := Indice - LongueurBloc;
    END LOOP;
    IF Courant /= NULL THEN              -- dans dernier bloc
	    Courant.SuiteCar(Indice) := Car;
	    IF Indice < LongueurBloc THEN
	      Courant.SuiteCar(Indice+1) := FinDeChaîne;
	    END IF;
    ELSE                                -- ajouter nouveau bloc
	    Nouvelle := NEW Bloc;
	    Nouvelle.Suivant := NULL;
	    Nouvelle.SuiteCar(Indice) := Car;
	    Nouvelle.SuiteCar(Indice+1) := FinDeChaîne;
	    IF Source.Début = NULL THEN       -- premier et dernier bloc
	      Source.Début := Nouvelle;
	    ELSIF Courant = NULL THEN         -- lien au dernier bloc plein
	      Dernier.Suivant := Nouvelle;
	    END IF;
    END IF;
    Source.Longueur := Source.Longueur + 1;
  END IF;
END RemplacerÉlément;

FUNCTION À_Statique(Chaîne: TypChaîne) RETURN String IS
-- Conversion d'une chaîne dynamique à une chaîne statique de type String.
NouveauBloc: PointeurBloc;
Index, IndiceBloc: Natural;
Résultat: String(1..MaxChaîne);
BEGIN
    NouveauBloc := Chaîne.Début;
    Index := 1; IndiceBloc := 1;
    WHILE Index <= Chaîne.Longueur LOOP
      Résultat(Index) := NouveauBloc.SuiteCar(IndiceBloc);
      Index := Index + 1; IndiceBloc := IndiceBloc + 1;
      IF (IndiceBloc > LongueurBloc) AND (Index <= Chaîne.Longueur) THEN
	      NouveauBloc := NouveauBloc.Suivant;
	      IndiceBloc := 1;
      END IF;
    END LOOP;
    RETURN Résultat(1..Chaîne.Longueur);
END À_Statique;
  
FUNCTION À_Dynamique(Source: String) RETURN TypChaîne IS
-- Conversion du type statique String au type chaîne dynamique. 
NouveauBloc: PointeurBloc;
Index, IndiceBloc: Natural;
Destination: TypChaîne;
BEGIN
  NouveauBloc := NEW Bloc;
  Destination.Début := NouveauBloc;
  Destination.Longueur := Source'Last;
  Index := 1; IndiceBloc := 1;
  WHILE Index <= Source'Last LOOP
    NouveauBloc.SuiteCar(IndiceBloc) := Source(Index);
    Index := Index + 1; IndiceBloc := IndiceBloc + 1;
    IF (IndiceBloc > LongueurBloc) AND (Index <= Source'Last) THEN
      NouveauBloc.Suivant := NEW Bloc;
	    NouveauBloc := NouveauBloc.Suivant;
	    IndiceBloc := 1;
    END IF;
  END LOOP;
  IF IndiceBloc <= LongueurBloc THEN
    NouveauBloc.SuiteCar(IndiceBloc) := FinDeChaîne;
  END IF;
  RETURN Destination;
END À_Dynamique;

PROCEDURE Initialize(Chaîne: IN OUT TypChaîne) IS
BEGIN
  NULL;
END Initialize;

PROCEDURE Finalize(Chaîne: IN OUT TypChaîne) IS
Blocs, ÀLibérer: PointeurBloc;
BEGIN
  Blocs := Chaîne.Début;
  WHILE Blocs /= NULL LOOP -- suivre la liste des blocs
	  ÀLibérer := Blocs;
	  Blocs := Blocs.Suivant;
    Libérer(ÀLibérer);
  END LOOP;
  Chaîne.Début := NULL;
  Chaîne.Longueur := 0;
END Finalize;

PROCEDURE Adjust(Chaîne: IN OUT TypChaîne) IS
Copie, Blocs: PointeurBloc;
BEGIN
  Blocs := Chaîne.Début; -- début liste de blocs à copier
  IF Blocs /= NULL THEN
    Copie := NEW Bloc'(Blocs.ALL);
    Chaîne.Début := Copie;
    WHILE Blocs.Suivant /= NULL LOOP -- copier les blocs
	    Blocs := Blocs.Suivant;
	    Copie.Suivant := NEW Bloc'(Blocs.ALL);
      Copie := Copie.Suivant;
    END LOOP;
  END IF;
END Adjust;

END ChaînesD;


