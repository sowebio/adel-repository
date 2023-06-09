--          Copyright � 1998 Philippe J. Gabrini
WITH Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY Cha�nesD IS
-- R�alisation des cha�nes dynamiques.
PROCEDURE Lib�rer IS NEW Unchecked_Deallocation(Bloc, PointeurBloc);

FUNCTION Longueur(Cha�ne: TypCha�ne) RETURN Natural IS
BEGIN
  RETURN Cha�ne.Longueur;
END Longueur;

FUNCTION Cha�neVide RETURN TypCha�ne IS
Vide: TypCha�ne;
BEGIN
  RETURN Vide;
END Cha�neVide;

FUNCTION Tranche(Source: TypCha�ne; Bas: Positive;
                 Haut: Natural) RETURN TypCha�ne IS
-- Retourne une partie de la cha�ne Source.
Interm�diaire: TypCha�ne := Source;       -- faire une copie
BEGIN
 IF Haut <= Longueur(Interm�diaire) THEN	-- sous-cha�ne valide
   Supprimer(Interm�diaire, Haut+1, 
				       Longueur(Interm�diaire));  -- fin
   Supprimer(Interm�diaire, 1, Bas-1);		-- caract�res pr�c�dents
   RETURN Interm�diaire;
 ELSE
   RAISE Erreur_Indice;
 END IF;
END Tranche;

PROCEDURE Supprimer(Source: IN OUT TypCha�ne; De, �: IN Natural) IS
-- Supprimer une partie d'une cha�ne Source.
LongueurSource, Index, IndexFin: Natural;
D�part, Fin, Dernier: PointeurBloc;
BEGIN
  IF Source.D�but /= NULL THEN
    Index := De;
    LongueurSource := Source.Longueur;
    IndexFin := � + 1;
    IF (IndexFin <= LongueurSource+1) AND (� > 0) AND (Index > 0) THEN
	    D�part := Source.D�but;
	    WHILE Index > LongueurBloc LOOP 
        -- rechercher bloc d�part de la sous-cha�ne
	      D�part := D�part.Suivant;
	      Index := Index - LongueurBloc;
	    END LOOP;
	    IF IndexFin <= LongueurSource THEN 
        -- copier fin de cha�ne par dessus partie supprim�e
	      Fin := Source.D�but;
	      WHILE IndexFin > LongueurBloc LOOP -- trouver bloc fin de sous-cha�ne
	        Fin := Fin.Suivant;
	        IndexFin := IndexFin - LongueurBloc;
	      END LOOP;
	      FOR Courant IN 1..LongueurSource-� LOOP
	        -- copier fin cha�ne par dessus sous-cha�ne
	        D�part.SuiteCar(Index) := Fin.SuiteCar(IndexFin);
	        Index := Index + 1;
	        IF Index > LongueurBloc THEN    -- bloc suivant
		        Dernier := D�part;
		        D�part := D�part.Suivant;
		        Index := 1;
	        END IF;
	        IndexFin := IndexFin + 1;
	        IF IndexFin > LongueurBloc THEN -- bloc suivant
		        Fin := Fin.Suivant;
		        IndexFin := 1;
	        END IF;
	      END LOOP;
	    END IF;
	    FOR Courant IN Index..LongueurBloc LOOP -- remplir bloc de z�ros
	      D�part.SuiteCar(Courant) := FinDeCha�ne;
	    END LOOP;
	    Fin := D�part.Suivant;   -- premier bloc � lib�rer
	    D�part.Suivant := NULL;
	    WHILE Fin /= NULL LOOP   -- lib�rer tous les autres blocs
	      D�part := Fin.Suivant;
	      Lib�rer(Fin);
	      Fin := D�part;
      END LOOP;
      Source.Longueur := Source.Longueur - (� - De + 1);
    END IF;
  END IF;
END Supprimer;

PROCEDURE Ins�rer(Source: IN OUT TypCha�ne; Avant: IN Positive;
                  Nouvelle: IN TypCha�ne) IS
-- Ins�rer Nouvelle dans Source avant Avant.
IndexCourant, Libre, LongueurNouvelle, Blocs,
DernierIndex, NouvelIndex, Index: Natural;
Dernier, Nouveau, Courant, Arri�re, �ltNouveau: PointeurBloc;
BEGIN
  LongueurNouvelle := Nouvelle.Longueur;
  IF LongueurNouvelle > 0 THEN   -- insertion
    Index := Avant;
    IndexCourant := Longueur(Source);
    IF Index <= IndexCourant+1 THEN	-- insertion possible
	    IF Source.D�but = NULL THEN	  -- Source vide: allouer premier bloc
	      Source.D�but := NEW Bloc;
	      Source.D�but.Suivant := NULL;
	      Source.D�but.SuiteCar(1) := FinDeCha�ne;
	    END IF;
	    Dernier := Source.D�but;
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
	      Blocs := (LongueurNouvelle - Libre) / LongueurBloc; -- nombre de blocs � ajouter
	      IF (LongueurNouvelle-Libre) MOD LongueurBloc /= 0 THEN
		      Blocs := Blocs + 1;
	      END IF;
	      FOR Nombre IN 1..Blocs LOOP  -- ajouter de nouveaux bloics � Source
		      Nouveau.Suivant := NEW Bloc;
		      Nouveau := Nouveau.Suivant;
	      END LOOP;
	    END IF;
	    NouvelIndex := (IndexCourant + LongueurNouvelle) MOD LongueurBloc + 1;
	    IF NouvelIndex /= 1 THEN
	      -- dernier bloc non plein: marquer fin de cha�ne
	      Nouveau.SuiteCar(NouvelIndex) := FinDeCha�ne;
		    NouvelIndex := NouvelIndex - 1;
	    END IF;
	    FOR Nombre IN 1..IndexCourant-Index+1 LOOP
	      -- d�placer fin de cha�ne Source pour faire de la place pour insertion
	      Nouveau.SuiteCar(NouvelIndex) := Dernier.SuiteCar(DernierIndex);
	      IF NouvelIndex = 1 THEN      -- bloc pr�c�dent
		      Arri�re := Source.D�but;
		      WHILE Arri�re.Suivant /= Nouveau LOOP  -- trouver bloc pr�c�dent
		        Arri�re := Arri�re.Suivant;
		      END LOOP;
		      Nouveau := Arri�re;
		      NouvelIndex := LongueurBloc;
	      ELSE
		      NouvelIndex := NouvelIndex - 1;         -- reculer
	      END IF;
	      IF DernierIndex = 1 THEN                  -- bloc pr�c�dent
		      Arri�re := Source.D�but;
		      WHILE (Arri�re /= NULL) AND THEN (Arri�re.Suivant /= Dernier) LOOP
		        Arri�re := Arri�re.Suivant;
		      END LOOP;
		      Dernier := Arri�re;
		      DernierIndex := LongueurBloc;
	      ELSE
		      DernierIndex := DernierIndex - 1;       -- reculer
	      END IF;
	    END LOOP;
	    Courant := Source.D�but;
	    WHILE Index > LongueurBloc LOOP  -- trouver bloc o� ins�rer
	      Courant := Courant.Suivant;
	      Index := Index - LongueurBloc;
	    END LOOP;
	    Libre := 1;
	    �ltNouveau := Nouvelle.D�but;
	    FOR Nombre IN 1..LongueurNouvelle LOOP	
        -- ins�rer LongueurNouvelle caract�res de nouveau
	      Courant.SuiteCar(Index) := �ltNouveau.SuiteCar(Libre);
	      Index := Index + 1;
	      IF Index > LongueurBloc THEN    -- bloc suivant
		      Index := 1;
		      Courant := Courant.Suivant;
	      END IF;
	      Libre := Libre + 1;
	      IF Libre > LongueurBloc THEN    -- bloc suivant
		      Libre := 1;
		      �ltNouveau := �ltNouveau.Suivant;
	      END IF;
      END LOOP;
      Source.Longueur := Source.Longueur + LongueurNouvelle;
    END IF;
  END IF;
END Ins�rer;

FUNCTION Position(Source, Patron: TypCha�ne) RETURN Natural IS
-- Recherche sous-cha�ne Patron dans cha�ne Source.
Index, Courant, Libre, MarqueSource: Natural;
LongueurPatron, LongueurSource, R�sultat, Ancien: Natural;
SousCha�ne, �ltSource, AncienBloc: PointeurBloc;
BEGIN
  LongueurPatron := Patron.Longueur;
  LongueurSource := Source.Longueur;
  R�sultat := 0;     -- r�sultat nul si pas trouv�
  IF LongueurPatron <= LongueurSource THEN
    SousCha�ne := Patron.D�but;
	  �ltSource := Source.D�but;
    Libre := 1;         -- indice g�n�ral sous-cha�ne 
	  Index := 1;         -- indice bloc sous-cha�ne
    MarqueSource := 1;  -- indice g�n�ral source
	  Courant := 1;       -- indice bloc source
    WHILE (Libre <= LongueurPatron) AND (MarqueSource <= LongueurSource) LOOP
      -- comparer caract�res
	    IF SousCha�ne.SuiteCar(Index) = �ltSource.SuiteCar(Courant) THEN
	      Libre := Libre + 1; Index := Index + 1;
	      IF Index > LongueurBloc THEN   -- prochain bloc sous-cha�ne
		      SousCha�ne := SousCha�ne.Suivant;
		      Index := 1;
	      END IF;
	      IF R�sultat = 0 THEN           -- r�sultat possible
		      R�sultat := MarqueSource;
		      Ancien := Courant;
          AncienBloc := �ltSource;
	      END IF;
	    ELSIF R�sultat /= 0 THEN         -- correspondance incompl�te
	      MarqueSource := R�sultat;      -- reculer indice
	      Courant := Ancien;
        �ltSource := AncienBloc;       -- reculer bloc
	      Libre := 1; Index := 1;
        SousCha�ne := Patron.D�but;    -- premier bloc patron
	      R�sultat := 0;
	    END IF;
	    MarqueSource := MarqueSource + 1;
      Courant := Courant + 1;
	    IF Courant > LongueurBloc THEN   -- prochain bloc Source
	      �ltSource := �ltSource.Suivant;
	      Courant := 1;
	    END IF;
    END LOOP;
    IF Libre <= LongueurPatron THEN    -- fin source mais pas fin patron
      R�sultat := 0;
    END IF;
  END IF;
  RETURN R�sultat;
END Position;

FUNCTION "&" (Gauche: TypCha�ne; Droite: TypCha�ne) RETURN TypCha�ne IS
-- Concat�ner cha�nes Gauche et Droite. 
LongueurGauche, LongueurDroite, Libre, MarqueSource: Natural;
Courant, �ltDroite: PointeurBloc;
Source: TypCha�ne;
BEGIN
  LongueurGauche := Longueur(Gauche);
  LongueurDroite := Longueur(Droite);
  IF LongueurGauche > 0 THEN    -- premi�re cha�ne non vide
    Source := Gauche;           -- copier Gauche
    IF LongueurDroite > 0 THEN  -- deuxi�me cha�ne non vide
	    Courant := Source.D�but;
	    �ltDroite := Droite.D�but;
	    WHILE Courant.Suivant /= NULL LOOP   -- trouver fin de Gauche
        LongueurGauche := LongueurGauche - LongueurBloc;
	      Courant := Courant.Suivant;
	    END LOOP;
	    Libre := 1; MarqueSource := 1;
      LongueurGauche := LongueurGauche + 1;   -- copier apr�s gauche
	    WHILE Libre <= LongueurDroite LOOP      -- copier caract�res Droite � la fin de Gauche
	      IF LongueurGauche > LongueurBloc THEN -- ajouter nouveau bloc
		      LongueurGauche := 1;
		      Courant.Suivant := NEW Bloc;
		      Courant := Courant.Suivant;
	      END IF;
	      Courant.SuiteCar(LongueurGauche) := �ltDroite.SuiteCar(MarqueSource);
	      Libre := Libre + 1;
        MarqueSource := MarqueSource + 1;
        LongueurGauche := LongueurGauche + 1;
	      IF MarqueSource > LongueurBloc THEN -- avancer au prochain bloc de Droite
		      �ltDroite := �ltDroite.Suivant;
		      MarqueSource := 1;
	      END IF;
	    END LOOP;
	    IF LongueurGauche <= LongueurBloc THEN
	      Courant.SuiteCar(LongueurGauche) := FinDeCha�ne; -- marquer fin de cha�ne
      END IF;
      Source.Longueur := Source.Longueur + LongueurDroite;
    END IF;
  ELSE
    Source := Droite;   -- comme Gauche est vide, copier Droite
  END IF;
  RETURN Source;
END "&";

FUNCTION "&" (Gauche: TypCha�ne; Droite: Character) RETURN TypCha�ne IS
-- Concat�ner cha�ne Gauche et caract�re Droite.
LongueurGauche: Natural;
Courant: PointeurBloc;
Source: TypCha�ne;
BEGIN
  LongueurGauche := Longueur(Gauche);
  IF LongueurGauche > 0 THEN    -- cha�ne non vide
    Source := Gauche;           -- copier Gauche
	  Courant := Source.D�but;
	  WHILE Courant.Suivant /= NULL LOOP      -- trouver fin de Gauche
      LongueurGauche := LongueurGauche - LongueurBloc;
	    Courant := Courant.Suivant;
	  END LOOP;
    LongueurGauche := LongueurGauche + 1;   -- copier apr�s gauche
	  IF LongueurGauche > LongueurBloc THEN   -- ajouter nouveau bloc
	    LongueurGauche := 1;
	    Courant.Suivant := NEW Bloc;
	    Courant := Courant.Suivant;
	  END IF;
	  Courant.SuiteCar(LongueurGauche) := Droite;
    LongueurGauche := LongueurGauche +1;
	  IF LongueurGauche <= LongueurBloc THEN
	    Courant.SuiteCar(LongueurGauche) := FinDeCha�ne; -- marquer fin de cha�ne
    END IF;
    Source.Longueur := Source.Longueur + 1;
  ELSE
    Source.D�but := NEW Bloc;            -- Gauche est vide, nouveau bloc 
    Source.D�but.SuiteCar(1) := Droite;  -- copier Droite
    Source.Longueur := 1;
  END IF;
  RETURN Source;
END "&";

FUNCTION "&" (Gauche: Character; Droite: TypCha�ne) RETURN TypCha�ne IS
-- Concat�ner caract�re Gauche et cha�ne Droite.
LongueurGauche, LongueurDroite, Libre, MarqueSource: Natural;
Courant, �ltDroite: PointeurBloc;
Source: TypCha�ne;
BEGIN
  LongueurDroite := Longueur(Droite);
  Source.D�but := NEW Bloc;   -- copier Gauche
  Source.D�but.SuiteCar(1) := Gauche;
  Source.Longueur := LongueurDroite + 1;
  LongueurGauche := 1;
  IF LongueurDroite > 0 THEN  -- deuxi�me cha�ne non vide
	  Courant := Source.D�but;
	  �ltDroite := Droite.D�but;
	  Libre := 1; MarqueSource := 1;
    LongueurGauche := LongueurGauche + 1;   -- copier apr�s gauche
	  WHILE Libre <= LongueurDroite LOOP      -- copier caract�res Droite � la fin de Gauche
	    IF LongueurGauche > LongueurBloc THEN -- ajouter nouveau bloc
		    LongueurGauche := 1;
		    Courant.Suivant := NEW Bloc;
		    Courant := Courant.Suivant;
	    END IF;
	    Courant.SuiteCar(LongueurGauche) := �ltDroite.SuiteCar(MarqueSource);
	    Libre := Libre + 1;
      MarqueSource := MarqueSource + 1;
      LongueurGauche := LongueurGauche + 1;
	    IF MarqueSource > LongueurBloc THEN -- avancer au prochain bloc de Droite
		    �ltDroite := �ltDroite.Suivant;
		    MarqueSource := 1;
	    END IF;
	  END LOOP;
	  IF LongueurGauche <= LongueurBloc THEN
	    Courant.SuiteCar(LongueurGauche) := FinDeCha�ne; -- marquer fin de cha�ne
    END IF;
  END IF;
  RETURN Source;
END "&";

FUNCTION "&" (Gauche: TypCha�ne; Droite: String) RETURN TypCha�ne IS
-- Concat�ner cha�ne Gauche et cah�ne statique Droite.
LongueurGauche, LongueurDroite, Libre: Natural;
Courant: PointeurBloc;
Source: TypCha�ne;
BEGIN
  LongueurGauche := Longueur(Gauche);
  LongueurDroite := Droite'Last;
  Source := Gauche;               -- copier Gauche
  IF LongueurDroite > 0 THEN
    IF LongueurGauche > 0 THEN    -- premi�re cha�ne non vide
	    Courant := Source.D�but;
	    WHILE Courant.Suivant /= NULL LOOP   -- trouver fin de Gauche
        LongueurGauche := LongueurGauche - LongueurBloc;
	      Courant := Courant.Suivant;
      END LOOP;
    ELSE
      Source.D�but := NEW Bloc;
      Courant := Source.D�but;
    END IF;
	  Libre := 1;
    LongueurGauche := LongueurGauche + 1;   -- copier apr�s gauche
	  WHILE Libre <= LongueurDroite LOOP      -- copier caract�res Droite � la fin de Gauche
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
	    Courant.SuiteCar(LongueurGauche) := FinDeCha�ne; -- marquer fin de cha�ne
    END IF;
    Source.Longueur := Source.Longueur + LongueurDroite;
  END IF;
  RETURN Source;
END "&";

FUNCTION "&" (Gauche: String; Droite: TypCha�ne) RETURN TypCha�ne IS
-- Concat�ner cha�ne statique Gauche et cha�ne Droite.
LongueurSource, LongueurGauche, LongueurDroite, Libre, MarqueDroite: Natural;
Courant, �ltDroite: PointeurBloc;
Source: TypCha�ne;
BEGIN
  LongueurGauche := Gauche'Last;
  LongueurDroite := Longueur(Droite);
  IF LongueurGauche > 0 THEN    -- premi�re cha�ne non vide
    Source.D�but := NEW Bloc;   -- copier Gauche
	  Courant := Source.D�but;
  	Libre := 1;
    LongueurSource := 1;                    -- premier carcat�re
	  WHILE Libre <= LongueurGauche LOOP      -- copier caract�res Gauche
	    IF LongueurSource > LongueurBloc THEN -- ajouter nouveau bloc
		    LongueurSource := 1;
		    Courant.Suivant := NEW Bloc;
		    Courant := Courant.Suivant;
	    END IF;
	    Courant.SuiteCar(LongueurSource) := Gauche(Libre);
	    Libre := Libre + 1;
      LongueurSource := LongueurSource + 1;
	  END LOOP;
    IF LongueurDroite > 0 THEN  -- deuxi�me cha�ne non vide
	    �ltDroite := Droite.D�but;
	    Libre := 1; MarqueDroite := 1;
	    WHILE Libre <= LongueurDroite LOOP      -- copier caract�res Droite � la fin de Gauche
	      IF LongueurSource > LongueurBloc THEN -- ajouter nouveau bloc
		      LongueurSource := 1;
		      Courant.Suivant := NEW Bloc;
		      Courant := Courant.Suivant;
	      END IF;
	      Courant.SuiteCar(LongueurSource) := �ltDroite.SuiteCar(MarqueDroite);
	      Libre := Libre + 1;
        MarqueDroite := MarqueDroite + 1;
        LongueurSource := LongueurSource + 1;
	      IF MarqueDroite > LongueurBloc THEN -- avancer au prochain bloc de Droite
		      �ltDroite := �ltDroite.Suivant;
		      MarqueDroite := 1;
	      END IF;
	    END LOOP;
	    IF LongueurSource <= LongueurBloc THEN
	      Courant.SuiteCar(LongueurSource) := FinDeCha�ne; -- marquer fin de cha�ne
      END IF;
    END IF;
    Source.Longueur := LongueurGauche + LongueurDroite;
  ELSE
    Source := Droite;   -- comme Gauche est vide, copier Droite
  END IF;
  RETURN Source;
END "&";

FUNCTION ">"(Gauche, Droite: TypCha�ne) RETURN Boolean IS
-- Comparaison lexicale selon l'ensemble de caract�res.
Index, Courant, Libre, MarqueSource: Natural;
�ltGauche, �ltDroite: PointeurBloc;
BEGIN
  Index := Gauche.Longueur;
  Courant := Droite.Longueur;
  IF (Index > 0) AND (Courant > 0) THEN
    �ltGauche := Gauche.D�but;
	  �ltDroite := Droite.D�but;
    Libre := 1;         -- indice g�n�ral
    MarqueSource := 1;  -- indice bloc
    WHILE (Libre <= Index) AND THEN (Libre <= Courant) -- deux cha�nes non vides
	   AND THEN (�ltGauche.SuiteCar(MarqueSource) 
	             = �ltDroite.SuiteCar(MarqueSource)) LOOP
	    Libre := Libre + 1; MarqueSource := MarqueSource + 1;
	    IF (Libre <= Index) AND (Libre <= Courant) AND 
	       (MarqueSource > LongueurBloc) THEN  -- prochain bloc
	      �ltGauche := �ltGauche.Suivant;
	      �ltDroite := �ltDroite.Suivant;
	      MarqueSource := 1;
	    END IF;
    END LOOP;
    IF (Libre = Index) AND (Libre = Courant) THEN       -- �gal
	    RETURN False;
    ELSIF (Libre = Index) AND (Index < Courant) THEN    -- inf�rieur
	    RETURN False;
    ELSIF (Libre = Courant) AND (Courant < Index) THEN  -- sup�rieur
	    RETURN True;
    ELSIF (Libre < Index) AND (Libre < Courant) 
	   AND (�ltGauche.SuiteCar(MarqueSource) < 
	        �ltDroite.SuiteCar(MarqueSource)) THEN
	    RETURN False;                                     -- inf�rieur
    ELSE
	    RETURN True;                                      -- sup�rieur
    END IF;
  ELSIF (Index = 0) AND (Courant = 0) THEN              -- �gal
    RETURN False;
  ELSIF Index = 0 THEN -- premi�re cha�ne vide: inf�rieur
    RETURN False;
  ELSE                 -- seconde cha�ne vide: sup�rieur
    RETURN True;
  END IF;
END ">";

FUNCTION "="(Gauche, Droite: TypCha�ne) RETURN Boolean IS
-- Comparaison de l'�galit� de deux cha�nes.
Index, Courant, Libre, MarqueSource: Natural;
�ltGauche, �ltDroite: PointeurBloc;
BEGIN
  Index := Longueur(Gauche);
  Courant := Longueur(Droite);
  IF (Index > 0) AND (Courant > 0) THEN
    �ltGauche := Gauche.D�but;
	  �ltDroite := Droite.D�but;
    Libre := 1;         -- indice g�n�ral
    MarqueSource := 1;  -- indice bloc
    WHILE (Libre <= Index) AND THEN (Libre <= Courant) -- deux cha�nes non vides
	   AND THEN (�ltGauche.SuiteCar(MarqueSource) = 
	             �ltDroite.SuiteCar(MarqueSource)) LOOP
	    Libre := Libre + 1; MarqueSource := MarqueSource + 1;
	    IF (Libre <= Index) AND (Libre <= Courant) AND 
	       (MarqueSource > LongueurBloc) THEN  -- prochain bloc
	      �ltGauche := �ltGauche.Suivant;
	      �ltDroite := �ltDroite.Suivant;
	      MarqueSource := 1;
	    END IF;
    END LOOP;
    IF (Libre = Index+1) AND (Libre = Courant+1) THEN  -- �gal
	    RETURN True;
    ELSE
	    RETURN False;                                    -- diff�rent
    END IF;
  ELSIF (Index = 0) AND (Courant = 0) THEN             -- �gal
    RETURN True;
  ELSE                                                 -- diff�rent
    RETURN False;
  END IF;
END "=";

FUNCTION �l�ment(Source: TypCha�ne; Index: Natural) RETURN Character IS
-- Extrait un caract�re d'une cha�ne.
Indice: Natural;
�ltNouveau: PointeurBloc;
BEGIN
  IF (Index > 0) AND (Index <= Longueur(Source)) THEN
    Indice := Index;
	  �ltNouveau := Source.D�but;
    WHILE Indice > LongueurBloc LOOP     -- trouver bloc
	    �ltNouveau := �ltNouveau.Suivant;
	    Indice := Indice - LongueurBloc;
    END LOOP;
    RETURN �ltNouveau.SuiteCar(Indice);
  ELSE
    RAISE Erreur_Indice;                 -- en dehors
  END IF;
END �l�ment;

PROCEDURE Remplacer�l�ment(Car: IN Character; 
                           Source: IN OUT TypCha�ne; Index: IN Natural) IS
-- Range un caract�re dans une cha�ne.
Courant, Nouvelle, Dernier: PointeurBloc;
Indice: Natural;
BEGIN
  Indice := Index;
  IF (Indice > 0) AND (Indice <= Longueur(Source)) THEN  -- changer
    Courant := Source.D�but;
    WHILE Indice > LongueurBloc LOOP     -- trouver bloc
	    Courant := Courant.Suivant;
	    Indice := Indice - LongueurBloc;
    END LOOP;
    Courant.SuiteCar(Indice) := Car;
  ELSIF Indice = Longueur(Source)+1 THEN -- ajouter un caract�re � la fin
    Courant := Source.D�but;
    WHILE Indice > LongueurBloc LOOP     -- trouver dernier bloc
	    Dernier := Courant;
	    Courant := Courant.Suivant;
	    Indice := Indice - LongueurBloc;
    END LOOP;
    IF Courant /= NULL THEN              -- dans dernier bloc
	    Courant.SuiteCar(Indice) := Car;
	    IF Indice < LongueurBloc THEN
	      Courant.SuiteCar(Indice+1) := FinDeCha�ne;
	    END IF;
    ELSE                                -- ajouter nouveau bloc
	    Nouvelle := NEW Bloc;
	    Nouvelle.Suivant := NULL;
	    Nouvelle.SuiteCar(Indice) := Car;
	    Nouvelle.SuiteCar(Indice+1) := FinDeCha�ne;
	    IF Source.D�but = NULL THEN       -- premier et dernier bloc
	      Source.D�but := Nouvelle;
	    ELSIF Courant = NULL THEN         -- lien au dernier bloc plein
	      Dernier.Suivant := Nouvelle;
	    END IF;
    END IF;
    Source.Longueur := Source.Longueur + 1;
  END IF;
END Remplacer�l�ment;

FUNCTION �_Statique(Cha�ne: TypCha�ne) RETURN String IS
-- Conversion d'une cha�ne dynamique � une cha�ne statique de type String.
NouveauBloc: PointeurBloc;
Index, IndiceBloc: Natural;
R�sultat: String(1..MaxCha�ne);
BEGIN
    NouveauBloc := Cha�ne.D�but;
    Index := 1; IndiceBloc := 1;
    WHILE Index <= Cha�ne.Longueur LOOP
      R�sultat(Index) := NouveauBloc.SuiteCar(IndiceBloc);
      Index := Index + 1; IndiceBloc := IndiceBloc + 1;
      IF (IndiceBloc > LongueurBloc) AND (Index <= Cha�ne.Longueur) THEN
	      NouveauBloc := NouveauBloc.Suivant;
	      IndiceBloc := 1;
      END IF;
    END LOOP;
    RETURN R�sultat(1..Cha�ne.Longueur);
END �_Statique;
  
FUNCTION �_Dynamique(Source: String) RETURN TypCha�ne IS
-- Conversion du type statique String au type cha�ne dynamique. 
NouveauBloc: PointeurBloc;
Index, IndiceBloc: Natural;
Destination: TypCha�ne;
BEGIN
  NouveauBloc := NEW Bloc;
  Destination.D�but := NouveauBloc;
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
    NouveauBloc.SuiteCar(IndiceBloc) := FinDeCha�ne;
  END IF;
  RETURN Destination;
END �_Dynamique;

PROCEDURE Initialize(Cha�ne: IN OUT TypCha�ne) IS
BEGIN
  NULL;
END Initialize;

PROCEDURE Finalize(Cha�ne: IN OUT TypCha�ne) IS
Blocs, �Lib�rer: PointeurBloc;
BEGIN
  Blocs := Cha�ne.D�but;
  WHILE Blocs /= NULL LOOP -- suivre la liste des blocs
	  �Lib�rer := Blocs;
	  Blocs := Blocs.Suivant;
    Lib�rer(�Lib�rer);
  END LOOP;
  Cha�ne.D�but := NULL;
  Cha�ne.Longueur := 0;
END Finalize;

PROCEDURE Adjust(Cha�ne: IN OUT TypCha�ne) IS
Copie, Blocs: PointeurBloc;
BEGIN
  Blocs := Cha�ne.D�but; -- d�but liste de blocs � copier
  IF Blocs /= NULL THEN
    Copie := NEW Bloc'(Blocs.ALL);
    Cha�ne.D�but := Copie;
    WHILE Blocs.Suivant /= NULL LOOP -- copier les blocs
	    Blocs := Blocs.Suivant;
	    Copie.Suivant := NEW Bloc'(Blocs.ALL);
      Copie := Copie.Suivant;
    END LOOP;
  END IF;
END Adjust;

END Cha�nesD;


