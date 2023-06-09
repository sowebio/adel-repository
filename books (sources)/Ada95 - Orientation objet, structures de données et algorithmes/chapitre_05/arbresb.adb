--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ArbresB IS

PROCEDURE Lib�rer IS NEW Ada.Unchecked_Deallocation(TypeNoeud, ArbreB);

PROCEDURE ChercherDansNoeud(Noeud: IN ArbreB; �l�ment: IN Type�l�ment;
                            Pos: IN OUT Natural; Succ�s: OUT BOOLEAN) IS
-- Retourne Vrai si la clef d'�l�ment a �t� trouv�e � la position Pos,
-- autrement la clef n'a pas �t� trouv�e et Pos pr�c�de la position de
-- la clef.
Compar�: Integer;
BEGIN
  Pos := 1;
  LOOP
    Compar� := Comparaison(�l�ment, Noeud.Items(Pos).�l�ment);
    IF Compar� = 0 THEN
      Succ�s := True;               -- trouv�
      RETURN;
    ELSIF Compar� = -1 THEN
      Pos := Pos - 1;
      Succ�s := False;              -- inf�rieur
      RETURN;
    ELSE                            -- Compar� = 1  sup�rieur
      IF Pos = Noeud.Dernier THEN
        Succ�s := False;            -- au del� du dernier noeud
        RETURN;
      ELSE
        Pos := Pos + 1;
      END IF;                       -- essayer prochaine clef
    END IF;
  END LOOP;
END ChercherDansNoeud;

PROCEDURE D�truireArbre(Arbre: IN OUT ArbreB) IS
-- Toute l'information de l'Arbre et les donn�es qu'il contient 
-- sont supprim�es.
BEGIN
  IF Arbre /= NULL THEN
    D�truireArbre(Arbre.PremierPointeur);     -- supprimer sous-arbres
    FOR Index IN 1..Arbre.Dernier LOOP
      D�truireArbre(Arbre.Items(Index).Pointeur);
    END LOOP;
    Lib�rer(Arbre);	-- rel�cher le noeud
  END IF;
END D�truireArbre;

PROCEDURE Ins�rerNoeud(Arbre: IN OUT ArbreB;
                       �l�ment: IN Type�l�ment) IS
-- Ins�rer �l�ment dans Arbre; la clef se trouve dans �l�ment.

  PROCEDURE Placer(Noeud: IN ArbreB; Cet�l�ment: IN Type�l�ment; Renvoi: OUT Boolean;
                   NouvelleDonn�e: IN OUT Item) IS
  -- Trouver l'endroit o� inserer Cet�l�ment et appeler RangerEtDiviser pour le faire
  -- effectivement en indiquant s'il faut r�percuter les changements.
  
    PROCEDURE RangerEtDiviser(CeNoeud: IN ArbreB; Encore: OUT Boolean;
                              Position: IN Natural; Donn�e: IN Item) IS
    -- Ins�rer Donn�e dans CeNoeud, en le divisant si n�cessaire.
    NouveauNoeud: ArbreB;
    NouvellePosition: Natural;
    BEGIN
      IF CeNoeud.Dernier < ClefsMax THEN  -- il y a de la place dans CeNoeud
        CeNoeud.Dernier := CeNoeud.Dernier + 1;
        Encore := False;                  -- arr�ter les frais
        FOR Index IN REVERSE Position+2..CeNoeud.Dernier LOOP -- d�caler
          CeNoeud.Items(Index) := CeNoeud.Items(Index-1);
        END LOOP;
        CeNoeud.Items(Position+1) := Donn�e;               -- ins�rer
      ELSE  
      -- CeNoeud plein, le diviser et placer �l�ment milieu dans NouvelleDonn�e
        NouveauNoeud := NEW TypeNoeud;    -- cr�er nouveau noeud
        IF Position <= ClefsMin THEN
          IF Position = ClefsMin THEN
            NouvelleDonn�e := Donn�e;                      -- �l�ment milieu
          ELSE
            NouvelleDonn�e := CeNoeud.Items(ClefsMin);     -- �l�ment milieu
            FOR Index IN REVERSE Position+2..ClefsMin LOOP -- d�caler � droite
              CeNoeud.Items(Index) := CeNoeud.Items(Index-1); 
              -- pour faire de la place
            END LOOP;
            CeNoeud.Items(Position+1) := Donn�e;           -- ins�rer �l�ment
          END IF;
          FOR Index IN 1..ClefsMin LOOP
            -- copier seconde moiti� dans nouveau noeud
            NouveauNoeud.Items(Index) := CeNoeud.Items(Index+ClefsMin);
          END LOOP;
        ELSE  -- ins�rer Donn�e dans nouveau noeud
          NouvellePosition := Position - ClefsMin;
          NouvelleDonn�e := CeNoeud.Items(ClefsMin+1);     -- �l�ment milieu
          FOR Index IN 1..NouvellePosition-1 LOOP   
            -- copier seconde moiti� dans nouveau noeud
            NouveauNoeud.Items(Index) := CeNoeud.Items(Index+ClefsMin+1);
          END LOOP;
          NouveauNoeud.Items(NouvellePosition) := Donn�e;  -- ins�rer �l�ment
          FOR Index IN NouvellePosition+1..ClefsMin LOOP   -- finir copie
            NouveauNoeud.Items(Index) := CeNoeud.Items(Index+ClefsMin);
          END LOOP;
        END IF;
        CeNoeud.Dernier := ClefsMin;        -- deux noeuds � moiti� pleins
        NouveauNoeud.Dernier := ClefsMin;
        NouveauNoeud.PremierPointeur := NouvelleDonn�e.Pointeur;
        NouvelleDonn�e.Pointeur := NouveauNoeud;
      END IF;
    END RangerEtDiviser;
	
  Pos: Natural;
  Donn�eCourante: Item;
  Trouv�: Boolean;
  BEGIN  -- Placer
    IF Noeud = NULL THEN   -- on a trouv� la feuille o� placer �l�ment
      Renvoi := True;      -- renvoyer �l�ment vers le haut pour insertion
      NouvelleDonn�e.�l�ment := Cet�l�ment;  -- cr�er �l�ment
      NouvelleDonn�e.Pointeur := NULL;
    ELSE
      ChercherDansNoeud(Noeud, Cet�l�ment, Pos, Trouv�);
      IF Trouv� THEN
        Renvoi := False;   -- �l�ment d�j� dans Arbre
      ELSE                 -- trouver position dans niveaux inf�rieurs
        IF Pos = 0 THEN    -- �l�ment pr�c�de �l�ment du premier noeud
          Placer(Noeud.PremierPointeur, Cet�l�ment, Renvoi, Donn�eCourante);
        ELSE               -- descendant suivant
          Placer(Noeud.Items(Pos).Pointeur, Cet�l�ment, Renvoi, Donn�eCourante);
        END IF;
        IF Renvoi THEN
          RangerEtDiviser(Noeud, Renvoi, Pos, Donn�eCourante);
        END IF;
      END IF;
    END IF;
  END Placer;

Racine: ArbreB;
NouvelleRacine: Boolean;
NouvelleDonn�e: Item;
BEGIN -- Ins�rerNoeud
  Placer(Arbre, �l�ment, NouvelleRacine, NouvelleDonn�e);
  IF NouvelleRacine THEN -- la racine a �t� divis�e, en cr�er une nouvelle
    Racine := NEW TypeNoeud;
    Racine.Dernier := 1;
    Racine.PremierPointeur := Arbre;
    Racine.Items(1) := NouvelleDonn�e;
    Arbre:=Racine;
  END IF;
END Ins�rerNoeud;

PROCEDURE Combiner(Parent, TropVide: IN OUT ArbreB; Pos: IN Natural;
                   TropPeu: OUT Boolean) IS
-- Le noeud TropVide sera fusionn� avec un autre descendant de Parent.
-- Pos est l'�l�ment du Parent qui pointe � TropVide.
Adjacent: ArbreB;   -- noeud adjacent
Nombre, DernierAdjacent, DernierParent, Position: Natural;
BEGIN
  Position := Pos;
  DernierParent := Parent.Dernier;
  IF Position < DernierParent THEN
    -- combiner avec le noeud adjacent � droite au noeud TropVide
    Position := Position + 1;
    Adjacent := Parent.Items(Position).Pointeur;
    DernierAdjacent := Adjacent.Dernier;
    -- calculer le nombre d'items disponibles dans le noeud adjacent
    Nombre := (DernierAdjacent - ClefsMin + 1) / 2;
    -- copier item du noeud Parent
    TropVide.Items(ClefsMin) := Parent.Items(Position);
    TropVide.Items(ClefsMin).Pointeur := Adjacent.PremierPointeur;
    IF Nombre > 0 THEN   -- d�placer Nombre items d'Adjacent � TropVide
      FOR Index IN 1..Nombre-1 LOOP
        TropVide.Items(Index+ClefsMin) := Adjacent.Items(Index);
      END LOOP;
      Parent.Items(Position) := Adjacent.Items(Nombre);
      Parent.Items(Position).Pointeur := Adjacent;
      Adjacent.PremierPointeur := Adjacent.Items(Nombre).Pointeur;
      DernierAdjacent := DernierAdjacent - Nombre;
      FOR Index IN 1..DernierAdjacent LOOP  
        -- d�caler � gauche les �l�ments restants
        Adjacent.Items(Index) := Adjacent.Items(Index+Nombre);
      END LOOP;
      Adjacent.Dernier := DernierAdjacent;
      TropVide.Dernier := ClefsMin - 1 + Nombre;
      TropPeu := False;
    ELSE         -- fusionner les noeuds TropVide et Adjacent
      FOR Index IN 1..ClefsMin LOOP  -- copier Adjacent dans TropVide
        TropVide.Items(Index+ClefsMin) := Adjacent.Items(Index);
      END LOOP;
      FOR Index IN Position..DernierParent-1 LOOP 
      -- d�caler � gauche les �l�ments de Parent
        Parent.Items(Index) := Parent.Items(Index+1);
      END LOOP;
      TropVide.Dernier := ClefsMax;
      Parent.Dernier := DernierParent - 1;
      Lib�rer(Adjacent);  -- rel�cher Noeud
      TropPeu := Parent.Dernier < ClefsMin;
    END IF;
  ELSE     -- combiner avec le noeud adjacent � gauche � TropVide
    IF Position = 1 THEN
      Adjacent := Parent.PremierPointeur;
    ELSE
      Adjacent := Parent.Items(Position-1).Pointeur;
    END IF;
    DernierAdjacent := Adjacent.Dernier + 1;
    Nombre := (DernierAdjacent - ClefsMin) / 2;
    IF Nombre > 0 THEN   -- d�placer Nombre items d'Adjacent � TropVide
      FOR Index IN REVERSE 1..ClefsMin-1 LOOP  
        -- d�caler � droite pour faire de la place
        TropVide.Items(Index+Nombre) := TropVide.Items(Index);
      END LOOP;
      TropVide.Items(Nombre) := Parent.Items(Position); 
      -- descendre item Parent
      TropVide.Items(Nombre).Pointeur := TropVide.PremierPointeur;
      DernierAdjacent := DernierAdjacent - Nombre;
      FOR Index IN REVERSE 1..Nombre-1 LOOP   -- copier du noeud Adjacent
        TropVide.Items(Index) := Adjacent.Items(Index+DernierAdjacent);
      END LOOP;
      TropVide.PremierPointeur := Adjacent.Items(DernierAdjacent).Pointeur;
      Parent.Items(Position) := Adjacent.Items(DernierAdjacent);
      Parent.Items(Position).Pointeur := TropVide;
      Adjacent.Dernier := DernierAdjacent - 1;
      TropVide.Dernier := ClefsMin - 1 + Nombre;
      TropPeu := False;
    ELSE    -- fusionner les noeuds
      Adjacent.Items(DernierAdjacent) := Parent.Items(Position);
      Adjacent.Items(DernierAdjacent).Pointeur := TropVide.PremierPointeur;
      FOR Index IN 1..ClefsMin-1 LOOP   -- copier TropVide dans Adjacent
        Adjacent.Items(Index+DernierAdjacent) := TropVide.Items(Index);
      END LOOP;
      Adjacent.Dernier := ClefsMax;
      Parent.Dernier := DernierParent - 1;
      Lib�rer(TropVide);   -- rel�cher Noeud
      TropPeu := Parent.Dernier < ClefsMin;
    END IF;
  END IF;
END Combiner;
  
PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreB; �l�ment: IN Type�l�ment) IS
-- Trouver le noeud de l'arbre poss�dant la clef d'�l�ment et l'enlever.

  PROCEDURE Enlever(Noeud: IN OUT ArbreB; Cet�l�ment: IN Type�l�ment; TropPeu: OUT Boolean) IS
  -- Trouver l'endroit o� enlever Noeud et l'�liminer directement ou en appelant Remplacer.
  
    PROCEDURE Remplacer(CeNoeud: IN OUT ArbreB; Position: IN Natural;
                        PositionNoeud: IN OUT ArbreB; Continuer: OUT Boolean) IS
    -- Suivre les pointeurs de droite et remplacer l'�l�ment � supprimer par  
    -- le plus � droite de la feuille.
    Point: ArbreB;
    BEGIN
      Point := PositionNoeud.Items(PositionNoeud.Dernier).Pointeur;
      IF Point /= NULL THEN
        Remplacer(CeNoeud, Position, Point, Continuer);
        IF Continuer THEN
          Combiner(PositionNoeud, Point, PositionNoeud.Dernier, Continuer);
        END IF;
      ELSE  
        -- remplacer �l�ment � supprimer par �l�ment inf�rieur le plus proche
        PositionNoeud.Items(PositionNoeud.Dernier).Pointeur :=
                                             CeNoeud.Items(Position).Pointeur;
        CeNoeud.Items(Position) := PositionNoeud.Items(PositionNoeud.Dernier);
        PositionNoeud.Dernier := PositionNoeud.Dernier - 1;
        -- supprimer �l�ment le plus � droite de la feuille
        Continuer := PositionNoeud.Dernier < ClefsMin;
      END IF;
    END Remplacer;
  
  Pos, Gauche: Natural;
  Suivant: ArbreB;
  Trouv�: Boolean;
  BEGIN -- Enlever
    IF Noeud = NULL THEN   -- pas dans l'Arbre
      TropPeu := False;
    ELSE
      ChercherDansNoeud(Noeud, Cet�l�ment, Pos, Trouv�);
      IF Trouv� THEN   -- trouv�
        Gauche := Pos - 1;
        IF Gauche = 0 THEN
          Suivant := Noeud.PremierPointeur;
        ELSE
          Suivant := Noeud.Items(Gauche).Pointeur;
        END IF;
        IF Suivant = NULL THEN   -- feuille
          Noeud.Dernier := Noeud.Dernier - 1;
          TropPeu := Noeud.Dernier < ClefsMin;
          FOR Index IN Pos..Noeud.Dernier LOOP  -- d�caler �l�ments � gauche
            Noeud.Items(Index) := Noeud.Items(Index+1);
          END LOOP;
        ELSE     -- pas une feuille, trouver un remplacement dans une feuille
          Remplacer(Noeud, Pos, Suivant, TropPeu);
          IF TropPeu THEN
            Combiner(Noeud, Suivant, Gauche, TropPeu);
          END IF;
        END IF;
      ELSE  -- continuer recherche
        IF Pos = 0 THEN
          Suivant := Noeud.PremierPointeur;
        ELSE
          Suivant := Noeud.Items(Pos).Pointeur;
        END IF;
        Enlever(Suivant, Cet�l�ment, TropPeu);   -- rechercher dans sous-arbre
        IF TropPeu THEN
          Combiner(Noeud, Suivant, Pos, TropPeu);
        END IF;
      END IF;
    END IF;
  END Enlever;

VieilleRacine: ArbreB;
TropPeu: Boolean;
BEGIN  -- SupprimerNoeud
  Enlever(Arbre, �l�ment, TropPeu);
  IF TropPeu THEN
    IF Arbre.Dernier = 0 THEN   -- enlever noeud vide
      VieilleRacine := Arbre;
      Arbre := VieilleRacine.PremierPointeur;
      Lib�rer(VieilleRacine);
    END IF;
  END IF;
END SupprimerNoeud;

PROCEDURE Traverser(Arbre: IN ArbreB; Proc_Traiter: IN Traitement) IS
-- Appliquer Proc_Traiter � chaque noeud de l'arbre dans l'ordre infixe. 
BEGIN
  IF Arbre /= NULL THEN
    Traverser(Arbre.PremierPointeur, Proc_Traiter);
    FOR Index IN 1..Arbre.Dernier LOOP
      Proc_Traiter(Arbre.Items(Index).�l�ment);
      Traverser(Arbre.Items(Index).Pointeur, Proc_Traiter);
    END LOOP;
  END IF;
END Traverser;

PROCEDURE Chercher(Arbre: IN ArbreB; �l�ment: IN OUT Type�l�ment;
                   Succ�s: OUT Boolean) IS
-- Recherche �l�ment dans l'Arbre. Si trouv�, Succ�s est Vrai et �l�ment
-- contient l'information du noeud, sinon Succ�s est Faux.
SousArbre: ArbreB;
Pos: Natural;
Trouv�: Boolean;
BEGIN   -- recherche non r�cursive efficace
  SousArbre := Arbre;
  LOOP
    IF SousArbre = NULL THEN  -- clef non trouv�e
      Succ�s := False;
      RETURN;
    END IF;
    ChercherDansNoeud(SousArbre, �l�ment, Pos, Trouv�);
    IF Trouv� THEN  -- clef trouv�e
      �l�ment := SousArbre.Items(Pos).�l�ment;
      EXIT;
    END IF;
    IF Pos = 0 THEN
      SousArbre := SousArbre.PremierPointeur;
    ELSE   -- essayer descendant
      SousArbre := SousArbre.Items(Pos).Pointeur;
    END IF;
  END LOOP;
  Succ�s := True;
END Chercher;


PROCEDURE AfficherArbre(Arbre: IN ArbreB; D�calage: IN Natural) IS
-- Afficher l'arbre avec d�calages pour montrer la structure.
BEGIN
  IF Arbre /= NULL THEN
    Ada.Text_IO.New_Line;
    FOR D�cal IN 1..D�calage LOOP
      Ada.Text_IO.Put(Item => "  ");
    END LOOP;
    FOR Index IN 1..Arbre.Dernier LOOP  -- donn�es
      Ada.Text_IO.Put(Item => " #");
      Afficher�l�ment(Arbre.Items(Index).�l�ment);
    END LOOP;
    AfficherArbre(Arbre.PremierPointeur, D�calage+1);  -- sous-arbres
    FOR Index IN 1..Arbre.Dernier LOOP
      AfficherArbre(Arbre.Items(Index).Pointeur, D�calage+1);
    END LOOP;
  END IF;
END AfficherArbre;

END ArbresB;

