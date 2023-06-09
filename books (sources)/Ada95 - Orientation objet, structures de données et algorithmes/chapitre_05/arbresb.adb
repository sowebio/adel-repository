--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ArbresB IS

PROCEDURE Libérer IS NEW Ada.Unchecked_Deallocation(TypeNoeud, ArbreB);

PROCEDURE ChercherDansNoeud(Noeud: IN ArbreB; Élément: IN TypeÉlément;
                            Pos: IN OUT Natural; Succès: OUT BOOLEAN) IS
-- Retourne Vrai si la clef d'Élément a été trouvée à la position Pos,
-- autrement la clef n'a pas été trouvée et Pos précède la position de
-- la clef.
Comparé: Integer;
BEGIN
  Pos := 1;
  LOOP
    Comparé := Comparaison(Élément, Noeud.Items(Pos).Élément);
    IF Comparé = 0 THEN
      Succès := True;               -- trouvé
      RETURN;
    ELSIF Comparé = -1 THEN
      Pos := Pos - 1;
      Succès := False;              -- inférieur
      RETURN;
    ELSE                            -- Comparé = 1  supérieur
      IF Pos = Noeud.Dernier THEN
        Succès := False;            -- au delà du dernier noeud
        RETURN;
      ELSE
        Pos := Pos + 1;
      END IF;                       -- essayer prochaine clef
    END IF;
  END LOOP;
END ChercherDansNoeud;

PROCEDURE DétruireArbre(Arbre: IN OUT ArbreB) IS
-- Toute l'information de l'Arbre et les données qu'il contient 
-- sont supprimées.
BEGIN
  IF Arbre /= NULL THEN
    DétruireArbre(Arbre.PremierPointeur);     -- supprimer sous-arbres
    FOR Index IN 1..Arbre.Dernier LOOP
      DétruireArbre(Arbre.Items(Index).Pointeur);
    END LOOP;
    Libérer(Arbre);	-- relâcher le noeud
  END IF;
END DétruireArbre;

PROCEDURE InsérerNoeud(Arbre: IN OUT ArbreB;
                       Élément: IN TypeÉlément) IS
-- Insérer Élément dans Arbre; la clef se trouve dans Élément.

  PROCEDURE Placer(Noeud: IN ArbreB; CetÉlément: IN TypeÉlément; Renvoi: OUT Boolean;
                   NouvelleDonnée: IN OUT Item) IS
  -- Trouver l'endroit où inserer CetÉlément et appeler RangerEtDiviser pour le faire
  -- effectivement en indiquant s'il faut répercuter les changements.
  
    PROCEDURE RangerEtDiviser(CeNoeud: IN ArbreB; Encore: OUT Boolean;
                              Position: IN Natural; Donnée: IN Item) IS
    -- Insérer Donnée dans CeNoeud, en le divisant si nécessaire.
    NouveauNoeud: ArbreB;
    NouvellePosition: Natural;
    BEGIN
      IF CeNoeud.Dernier < ClefsMax THEN  -- il y a de la place dans CeNoeud
        CeNoeud.Dernier := CeNoeud.Dernier + 1;
        Encore := False;                  -- arrêter les frais
        FOR Index IN REVERSE Position+2..CeNoeud.Dernier LOOP -- décaler
          CeNoeud.Items(Index) := CeNoeud.Items(Index-1);
        END LOOP;
        CeNoeud.Items(Position+1) := Donnée;               -- insérer
      ELSE  
      -- CeNoeud plein, le diviser et placer élément milieu dans NouvelleDonnée
        NouveauNoeud := NEW TypeNoeud;    -- créer nouveau noeud
        IF Position <= ClefsMin THEN
          IF Position = ClefsMin THEN
            NouvelleDonnée := Donnée;                      -- élément milieu
          ELSE
            NouvelleDonnée := CeNoeud.Items(ClefsMin);     -- élément milieu
            FOR Index IN REVERSE Position+2..ClefsMin LOOP -- décaler à droite
              CeNoeud.Items(Index) := CeNoeud.Items(Index-1); 
              -- pour faire de la place
            END LOOP;
            CeNoeud.Items(Position+1) := Donnée;           -- insérer élément
          END IF;
          FOR Index IN 1..ClefsMin LOOP
            -- copier seconde moitié dans nouveau noeud
            NouveauNoeud.Items(Index) := CeNoeud.Items(Index+ClefsMin);
          END LOOP;
        ELSE  -- insérer Donnée dans nouveau noeud
          NouvellePosition := Position - ClefsMin;
          NouvelleDonnée := CeNoeud.Items(ClefsMin+1);     -- élément milieu
          FOR Index IN 1..NouvellePosition-1 LOOP   
            -- copier seconde moitié dans nouveau noeud
            NouveauNoeud.Items(Index) := CeNoeud.Items(Index+ClefsMin+1);
          END LOOP;
          NouveauNoeud.Items(NouvellePosition) := Donnée;  -- insérer élément
          FOR Index IN NouvellePosition+1..ClefsMin LOOP   -- finir copie
            NouveauNoeud.Items(Index) := CeNoeud.Items(Index+ClefsMin);
          END LOOP;
        END IF;
        CeNoeud.Dernier := ClefsMin;        -- deux noeuds à moitié pleins
        NouveauNoeud.Dernier := ClefsMin;
        NouveauNoeud.PremierPointeur := NouvelleDonnée.Pointeur;
        NouvelleDonnée.Pointeur := NouveauNoeud;
      END IF;
    END RangerEtDiviser;
	
  Pos: Natural;
  DonnéeCourante: Item;
  Trouvé: Boolean;
  BEGIN  -- Placer
    IF Noeud = NULL THEN   -- on a trouvé la feuille où placer Élément
      Renvoi := True;      -- renvoyer Élément vers le haut pour insertion
      NouvelleDonnée.Élément := CetÉlément;  -- créer élément
      NouvelleDonnée.Pointeur := NULL;
    ELSE
      ChercherDansNoeud(Noeud, CetÉlément, Pos, Trouvé);
      IF Trouvé THEN
        Renvoi := False;   -- Élément déjà dans Arbre
      ELSE                 -- trouver position dans niveaux inférieurs
        IF Pos = 0 THEN    -- Élément précède élément du premier noeud
          Placer(Noeud.PremierPointeur, CetÉlément, Renvoi, DonnéeCourante);
        ELSE               -- descendant suivant
          Placer(Noeud.Items(Pos).Pointeur, CetÉlément, Renvoi, DonnéeCourante);
        END IF;
        IF Renvoi THEN
          RangerEtDiviser(Noeud, Renvoi, Pos, DonnéeCourante);
        END IF;
      END IF;
    END IF;
  END Placer;

Racine: ArbreB;
NouvelleRacine: Boolean;
NouvelleDonnée: Item;
BEGIN -- InsérerNoeud
  Placer(Arbre, Élément, NouvelleRacine, NouvelleDonnée);
  IF NouvelleRacine THEN -- la racine a été divisée, en créer une nouvelle
    Racine := NEW TypeNoeud;
    Racine.Dernier := 1;
    Racine.PremierPointeur := Arbre;
    Racine.Items(1) := NouvelleDonnée;
    Arbre:=Racine;
  END IF;
END InsérerNoeud;

PROCEDURE Combiner(Parent, TropVide: IN OUT ArbreB; Pos: IN Natural;
                   TropPeu: OUT Boolean) IS
-- Le noeud TropVide sera fusionné avec un autre descendant de Parent.
-- Pos est l'élément du Parent qui pointe à TropVide.
Adjacent: ArbreB;   -- noeud adjacent
Nombre, DernierAdjacent, DernierParent, Position: Natural;
BEGIN
  Position := Pos;
  DernierParent := Parent.Dernier;
  IF Position < DernierParent THEN
    -- combiner avec le noeud adjacent à droite au noeud TropVide
    Position := Position + 1;
    Adjacent := Parent.Items(Position).Pointeur;
    DernierAdjacent := Adjacent.Dernier;
    -- calculer le nombre d'items disponibles dans le noeud adjacent
    Nombre := (DernierAdjacent - ClefsMin + 1) / 2;
    -- copier item du noeud Parent
    TropVide.Items(ClefsMin) := Parent.Items(Position);
    TropVide.Items(ClefsMin).Pointeur := Adjacent.PremierPointeur;
    IF Nombre > 0 THEN   -- déplacer Nombre items d'Adjacent à TropVide
      FOR Index IN 1..Nombre-1 LOOP
        TropVide.Items(Index+ClefsMin) := Adjacent.Items(Index);
      END LOOP;
      Parent.Items(Position) := Adjacent.Items(Nombre);
      Parent.Items(Position).Pointeur := Adjacent;
      Adjacent.PremierPointeur := Adjacent.Items(Nombre).Pointeur;
      DernierAdjacent := DernierAdjacent - Nombre;
      FOR Index IN 1..DernierAdjacent LOOP  
        -- décaler à gauche les éléments restants
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
      -- décaler à gauche les éléments de Parent
        Parent.Items(Index) := Parent.Items(Index+1);
      END LOOP;
      TropVide.Dernier := ClefsMax;
      Parent.Dernier := DernierParent - 1;
      Libérer(Adjacent);  -- relâcher Noeud
      TropPeu := Parent.Dernier < ClefsMin;
    END IF;
  ELSE     -- combiner avec le noeud adjacent à gauche à TropVide
    IF Position = 1 THEN
      Adjacent := Parent.PremierPointeur;
    ELSE
      Adjacent := Parent.Items(Position-1).Pointeur;
    END IF;
    DernierAdjacent := Adjacent.Dernier + 1;
    Nombre := (DernierAdjacent - ClefsMin) / 2;
    IF Nombre > 0 THEN   -- déplacer Nombre items d'Adjacent à TropVide
      FOR Index IN REVERSE 1..ClefsMin-1 LOOP  
        -- décaler à droite pour faire de la place
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
      Libérer(TropVide);   -- relâcher Noeud
      TropPeu := Parent.Dernier < ClefsMin;
    END IF;
  END IF;
END Combiner;
  
PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreB; Élément: IN TypeÉlément) IS
-- Trouver le noeud de l'arbre possédant la clef d'Élément et l'enlever.

  PROCEDURE Enlever(Noeud: IN OUT ArbreB; CetÉlément: IN TypeÉlément; TropPeu: OUT Boolean) IS
  -- Trouver l'endroit où enlever Noeud et l'éliminer directement ou en appelant Remplacer.
  
    PROCEDURE Remplacer(CeNoeud: IN OUT ArbreB; Position: IN Natural;
                        PositionNoeud: IN OUT ArbreB; Continuer: OUT Boolean) IS
    -- Suivre les pointeurs de droite et remplacer l'élément à supprimer par  
    -- le plus à droite de la feuille.
    Point: ArbreB;
    BEGIN
      Point := PositionNoeud.Items(PositionNoeud.Dernier).Pointeur;
      IF Point /= NULL THEN
        Remplacer(CeNoeud, Position, Point, Continuer);
        IF Continuer THEN
          Combiner(PositionNoeud, Point, PositionNoeud.Dernier, Continuer);
        END IF;
      ELSE  
        -- remplacer élément à supprimer par élément inférieur le plus proche
        PositionNoeud.Items(PositionNoeud.Dernier).Pointeur :=
                                             CeNoeud.Items(Position).Pointeur;
        CeNoeud.Items(Position) := PositionNoeud.Items(PositionNoeud.Dernier);
        PositionNoeud.Dernier := PositionNoeud.Dernier - 1;
        -- supprimer élément le plus à droite de la feuille
        Continuer := PositionNoeud.Dernier < ClefsMin;
      END IF;
    END Remplacer;
  
  Pos, Gauche: Natural;
  Suivant: ArbreB;
  Trouvé: Boolean;
  BEGIN -- Enlever
    IF Noeud = NULL THEN   -- pas dans l'Arbre
      TropPeu := False;
    ELSE
      ChercherDansNoeud(Noeud, CetÉlément, Pos, Trouvé);
      IF Trouvé THEN   -- trouvé
        Gauche := Pos - 1;
        IF Gauche = 0 THEN
          Suivant := Noeud.PremierPointeur;
        ELSE
          Suivant := Noeud.Items(Gauche).Pointeur;
        END IF;
        IF Suivant = NULL THEN   -- feuille
          Noeud.Dernier := Noeud.Dernier - 1;
          TropPeu := Noeud.Dernier < ClefsMin;
          FOR Index IN Pos..Noeud.Dernier LOOP  -- décaler éléments à gauche
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
        Enlever(Suivant, CetÉlément, TropPeu);   -- rechercher dans sous-arbre
        IF TropPeu THEN
          Combiner(Noeud, Suivant, Pos, TropPeu);
        END IF;
      END IF;
    END IF;
  END Enlever;

VieilleRacine: ArbreB;
TropPeu: Boolean;
BEGIN  -- SupprimerNoeud
  Enlever(Arbre, Élément, TropPeu);
  IF TropPeu THEN
    IF Arbre.Dernier = 0 THEN   -- enlever noeud vide
      VieilleRacine := Arbre;
      Arbre := VieilleRacine.PremierPointeur;
      Libérer(VieilleRacine);
    END IF;
  END IF;
END SupprimerNoeud;

PROCEDURE Traverser(Arbre: IN ArbreB; Proc_Traiter: IN Traitement) IS
-- Appliquer Proc_Traiter à chaque noeud de l'arbre dans l'ordre infixe. 
BEGIN
  IF Arbre /= NULL THEN
    Traverser(Arbre.PremierPointeur, Proc_Traiter);
    FOR Index IN 1..Arbre.Dernier LOOP
      Proc_Traiter(Arbre.Items(Index).Élément);
      Traverser(Arbre.Items(Index).Pointeur, Proc_Traiter);
    END LOOP;
  END IF;
END Traverser;

PROCEDURE Chercher(Arbre: IN ArbreB; Élément: IN OUT TypeÉlément;
                   Succès: OUT Boolean) IS
-- Recherche Élément dans l'Arbre. Si trouvé, Succès est Vrai et Élément
-- contient l'information du noeud, sinon Succès est Faux.
SousArbre: ArbreB;
Pos: Natural;
Trouvé: Boolean;
BEGIN   -- recherche non récursive efficace
  SousArbre := Arbre;
  LOOP
    IF SousArbre = NULL THEN  -- clef non trouvée
      Succès := False;
      RETURN;
    END IF;
    ChercherDansNoeud(SousArbre, Élément, Pos, Trouvé);
    IF Trouvé THEN  -- clef trouvée
      Élément := SousArbre.Items(Pos).Élément;
      EXIT;
    END IF;
    IF Pos = 0 THEN
      SousArbre := SousArbre.PremierPointeur;
    ELSE   -- essayer descendant
      SousArbre := SousArbre.Items(Pos).Pointeur;
    END IF;
  END LOOP;
  Succès := True;
END Chercher;


PROCEDURE AfficherArbre(Arbre: IN ArbreB; Décalage: IN Natural) IS
-- Afficher l'arbre avec décalages pour montrer la structure.
BEGIN
  IF Arbre /= NULL THEN
    Ada.Text_IO.New_Line;
    FOR Décal IN 1..Décalage LOOP
      Ada.Text_IO.Put(Item => "  ");
    END LOOP;
    FOR Index IN 1..Arbre.Dernier LOOP  -- données
      Ada.Text_IO.Put(Item => " #");
      AfficherÉlément(Arbre.Items(Index).Élément);
    END LOOP;
    AfficherArbre(Arbre.PremierPointeur, Décalage+1);  -- sous-arbres
    FOR Index IN 1..Arbre.Dernier LOOP
      AfficherArbre(Arbre.Items(Index).Pointeur, Décalage+1);
    END LOOP;
  END IF;
END AfficherArbre;

END ArbresB;

