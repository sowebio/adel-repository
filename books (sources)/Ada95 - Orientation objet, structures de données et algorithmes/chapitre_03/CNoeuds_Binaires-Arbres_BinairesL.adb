--          Copyright © 1998 Philippe J. Gabrini
-- Programmeurs:     Denis Mouraux et Philippe Gabrini
-- Date:             Juin 1998
--
-- Ce module définit le type Arbre_Binaire, qui permet de déclarer des
-- arbres binaires.

WITH Ada.Text_IO; -- Utilisé par Afficher_Arbre
PACKAGE BODY CNoeuds_Binaires.Arbres_BinairesL IS

PROCEDURE Vider_Arbre(Arbre: IN OUT Arbre_Binaire) IS
-- Détruit tous les descendants de l'Arbre.
BEGIN
  Arbre.Nombre_Éléments := 0;
  Arbre.Courant := NULL;
  Détruire_Noeud_Et_Descendants(Arbre.Racine);
END Vider_Arbre;

PROCEDURE Insérer_Noeud(Arbre: IN OUT Arbre_Binaire;
                        Élément: IN Type_Élément;
                        Où: IN Position) IS
-- Insère Élément dans l'Arbre à l'endroit indiqué par rapport
-- à l'élément courant.
Nouveau: Noeud_Binaire;
BEGIN
  CASE Où IS
    WHEN Racine => IF Arbre.Nombre_Éléments = 0 THEN
                     Nouveau := Nouveau_Noeud(Élément);
                     Arbre.Racine := Nouveau;
                     Arbre.Courant := Arbre.Racine;
                     Arbre.Nombre_Éléments := 1;
                   ELSE
                     RAISE Erreur_Arbre;
                   END IF;
    WHEN Gauche => IF Noeud_Existe(Arbre.Courant)
                      AND THEN NOT Noeud_Existe(Arbre.Courant, Gauche) THEN
                     Nouveau := Nouveau_Noeud(Élément);
                     Lier_Noeuds(Arbre.Courant, Nouveau, Gauche);
                     Arbre.Courant := Nouveau;
                     Arbre.Nombre_Éléments := Arbre.Nombre_Éléments + 1;
                   ELSE                                                
                     RAISE Erreur_Arbre;
                   END IF;
    WHEN Droite => IF Noeud_Existe(Arbre.Courant)
                      AND THEN NOT Noeud_Existe(Arbre.Courant, Droite) THEN
                     Nouveau := Nouveau_Noeud(Élément);
                     Lier_Noeuds(Arbre.Courant, Nouveau, Droite);
                     Arbre.Courant := Nouveau;
                     Arbre.Nombre_Éléments := Arbre.Nombre_Éléments + 1;
                   ELSE
                     RAISE Erreur_Arbre;
                   END IF;
    WHEN OTHERS => RAISE Erreur_Arbre;
  END CASE;
END Insérer_Noeud;

PROCEDURE Détruire_Courant(Arbre: IN OUT Arbre_Binaire) IS
-- Détruit le noeud courant de l'Arbre, sauf si c'est la racine.
Père, Fils_Gauche, Fils_Droite: Noeud_Binaire;
Côté: Endroit;
BEGIN
  IF Noeud_Existe(Arbre.Courant) 
     AND Arbre.Courant /= Arbre.Racine THEN
    Père := Noeud_Voisin(Arbre.Courant, Parent);
    Côté := Parenté(Père, Arbre.Courant);
    IF Côté /= Gauche AND Côté /= Droite THEN
      RAISE Erreur_Arbre;
    ELSIF NOT Noeud_Existe(Arbre.Courant, Gauche) THEN
      -- La branche de gauche est vide, on n'a qu'à raccorder la
      -- branche de droite au parent.
      Fils_Droite := Noeud_Voisin(Arbre.Courant, Droite);
      Détacher_Noeud(Arbre.Courant, Ici);
      IF Noeud_Existe(Fils_Droite) THEN
        -- Il y a un fils de droite.
        Lier_Noeuds(Père, Fils_Droite, Côté);
      END IF;
    ELSIF NOT Noeud_Existe(Arbre.Courant, Droite) THEN
      -- La branche de droite est vide, on n'a qu'à raccorder la
      -- branche de gauche au parent.
      Fils_Gauche := Noeud_Voisin(Arbre.Courant, Gauche);
      Détacher_Noeud(Arbre.Courant, Ici);
      -- On sait qu'il y a un fils de gauche, donc:
      Lier_Noeuds(Père, Fils_Gauche, Côté);
    ELSE
      -- Aucune des deux branches n'est vide.  On remplace donc le
      -- noeud courant par son fils de gauche, puis on raccorde son
      -- fils de droite à l'extrémité droite du fils de gauche (qui
      -- remplace maintenant son parent).
      Fils_Gauche := Noeud_Voisin(Arbre.Courant, Gauche);
      Fils_Droite := Noeud_Voisin (Arbre.Courant, Droite);
      Détacher_Noeud(Arbre.Courant, Ici);
      Lier_Noeuds(Père, Fils_Gauche, Côté);
      WHILE Noeud_Existe(Fils_Gauche, Droite) LOOP
        Fils_Gauche := Noeud_Voisin(Fils_Gauche, Droite);
      END LOOP;
      Lier_Noeuds(Fils_Gauche, Fils_Droite, Droite);
    END IF;
    Détruire_Noeud(Arbre.Courant);
    Arbre.Courant := NULL;
    Arbre.Nombre_Éléments := Arbre.Nombre_Éléments -1;
  ELSE
    RAISE Erreur_Arbre;
  END IF;
END Détruire_Courant;

PROCEDURE Traverser_Arbre(Arbre: IN Arbre_Binaire;
                          Mode: IN Type_Mode := Infixe;
                          Proc_Traiter: IN Traitement) IS
-- Parcourt l'Arbre, et applique la procédure générique Proc_Traiter
-- à chacun des Éléments. Le type de parcours est déterminé par
-- Mode (Préfixe, Infixe ou Suffixe).

  PROCEDURE Traverser_Préfixe(Noeud: IN Noeud_Binaire) IS
  Voisin: Noeud_Binaire;
  BEGIN
    IF Noeud_Existe(Noeud) THEN
      Proc_Traiter(Valeur_Noeud(Noeud));
      Voisin := Noeud_Voisin(Noeud, Gauche);
      Traverser_Préfixe(Voisin);
      Voisin := Noeud_Voisin(Noeud, Droite);
      Traverser_Préfixe(Voisin);
    END IF;
  END Traverser_Préfixe;

  PROCEDURE Traverser_Infixe(Noeud: IN Noeud_Binaire) IS
  Voisin: Noeud_Binaire;
  BEGIN
    IF Noeud_Existe(Noeud) THEN
      Voisin := Noeud_Voisin(Noeud, Gauche);
      Traverser_Infixe(Voisin);
      Proc_Traiter(Valeur_Noeud(Noeud));
      Voisin := Noeud_Voisin(Noeud, Droite);
      Traverser_Infixe(Voisin);
    END IF;
  END Traverser_Infixe;

  PROCEDURE Traverser_Suffixe(Noeud: IN Noeud_Binaire) IS
  Voisin: Noeud_Binaire;
  BEGIN
    IF Noeud_Existe(Noeud) THEN
      Voisin := Noeud_Voisin(Noeud, Gauche);
      Traverser_Suffixe(Voisin);
      Voisin := Noeud_Voisin(Noeud, Droite);
      Traverser_Suffixe(Voisin);
      Proc_Traiter(Valeur_Noeud(Noeud));
    END IF;
  END Traverser_Suffixe;
                     
BEGIN
  CASE Mode IS
    WHEN Préfixe => Traverser_Préfixe(Arbre.Racine);
    WHEN Infixe  => Traverser_Infixe(Arbre.Racine);
    WHEN Suffixe => Traverser_Suffixe(Arbre.Racine);
  END CASE;
END Traverser_Arbre;

PROCEDURE Chercher(Arbre: IN OUT Arbre_Binaire;
                   Élément: IN Type_Élément) IS
-- Cherche Élément dans l'Arbre.

  PROCEDURE Chercher_Préfixe(Arbre: IN OUT Arbre_Binaire;
                             Noeud: IN Noeud_Binaire;
                             Élément: IN Type_Élément;
                             Trouvé: IN OUT Boolean) IS
  BEGIN
    IF Noeud_Existe(Noeud) AND NOT Trouvé THEN
      IF Éléments_Égaux (Élément, Valeur_Noeud(Noeud)) THEN
        Arbre.Courant := Noeud;
        Trouvé := True;
      ELSE
        Chercher_Préfixe(Arbre, Noeud_Voisin(Noeud, Gauche), Élément, Trouvé);
        Chercher_Préfixe(Arbre, Noeud_Voisin(Noeud, Droite), Élément, Trouvé);
      END IF;
    END IF;
  END Chercher_Préfixe;

Trouvé : Boolean := False;
BEGIN -- Chercher
  Chercher_Préfixe(Arbre, Arbre.Racine, Élément, Trouvé);
  IF NOT Trouvé THEN
    Arbre.Courant := NULL;
  END IF;
END Chercher;

FUNCTION Noeud_Dans_Arbre(Arbre: Arbre_Binaire;
                          Noeud: Noeud_Binaire) RETURN Boolean IS
-- Vérifie si un Noeud appartient bien à un Arbre donné.

  PROCEDURE Chercher_Préfixe(Noeud: IN Noeud_Binaire;
                             Noeud_Cherché: IN Noeud_Binaire;
                             Trouvé: IN OUT Boolean) IS
  BEGIN
    IF Noeud_Existe(Noeud) AND NOT Trouvé THEN
      IF Noeud = Noeud_Cherché THEN
        Trouvé := True;
      ELSE
        Chercher_Préfixe(Noeud_Voisin(Noeud, Gauche), Noeud_Cherché, Trouvé);
        Chercher_Préfixe (Noeud_Voisin(Noeud, Droite), Noeud_Cherché, Trouvé);
      END IF;
    END IF;
  END Chercher_Préfixe;

Trouvé : Boolean := False;
BEGIN -- Noeud_Dans_Arbre
  IF Arbre.Nombre_Éléments = 0 OR NOT Noeud_Existe(Noeud) THEN
    RAISE Erreur_Arbre;
  ELSE
    Chercher_Préfixe(Arbre.Racine, Noeud, Trouvé);
    RETURN Trouvé;
  END IF;
END Noeud_Dans_Arbre;

PROCEDURE Déplacer_Courant(Arbre: IN OUT Arbre_Binaire;
                           Où: IN Position) IS
-- Déplace le noeud courant de l'Arbre vers l'endroit indiqué par Où.
BEGIN
  IF Où = Racine THEN
    Arbre.Courant := Arbre.Racine;
  ELSE
    IF Noeud_Existe(Arbre.Courant) THEN
      CASE Où IS
        WHEN Parent =>
          IF Noeud_Existe(Arbre.Courant, Parent) THEN
            Arbre.Courant := Noeud_Voisin(Arbre.Courant, Parent);
          ELSE
            RAISE Erreur_Arbre;
          END IF;
        WHEN Gauche =>
          IF Noeud_Existe(Arbre.Courant, Gauche) THEN
            Arbre.Courant := Noeud_Voisin(Arbre.Courant, Gauche);
          ELSE
            RAISE Erreur_Arbre;
          END IF;
        WHEN Droite =>
          IF Noeud_Existe(Arbre.Courant, Droite) THEN
            Arbre.Courant := Noeud_Voisin(Arbre.Courant, Droite);
          ELSE
            RAISE Erreur_Arbre;
          END IF;
        WHEN OTHERS =>
          NULL; -- Où = Pointeur
      END CASE;
    ELSE
      RAISE Erreur_Arbre;
    END IF;
  END IF;
END Déplacer_Courant;

PROCEDURE Modifier_Courant(Arbre: IN OUT Arbre_Binaire;
                           Élément: IN Type_Élément) IS
-- Change la valeur de l'élément au noeud courant pour la valeur de Élément.
BEGIN
  IF Noeud_Existe(Arbre.Courant) THEN
    Modifier_Noeud(Arbre.Courant, Élément);
  ELSE
    RAISE Erreur_Arbre;
  END IF;
END Modifier_Courant;

FUNCTION Valeur_Courante(Arbre: Arbre_Binaire) RETURN Type_Élément IS
-- Retourne la valeur du noeud courant de l'Arbre.
BEGIN
  IF Noeud_Existe(Arbre.Courant) THEN
    RETURN Valeur_Noeud(Arbre.Courant);
  ELSE
    RAISE Erreur_Arbre;
  END IF;
END Valeur_Courante;

FUNCTION Nombre_Noeuds(Arbre: Arbre_Binaire) RETURN Natural IS
-- Donne le nombre de noeuds dans l'Arbre.
BEGIN
  RETURN Arbre.Nombre_Éléments;
END Nombre_Noeuds;

PROCEDURE Afficher_Arbre(Arbre: IN Arbre_Binaire) IS
  -- Affiche le sous-arbre Courant avec décalages pour montrer sa structure
  -- hiérarchique en utilisant la procédure générique Afficher_Élément.  L'Arbre
  -- est affiché vers le côté.  La sortie se fait sur l'affichage courant.
  PROCEDURE Afficher_Noeuds(Noeud: IN Noeud_Binaire;
                            Décalages: IN Natural) IS
  BEGIN
    IF Noeud_Existe(Noeud) THEN
      Afficher_Noeuds(Noeud_Voisin(Noeud, Droite), Décalages + 1);
      FOR I IN 1..Décalages LOOP
        Ada.Text_IO.Put (' ');
      END LOOP;
      Afficher_Élément(Valeur_Noeud(Noeud));
      Ada.Text_IO.New_Line;
      Afficher_Noeuds(Noeud_Voisin(Noeud, Gauche), Décalages + 1);
    END IF;
    END Afficher_Noeuds;
    
BEGIN -- Afficher_Arbre
  Afficher_Noeuds(Arbre.Courant, 0);
END Afficher_Arbre;

FUNCTION Noeud_Courant(Arbre: Arbre_Binaire) RETURN Noeud_Binaire IS
-- Donne un pointeur vers le noeud courant.
BEGIN
  RETURN Arbre.Courant;
END Noeud_Courant;

PROCEDURE Positionner_Noeud_Courant(Arbre: IN OUT Arbre_Binaire;
                                    Noeud: IN Noeud_Binaire) IS
-- Met le noeud courant à Noeud.
BEGIN -- Choisir_Noeud_Courant
  IF Noeud_Dans_Arbre(Arbre, Noeud) THEN
    Arbre.Courant := Noeud;
  ELSE
    RAISE Erreur_Arbre;
  END IF;
END Positionner_Noeud_Courant;

FUNCTION Noeud_Existe(Arbre: Arbre_Binaire;
                      Où: Position) RETURN Boolean IS
-- Vérifie si un noeud existe dans l'Arbre à l'endroit indiqué par
-- rapport au noeud courant.
Où_Noeud : Endroit;
BEGIN
  IF Où = Racine THEN
    RETURN Noeud_Existe(Arbre.Racine, Ici);
  ELSE
    CASE Où IS
      WHEN Courant => Où_Noeud := Ici;
      WHEN Parent  => Où_Noeud := Parent;
      WHEN Gauche  => Où_Noeud := Gauche;
      WHEN Droite  => Où_Noeud := Droite;
      WHEN OTHERS  => NULL; -- Où = Racine est déjà traité au début du IF
    END CASE;
    RETURN Noeud_Existe(Arbre.Courant, Où_Noeud);
  END IF;
EXCEPTION -- Noeud_Existe
  WHEN Erreur_Noeud => RAISE Erreur_Arbre;
END Noeud_Existe;

PROCEDURE Initialize(A: IN OUT Arbre_Binaire) IS
BEGIN
  A.Courant := NULL;
  A.Racine := NULL;
  A.Nombre_Éléments := 0;
END Initialize;

PROCEDURE Finalize(A: IN OUT Arbre_Binaire) IS
BEGIN
  Vider_Arbre(A);
END Finalize;

END CNoeuds_Binaires.Arbres_BinairesL;
