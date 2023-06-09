--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ArbresBinRechFilés IS
-- Ce module présente la réalisation d'arbres binaires de recherche qui
-- utilise un arbre doublement "filé".  L'utilisation de "fils" permet
-- non seulement de traverser l'arbre de façon itérative dans toutes
-- les directions, mais aussi de faire des traversées incrémentales,
-- un noeud à la fois.

PROCEDURE Libérer IS NEW
             Ada.Unchecked_Deallocation(NoeudArbre, Arbre_Bin_Rech_Filé);

FUNCTION ArbreVide(Arbre: Arbre_Bin_Rech_Filé) RETURN Boolean IS
-- Retourne Vrai si l'Arbre est vide, et Faux autrement.
BEGIN
  RETURN Arbre = NULL;
END ArbreVide;

PROCEDURE DétruireArbre(Arbre: IN OUT Arbre_Bin_Rech_Filé)IS
-- Détruit toute la structure d'Arbre.
BEGIN
  IF Arbre /= NULL THEN
    IF NOT Arbre.FilGauche THEN
      DétruireArbre(Arbre.Gauche);
    END IF;
    IF NOT Arbre.FilDroit THEN
      DétruireArbre(Arbre.Droite);
    END IF;
    Libérer(Arbre);
  END IF;
END DétruireArbre;

FUNCTION Prédécesseur(Arbre: Arbre_Bin_Rech_Filé)
                     RETURN Arbre_Bin_Rech_Filé IS
-- Retourne le prédécesseur d'un noeud donné d'Arbre.
Noeud: Arbre_Bin_Rech_Filé;
BEGIN
  IF Arbre = NULL THEN
    RETURN NULL;
  ELSE
    Noeud := Arbre.Gauche;
    IF NOT Arbre.FilGauche THEN
      WHILE NOT Noeud.FilDroit LOOP
        Noeud := Noeud.Droite;
      END LOOP;
    END IF;
    RETURN Noeud;
  END IF;
END Prédécesseur;

FUNCTION Successeur(Arbre: IN Arbre_Bin_Rech_Filé)
                   RETURN Arbre_Bin_Rech_Filé IS
-- Retourne le successeur d'un noeud donné d'Arbre.
Noeud: Arbre_Bin_Rech_Filé;
BEGIN
  IF Arbre = NULL THEN
    RETURN NULL;
  ELSE
    Noeud := Arbre.Droite;
    IF NOT Arbre.FilDroit THEN
      WHILE NOT Noeud.FilGauche LOOP
        Noeud := Noeud.Gauche;
      END LOOP;
    END IF;
    RETURN Noeud;
  END IF;
END Successeur;

FUNCTION Noeud(Arbre: IN Arbre_Bin_Rech_Filé; Clef: IN TypeÉlément)
               RETURN Arbre_Bin_Rech_Filé IS
-- Retourne un pointeur au noeud qui possède la Clef donnée; si la Clef n'est
-- pas dans Arbre, on retourne NULL.
Où: Arbre_Bin_Rech_Filé;
BEGIN
  Où := Arbre;
  IF (Où = NULL) OR ELSE ÉlémentsÉgaux(Où.Élément, Clef) THEN
    RETURN Où;
  ELSIF Inférieur(Où.Élément, Clef) THEN
    IF Où.FilDroit THEN
      RETURN NULL;
    ELSE
      RETURN Noeud(Où.Droite, Clef);
    END IF;
  ELSE
    IF Où.FilGauche THEN
      RETURN NULL;
    ELSE
      RETURN Noeud(Où.Gauche, Clef);
    END IF;
  END IF;
END Noeud;

PROCEDURE InsérerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Filé;
                       Élément: IN TypeÉlément) IS
-- Insère Élément dans Arbre; si Élément existe déjà dans l'Arbre, ce dernier
-- n'est pas modifié. 
Parent: Arbre_Bin_Rech_Filé;
Trouvé: Boolean;

  PROCEDURE CréerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Filé;
                       Élt: IN TypeÉlément;
                       Gauche, Droite: IN Arbre_Bin_Rech_Filé) IS
  -- Crée le noeud Arbre avec les fils mis à Vrai
  BEGIN
    Arbre := NEW NoeudArbre;
    Arbre.Élément := Élt;
    Arbre.Gauche := Gauche;
    Arbre.Droite := Droite;
    Arbre.FilGauche := True;
    Arbre.FilDroit := True;
  END CréerNoeud;

BEGIN
  IF Arbre = NULL THEN
    CréerNoeud(Arbre, Élément, NULL, NULL);
  ELSE
    Trouvé := False;
    Parent := Arbre;
    WHILE NOT Trouvé LOOP 
      IF Inférieur(Élément, Parent.Élément) THEN -- regarder à gauche
        IF Parent.FilGauche  THEN
          -- insérer comme fils gauche de Parent
          Parent.FilGauche := False;
          CréerNoeud(Parent.Gauche, Élément, Parent.Gauche, Parent);
          Trouvé := True;
        ELSE -- continuer à chercher
          Parent := Parent.Gauche;
        END IF;
      ELSIF ÉlémentsÉgaux(Élément, Parent.Élément) THEN
        -- déjà dans Arbre
        Trouvé := True;
      ELSE       -- regarder à droite
        IF Parent.FilDroit THEN
          -- insérer comme fils droit de Parent
          Parent.FilDroit := False;
          CréerNoeud(Parent.Droite, Élément, Parent, Parent.Droite);
          Trouvé := True;
        ELSE
          Parent := Parent.Droite;
        END IF;
      END IF;
    END LOOP;
  END IF;
END InsérerNoeud;

PROCEDURE SupprimerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Filé;
                         Élément: IN TypeÉlément) IS
-- Supprime le noeud de l'Arbre ayant la même clef qu'Élément; s'il n'y a pas
-- de noeud possédant la même clef, Arbre n'est pas modifié.
 
  PROCEDURE SupprimerDescDroit(Parent: IN Arbre_Bin_Rech_Filé) IS
  -- Supprime le descendant droit du Parent.
  Courant, Droit, Précédent: Arbre_Bin_Rech_Filé;
  BEGIN
    NULL;
  END SupprimerDescDroit;
  
  PROCEDURE SupprimerDescGauche(Parent: IN Arbre_Bin_Rech_Filé) IS
  -- Supprime le descendant gauche du Parent. 
  Courant, Gauche, Précédent: Arbre_Bin_Rech_Filé;
  BEGIN
    NULL;
  END SupprimerDescGauche;
  
  PROCEDURE SupprimerRacine(Arbre: IN OUT Arbre_Bin_Rech_Filé) IS
  -- Supprime la racine de l'Arbre. 
  Courant, Précédent: Arbre_Bin_Rech_Filé;
  BEGIN
    NULL;
  END SupprimerRacine;
  
Parent, Noeud: Arbre_Bin_Rech_Filé;
Fait: Boolean := False;

BEGIN
  NULL;
END SupprimerNoeud;

PROCEDURE TraverserAvant(Arbre: IN Arbre_Bin_Rech_Filé) IS
-- Effectue une traversée infixe de l'Arbre et affiche les éléments en ordre  
-- ascendant des clefs; on utilise AfficherÉlément et on sépare les valeurs 
-- par un espace.
Courant: Arbre_Bin_Rech_Filé;
BEGIN
  IF Arbre /= NULL THEN
    Courant := Arbre;
    WHILE NOT Courant.FilGauche LOOP
     -- trouver le premier élément de la traversée
      Courant := Courant.Gauche;
    END LOOP;
    WHILE Courant /= NULL LOOP
      -- trouver les éléments suivants au moyen de Successeur
      AfficherÉlément(Courant.Élément); Ada.Text_IO.Put(Item => ' ');
      Courant := Successeur(Courant);
    END LOOP;
  ELSE
    Ada.Text_IO.Put(Item => "arbre vide"); Ada.Text_IO.New_Line;
  END IF;
END TraverserAvant;

PROCEDURE TraverserArrière(Arbre: IN Arbre_Bin_Rech_Filé) IS
-- Effectue une traversée infixe de l'Arbre et affiche les éléments en ordre  
-- descendant des clefs; on utilise AfficherÉlément et on sépare les valeurs 
-- par un espace.
Courant: Arbre_Bin_Rech_Filé;
BEGIN
  IF Arbre /= NULL THEN
    Courant := Arbre;
    WHILE NOT Courant.FilDroit LOOP
      Courant := Courant.Droite;
    END LOOP;
    WHILE Courant /= NULL LOOP
      AfficherÉlément(Courant.Élément); Ada.Text_IO.Put(Item => ' ');
      Courant := Prédécesseur(Courant);
    END LOOP;
  ELSE
    Ada.Text_IO.Put(Item =>" arbre vide "); Ada.Text_IO.New_Line;
  END IF;
END TraverserArrière;

PROCEDURE AfficherNoeud(Arbre: IN Arbre_Bin_Rech_Filé) IS
-- Affiche la valeur de l'élément au noeud Arbre.
BEGIN
  AfficherÉlément(Arbre.Élément);
END AfficherNoeud;

PROCEDURE AfficherArbre(Arbre: IN Arbre_Bin_Rech_Filé) IS
-- Affiche l'Arbre de gauche à droite avec la racine à gauche.

  PROCEDURE Afficher(Arbre: IN Arbre_Bin_Rech_Filé;
                     Décalage: IN Natural) IS
  -- Fait l'affichage de l'Arbre avec décalages.
  BEGIN
    IF Arbre /= NULL THEN
      IF NOT Arbre.FilDroit THEN
        Afficher(Arbre.Droite, Décalage+1);
      END IF;
      FOR Décal IN 1..Décalage LOOP
        Ada.Text_IO.Put(Item => "     ");
      END LOOP;
      AfficherÉlément(Arbre.Élément); Ada.Text_IO.New_Line;
      IF NOT Arbre.FilGauche THEN
        Afficher(Arbre.Gauche, Décalage+1);
      END IF;
    END IF;
  END Afficher;

BEGIN
  Afficher(Arbre, 0);
END AfficherArbre;

END ArbresBinRechFilés;

