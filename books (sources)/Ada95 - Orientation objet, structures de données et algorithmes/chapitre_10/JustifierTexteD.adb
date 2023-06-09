--          Copyright © 1998 Philippe J. Gabrini
-- Réalisation des arbres binaires de recherche au moyen
-- d'algorithmes récursifs.
--     Philippe Gabrini     Juin 1998
WITH Ada.Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ArbresBinRecherche IS

PROCEDURE Libérer IS NEW 
         Ada.Unchecked_Deallocation(NoeudArbre'CLASS, ArbreBinaireRecherche);

PROCEDURE SupprimerArbre(Arbre: IN OUT ArbreBinaireRecherche) IS
-- Toute l'information sur l'Arbre et les données qu'il contient est détruite.
BEGIN
  IF Arbre /= NULL THEN
    SupprimerArbre(Arbre.Gauche);
    SupprimerArbre(Arbre.Droite);
    Libérer(Arbre);
  END IF;
END SupprimerArbre;

PROCEDURE InsérerNoeud(Arbre: IN OUT ArbreBinaireRecherche;
                       Élément: IN TypeÉlément) IS
-- Insérer Élément à la position voulue dans Arbre selon la clef de
-- Élément.  Utiliser Comparaison pour la comparison des clefs.  S'il
-- existait déjà un noeud avec la même clef, on lui donne la valeur Élément.
Diff: Integer;
BEGIN
  IF Arbre = NULL THEN
    Arbre := NEW NoeudArbre'(Élément, NULL, NULL);
  ELSE
    Diff := Comparaison(Élément, Arbre.Élément);
    IF Diff < 0 THEN
      InsérerNoeud(Arbre.Gauche, Élément);
    ELSIF Diff > 0 THEN
      InsérerNoeud(Arbre.Droite, Élément);
    ELSE -- déjà dans Arbre mettre noeud à jour
      Arbre.Élément := Élément;
    END IF;
  END IF;
END InsérerNoeud;

PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreBinaireRecherche;
                         Élément: IN TypeÉlément) IS
-- Trouver le noeud avec la clef d'Élément et le supprimer de l'Arbre.
-- Utiliser Comparaison pour les comparaisons.  Si aucun noeud ne 
-- possède cette clef, Arbre demeure inchangé.

  PROCEDURE TrouverPrédécesseur(Arbre: IN ArbreBinaireRecherche;
                                Noeud: IN OUT ArbreBinaireRecherche) IS
  -- Trouver le noeud le plus à droite du sous-arbre gauche.
  BEGIN
    Noeud := Arbre.Gauche;
    WHILE Noeud.Droite /= NULL LOOP
      Noeud := Noeud.Droite;
    END LOOP;
  END TrouverPrédécesseur;

Diff: Integer;
Noeud: ArbreBinaireRecherche;
BEGIN
  IF Arbre /= NULL THEN
    Diff := Comparaison(Arbre.Élément, Élément);
    IF Diff = 0 THEN
      IF Arbre.Gauche = NULL THEN                    -- branche gauche vide
        Noeud := Arbre;
        Arbre := Arbre.Droite;
        Libérer(Noeud);
      ELSIF Arbre.Droite = NULL THEN                 -- branche droite vide
        Noeud := Arbre;
        Arbre := Arbre.Gauche;
        Libérer(Noeud);
      ELSE -- pas de branche vide, trouver prédécesseur infixe
        TrouverPrédécesseur(Arbre, Noeud);
        Arbre.Élément := Noeud.Élément;              -- copier valeur
        SupprimerNoeud(Arbre.Gauche, Arbre.Élément); -- supprimer noeud copié
      END IF;
    ELSIF Diff < 0 THEN
      SupprimerNoeud(Arbre.Gauche, Élément);
    ELSE
      SupprimerNoeud(Arbre.Droite, Élément);
    END IF;
  END IF;
END SupprimerNoeud;

PROCEDURE TraverserArbre(Arbre: IN ArbreBinaireRecherche;
                         Traiter: IN ProcTraitement) IS
-- Appliquer Traiter à chaque noeud de l'Arbre en ordre infixe.
BEGIN
  IF Arbre /= NULL THEN
    TraverserArbre(Arbre.Gauche, Traiter);
    Traiter(Arbre.Élément);
    TraverserArbre(Arbre.Droite, Traiter);
  END IF;
END TraverserArbre;

PROCEDURE Rechercher(Arbre: IN ArbreBinaireRecherche; Élément: IN OUT TypeÉlément;
                     Succès: OUT Boolean) IS
-- Rechercher un Élément dans Arbre identifié par la clef d'Élément.  Utiliser 
-- Comparaison pour la comparaison des éléments et retourner Élément pris dans Arbre.
Diff: Integer;
BEGIN
  IF Arbre = NULL THEN
    Succès := False;
  ELSE
    Diff := Comparaison(Élément, Arbre.Élément);
    IF Diff = 0 THEN
      Élément := Arbre.Élément;
      Succès := True;
    ELSIF Diff < 0 THEN
      Rechercher(Arbre.Gauche, Élément, Succès);
    ELSE
      Rechercher(Arbre.Droite, Élément, Succès);
    END IF;
  END IF;
END Rechercher;

PROCEDURE AfficherArbre(Arbre: IN ArbreBinaireRecherche;
                        Décalage: IN Natural) IS
-- Afficher les clefs de l'Arbre avec décalages en utilisant AfficherÉlément 
-- pour montrer la structure de l'arbre.
BEGIN
  IF Arbre /= NULL THEN
    AfficherArbre(Arbre.Droite, Décalage + 1);
    FOR Décale IN 1..Décalage LOOP
      Ada.Text_IO.Put(Item => "      ");
    END LOOP;
    AfficherÉlément(Arbre.Élément);
    Ada.Text_IO.New_Line;
    AfficherArbre(Arbre.Gauche, Décalage + 1);
  END IF;
END AfficherArbre;

END ArbresBinRecherche;


