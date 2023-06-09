--          Copyright � 1998 Philippe J. Gabrini
-- R�alisation des arbres binaires de recherche au moyen
-- d'algorithmes r�cursifs.
--     Philippe Gabrini     Juin 1998
WITH Ada.Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ArbresBinRecherche IS

PROCEDURE Lib�rer IS NEW 
         Ada.Unchecked_Deallocation(NoeudArbre'CLASS, ArbreBinaireRecherche);

PROCEDURE SupprimerArbre(Arbre: IN OUT ArbreBinaireRecherche) IS
-- Toute l'information sur l'Arbre et les donn�es qu'il contient est d�truite.
BEGIN
  IF Arbre /= NULL THEN
    SupprimerArbre(Arbre.Gauche);
    SupprimerArbre(Arbre.Droite);
    Lib�rer(Arbre);
  END IF;
END SupprimerArbre;

PROCEDURE Ins�rerNoeud(Arbre: IN OUT ArbreBinaireRecherche;
                       �l�ment: IN Type�l�ment) IS
-- Ins�rer �l�ment � la position voulue dans Arbre selon la clef de
-- �l�ment.  Utiliser Comparaison pour la comparison des clefs.  S'il
-- existait d�j� un noeud avec la m�me clef, on lui donne la valeur �l�ment.
Diff: Integer;
BEGIN
  IF Arbre = NULL THEN
    Arbre := NEW NoeudArbre'(�l�ment, NULL, NULL);
  ELSE
    Diff := Comparaison(�l�ment, Arbre.�l�ment);
    IF Diff < 0 THEN
      Ins�rerNoeud(Arbre.Gauche, �l�ment);
    ELSIF Diff > 0 THEN
      Ins�rerNoeud(Arbre.Droite, �l�ment);
    ELSE -- d�j� dans Arbre mettre noeud � jour
      Arbre.�l�ment := �l�ment;
    END IF;
  END IF;
END Ins�rerNoeud;

PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreBinaireRecherche;
                         �l�ment: IN Type�l�ment) IS
-- Trouver le noeud avec la clef d'�l�ment et le supprimer de l'Arbre.
-- Utiliser Comparaison pour les comparaisons.  Si aucun noeud ne 
-- poss�de cette clef, Arbre demeure inchang�.

  PROCEDURE TrouverPr�d�cesseur(Arbre: IN ArbreBinaireRecherche;
                                Noeud: IN OUT ArbreBinaireRecherche) IS
  -- Trouver le noeud le plus � droite du sous-arbre gauche.
  BEGIN
    Noeud := Arbre.Gauche;
    WHILE Noeud.Droite /= NULL LOOP
      Noeud := Noeud.Droite;
    END LOOP;
  END TrouverPr�d�cesseur;

Diff: Integer;
Noeud: ArbreBinaireRecherche;
BEGIN
  IF Arbre /= NULL THEN
    Diff := Comparaison(Arbre.�l�ment, �l�ment);
    IF Diff = 0 THEN
      IF Arbre.Gauche = NULL THEN                    -- branche gauche vide
        Noeud := Arbre;
        Arbre := Arbre.Droite;
        Lib�rer(Noeud);
      ELSIF Arbre.Droite = NULL THEN                 -- branche droite vide
        Noeud := Arbre;
        Arbre := Arbre.Gauche;
        Lib�rer(Noeud);
      ELSE -- pas de branche vide, trouver pr�d�cesseur infixe
        TrouverPr�d�cesseur(Arbre, Noeud);
        Arbre.�l�ment := Noeud.�l�ment;              -- copier valeur
        SupprimerNoeud(Arbre.Gauche, Arbre.�l�ment); -- supprimer noeud copi�
      END IF;
    ELSIF Diff < 0 THEN
      SupprimerNoeud(Arbre.Gauche, �l�ment);
    ELSE
      SupprimerNoeud(Arbre.Droite, �l�ment);
    END IF;
  END IF;
END SupprimerNoeud;

PROCEDURE TraverserArbre(Arbre: IN ArbreBinaireRecherche;
                         Traiter: IN ProcTraitement) IS
-- Appliquer Traiter � chaque noeud de l'Arbre en ordre infixe.
BEGIN
  IF Arbre /= NULL THEN
    TraverserArbre(Arbre.Gauche, Traiter);
    Traiter(Arbre.�l�ment);
    TraverserArbre(Arbre.Droite, Traiter);
  END IF;
END TraverserArbre;

PROCEDURE Rechercher(Arbre: IN ArbreBinaireRecherche; �l�ment: IN OUT Type�l�ment;
                     Succ�s: OUT Boolean) IS
-- Rechercher un �l�ment dans Arbre identifi� par la clef d'�l�ment.  Utiliser 
-- Comparaison pour la comparaison des �l�ments et retourner �l�ment pris dans Arbre.
Diff: Integer;
BEGIN
  IF Arbre = NULL THEN
    Succ�s := False;
  ELSE
    Diff := Comparaison(�l�ment, Arbre.�l�ment);
    IF Diff = 0 THEN
      �l�ment := Arbre.�l�ment;
      Succ�s := True;
    ELSIF Diff < 0 THEN
      Rechercher(Arbre.Gauche, �l�ment, Succ�s);
    ELSE
      Rechercher(Arbre.Droite, �l�ment, Succ�s);
    END IF;
  END IF;
END Rechercher;

PROCEDURE AfficherArbre(Arbre: IN ArbreBinaireRecherche;
                        D�calage: IN Natural) IS
-- Afficher les clefs de l'Arbre avec d�calages en utilisant Afficher�l�ment 
-- pour montrer la structure de l'arbre.
BEGIN
  IF Arbre /= NULL THEN
    AfficherArbre(Arbre.Droite, D�calage + 1);
    FOR D�cale IN 1..D�calage LOOP
      Ada.Text_IO.Put(Item => "      ");
    END LOOP;
    Afficher�l�ment(Arbre.�l�ment);
    Ada.Text_IO.New_Line;
    AfficherArbre(Arbre.Gauche, D�calage + 1);
  END IF;
END AfficherArbre;

END ArbresBinRecherche;


