--          Copyright � 1998 Philippe J. Gabrini
-- Programmeurs:     Denis Mouraux et Philippe Gabrini
-- Date:             Juin 1998
--
-- Ce module d�finit le type Arbre_Binaire, qui permet de d�clarer des
-- arbres binaires.

WITH Ada.Text_IO; -- Utilis� par Afficher_Arbre
PACKAGE BODY CNoeuds_Binaires.Arbres_BinairesL IS

PROCEDURE Vider_Arbre(Arbre: IN OUT Arbre_Binaire) IS
-- D�truit tous les descendants de l'Arbre.
BEGIN
  Arbre.Nombre_�l�ments := 0;
  Arbre.Courant := NULL;
  D�truire_Noeud_Et_Descendants(Arbre.Racine);
END Vider_Arbre;

PROCEDURE Ins�rer_Noeud(Arbre: IN OUT Arbre_Binaire;
                        �l�ment: IN Type_�l�ment;
                        O�: IN Position) IS
-- Ins�re �l�ment dans l'Arbre � l'endroit indiqu� par rapport
-- � l'�l�ment courant.
Nouveau: Noeud_Binaire;
BEGIN
  CASE O� IS
    WHEN Racine => IF Arbre.Nombre_�l�ments = 0 THEN
                     Nouveau := Nouveau_Noeud(�l�ment);
                     Arbre.Racine := Nouveau;
                     Arbre.Courant := Arbre.Racine;
                     Arbre.Nombre_�l�ments := 1;
                   ELSE
                     RAISE Erreur_Arbre;
                   END IF;
    WHEN Gauche => IF Noeud_Existe(Arbre.Courant)
                      AND THEN NOT Noeud_Existe(Arbre.Courant, Gauche) THEN
                     Nouveau := Nouveau_Noeud(�l�ment);
                     Lier_Noeuds(Arbre.Courant, Nouveau, Gauche);
                     Arbre.Courant := Nouveau;
                     Arbre.Nombre_�l�ments := Arbre.Nombre_�l�ments + 1;
                   ELSE                                                
                     RAISE Erreur_Arbre;
                   END IF;
    WHEN Droite => IF Noeud_Existe(Arbre.Courant)
                      AND THEN NOT Noeud_Existe(Arbre.Courant, Droite) THEN
                     Nouveau := Nouveau_Noeud(�l�ment);
                     Lier_Noeuds(Arbre.Courant, Nouveau, Droite);
                     Arbre.Courant := Nouveau;
                     Arbre.Nombre_�l�ments := Arbre.Nombre_�l�ments + 1;
                   ELSE
                     RAISE Erreur_Arbre;
                   END IF;
    WHEN OTHERS => RAISE Erreur_Arbre;
  END CASE;
END Ins�rer_Noeud;

PROCEDURE D�truire_Courant(Arbre: IN OUT Arbre_Binaire) IS
-- D�truit le noeud courant de l'Arbre, sauf si c'est la racine.
P�re, Fils_Gauche, Fils_Droite: Noeud_Binaire;
C�t�: Endroit;
BEGIN
  IF Noeud_Existe(Arbre.Courant) 
     AND Arbre.Courant /= Arbre.Racine THEN
    P�re := Noeud_Voisin(Arbre.Courant, Parent);
    C�t� := Parent�(P�re, Arbre.Courant);
    IF C�t� /= Gauche AND C�t� /= Droite THEN
      RAISE Erreur_Arbre;
    ELSIF NOT Noeud_Existe(Arbre.Courant, Gauche) THEN
      -- La branche de gauche est vide, on n'a qu'� raccorder la
      -- branche de droite au parent.
      Fils_Droite := Noeud_Voisin(Arbre.Courant, Droite);
      D�tacher_Noeud(Arbre.Courant, Ici);
      IF Noeud_Existe(Fils_Droite) THEN
        -- Il y a un fils de droite.
        Lier_Noeuds(P�re, Fils_Droite, C�t�);
      END IF;
    ELSIF NOT Noeud_Existe(Arbre.Courant, Droite) THEN
      -- La branche de droite est vide, on n'a qu'� raccorder la
      -- branche de gauche au parent.
      Fils_Gauche := Noeud_Voisin(Arbre.Courant, Gauche);
      D�tacher_Noeud(Arbre.Courant, Ici);
      -- On sait qu'il y a un fils de gauche, donc:
      Lier_Noeuds(P�re, Fils_Gauche, C�t�);
    ELSE
      -- Aucune des deux branches n'est vide.  On remplace donc le
      -- noeud courant par son fils de gauche, puis on raccorde son
      -- fils de droite � l'extr�mit� droite du fils de gauche (qui
      -- remplace maintenant son parent).
      Fils_Gauche := Noeud_Voisin(Arbre.Courant, Gauche);
      Fils_Droite := Noeud_Voisin (Arbre.Courant, Droite);
      D�tacher_Noeud(Arbre.Courant, Ici);
      Lier_Noeuds(P�re, Fils_Gauche, C�t�);
      WHILE Noeud_Existe(Fils_Gauche, Droite) LOOP
        Fils_Gauche := Noeud_Voisin(Fils_Gauche, Droite);
      END LOOP;
      Lier_Noeuds(Fils_Gauche, Fils_Droite, Droite);
    END IF;
    D�truire_Noeud(Arbre.Courant);
    Arbre.Courant := NULL;
    Arbre.Nombre_�l�ments := Arbre.Nombre_�l�ments -1;
  ELSE
    RAISE Erreur_Arbre;
  END IF;
END D�truire_Courant;

PROCEDURE Traverser_Arbre(Arbre: IN Arbre_Binaire;
                          Mode: IN Type_Mode := Infixe;
                          Proc_Traiter: IN Traitement) IS
-- Parcourt l'Arbre, et applique la proc�dure g�n�rique Proc_Traiter
-- � chacun des �l�ments. Le type de parcours est d�termin� par
-- Mode (Pr�fixe, Infixe ou Suffixe).

  PROCEDURE Traverser_Pr�fixe(Noeud: IN Noeud_Binaire) IS
  Voisin: Noeud_Binaire;
  BEGIN
    IF Noeud_Existe(Noeud) THEN
      Proc_Traiter(Valeur_Noeud(Noeud));
      Voisin := Noeud_Voisin(Noeud, Gauche);
      Traverser_Pr�fixe(Voisin);
      Voisin := Noeud_Voisin(Noeud, Droite);
      Traverser_Pr�fixe(Voisin);
    END IF;
  END Traverser_Pr�fixe;

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
    WHEN Pr�fixe => Traverser_Pr�fixe(Arbre.Racine);
    WHEN Infixe  => Traverser_Infixe(Arbre.Racine);
    WHEN Suffixe => Traverser_Suffixe(Arbre.Racine);
  END CASE;
END Traverser_Arbre;

PROCEDURE Chercher(Arbre: IN OUT Arbre_Binaire;
                   �l�ment: IN Type_�l�ment) IS
-- Cherche �l�ment dans l'Arbre.

  PROCEDURE Chercher_Pr�fixe(Arbre: IN OUT Arbre_Binaire;
                             Noeud: IN Noeud_Binaire;
                             �l�ment: IN Type_�l�ment;
                             Trouv�: IN OUT Boolean) IS
  BEGIN
    IF Noeud_Existe(Noeud) AND NOT Trouv� THEN
      IF �l�ments_�gaux (�l�ment, Valeur_Noeud(Noeud)) THEN
        Arbre.Courant := Noeud;
        Trouv� := True;
      ELSE
        Chercher_Pr�fixe(Arbre, Noeud_Voisin(Noeud, Gauche), �l�ment, Trouv�);
        Chercher_Pr�fixe(Arbre, Noeud_Voisin(Noeud, Droite), �l�ment, Trouv�);
      END IF;
    END IF;
  END Chercher_Pr�fixe;

Trouv� : Boolean := False;
BEGIN -- Chercher
  Chercher_Pr�fixe(Arbre, Arbre.Racine, �l�ment, Trouv�);
  IF NOT Trouv� THEN
    Arbre.Courant := NULL;
  END IF;
END Chercher;

FUNCTION Noeud_Dans_Arbre(Arbre: Arbre_Binaire;
                          Noeud: Noeud_Binaire) RETURN Boolean IS
-- V�rifie si un Noeud appartient bien � un Arbre donn�.

  PROCEDURE Chercher_Pr�fixe(Noeud: IN Noeud_Binaire;
                             Noeud_Cherch�: IN Noeud_Binaire;
                             Trouv�: IN OUT Boolean) IS
  BEGIN
    IF Noeud_Existe(Noeud) AND NOT Trouv� THEN
      IF Noeud = Noeud_Cherch� THEN
        Trouv� := True;
      ELSE
        Chercher_Pr�fixe(Noeud_Voisin(Noeud, Gauche), Noeud_Cherch�, Trouv�);
        Chercher_Pr�fixe (Noeud_Voisin(Noeud, Droite), Noeud_Cherch�, Trouv�);
      END IF;
    END IF;
  END Chercher_Pr�fixe;

Trouv� : Boolean := False;
BEGIN -- Noeud_Dans_Arbre
  IF Arbre.Nombre_�l�ments = 0 OR NOT Noeud_Existe(Noeud) THEN
    RAISE Erreur_Arbre;
  ELSE
    Chercher_Pr�fixe(Arbre.Racine, Noeud, Trouv�);
    RETURN Trouv�;
  END IF;
END Noeud_Dans_Arbre;

PROCEDURE D�placer_Courant(Arbre: IN OUT Arbre_Binaire;
                           O�: IN Position) IS
-- D�place le noeud courant de l'Arbre vers l'endroit indiqu� par O�.
BEGIN
  IF O� = Racine THEN
    Arbre.Courant := Arbre.Racine;
  ELSE
    IF Noeud_Existe(Arbre.Courant) THEN
      CASE O� IS
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
          NULL; -- O� = Pointeur
      END CASE;
    ELSE
      RAISE Erreur_Arbre;
    END IF;
  END IF;
END D�placer_Courant;

PROCEDURE Modifier_Courant(Arbre: IN OUT Arbre_Binaire;
                           �l�ment: IN Type_�l�ment) IS
-- Change la valeur de l'�l�ment au noeud courant pour la valeur de �l�ment.
BEGIN
  IF Noeud_Existe(Arbre.Courant) THEN
    Modifier_Noeud(Arbre.Courant, �l�ment);
  ELSE
    RAISE Erreur_Arbre;
  END IF;
END Modifier_Courant;

FUNCTION Valeur_Courante(Arbre: Arbre_Binaire) RETURN Type_�l�ment IS
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
  RETURN Arbre.Nombre_�l�ments;
END Nombre_Noeuds;

PROCEDURE Afficher_Arbre(Arbre: IN Arbre_Binaire) IS
  -- Affiche le sous-arbre Courant avec d�calages pour montrer sa structure
  -- hi�rarchique en utilisant la proc�dure g�n�rique Afficher_�l�ment.  L'Arbre
  -- est affich� vers le c�t�.  La sortie se fait sur l'affichage courant.
  PROCEDURE Afficher_Noeuds(Noeud: IN Noeud_Binaire;
                            D�calages: IN Natural) IS
  BEGIN
    IF Noeud_Existe(Noeud) THEN
      Afficher_Noeuds(Noeud_Voisin(Noeud, Droite), D�calages + 1);
      FOR I IN 1..D�calages LOOP
        Ada.Text_IO.Put (' ');
      END LOOP;
      Afficher_�l�ment(Valeur_Noeud(Noeud));
      Ada.Text_IO.New_Line;
      Afficher_Noeuds(Noeud_Voisin(Noeud, Gauche), D�calages + 1);
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
-- Met le noeud courant � Noeud.
BEGIN -- Choisir_Noeud_Courant
  IF Noeud_Dans_Arbre(Arbre, Noeud) THEN
    Arbre.Courant := Noeud;
  ELSE
    RAISE Erreur_Arbre;
  END IF;
END Positionner_Noeud_Courant;

FUNCTION Noeud_Existe(Arbre: Arbre_Binaire;
                      O�: Position) RETURN Boolean IS
-- V�rifie si un noeud existe dans l'Arbre � l'endroit indiqu� par
-- rapport au noeud courant.
O�_Noeud : Endroit;
BEGIN
  IF O� = Racine THEN
    RETURN Noeud_Existe(Arbre.Racine, Ici);
  ELSE
    CASE O� IS
      WHEN Courant => O�_Noeud := Ici;
      WHEN Parent  => O�_Noeud := Parent;
      WHEN Gauche  => O�_Noeud := Gauche;
      WHEN Droite  => O�_Noeud := Droite;
      WHEN OTHERS  => NULL; -- O� = Racine est d�j� trait� au d�but du IF
    END CASE;
    RETURN Noeud_Existe(Arbre.Courant, O�_Noeud);
  END IF;
EXCEPTION -- Noeud_Existe
  WHEN Erreur_Noeud => RAISE Erreur_Arbre;
END Noeud_Existe;

PROCEDURE Initialize(A: IN OUT Arbre_Binaire) IS
BEGIN
  A.Courant := NULL;
  A.Racine := NULL;
  A.Nombre_�l�ments := 0;
END Initialize;

PROCEDURE Finalize(A: IN OUT Arbre_Binaire) IS
BEGIN
  Vider_Arbre(A);
END Finalize;

END CNoeuds_Binaires.Arbres_BinairesL;
