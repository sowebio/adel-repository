--          Copyright � 1998 Philippe J. Gabrini
-- Programmeur:     Denis Mouraux et Philippe Gabrini
-- Date:            Juin 1998
--
-- Ce module d�finit le type Noeud_Binaire, qui sera
-- utilis� pour la r�alisation d'arbres binaires.

WITH Ada.Unchecked_Deallocation;
PACKAGE BODY CNoeuds_Binaires IS

PROCEDURE Lib�rer_Noeud IS NEW Ada.Unchecked_Deallocation(Contenu_Noeud, Noeud_Binaire);

FUNCTION Nouveau_Noeud(�l�ment: Type_�l�ment) RETURN Noeud_Binaire IS
-- Cr�e un nouveau noeud orphelin sans enfants.
Nouveau: Noeud_Binaire;
BEGIN
  Nouveau := NEW Contenu_Noeud;
  Nouveau.�l�ment := �l�ment;
  RETURN Nouveau;
EXCEPTION
  WHEN Storage_Error =>  RAISE Erreur_Noeud; -- plus de place
END Nouveau_Noeud;

PROCEDURE Lier_Noeuds(G�niteur, Rejeton: IN Noeud_Binaire;
                      O�: IN Endroit) IS
-- Relie G�niteur (le parent) et Rejeton (le descendant).
BEGIN
  IF G�niteur = NULL OR Rejeton = NULL THEN
    -- Un des deux noeuds n'existe pas
    RAISE Erreur_Noeud;
  ELSIF O� = Gauche THEN
    IF G�niteur.Gauche = NULL
       AND Rejeton.Parent = NULL THEN
      G�niteur.Gauche := Rejeton;
      Rejeton.Parent := G�niteur;
    ELSE
      -- G�niteur a d�j� un fils gauche, ou Rejeton a d�j� un parent
      RAISE Erreur_Noeud;
    END IF;
  ELSIF O� = Droite THEN
    IF G�niteur.Droite = NULL
        AND Rejeton.Parent = NULL THEN
      G�niteur.Droite := Rejeton;
      Rejeton.Parent := G�niteur;
    ELSE
      -- G�niteur a d�j� un fils gauche, ou Rejeton a d�j� un parent
      RAISE Erreur_Noeud;
    END IF;
  ELSE
    -- O� = Parent OR O� = Ici
    RAISE Erreur_Noeud;
  END IF;
END Lier_Noeuds;

PROCEDURE D�tacher_Noeud(Noeud: IN Noeud_Binaire; O�: IN Endroit) IS
-- D�tache le noeud du ou des noeuds voisins (selon O�).
BEGIN
  IF NOT Noeud_Existe(Noeud) THEN
    RAISE Erreur_Noeud;
  ELSIF O� = Parent AND Noeud_Existe(Noeud, Parent) THEN
    -- On d�tache le parent
    IF Noeud.Parent.Gauche = Noeud THEN
      -- Noeud est le fils gauche de son parent
      Noeud.Parent.Gauche := NULL;
    ELSIF Noeud.Parent.Droite = Noeud THEN
      -- Noeud est le fils droit de son parent
      Noeud.Parent.Droite := NULL;
    ELSE
      -- Noeud n'est pas le fils de son parent!
      RAISE Erreur_Noeud;
    END IF;
    Noeud.Parent := NULL;
  ELSIF  O� = Gauche AND Noeud_Existe (Noeud, Gauche) THEN
    -- On d�tache le fils de gauche
    IF Noeud.Gauche.Parent = Noeud THEN
      Noeud.Gauche.Parent := NULL;
    ELSE
      -- Le fils de gauche avait reni� son p�re...
      RAISE Erreur_Noeud;
    END IF;
    Noeud.Gauche := NULL;
  ELSIF  O� = Droite AND Noeud_Existe (Noeud, Droite) THEN
    -- On d�tache le fils de droite
    IF Noeud.Droite.Parent = Noeud THEN
      Noeud.Droite.Parent := NULL;
    ELSE
      -- Le fils de droite avait reni� son p�re...
      RAISE Erreur_Noeud;
    END IF;
    Noeud.Droite := NULL;
  ELSIF O� = Ici THEN
    -- On d�tache tous les voisins
    D�tacher_Noeud (Noeud, Parent);
    D�tacher_Noeud (Noeud, Gauche);
    D�tacher_Noeud (Noeud, Droite);
  END IF;
END D�tacher_Noeud;

PROCEDURE Modifier_Noeud(Noeud: IN Noeud_Binaire;
                         �l�ment: IN Type_�l�ment) IS
-- Remplace la valeur de l'�l�ment de Noeud par �l�ment.
BEGIN
  IF NOT Noeud_Existe(Noeud) THEN
    RAISE Erreur_Noeud;
  ELSE
    Noeud.�l�ment := �l�ment;
  END IF;
END Modifier_Noeud;

PROCEDURE D�truire_Noeud(Noeud: IN OUT Noeud_Binaire) IS
-- D�truit Noeud.
BEGIN
  IF NOT Noeud_Existe(Noeud)
     OR ELSE (Noeud_Existe(Noeud, Parent)
              OR Noeud_Existe(Noeud, Gauche)
              OR Noeud_Existe(Noeud, Droite)) THEN
    -- Si le noeud n'existe pas, ou s'il existe mais est encore attach�
    -- � d'autres noeuds, ces derniers auront des pointeurs vers un
    -- noeud qui n'existera plus...
    RAISE Erreur_Noeud;
  ELSE
    Lib�rer_Noeud(Noeud);
    Noeud := NULL;
  END IF;
END D�truire_Noeud;

PROCEDURE D�truire_Noeud_Et_Descendants(Noeud: IN OUT Noeud_Binaire) IS
-- D�truit le noeud d�sign� par Noeud ainsi que tous ses descendants.
BEGIN
  IF Noeud /= NULL THEN
    D�truire_Noeud_Et_Descendants(Noeud.Gauche);
    D�truire_Noeud_Et_Descendants(Noeud.Droite);
    D�tacher_Noeud (Noeud, Parent);
    D�truire_Noeud (Noeud);
  END IF;
END D�truire_Noeud_Et_Descendants;

FUNCTION Noeud_Existe(Noeud: Noeud_Binaire) RETURN Boolean IS
-- V�rifie si Noeud existe.
BEGIN
  RETURN Noeud_Existe(Noeud, Ici);
END Noeud_Existe;

FUNCTION Noeud_Existe(Noeud: Noeud_Binaire;
                      O�: Endroit) RETURN Boolean IS
-- V�rifie si le noeud indiqu� par Noeud et O� existe.
BEGIN
  CASE O� IS
	WHEN Ici    => RETURN Noeud /= NULL;
	WHEN Parent => RETURN Noeud.Parent /= NULL;
	WHEN Gauche => RETURN Noeud.Gauche /= NULL;
	WHEN Droite => RETURN Noeud.Droite /= NULL;
  END CASE;
END Noeud_Existe;

FUNCTION Valeur_Noeud (Noeud: Noeud_Binaire) RETURN Type_�l�ment IS
-- Retourne l'�l�ment situ� dans le Noeud.
BEGIN
  IF Noeud = NULL THEN
    RAISE Erreur_Noeud;
  ELSE
    RETURN Noeud.�l�ment;
  END IF;
END Valeur_Noeud;

FUNCTION Noeud_Voisin(Noeud: Noeud_Binaire;
                      O�: Endroit) RETURN Noeud_Binaire IS
-- Retourne le voisin de Noeud sp�cifi� par O�.
BEGIN
  IF Noeud = NULL THEN
    RAISE Erreur_Noeud;
  ELSE
	  CASE O� IS
	    WHEN Ici    => RETURN Noeud;
	    WHEN Parent => RETURN Noeud.Parent;
	    WHEN Gauche => RETURN Noeud.Gauche;
	    WHEN Droite => RETURN Noeud.Droite;
	  END CASE;
  END IF;
END Noeud_Voisin;

FUNCTION Parent�(Noeud: Noeud_Binaire;
                 Voisin: Noeud_Binaire) RETURN Endroit IS
-- Retourne le lien de parent� de Voisin par rapport � Noeud.
BEGIN -- Parent�
  IF Noeud /= NULL THEN
    IF Voisin = Noeud THEN
      RETURN Ici;
    ELSIF Voisin = Noeud.Parent THEN
      RETURN Parent;
    ELSIF Voisin = Noeud.Gauche THEN
      RETURN Gauche;
    ELSIF Voisin = Noeud.Droite THEN
      RETURN Droite;
    ELSE
      RAISE Erreur_Noeud;
    END IF;
  ELSE
    RAISE Erreur_Noeud;
  END IF;
END Parent�;

END CNoeuds_Binaires;
