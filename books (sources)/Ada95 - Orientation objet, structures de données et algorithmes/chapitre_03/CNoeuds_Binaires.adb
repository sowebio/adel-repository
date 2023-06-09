--          Copyright © 1998 Philippe J. Gabrini
-- Programmeur:     Denis Mouraux et Philippe Gabrini
-- Date:            Juin 1998
--
-- Ce module définit le type Noeud_Binaire, qui sera
-- utilisé pour la réalisation d'arbres binaires.

WITH Ada.Unchecked_Deallocation;
PACKAGE BODY CNoeuds_Binaires IS

PROCEDURE Libérer_Noeud IS NEW Ada.Unchecked_Deallocation(Contenu_Noeud, Noeud_Binaire);

FUNCTION Nouveau_Noeud(Élément: Type_Élément) RETURN Noeud_Binaire IS
-- Crée un nouveau noeud orphelin sans enfants.
Nouveau: Noeud_Binaire;
BEGIN
  Nouveau := NEW Contenu_Noeud;
  Nouveau.Élément := Élément;
  RETURN Nouveau;
EXCEPTION
  WHEN Storage_Error =>  RAISE Erreur_Noeud; -- plus de place
END Nouveau_Noeud;

PROCEDURE Lier_Noeuds(Géniteur, Rejeton: IN Noeud_Binaire;
                      Où: IN Endroit) IS
-- Relie Géniteur (le parent) et Rejeton (le descendant).
BEGIN
  IF Géniteur = NULL OR Rejeton = NULL THEN
    -- Un des deux noeuds n'existe pas
    RAISE Erreur_Noeud;
  ELSIF Où = Gauche THEN
    IF Géniteur.Gauche = NULL
       AND Rejeton.Parent = NULL THEN
      Géniteur.Gauche := Rejeton;
      Rejeton.Parent := Géniteur;
    ELSE
      -- Géniteur a déjà un fils gauche, ou Rejeton a déjà un parent
      RAISE Erreur_Noeud;
    END IF;
  ELSIF Où = Droite THEN
    IF Géniteur.Droite = NULL
        AND Rejeton.Parent = NULL THEN
      Géniteur.Droite := Rejeton;
      Rejeton.Parent := Géniteur;
    ELSE
      -- Géniteur a déjà un fils gauche, ou Rejeton a déjà un parent
      RAISE Erreur_Noeud;
    END IF;
  ELSE
    -- Où = Parent OR Où = Ici
    RAISE Erreur_Noeud;
  END IF;
END Lier_Noeuds;

PROCEDURE Détacher_Noeud(Noeud: IN Noeud_Binaire; Où: IN Endroit) IS
-- Détache le noeud du ou des noeuds voisins (selon Où).
BEGIN
  IF NOT Noeud_Existe(Noeud) THEN
    RAISE Erreur_Noeud;
  ELSIF Où = Parent AND Noeud_Existe(Noeud, Parent) THEN
    -- On détache le parent
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
  ELSIF  Où = Gauche AND Noeud_Existe (Noeud, Gauche) THEN
    -- On détache le fils de gauche
    IF Noeud.Gauche.Parent = Noeud THEN
      Noeud.Gauche.Parent := NULL;
    ELSE
      -- Le fils de gauche avait renié son père...
      RAISE Erreur_Noeud;
    END IF;
    Noeud.Gauche := NULL;
  ELSIF  Où = Droite AND Noeud_Existe (Noeud, Droite) THEN
    -- On détache le fils de droite
    IF Noeud.Droite.Parent = Noeud THEN
      Noeud.Droite.Parent := NULL;
    ELSE
      -- Le fils de droite avait renié son père...
      RAISE Erreur_Noeud;
    END IF;
    Noeud.Droite := NULL;
  ELSIF Où = Ici THEN
    -- On détache tous les voisins
    Détacher_Noeud (Noeud, Parent);
    Détacher_Noeud (Noeud, Gauche);
    Détacher_Noeud (Noeud, Droite);
  END IF;
END Détacher_Noeud;

PROCEDURE Modifier_Noeud(Noeud: IN Noeud_Binaire;
                         Élément: IN Type_Élément) IS
-- Remplace la valeur de l'élément de Noeud par Élément.
BEGIN
  IF NOT Noeud_Existe(Noeud) THEN
    RAISE Erreur_Noeud;
  ELSE
    Noeud.Élément := Élément;
  END IF;
END Modifier_Noeud;

PROCEDURE Détruire_Noeud(Noeud: IN OUT Noeud_Binaire) IS
-- Détruit Noeud.
BEGIN
  IF NOT Noeud_Existe(Noeud)
     OR ELSE (Noeud_Existe(Noeud, Parent)
              OR Noeud_Existe(Noeud, Gauche)
              OR Noeud_Existe(Noeud, Droite)) THEN
    -- Si le noeud n'existe pas, ou s'il existe mais est encore attaché
    -- à d'autres noeuds, ces derniers auront des pointeurs vers un
    -- noeud qui n'existera plus...
    RAISE Erreur_Noeud;
  ELSE
    Libérer_Noeud(Noeud);
    Noeud := NULL;
  END IF;
END Détruire_Noeud;

PROCEDURE Détruire_Noeud_Et_Descendants(Noeud: IN OUT Noeud_Binaire) IS
-- Détruit le noeud désigné par Noeud ainsi que tous ses descendants.
BEGIN
  IF Noeud /= NULL THEN
    Détruire_Noeud_Et_Descendants(Noeud.Gauche);
    Détruire_Noeud_Et_Descendants(Noeud.Droite);
    Détacher_Noeud (Noeud, Parent);
    Détruire_Noeud (Noeud);
  END IF;
END Détruire_Noeud_Et_Descendants;

FUNCTION Noeud_Existe(Noeud: Noeud_Binaire) RETURN Boolean IS
-- Vérifie si Noeud existe.
BEGIN
  RETURN Noeud_Existe(Noeud, Ici);
END Noeud_Existe;

FUNCTION Noeud_Existe(Noeud: Noeud_Binaire;
                      Où: Endroit) RETURN Boolean IS
-- Vérifie si le noeud indiqué par Noeud et Où existe.
BEGIN
  CASE Où IS
	WHEN Ici    => RETURN Noeud /= NULL;
	WHEN Parent => RETURN Noeud.Parent /= NULL;
	WHEN Gauche => RETURN Noeud.Gauche /= NULL;
	WHEN Droite => RETURN Noeud.Droite /= NULL;
  END CASE;
END Noeud_Existe;

FUNCTION Valeur_Noeud (Noeud: Noeud_Binaire) RETURN Type_Élément IS
-- Retourne l'élément situé dans le Noeud.
BEGIN
  IF Noeud = NULL THEN
    RAISE Erreur_Noeud;
  ELSE
    RETURN Noeud.Élément;
  END IF;
END Valeur_Noeud;

FUNCTION Noeud_Voisin(Noeud: Noeud_Binaire;
                      Où: Endroit) RETURN Noeud_Binaire IS
-- Retourne le voisin de Noeud spécifié par Où.
BEGIN
  IF Noeud = NULL THEN
    RAISE Erreur_Noeud;
  ELSE
	  CASE Où IS
	    WHEN Ici    => RETURN Noeud;
	    WHEN Parent => RETURN Noeud.Parent;
	    WHEN Gauche => RETURN Noeud.Gauche;
	    WHEN Droite => RETURN Noeud.Droite;
	  END CASE;
  END IF;
END Noeud_Voisin;

FUNCTION Parenté(Noeud: Noeud_Binaire;
                 Voisin: Noeud_Binaire) RETURN Endroit IS
-- Retourne le lien de parenté de Voisin par rapport à Noeud.
BEGIN -- Parenté
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
END Parenté;

END CNoeuds_Binaires;
