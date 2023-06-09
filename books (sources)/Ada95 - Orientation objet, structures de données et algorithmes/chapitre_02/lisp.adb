-- Réalisation dynamique du type de données abstrait Listes Généralisées.
-- Listes chaînées contrôlées permettant la récupération automatique
-- de l'espace mémoire utilisé.
--
--          Copyright © 1998 Philippe J. Gabrini
--
----------------------------------------------------------------------------
WITH Ada.Unchecked_Deallocation, Ada.Integer_Text_IO;
--WITH Ada.Unchecked_Conversion;
PACKAGE BODY LISP IS

PROCEDURE Libérer IS 
       NEW Ada.Unchecked_Deallocation(Type_Élément'CLASS, Ptr_Élément);
--FUNCTION Convertir IS 
--       NEW Ada.Unchecked_Conversion(Ptr_Élément, Integer);

FUNCTION Vide(Liste: Type_Liste) RETURN Boolean IS
-- Indique si Liste est vide.
BEGIN
  RETURN Liste.Véritable = NULL;
END Vide;

FUNCTION Atome(Élément: Ptr_Élément) RETURN Boolean IS
-- Vérifie si l'Élément auquel on pointe est un atome ou une liste.
BEGIN
  IF Élément /= NULL THEN
    RETURN Élément.Atomique;
  ELSE
    RETURN False;
  END IF;
END Atome;

FUNCTION Liste_Vide RETURN Type_Liste IS
--Retourne une liste vide.
Résultat: Type_Liste;
BEGIN
  RETURN Résultat;
END Liste_Vide;
    
FUNCTION Nouvel_Élément(Élément: Type_Élément'CLASS) RETURN Ptr_Élément IS
-- Construit un objet de valeur Élément.
Résultat: Ptr_Élément := NEW Type_Élément'CLASS'(Élément);
BEGIN
  Résultat.Atomique := True;
  RETURN Résultat;
END Nouvel_Élément;

PROCEDURE Copier(Source: IN Ptr_Élément; Cible: IN OUT Ptr_Élément) IS
-- Copier la structure indiquée par Source dans une nouvelle structure
-- indiquée par Cible.
BEGIN
  IF Source /= NULL THEN  -- recopier tous les éléments
    Cible := NEW Type_Élément'CLASS'(Source.ALL);  -- premier élément
    IF NOT Source.Atomique THEN  -- copier la structure sous-jacente
      Copier(En_Tête(Source.ALL).Sous_Niveau, En_Tête(Cible.ALL).Sous_Niveau);
    END IF;
    Copier(Source.Suivant, Cible.Suivant);  -- copier la suite
  ELSE
    Cible := NULL;        -- copie vide
  END IF;
END Copier;

FUNCTION Pointeur_Tête(Liste: Type_Liste) RETURN Ptr_Élément IS
-- Retourne un pointeur à une copie de la tête de la Liste.
BEGIN
  IF Liste.Véritable = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    DECLARE
      Résultat: Ptr_Élément := NEW Type_Élément'CLASS'(Liste.Véritable.ALL);
    BEGIN
      Résultat.Suivant := NULL;     -- détacher copie du reste
      IF NOT Résultat.Atomique THEN -- faire une copie du reste de la structure
        Copier(En_Tête(Liste.Véritable.ALL).Sous_Niveau, En_Tête(Résultat.ALL).Sous_Niveau);
      END IF;
      RETURN Résultat;
    END;
  END IF;
END Pointeur_Tête;

FUNCTION Sous_Liste(Élément: Ptr_Élément) RETURN Type_Liste IS
-- Retourne une copie de la sous-liste de l'Élément.
Résultat: Type_Liste;
BEGIN
  IF Élément = NULL OR ELSE Élément.Atomique THEN
    RAISE Erreur_Liste;
  ELSE
    Copier(En_Tête(Élément.ALL).Sous_Niveau, Résultat.Véritable);
  END IF;
  RETURN Résultat;
END Sous_Liste;

FUNCTION Queue(Liste: Type_Liste) RETURN Type_Liste IS
-- Retourne une copie de la queue de la Liste.
L: Ptr_Élément;
Résultat: Type_Liste := Liste; -- copie
BEGIN
  IF Résultat.Véritable /= NULL THEN
    L := Résultat.Véritable;
    Résultat.Véritable := Résultat.Véritable.Suivant;
    Libérer(L);
  END IF;
  RETURN Résultat;
END Queue;

PROCEDURE Copier_Tête(Liste: IN Type_Liste;
                      Élément: OUT Type_Élément'CLASS) IS
-- Copie dans Élément la valeur de la tête de Liste.
BEGIN
  IF Liste.Véritable = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    Élément := Liste.Véritable.ALL;
  END IF;
END Copier_Tête;

FUNCTION Composition(Élément: IN Ptr_Élément;
                     Liste: IN Type_Liste) RETURN Type_Liste IS
-- Construire une nouvelle liste à partir d'Élément et de Liste.
Résultat: Type_Liste;
Nouveau: Ptr_Élément;
BEGIN
  IF Élément = NULL THEN
    Résultat := Liste;
  ELSIF Vide(Liste) THEN
    Résultat.Véritable := NEW Type_Élément'CLASS'(Élément.ALL); -- copie Élément
  ELSE
    IF Élément.Suivant = NULL THEN              -- un seul élément
      Nouveau := NEW Type_Élément'CLASS'(Élément.ALL);
      Copier(Liste.Véritable, Nouveau.Suivant); -- queue
      IF NOT Élément.Atomique THEN              -- faire une copie de la structure
        Copier(En_Tête(Élément.ALL).Sous_Niveau, En_Tête(Nouveau.ALL).Sous_Niveau);
      END IF;
      Résultat.Véritable := Nouveau;
    ELSE                                        -- une liste devient sous-liste
      Résultat.Véritable := NEW En_Tête;
      Copier(Liste.Véritable, Résultat.Véritable.Suivant);    -- queue
      Copier(Élément, Nouveau);                               -- faire une copie d'Élément
      En_Tête(Résultat.Véritable.ALL).Sous_Niveau := Nouveau; -- tête
    END IF;
  END IF;
  RETURN Résultat;
END Composition;

FUNCTION Composition(Première: IN Type_Liste;
                     Seconde: IN Type_Liste) RETURN Type_Liste IS
-- Construire une nouvelle liste à partir de Première et de Seconde.
Résultat: Type_Liste;
Nouveau: Ptr_Élément;
BEGIN
  IF Première.Véritable = NULL THEN
    Résultat := Seconde;
  ELSIF Seconde.Véritable = NULL THEN
    Résultat := Première;
  ELSE
    IF Première.Véritable.Suivant = NULL THEN     -- un seul élément dans Première
      Nouveau := NEW Type_Élément'CLASS'(Première.Véritable.ALL);
      Copier(Seconde.Véritable, Nouveau.Suivant); -- queue
      IF NOT Première.Véritable.Atomique THEN     -- faire une copie de la structure
        Copier(En_Tête(Première.Véritable.ALL).Sous_Niveau, En_Tête(Nouveau.ALL).Sous_Niveau);
      END IF;
      Résultat.Véritable := Nouveau;
    ELSE                                          -- Première devient sous-liste
      Résultat.Véritable := NEW En_Tête;
      Copier(Seconde.Véritable, Résultat.Véritable.Suivant);  -- queue
      Copier(Première.Véritable, Nouveau);                    -- faire une copie de Première
      En_Tête(Résultat.Véritable.ALL).Sous_Niveau := Nouveau; -- tête
    END IF;
  END IF;
  RETURN Résultat;
END Composition;

PROCEDURE Échanger(Source : IN OUT Type_Liste;
                   Cible: IN OUT Type_Liste) IS
-- Échanger efficacement Source et Cible.
Temp: Ptr_Élément;
BEGIN
  Temp := Cible.Véritable;
  Cible.Véritable := Source.Véritable;
  Source.Véritable := Temp;
END Échanger;

PROCEDURE Relier(Courant : IN OUT Ptr_Élément;
                 Suite: IN Ptr_Élément) IS
-- Relier liste Courant à élément Suite.
Ptr:  Ptr_Élément := Courant;
BEGIN
  IF Courant /= NULL THEN
    WHILE Ptr.Suivant /= NULL LOOP
      Ptr := Ptr.Suivant;
    END LOOP;
    Ptr.Suivant := Suite;
  ELSE
    Courant := Suite;
  END IF;
END Relier;

PROCEDURE Créer_Sous_Liste(Courant : IN OUT Ptr_Élément;
                           Sous_Liste: IN Ptr_Élément) IS
-- Relier Sous_Liste comme une sous-liste à la fin de Courant.
Ptr:  Ptr_Élément := Courant;
Tête: Ptr_Élément;
BEGIN
  IF Sous_Liste /= NULL THEN
    Tête := NEW En_Tête;
    En_Tête(Tête.ALL).Sous_Niveau := Sous_Liste;
    IF Courant /= NULL THEN
      WHILE Ptr.Suivant /= NULL LOOP
        Ptr := Ptr.Suivant;
      END LOOP;
      Ptr.Suivant := Tête;
    ELSE
      Courant := Tête;
    END IF;
  END IF;
END Créer_Sous_Liste;

FUNCTION Valeur_Tête(Liste: Type_Liste) RETURN Type_Étendu IS
-- Retourner la valeur de la tête de Liste en type étendu.
BEGIN
  IF Liste.Véritable = NULL OR ELSE NOT Liste.Véritable.Atomique THEN
    RAISE Erreur_Liste;
  ELSE
    DECLARE
      Résultat: Type_Étendu := Type_Étendu(Liste.Véritable.ALL);
    BEGIN
      RETURN Résultat;
    END;
  END IF;
END Valeur_Tête;

PROCEDURE Initialize(L: IN OUT Type_Liste)IS
BEGIN
  L.Véritable := NULL;
END Initialize;

PROCEDURE Finalize(L: IN OUT Type_Liste) IS

  PROCEDURE Relâcher(Liste: IN Ptr_Élément) IS
  -- libérer la mémoire allouée aux éléments de la structure
  Élément, Copie: Ptr_Élément;
  BEGIN
    Élément := Liste;
    WHILE Élément /= NULL LOOP     -- détacher et libérer
      IF NOT Élément.Atomique THEN -- descendre dans la structure
        Relâcher(En_Tête(Élément.ALL).Sous_Niveau);
      END IF;
      Copie := Élément;
      Élément := Élément.Suivant;  -- élément suivant
      Libérer(Copie);              -- relâcher l'élément courant
    END LOOP;
    --EXCEPTION
    --  WHEN Program_Error => Ada.Integer_Text_IO.Put(Item=>Convertir(Copie),Base=>16);
    --                        Ada.Integer_Text_IO.Put(Item=>Convertir(Élément),Base=>16);
  END Relâcher;

BEGIN
  IF L.Véritable /= NULL THEN  -- liste non vide, libérer
    Relâcher(L.Véritable);     -- l'espace mémoire de la structure
    L.Véritable := NULL;       -- la liste résultante est vide
  END IF;
END Finalize;

PROCEDURE Adjust(L: IN OUT Type_Liste) IS
Pointeur: Ptr_Élément;
BEGIN
  Pointeur := L.Véritable;       -- point de départ de la copie
  L.Véritable := NULL;           -- nouvelle liste vide
  Copier(Pointeur, L.Véritable); -- effectuer la copie
END Adjust;

END LISP;
