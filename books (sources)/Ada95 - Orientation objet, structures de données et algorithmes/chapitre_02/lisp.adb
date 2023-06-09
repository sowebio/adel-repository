-- R�alisation dynamique du type de donn�es abstrait Listes G�n�ralis�es.
-- Listes cha�n�es contr�l�es permettant la r�cup�ration automatique
-- de l'espace m�moire utilis�.
--
--          Copyright � 1998 Philippe J. Gabrini
--
----------------------------------------------------------------------------
WITH Ada.Unchecked_Deallocation, Ada.Integer_Text_IO;
--WITH Ada.Unchecked_Conversion;
PACKAGE BODY LISP IS

PROCEDURE Lib�rer IS 
       NEW Ada.Unchecked_Deallocation(Type_�l�ment'CLASS, Ptr_�l�ment);
--FUNCTION Convertir IS 
--       NEW Ada.Unchecked_Conversion(Ptr_�l�ment, Integer);

FUNCTION Vide(Liste: Type_Liste) RETURN Boolean IS
-- Indique si Liste est vide.
BEGIN
  RETURN Liste.V�ritable = NULL;
END Vide;

FUNCTION Atome(�l�ment: Ptr_�l�ment) RETURN Boolean IS
-- V�rifie si l'�l�ment auquel on pointe est un atome ou une liste.
BEGIN
  IF �l�ment /= NULL THEN
    RETURN �l�ment.Atomique;
  ELSE
    RETURN False;
  END IF;
END Atome;

FUNCTION Liste_Vide RETURN Type_Liste IS
--Retourne une liste vide.
R�sultat: Type_Liste;
BEGIN
  RETURN R�sultat;
END Liste_Vide;
    
FUNCTION Nouvel_�l�ment(�l�ment: Type_�l�ment'CLASS) RETURN Ptr_�l�ment IS
-- Construit un objet de valeur �l�ment.
R�sultat: Ptr_�l�ment := NEW Type_�l�ment'CLASS'(�l�ment);
BEGIN
  R�sultat.Atomique := True;
  RETURN R�sultat;
END Nouvel_�l�ment;

PROCEDURE Copier(Source: IN Ptr_�l�ment; Cible: IN OUT Ptr_�l�ment) IS
-- Copier la structure indiqu�e par Source dans une nouvelle structure
-- indiqu�e par Cible.
BEGIN
  IF Source /= NULL THEN  -- recopier tous les �l�ments
    Cible := NEW Type_�l�ment'CLASS'(Source.ALL);  -- premier �l�ment
    IF NOT Source.Atomique THEN  -- copier la structure sous-jacente
      Copier(En_T�te(Source.ALL).Sous_Niveau, En_T�te(Cible.ALL).Sous_Niveau);
    END IF;
    Copier(Source.Suivant, Cible.Suivant);  -- copier la suite
  ELSE
    Cible := NULL;        -- copie vide
  END IF;
END Copier;

FUNCTION Pointeur_T�te(Liste: Type_Liste) RETURN Ptr_�l�ment IS
-- Retourne un pointeur � une copie de la t�te de la Liste.
BEGIN
  IF Liste.V�ritable = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    DECLARE
      R�sultat: Ptr_�l�ment := NEW Type_�l�ment'CLASS'(Liste.V�ritable.ALL);
    BEGIN
      R�sultat.Suivant := NULL;     -- d�tacher copie du reste
      IF NOT R�sultat.Atomique THEN -- faire une copie du reste de la structure
        Copier(En_T�te(Liste.V�ritable.ALL).Sous_Niveau, En_T�te(R�sultat.ALL).Sous_Niveau);
      END IF;
      RETURN R�sultat;
    END;
  END IF;
END Pointeur_T�te;

FUNCTION Sous_Liste(�l�ment: Ptr_�l�ment) RETURN Type_Liste IS
-- Retourne une copie de la sous-liste de l'�l�ment.
R�sultat: Type_Liste;
BEGIN
  IF �l�ment = NULL OR ELSE �l�ment.Atomique THEN
    RAISE Erreur_Liste;
  ELSE
    Copier(En_T�te(�l�ment.ALL).Sous_Niveau, R�sultat.V�ritable);
  END IF;
  RETURN R�sultat;
END Sous_Liste;

FUNCTION Queue(Liste: Type_Liste) RETURN Type_Liste IS
-- Retourne une copie de la queue de la Liste.
L: Ptr_�l�ment;
R�sultat: Type_Liste := Liste; -- copie
BEGIN
  IF R�sultat.V�ritable /= NULL THEN
    L := R�sultat.V�ritable;
    R�sultat.V�ritable := R�sultat.V�ritable.Suivant;
    Lib�rer(L);
  END IF;
  RETURN R�sultat;
END Queue;

PROCEDURE Copier_T�te(Liste: IN Type_Liste;
                      �l�ment: OUT Type_�l�ment'CLASS) IS
-- Copie dans �l�ment la valeur de la t�te de Liste.
BEGIN
  IF Liste.V�ritable = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    �l�ment := Liste.V�ritable.ALL;
  END IF;
END Copier_T�te;

FUNCTION Composition(�l�ment: IN Ptr_�l�ment;
                     Liste: IN Type_Liste) RETURN Type_Liste IS
-- Construire une nouvelle liste � partir d'�l�ment et de Liste.
R�sultat: Type_Liste;
Nouveau: Ptr_�l�ment;
BEGIN
  IF �l�ment = NULL THEN
    R�sultat := Liste;
  ELSIF Vide(Liste) THEN
    R�sultat.V�ritable := NEW Type_�l�ment'CLASS'(�l�ment.ALL); -- copie �l�ment
  ELSE
    IF �l�ment.Suivant = NULL THEN              -- un seul �l�ment
      Nouveau := NEW Type_�l�ment'CLASS'(�l�ment.ALL);
      Copier(Liste.V�ritable, Nouveau.Suivant); -- queue
      IF NOT �l�ment.Atomique THEN              -- faire une copie de la structure
        Copier(En_T�te(�l�ment.ALL).Sous_Niveau, En_T�te(Nouveau.ALL).Sous_Niveau);
      END IF;
      R�sultat.V�ritable := Nouveau;
    ELSE                                        -- une liste devient sous-liste
      R�sultat.V�ritable := NEW En_T�te;
      Copier(Liste.V�ritable, R�sultat.V�ritable.Suivant);    -- queue
      Copier(�l�ment, Nouveau);                               -- faire une copie d'�l�ment
      En_T�te(R�sultat.V�ritable.ALL).Sous_Niveau := Nouveau; -- t�te
    END IF;
  END IF;
  RETURN R�sultat;
END Composition;

FUNCTION Composition(Premi�re: IN Type_Liste;
                     Seconde: IN Type_Liste) RETURN Type_Liste IS
-- Construire une nouvelle liste � partir de Premi�re et de Seconde.
R�sultat: Type_Liste;
Nouveau: Ptr_�l�ment;
BEGIN
  IF Premi�re.V�ritable = NULL THEN
    R�sultat := Seconde;
  ELSIF Seconde.V�ritable = NULL THEN
    R�sultat := Premi�re;
  ELSE
    IF Premi�re.V�ritable.Suivant = NULL THEN     -- un seul �l�ment dans Premi�re
      Nouveau := NEW Type_�l�ment'CLASS'(Premi�re.V�ritable.ALL);
      Copier(Seconde.V�ritable, Nouveau.Suivant); -- queue
      IF NOT Premi�re.V�ritable.Atomique THEN     -- faire une copie de la structure
        Copier(En_T�te(Premi�re.V�ritable.ALL).Sous_Niveau, En_T�te(Nouveau.ALL).Sous_Niveau);
      END IF;
      R�sultat.V�ritable := Nouveau;
    ELSE                                          -- Premi�re devient sous-liste
      R�sultat.V�ritable := NEW En_T�te;
      Copier(Seconde.V�ritable, R�sultat.V�ritable.Suivant);  -- queue
      Copier(Premi�re.V�ritable, Nouveau);                    -- faire une copie de Premi�re
      En_T�te(R�sultat.V�ritable.ALL).Sous_Niveau := Nouveau; -- t�te
    END IF;
  END IF;
  RETURN R�sultat;
END Composition;

PROCEDURE �changer(Source : IN OUT Type_Liste;
                   Cible: IN OUT Type_Liste) IS
-- �changer efficacement Source et Cible.
Temp: Ptr_�l�ment;
BEGIN
  Temp := Cible.V�ritable;
  Cible.V�ritable := Source.V�ritable;
  Source.V�ritable := Temp;
END �changer;

PROCEDURE Relier(Courant : IN OUT Ptr_�l�ment;
                 Suite: IN Ptr_�l�ment) IS
-- Relier liste Courant � �l�ment Suite.
Ptr:  Ptr_�l�ment := Courant;
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

PROCEDURE Cr�er_Sous_Liste(Courant : IN OUT Ptr_�l�ment;
                           Sous_Liste: IN Ptr_�l�ment) IS
-- Relier Sous_Liste comme une sous-liste � la fin de Courant.
Ptr:  Ptr_�l�ment := Courant;
T�te: Ptr_�l�ment;
BEGIN
  IF Sous_Liste /= NULL THEN
    T�te := NEW En_T�te;
    En_T�te(T�te.ALL).Sous_Niveau := Sous_Liste;
    IF Courant /= NULL THEN
      WHILE Ptr.Suivant /= NULL LOOP
        Ptr := Ptr.Suivant;
      END LOOP;
      Ptr.Suivant := T�te;
    ELSE
      Courant := T�te;
    END IF;
  END IF;
END Cr�er_Sous_Liste;

FUNCTION Valeur_T�te(Liste: Type_Liste) RETURN Type_�tendu IS
-- Retourner la valeur de la t�te de Liste en type �tendu.
BEGIN
  IF Liste.V�ritable = NULL OR ELSE NOT Liste.V�ritable.Atomique THEN
    RAISE Erreur_Liste;
  ELSE
    DECLARE
      R�sultat: Type_�tendu := Type_�tendu(Liste.V�ritable.ALL);
    BEGIN
      RETURN R�sultat;
    END;
  END IF;
END Valeur_T�te;

PROCEDURE Initialize(L: IN OUT Type_Liste)IS
BEGIN
  L.V�ritable := NULL;
END Initialize;

PROCEDURE Finalize(L: IN OUT Type_Liste) IS

  PROCEDURE Rel�cher(Liste: IN Ptr_�l�ment) IS
  -- lib�rer la m�moire allou�e aux �l�ments de la structure
  �l�ment, Copie: Ptr_�l�ment;
  BEGIN
    �l�ment := Liste;
    WHILE �l�ment /= NULL LOOP     -- d�tacher et lib�rer
      IF NOT �l�ment.Atomique THEN -- descendre dans la structure
        Rel�cher(En_T�te(�l�ment.ALL).Sous_Niveau);
      END IF;
      Copie := �l�ment;
      �l�ment := �l�ment.Suivant;  -- �l�ment suivant
      Lib�rer(Copie);              -- rel�cher l'�l�ment courant
    END LOOP;
    --EXCEPTION
    --  WHEN Program_Error => Ada.Integer_Text_IO.Put(Item=>Convertir(Copie),Base=>16);
    --                        Ada.Integer_Text_IO.Put(Item=>Convertir(�l�ment),Base=>16);
  END Rel�cher;

BEGIN
  IF L.V�ritable /= NULL THEN  -- liste non vide, lib�rer
    Rel�cher(L.V�ritable);     -- l'espace m�moire de la structure
    L.V�ritable := NULL;       -- la liste r�sultante est vide
  END IF;
END Finalize;

PROCEDURE Adjust(L: IN OUT Type_Liste) IS
Pointeur: Ptr_�l�ment;
BEGIN
  Pointeur := L.V�ritable;       -- point de d�part de la copie
  L.V�ritable := NULL;           -- nouvelle liste vide
  Copier(Pointeur, L.V�ritable); -- effectuer la copie
END Adjust;

END LISP;
