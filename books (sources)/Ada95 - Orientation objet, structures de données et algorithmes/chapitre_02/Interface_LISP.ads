WITH LISP;
PACKAGE Interface_LISP IS
-- Extension du type liste au niveau paquetage de bibliothèque.
TYPE Caractères IS NEW LISP.Type_Élément WITH
  RECORD
    Donnée: Character;   -- pour listes de caractères
  END RECORD;
END Interface_LISP;  

