WITH LISP;
PACKAGE Interface_LISP IS
-- Extension du type liste au niveau paquetage de biblioth�que.
TYPE Caract�res IS NEW LISP.Type_�l�ment WITH
  RECORD
    Donn�e: Character;   -- pour listes de caract�res
  END RECORD;
END Interface_LISP;  

