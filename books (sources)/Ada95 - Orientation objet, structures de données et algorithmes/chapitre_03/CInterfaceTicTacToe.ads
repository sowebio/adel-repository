WITH Ada.Text_IO, CNoeuds_Binaires.Arbres_BinairesL;
PACKAGE CInterfaceTicTacToe IS

C�t�: CONSTANT Natural := 3;              -- longueur des c�t�s du carr� de jeu
Nombre_Lignes: CONSTANT := (C�t�+C�t�+2); -- nombre de lignes horizontales,
                                          -- + nombre de lignes verticales,
                                          -- + nombre de diagonales
TYPE Type_Case IS (X, O, Libre);
TYPE Type_Niveau IS (Plus, Moins);
SUBTYPE Indice_Carr� IS Positive RANGE 1..C�t�;
TYPE Type_Carr� IS ARRAY(Indice_Carr�, Indice_Carr�) OF Type_Case;
TYPE Situation IS RECORD
                    Carr�: Type_Carr�;
                    Tour: Type_Niveau;
                  END RECORD;

PROCEDURE Afficher(�l�ment: IN Situation);

PACKAGE Noeuds IS NEW CNoeuds_Binaires(Type_�l�ment => Situation);
PACKAGE Arbres IS NEW Noeuds.Arbres_BinairesL(�l�ments_�gaux   => "=",
                                              Afficher_�l�ment => Afficher);

END CInterfaceTicTacToe;
