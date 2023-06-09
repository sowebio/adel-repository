WITH Ada.Text_IO, CNoeuds_Binaires.Arbres_BinairesL;
PACKAGE CInterfaceTicTacToe IS

Côté: CONSTANT Natural := 3;              -- longueur des côtés du carré de jeu
Nombre_Lignes: CONSTANT := (Côté+Côté+2); -- nombre de lignes horizontales,
                                          -- + nombre de lignes verticales,
                                          -- + nombre de diagonales
TYPE Type_Case IS (X, O, Libre);
TYPE Type_Niveau IS (Plus, Moins);
SUBTYPE Indice_Carré IS Positive RANGE 1..Côté;
TYPE Type_Carré IS ARRAY(Indice_Carré, Indice_Carré) OF Type_Case;
TYPE Situation IS RECORD
                    Carré: Type_Carré;
                    Tour: Type_Niveau;
                  END RECORD;

PROCEDURE Afficher(Élément: IN Situation);

PACKAGE Noeuds IS NEW CNoeuds_Binaires(Type_Élément => Situation);
PACKAGE Arbres IS NEW Noeuds.Arbres_BinairesL(Éléments_Égaux   => "=",
                                              Afficher_Élément => Afficher);

END CInterfaceTicTacToe;
