PACKAGE Pizzas.Garnies IS

TYPE Pissaladière IS NEW Pizza WITH           -- type dérivé étendu
  RECORD
    Garniture1: Garnitures_Végétariennes := oignons;
    Garniture2: Garnitures_Végétariennes := olives;
  END RECORD;

PROCEDURE Faire_Pizza(Croûte: IN OUT Pissaladière);
-- opération primitive redéfinie
 
TYPE Pizza_Napolitaine IS NEW Pizza WITH      -- type dérivé étendu
  RECORD
    GarnitureVeg: Garnitures_Végétariennes := olives;
    GarnitureCarn: Garnitures_Carnivores := anchois;
  END RECORD;

PROCEDURE Faire_Pizza(Croûte: IN OUT Pizza_Napolitaine);
-- opération primitive redéfinie

TYPE Pizza_Marine IS NEW Pizza_Napolitaine WITH -- type dérivé étendu
  RECORD
    Poisson1: Garnitures_Carnivores := crevettes;
    Poisson2: Garnitures_Carnivores := saumon;
  END RECORD;

PROCEDURE Faire_Pizza(Croûte: IN OUT Pizza_Marine);
-- opération primitive redéfinie

TYPE Ma_Pizza IS NEW Pizza_Marine WITH NULL RECORD;
-- type dérivé avec extension nulle, hérite de l'opération
-- primitive Faire_Pizza

PROCEDURE Traiter_les_pizzas(Tarte: IN OUT Pizza'Class); -- toutes

PROCEDURE Traiter_Pizza(Tarte: IN Pizza'Class);      -- toutes les sortes
  
TYPE Accès_Pizza IS ACCESS Pizza'Class;              -- pointeur à toutes

END Pizzas.Garnies;
