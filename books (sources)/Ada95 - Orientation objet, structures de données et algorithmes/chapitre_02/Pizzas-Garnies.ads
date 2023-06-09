PACKAGE Pizzas.Garnies IS

TYPE Pissaladi�re IS NEW Pizza WITH           -- type d�riv� �tendu
  RECORD
    Garniture1: Garnitures_V�g�tariennes := oignons;
    Garniture2: Garnitures_V�g�tariennes := olives;
  END RECORD;

PROCEDURE Faire_Pizza(Cro�te: IN OUT Pissaladi�re);
-- op�ration primitive red�finie
 
TYPE Pizza_Napolitaine IS NEW Pizza WITH      -- type d�riv� �tendu
  RECORD
    GarnitureVeg: Garnitures_V�g�tariennes := olives;
    GarnitureCarn: Garnitures_Carnivores := anchois;
  END RECORD;

PROCEDURE Faire_Pizza(Cro�te: IN OUT Pizza_Napolitaine);
-- op�ration primitive red�finie

TYPE Pizza_Marine IS NEW Pizza_Napolitaine WITH -- type d�riv� �tendu
  RECORD
    Poisson1: Garnitures_Carnivores := crevettes;
    Poisson2: Garnitures_Carnivores := saumon;
  END RECORD;

PROCEDURE Faire_Pizza(Cro�te: IN OUT Pizza_Marine);
-- op�ration primitive red�finie

TYPE Ma_Pizza IS NEW Pizza_Marine WITH NULL RECORD;
-- type d�riv� avec extension nulle, h�rite de l'op�ration
-- primitive Faire_Pizza

PROCEDURE Traiter_les_pizzas(Tarte: IN OUT Pizza'Class); -- toutes

PROCEDURE Traiter_Pizza(Tarte: IN Pizza'Class);      -- toutes les sortes
  
TYPE Acc�s_Pizza IS ACCESS Pizza'Class;              -- pointeur � toutes

END Pizzas.Garnies;
