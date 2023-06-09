--          Copyright © 1998 Philippe J. Gabrini
PACKAGE Pizzas IS

TYPE Pâte_à_pain IS (blé_dur, blé_entier, seigle);
TYPE Sortes_de_Fromages IS 
    (mozzarella, gruyère, emmenthal, bleu, roquefort, feta, aucun);
TYPE Sortes_de_Sauces IS 
    (tomate, viande, piquante, haricots_noirs, curry, aucune);
TYPE Garnitures_Végétariennes IS 
    (oignons, olives, champignons, poivrons, choucroute,
     chou, tofu);
TYPE Garnitures_Carnivores IS 
    (anchois, pepperoni, jambon, saucisse, bacon, calmar, sardines, 
     thon, maquereau, saumon, poulet, crevettes, mouton);

TYPE Pizza IS TAGGED                          -- le type de base
  RECORD
    Pâte: Pâte_à_pain := blé_dur;
    Fromage: Sortes_de_Fromages := mozzarella;
    Sauce: Sortes_de_Sauces := tomate;
  END RECORD;

PROCEDURE Faire_Pizza(Croûte: IN OUT Pizza);  -- opération primitive

END Pizzas;

