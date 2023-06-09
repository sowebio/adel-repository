--          Copyright � 1998 Philippe J. Gabrini
PACKAGE Pizzas IS

TYPE P�te_�_pain IS (bl�_dur, bl�_entier, seigle);
TYPE Sortes_de_Fromages IS 
    (mozzarella, gruy�re, emmenthal, bleu, roquefort, feta, aucun);
TYPE Sortes_de_Sauces IS 
    (tomate, viande, piquante, haricots_noirs, curry, aucune);
TYPE Garnitures_V�g�tariennes IS 
    (oignons, olives, champignons, poivrons, choucroute,
     chou, tofu);
TYPE Garnitures_Carnivores IS 
    (anchois, pepperoni, jambon, saucisse, bacon, calmar, sardines, 
     thon, maquereau, saumon, poulet, crevettes, mouton);

TYPE Pizza IS TAGGED                          -- le type de base
  RECORD
    P�te: P�te_�_pain := bl�_dur;
    Fromage: Sortes_de_Fromages := mozzarella;
    Sauce: Sortes_de_Sauces := tomate;
  END RECORD;

PROCEDURE Faire_Pizza(Cro�te: IN OUT Pizza);  -- op�ration primitive

END Pizzas;

