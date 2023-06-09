--          Copyright © 1998 Philippe J. Gabrini
WITH Pizzas.Garnies, Ada.Text_IO, Ada.Tags; 
USE Pizzas, Pizzas.Garnies, Ada.Tags;  -- USE pour simplifier la lecture
PROCEDURE Essayer_Pizza_Garnies IS

Pizza_bon_marché: Pizza := (Pâte => blé_dur,
                            Fromage => mozzarella,
                            Sauce => tomate);
Pizza_grand_père: Pissaladière := (Pâte => seigle,
                                   Fromage => aucun,
                                   Sauce => aucune,
                                   Garniture1 => oignons,
                                   Garniture2 => olives);
Pizza_bourgeoise: Pizza_Napolitaine := (Pâte => blé_dur,
                                        Fromage => gruyère,
                                        Sauce => viande,
                                        GarnitureVeg => champignons,
                                        GarnitureCarn => pepperoni);
LaMienne: Ma_Pizza := (Pâte => blé_entier,
                       Fromage => emmenthal,
                       Sauce => tomate,
                       GarnitureVeg => poivrons, 
                       GarnitureCarn => pepperoni,
                       Poisson1 => sardines, 
                       Poisson2 => thon);
Pizza_toute_habillée: Pizza_Marine := Pizza_Marine(LaMienne); 
-- conversion vers type de base

BEGIN
  Pizza_bon_marché := Pizza(Pizza_toute_habillée);
--Pizza_toute_habillée:= Pizza_Marine(Pizza_bon_marché);   -- invalide
  Pizza_toute_habillée:= (Pizza_bourgeoise WITH
                                      Poisson1 => calmar,
                                      Poisson2 => maquereau);
  Pizza_toute_habillée:= (Pizza_bon_marché WITH
                                      GarnitureVeg => oignons, 
                                      GarnitureCarn => jambon,
                                      Poisson1 => thon,
                                      Poisson2 => maquereau);
  
  Traiter_Pizza(Pizza_bon_marché);   -- répartition statique
  Traiter_Pizza(Pizza_bourgeoise);   -- répartition statique
  Traiter_Pizza(LaMienne);           -- opération héritée répartition statique
  
  DECLARE
    TYPE Rangée_de_pizzas IS ARRAY(Positive RANGE <>) OF Accès_Pizza;
    Rang: Rangée_de_pizzas(1..3) := (NEW Pizza'(Pizza_bon_marché),
                                     NEW Pizza_Napolitaine'(Pizza_Bourgeoise),
                                     NEW Pizza_Marine'(Pizza_toute_habillée));
  BEGIN
    FOR Indice IN Rang'RANGE LOOP
      Faire_Pizza(Rang(Indice).ALL);  -- répartition dynamique
    END LOOP;

    IF Rang(1)'TAG = Rang(2)'TAG THEN
      Ada.Text_IO.Put("C'est pas vrai!");
    ELSE
      Ada.Text_IO.Put("C'est faux!");
    END IF;

  END;

  IF Pizza_bon_marché IN Pizza'CLASS THEN
    Ada.Text_IO.Put("C'est vrai!");
  ELSIF Pizza_toute_habillée IN Pizza_Napolitaine'CLASS THEN
    Ada.Text_IO.Put("C'est pas faux!");
  ELSE
    Ada.Text_IO.Put("C'est tout faux!");
  END IF;

END Essayer_Pizza_Garnies;
