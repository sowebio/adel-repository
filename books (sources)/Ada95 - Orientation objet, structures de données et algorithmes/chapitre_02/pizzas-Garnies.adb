PACKAGE BODY Pizzas.Garnies IS

PROCEDURE Faire_Pizza(Cro�te: IN OUT Pissaladi�re) IS
-- op�ration primitive red�finie
BEGIN
  NULL;
END Faire_Pizza;
 
PROCEDURE Faire_Pizza(Cro�te: IN OUT Pizza_Napolitaine) IS
-- op�ration primitive red�finie
BEGIN
  NULL;
END Faire_Pizza;

PROCEDURE Faire_Pizza(Cro�te: IN OUT Pizza_Marine) IS
-- op�ration primitive red�finie
BEGIN
  NULL;
END Faire_Pizza;

PROCEDURE Traiter_Pizza(Tarte: IN Pizza'Class) IS
BEGIN
  NULL;
END Traiter_Pizza;
  
PROCEDURE Traiter_les_pizzas(Tarte: IN OUT Pizza'Class) IS
BEGIN
  Faire_Pizza(Tarte);
END Traiter_les_pizzas;

END Pizzas.Garnies;
