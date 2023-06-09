--          Copyright © 1998 Philippe J. Gabrini
WITH Tables.Ordonnées, Chiffreur, Chaînes, Ada.Text_IO;
PACKAGE BODY SystèmeRequêtes IS

FUNCTION Comparés(E1, E2: TypeÉléments) RETURN Integer;
PROCEDURE Afficher(E: IN TypeÉléments);
PROCEDURE MontrerClef(E: IN TypeÉléments);
PACKAGE TablesReq IS NEW Tables(Type_Élément => TypeÉléments,
                                Comparaison => Comparés,
							                  AfficherÉlément => Afficher,
                                AfficherClef => MontrerClef);
							   
PACKAGE TablesRequêtes IS NEW TablesReq.Ordonnées;
							   
Espaces: CONSTANT String(1..40) := (1..40 => ' ');
TableDuMonde: TablesRequêtes.Table;

FUNCTION Comparés(E1, E2: TypeÉléments) RETURN Integer IS
BEGIN
  IF Chaînes.">"(E1.Clef, E2.Clef) THEN
    RETURN 1;
  ELSIF Chaînes."="(E1.Clef, E2.Clef) THEN
    RETURN 0;
  ELSE
    RETURN -1;
  END IF;
END Comparés;

PROCEDURE Afficher(E: IN TypeÉléments) IS
BEGIN
  Ada.Text_IO.Put_Line(Chaînes.À_Statique(E.Clef));
  Ada.Text_IO.Put_Line(Chaînes.À_Statique(E.Nom));
  Ada.Text_IO.Put_Line(Chaînes.À_Statique(E.Adresse));
  Ada.Text_IO.Put_Line(Chaînes.À_Statique(E.Téléphone));
  Ada.Text_IO.Put_Line(Chaînes.À_Statique(E.MotPasse));
END Afficher;

PROCEDURE MontrerClef(E: IN TypeÉléments) IS
BEGIN
  Ada.Text_IO.Put(Chaînes.À_Statique(E.Clef)); Ada.Text_IO.Put_Line("*");
END MontrerClef;

PROCEDURE Initialiser IS
Élt: TypeÉléments;
F: Ada.Text_IO.File_Type;
Nom, Indicateur: String(1..Chaînes.TailleChaîne);
Longueur: Natural;
BEGIN
  Ada.Text_IO.Put(Item => "Donnez le nom du fichier de données: ");
  Ada.Text_IO.Get_Line(Item => Nom, Last => Longueur);
  Ada.Text_IO.Open(File => F, Mode => Ada.Text_IO.In_File,
                   Name => Nom(1..Longueur));
  Ada.Text_IO.Set_Input(File => F);
  Ada.Text_IO.Put_Line(Item => "Initialisation de la table...");
  Ada.Text_IO.Get_Line(Indicateur, Longueur); -- mots de passes chiffrés
  WHILE NOT Ada.Text_IO.End_Of_File LOOP
    Ada.Text_IO.Get_Line(Nom, Longueur);
    Élt.Clef := Chaînes.À_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Get_Line(Nom, Longueur);
    Élt.Nom := Chaînes.À_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Get_Line(Nom, Longueur);
    Élt.Adresse := Chaînes.À_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Get_Line(Nom, Longueur);
    Élt.Téléphone := Chaînes.À_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Get_Line(Nom, Longueur);
    Élt.MotPasse := Chaînes.À_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Put_Line(Nom(1..Longueur));
    IF Indicateur(1..7) /= "Chiffré" THEN
      Chiffreur.Chiffrer(Élt.MotPasse); -- fichier original en clair
    END IF;
    TablesRequêtes.Insérer(TableDuMonde, Élt);
  END LOOP;
  Ada.Text_IO.Set_Input(File => Ada.Text_IO.Standard_Input);
  Ada.Text_IO.Close(F);
END Initialiser;

FUNCTION Présent(Identité: TypeClefs) RETURN Boolean IS
Élt: TypeÉléments;
OK: Boolean;
BEGIN
  Élt.Clef := Identité;
  TablesRequêtes.Chercher(TableDuMonde, Élt, OK);
  RETURN OK;
END Présent;

PROCEDURE AjouterMembre(Identité: IN TypeClefs) IS
Élt: TypeÉléments;
Nom: String(1..Chaînes.TailleChaîne);
Long: Natural;
BEGIN
  Élt.Clef := Identité;
  Ada.Text_IO.Put(Item => "Donnez le nom: ");
  Ada.Text_IO.Get_Line(Nom, Long); 
  Élt.Nom := Chaînes.À_Dynamique(Nom(1..Long));
  Ada.Text_IO.Put(Item => "Donnez l'adresse: ");
  Ada.Text_IO.Get_Line(Nom, Long); 
  Élt.Adresse := Chaînes.À_Dynamique(Nom(1..Long));
  Ada.Text_IO.Put(Item => "Donnez le numéro de téléphone: ");
  Ada.Text_IO.Get_Line(Nom, Long); 
  Élt.Téléphone := Chaînes.À_Dynamique(Nom(1..Long));
  Ada.Text_IO.Put(Item => "Donnez le mot de passe: ");
  Ada.Text_IO.Get_Line(Nom, Long); 
  Élt.MotPasse := Chaînes.À_Dynamique(Nom(1..Long));
  Chiffreur.Chiffrer(Élt.MotPasse);
  TablesRequêtes.Insérer(TableDuMonde, Élt);
  Ada.Text_IO.Put(Item => "Membre ajouté");
END AjouterMembre;

PROCEDURE TrouverNom(Identité: IN TypeClefs; Nom: OUT Chaînes.TypChaîne;
                     Trouvé: IN OUT Boolean) IS
Élt: TypeÉléments;
BEGIN
  Élt.Clef := Identité;
  TablesRequêtes.Chercher(TableDuMonde, Élt, Trouvé);
  IF Trouvé THEN
    Nom := Élt.Nom;
  ELSE
    Nom := Chaînes.À_Dynamique(Espaces(1..15));
  END IF;
END TrouverNom;

FUNCTION MotPasseValide(Identité: TypeClefs;
                        MotPasse: Chaînes.TypChaîne)
                       RETURN Boolean IS
Élt: TypeÉléments;
OK: Boolean;
Mot: Chaînes.TypChaîne;
BEGIN
  Mot := MotPasse;
  Chiffreur.Chiffrer(Mot);
  Élt.Clef := Identité;
  TablesRequêtes.Chercher(TableDuMonde, Élt, OK);
  RETURN OK AND
         Chaînes."="(Mot, Élt.MotPasse);
END MotPasseValide;

PROCEDURE TrouverTéléphone(Identité: IN TypeClefs;
                           Téléphone: OUT Chaînes.TypChaîne;
                           Trouvé: IN OUT Boolean) IS
Élt: TypeÉléments;
BEGIN
  Élt.Clef := Identité;
  TablesRequêtes.Chercher(TableDuMonde, Élt, Trouvé);
  IF Trouvé THEN
    Téléphone := Élt.Téléphone;
  ELSE
    Téléphone := Chaînes.À_Dynamique(Espaces(1..15));
  END IF;
END TrouverTéléphone;

PROCEDURE TrouverAdresse(Identité: IN TypeClefs;
                         Adresse: OUT Chaînes.TypChaîne;
                         Trouvé: IN OUT Boolean) IS
Élt: TypeÉléments;
BEGIN
  Élt.Clef := Identité;
  TablesRequêtes.Chercher(TableDuMonde, Élt, Trouvé);
  IF Trouvé THEN
    Adresse := Élt.Adresse;
  ELSE
    Adresse := Chaînes.À_Dynamique(Espaces(1..15));
  END IF;
END TrouverAdresse;

PROCEDURE Éliminer(Identité: IN TypeClefs; Succès: OUT Boolean) IS
MotPasse: Chaînes.TypChaîne;
Élt: TypeÉléments;
Mot: String(1..Chaînes.TailleChaîne);
Long: Natural;
BEGIN
  Ada.Text_IO.Put(Item => "Donnez le mot de passe du membre à éliminer: ");
  Ada.Text_IO.Get_Line(Mot, Long);
  MotPasse := Chaînes.À_Dynamique(Mot(1..Long));
  IF MotPasseValide(Identité, MotPasse) THEN
    Élt.Clef := Identité;
    TablesRequêtes.Supprimer(TableDuMonde, Élt, Succès);
    TablesRequêtes.AfficherTable(TableDuMonde);
  ELSE
    Succès := False;
  END IF;
END Éliminer;

PROCEDURE ChangerMotPasse(Identité: IN TypeClefs;
                          MotDePasse: IN Chaînes.TypChaîne;
                          Succès: OUT Boolean) IS
Vieux, Nouveau: Chaînes.TypChaîne;
Élt: TypeÉléments;
Mot: String(1..Chaînes.TailleChaîne);
Long: natural;
BEGIN
  Ada.Text_IO.Put(Item => "Donnez votre vieux mot de passe: ");
  Ada.Text_IO.New_Line; Ada.Text_IO.Get_Line(Mot, Long);
  Vieux := Chaînes.À_Dynamique(Mot(1..Long));
  IF MotPasseValide(Identité, Vieux) THEN
    Élt.Clef := Identité;
    TablesRequêtes.Chercher(TableDuMonde, Élt, Succès);
    IF Succès THEN
      Ada.Text_IO.Put(Item => "Redonnez votre nouveau mot de passe: ");
      Ada.Text_IO.New_Line; Ada.Text_IO.Get_Line(Mot, Long);
      Nouveau := Chaînes.À_Dynamique(Mot(1..Long));
      IF Chaînes."="(MotDePasse, Nouveau) THEN
        Chiffreur.Chiffrer(Nouveau);
        Élt.MotPasse := Nouveau;
        TablesRequêtes.Insérer(TableDuMonde, Élt); -- mettre ˆ jour
        Succès := True;
      ELSE
        Succès := False;
        Ada.Text_IO.Put_Line(Item => "Vous ne semblez pas sûr.");
      END IF;
    ELSE
      Ada.Text_IO.Put_Line(Item => "Ce n'est pas un membre!");
    END IF;
  ELSE
    Ada.Text_IO.Put_Line(Item => "Mauvais mot de passe.");
    Succès := False;
  END IF;
END ChangerMotPasse;
  
PROCEDURE ListerTous IS
BEGIN
  TablesRequêtes.AfficherTable(TableDuMonde);
END ListerTous;

PROCEDURE Sauvegarder IS
BEGIN
  TablesRequêtes.Rangertable(TableDuMonde);
END Sauvegarder;

BEGIN
  Initialiser;
  Ada.Text_IO.New_Line;
  TablesRequêtes.AfficherTable(TableDuMonde);
END SystèmeRequêtes;
