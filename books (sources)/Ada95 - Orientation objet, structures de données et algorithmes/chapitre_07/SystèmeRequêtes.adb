--          Copyright � 1998 Philippe J. Gabrini
WITH Tables.Ordonn�es, Chiffreur, Cha�nes, Ada.Text_IO;
PACKAGE BODY Syst�meRequ�tes IS

FUNCTION Compar�s(E1, E2: Type�l�ments) RETURN Integer;
PROCEDURE Afficher(E: IN Type�l�ments);
PROCEDURE MontrerClef(E: IN Type�l�ments);
PACKAGE TablesReq IS NEW Tables(Type_�l�ment => Type�l�ments,
                                Comparaison => Compar�s,
							                  Afficher�l�ment => Afficher,
                                AfficherClef => MontrerClef);
							   
PACKAGE TablesRequ�tes IS NEW TablesReq.Ordonn�es;
							   
Espaces: CONSTANT String(1..40) := (1..40 => ' ');
TableDuMonde: TablesRequ�tes.Table;

FUNCTION Compar�s(E1, E2: Type�l�ments) RETURN Integer IS
BEGIN
  IF Cha�nes.">"(E1.Clef, E2.Clef) THEN
    RETURN 1;
  ELSIF Cha�nes."="(E1.Clef, E2.Clef) THEN
    RETURN 0;
  ELSE
    RETURN -1;
  END IF;
END Compar�s;

PROCEDURE Afficher(E: IN Type�l�ments) IS
BEGIN
  Ada.Text_IO.Put_Line(Cha�nes.�_Statique(E.Clef));
  Ada.Text_IO.Put_Line(Cha�nes.�_Statique(E.Nom));
  Ada.Text_IO.Put_Line(Cha�nes.�_Statique(E.Adresse));
  Ada.Text_IO.Put_Line(Cha�nes.�_Statique(E.T�l�phone));
  Ada.Text_IO.Put_Line(Cha�nes.�_Statique(E.MotPasse));
END Afficher;

PROCEDURE MontrerClef(E: IN Type�l�ments) IS
BEGIN
  Ada.Text_IO.Put(Cha�nes.�_Statique(E.Clef)); Ada.Text_IO.Put_Line("*");
END MontrerClef;

PROCEDURE Initialiser IS
�lt: Type�l�ments;
F: Ada.Text_IO.File_Type;
Nom, Indicateur: String(1..Cha�nes.TailleCha�ne);
Longueur: Natural;
BEGIN
  Ada.Text_IO.Put(Item => "Donnez le nom du fichier de donn�es: ");
  Ada.Text_IO.Get_Line(Item => Nom, Last => Longueur);
  Ada.Text_IO.Open(File => F, Mode => Ada.Text_IO.In_File,
                   Name => Nom(1..Longueur));
  Ada.Text_IO.Set_Input(File => F);
  Ada.Text_IO.Put_Line(Item => "Initialisation de la table...");
  Ada.Text_IO.Get_Line(Indicateur, Longueur); -- mots de passes chiffr�s
  WHILE NOT Ada.Text_IO.End_Of_File LOOP
    Ada.Text_IO.Get_Line(Nom, Longueur);
    �lt.Clef := Cha�nes.�_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Get_Line(Nom, Longueur);
    �lt.Nom := Cha�nes.�_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Get_Line(Nom, Longueur);
    �lt.Adresse := Cha�nes.�_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Get_Line(Nom, Longueur);
    �lt.T�l�phone := Cha�nes.�_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Get_Line(Nom, Longueur);
    �lt.MotPasse := Cha�nes.�_Dynamique(Nom(1..Longueur));
    Ada.Text_IO.Put_Line(Nom(1..Longueur));
    IF Indicateur(1..7) /= "Chiffr�" THEN
      Chiffreur.Chiffrer(�lt.MotPasse); -- fichier original en clair
    END IF;
    TablesRequ�tes.Ins�rer(TableDuMonde, �lt);
  END LOOP;
  Ada.Text_IO.Set_Input(File => Ada.Text_IO.Standard_Input);
  Ada.Text_IO.Close(F);
END Initialiser;

FUNCTION Pr�sent(Identit�: TypeClefs) RETURN Boolean IS
�lt: Type�l�ments;
OK: Boolean;
BEGIN
  �lt.Clef := Identit�;
  TablesRequ�tes.Chercher(TableDuMonde, �lt, OK);
  RETURN OK;
END Pr�sent;

PROCEDURE AjouterMembre(Identit�: IN TypeClefs) IS
�lt: Type�l�ments;
Nom: String(1..Cha�nes.TailleCha�ne);
Long: Natural;
BEGIN
  �lt.Clef := Identit�;
  Ada.Text_IO.Put(Item => "Donnez le nom: ");
  Ada.Text_IO.Get_Line(Nom, Long); 
  �lt.Nom := Cha�nes.�_Dynamique(Nom(1..Long));
  Ada.Text_IO.Put(Item => "Donnez l'adresse: ");
  Ada.Text_IO.Get_Line(Nom, Long); 
  �lt.Adresse := Cha�nes.�_Dynamique(Nom(1..Long));
  Ada.Text_IO.Put(Item => "Donnez le num�ro de t�l�phone: ");
  Ada.Text_IO.Get_Line(Nom, Long); 
  �lt.T�l�phone := Cha�nes.�_Dynamique(Nom(1..Long));
  Ada.Text_IO.Put(Item => "Donnez le mot de passe: ");
  Ada.Text_IO.Get_Line(Nom, Long); 
  �lt.MotPasse := Cha�nes.�_Dynamique(Nom(1..Long));
  Chiffreur.Chiffrer(�lt.MotPasse);
  TablesRequ�tes.Ins�rer(TableDuMonde, �lt);
  Ada.Text_IO.Put(Item => "Membre ajout�");
END AjouterMembre;

PROCEDURE TrouverNom(Identit�: IN TypeClefs; Nom: OUT Cha�nes.TypCha�ne;
                     Trouv�: IN OUT Boolean) IS
�lt: Type�l�ments;
BEGIN
  �lt.Clef := Identit�;
  TablesRequ�tes.Chercher(TableDuMonde, �lt, Trouv�);
  IF Trouv� THEN
    Nom := �lt.Nom;
  ELSE
    Nom := Cha�nes.�_Dynamique(Espaces(1..15));
  END IF;
END TrouverNom;

FUNCTION MotPasseValide(Identit�: TypeClefs;
                        MotPasse: Cha�nes.TypCha�ne)
                       RETURN Boolean IS
�lt: Type�l�ments;
OK: Boolean;
Mot: Cha�nes.TypCha�ne;
BEGIN
  Mot := MotPasse;
  Chiffreur.Chiffrer(Mot);
  �lt.Clef := Identit�;
  TablesRequ�tes.Chercher(TableDuMonde, �lt, OK);
  RETURN OK AND
         Cha�nes."="(Mot, �lt.MotPasse);
END MotPasseValide;

PROCEDURE TrouverT�l�phone(Identit�: IN TypeClefs;
                           T�l�phone: OUT Cha�nes.TypCha�ne;
                           Trouv�: IN OUT Boolean) IS
�lt: Type�l�ments;
BEGIN
  �lt.Clef := Identit�;
  TablesRequ�tes.Chercher(TableDuMonde, �lt, Trouv�);
  IF Trouv� THEN
    T�l�phone := �lt.T�l�phone;
  ELSE
    T�l�phone := Cha�nes.�_Dynamique(Espaces(1..15));
  END IF;
END TrouverT�l�phone;

PROCEDURE TrouverAdresse(Identit�: IN TypeClefs;
                         Adresse: OUT Cha�nes.TypCha�ne;
                         Trouv�: IN OUT Boolean) IS
�lt: Type�l�ments;
BEGIN
  �lt.Clef := Identit�;
  TablesRequ�tes.Chercher(TableDuMonde, �lt, Trouv�);
  IF Trouv� THEN
    Adresse := �lt.Adresse;
  ELSE
    Adresse := Cha�nes.�_Dynamique(Espaces(1..15));
  END IF;
END TrouverAdresse;

PROCEDURE �liminer(Identit�: IN TypeClefs; Succ�s: OUT Boolean) IS
MotPasse: Cha�nes.TypCha�ne;
�lt: Type�l�ments;
Mot: String(1..Cha�nes.TailleCha�ne);
Long: Natural;
BEGIN
  Ada.Text_IO.Put(Item => "Donnez le mot de passe du membre � �liminer: ");
  Ada.Text_IO.Get_Line(Mot, Long);
  MotPasse := Cha�nes.�_Dynamique(Mot(1..Long));
  IF MotPasseValide(Identit�, MotPasse) THEN
    �lt.Clef := Identit�;
    TablesRequ�tes.Supprimer(TableDuMonde, �lt, Succ�s);
    TablesRequ�tes.AfficherTable(TableDuMonde);
  ELSE
    Succ�s := False;
  END IF;
END �liminer;

PROCEDURE ChangerMotPasse(Identit�: IN TypeClefs;
                          MotDePasse: IN Cha�nes.TypCha�ne;
                          Succ�s: OUT Boolean) IS
Vieux, Nouveau: Cha�nes.TypCha�ne;
�lt: Type�l�ments;
Mot: String(1..Cha�nes.TailleCha�ne);
Long: natural;
BEGIN
  Ada.Text_IO.Put(Item => "Donnez votre vieux mot de passe: ");
  Ada.Text_IO.New_Line; Ada.Text_IO.Get_Line(Mot, Long);
  Vieux := Cha�nes.�_Dynamique(Mot(1..Long));
  IF MotPasseValide(Identit�, Vieux) THEN
    �lt.Clef := Identit�;
    TablesRequ�tes.Chercher(TableDuMonde, �lt, Succ�s);
    IF Succ�s THEN
      Ada.Text_IO.Put(Item => "Redonnez votre nouveau mot de passe: ");
      Ada.Text_IO.New_Line; Ada.Text_IO.Get_Line(Mot, Long);
      Nouveau := Cha�nes.�_Dynamique(Mot(1..Long));
      IF Cha�nes."="(MotDePasse, Nouveau) THEN
        Chiffreur.Chiffrer(Nouveau);
        �lt.MotPasse := Nouveau;
        TablesRequ�tes.Ins�rer(TableDuMonde, �lt); -- mettre � jour
        Succ�s := True;
      ELSE
        Succ�s := False;
        Ada.Text_IO.Put_Line(Item => "Vous ne semblez pas s�r.");
      END IF;
    ELSE
      Ada.Text_IO.Put_Line(Item => "Ce n'est pas un membre!");
    END IF;
  ELSE
    Ada.Text_IO.Put_Line(Item => "Mauvais mot de passe.");
    Succ�s := False;
  END IF;
END ChangerMotPasse;
  
PROCEDURE ListerTous IS
BEGIN
  TablesRequ�tes.AfficherTable(TableDuMonde);
END ListerTous;

PROCEDURE Sauvegarder IS
BEGIN
  TablesRequ�tes.Rangertable(TableDuMonde);
END Sauvegarder;

BEGIN
  Initialiser;
  Ada.Text_IO.New_Line;
  TablesRequ�tes.AfficherTable(TableDuMonde);
END Syst�meRequ�tes;
