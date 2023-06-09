--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PROCEDURE TriFusion IS
-- Application de l'algorithme de fusion � un fichier d'enregistrements
--             P. Gabrini     F�vrier 1989

LongueurClef: CONSTANT Natural := 2;
LongueurInfo: CONSTANT Natural := 8;
LongueurEnregistrement: CONSTANT Natural := LongueurClef + LongueurInfo;
 
TYPE �l�ments IS RECORD
                   Clef: String(1..LongueurClef);
                   Info: String(1..LongueurInfo);
                 END RECORD;
TYPE VecteurFichiers IS ARRAY(0..3) OF Ada.Text_IO.File_Type;
 
PROCEDURE LireArticle(F: IN OUT Ada.Text_IO.File_Type; �lt: IN OUT �l�ments) IS
-- Lire un article � partir du fichier F. Terminer si fin de fichier. 
BEGIN
  IF NOT Ada.Text_IO.End_Of_File(F) THEN
    FOR i IN 1..LongueurClef LOOP
      Ada.Text_IO.Get(File => F, Item => �lt.Clef(i));
    END LOOP;
    FOR i IN 1..LongueurInfo LOOP
      Ada.Text_IO.Get(File => F, Item => �lt.Info(i));
    END LOOP;
  END IF;
END LireArticle;
 
PROCEDURE �crireArticle(F: IN OUT Ada.Text_IO.File_Type; �lt: IN �l�ments) IS
-- �crire un article dans le fichier F 
BEGIN
  FOR i IN 1..LongueurClef LOOP
    Ada.Text_IO.Put(File => F, Item => �lt.Clef(i));
  END LOOP; 
  FOR i IN 1..LongueurInfo LOOP
    Ada.Text_IO.Put(File => F, Item => �lt.Info(i));
  END LOOP; 
END �crireArticle;
 
PROCEDURE TrierParFusion(F: IN OUT VecteurFichiers; R�sultant: OUT Natural;
                         NombreArticles: IN Natural) IS
-- Tri d'un fichier d'enregistrements sur le champ Clef.  Le tri est
-- fait en combinant (fusionnant) des s�ries de longueur 2, 4, 8 ...
-- en utilisant 4 fichiers temporaires F[0],F[1],F[2],F[3].
-- Initialement on a des s�ries de longueur 2 dans les fichiers F[0]
-- et F[1]. Le tri est fait en plusieurs passes, chaque passe 
-- combinant des s�ries d'une longueur donn�e jusqu'� ce que la 
-- longueur des s�ries d�passe la taille du fichier.  � la fin du tri 
-- les enregistrements tri�s seront dans le fichier temporaire 
-- F[R�sultant].
 
Entr�e, Sortie,                -- Designe les fichiers d'entr�e et de sortie 
Limite,                        -- Nombre d'enregistrements qui restent � trier 
Longueur,                      -- Longueur de la s�rie en cours 
Longueur1, Longueur2: Natural; -- Longueurs de s�rie1 et s�rie2 
TicTac: Boolean;               -- Indique quels fichiers sont utilis�s 
Article1, Article2: �l�ments;  -- Enregistrements temporaraires
 
BEGIN
  -- Ouvrir les fichiers pour qu'ils puissent �tre �chang�s tout de suite 
  Ada.Text_IO.Open(File => F(0), Mode => Ada.Text_IO.In_File, Name => "MFile1.dat");
  Ada.Text_IO.Open(File => F(1), Mode => Ada.Text_IO.In_File, Name => "MFile2.dat");
  Ada.Text_IO.Open(File => F(2), Mode => Ada.Text_IO.In_File, Name => "MFile3.dat");
  Ada.Text_IO.Open(File => F(3), Mode => Ada.Text_IO.In_File, Name => "MFile4.dat");
  TicTac := True;
  Longueur := 2;              -- Commencer avec des s�ries de longueur 2 
  LOOP                        -- Trier les s�ries 
    Limite := NombreArticles;
    IF TicTac THEN  -- Premiers fichiers � trier, seconds pour rangement 
      Entr�e := 0;
      Sortie := 3;
      Ada.Text_IO.Reset(F(0), Ada.Text_IO.In_File);
	    Ada.Text_IO.Reset(F(1), Ada.Text_IO.In_File);
      Ada.Text_IO.Reset(F(2), Ada.Text_IO.Out_File);
	    Ada.Text_IO.Reset(F(3), Ada.Text_IO.Out_File);
    ELSE   -- Seconds fichiers � trier, premiers fichiers pour rangement 
      Entr�e := 2;
      Sortie := 1;
      Ada.Text_IO.Reset(F(2), Ada.Text_IO.In_File);
	    Ada.Text_IO.Reset(F(3), Ada.Text_IO.In_File);
      Ada.Text_IO.Reset(F(0), Ada.Text_IO.Out_File);
	    Ada.Text_IO.Reset(F(1), Ada.Text_IO.Out_File);
    END IF; 
    LireArticle(F(Entr�e), Article1);
    LireArticle(F(Entr�e+1), Article2);
    LOOP -- Fusionner les s�ries des fichiers d'entr�e aux fichiers de sortie 
      IF (Sortie MOD 2 /= 0) THEN  -- Distribuer sur deux fichiers de sortie 
        Sortie := Sortie - 1;      -- Passer de 1 � 0, ou de 3 � 2 
      ELSE
        Sortie := Sortie + 1;      -- Passer de 0 � 1, ou de 2 � 3 
      END IF; 
      IF Limite >= Longueur THEN   -- Calculer les longueurs des fichiers 
        Longueur1 := Longueur;
      ELSE
        Longueur1 := Limite;
      END IF; 
      Limite := Limite - Longueur1;-- Mise � jour limite 
      IF Limite >= Longueur THEN
        Longueur2 := Longueur;
      ELSE
        Longueur2 := Limite;
      END IF; 
      Limite := Limite - Longueur2;
      R�sultant := Sortie;         -- Dernier fichier �crit 
      WHILE (Longueur1 /= 0) AND (Longueur2 /= 0) LOOP -- Fusion 
        IF Article2.Clef > Article1.Clef THEN
          �crireArticle(F(Sortie), Article1);
          LireArticle(F(Entr�e), Article1);
          Longueur1 := Longueur1 - 1;
        ELSE
          �crireArticle(F(Sortie), Article2);
          LireArticle(F(Entr�e+1), Article2);
          Longueur2 := Longueur2 - 1;
        END IF; 
      END LOOP;
      -- Copier la fin du second fichier 
      WHILE Longueur2 /= 0 LOOP
        �crireArticle(F(Sortie), Article2);
        LireArticle(F(Entr�e+1), Article2);
        Longueur2 := Longueur2 - 1;
      END LOOP; 
      -- Copier la fin du premier fichier 
      WHILE Longueur1 /= 0 LOOP
        �crireArticle(F(Sortie), Article1);
        LireArticle(F(Entr�e), Article1);
        Longueur1 := Longueur1 - 1;
      END LOOP; 
      EXIT WHEN Limite=0;
	  END LOOP;
    TicTac := NOT TicTac; -- �changer fichiers d'entr�e et de sortie 
    Longueur := 2 * Longueur;   -- Augmenter la longueur des s�ries 
    EXIT WHEN Longueur >= NombreArticles;
  END LOOP;
  Ada.Text_IO.Close(F(Sortie));
  Ada.Text_IO.Close(F(Sortie+1));
  Ada.Text_IO.Close(F(Entr�e));
  Ada.Text_IO.Close(F(Entr�e+1));
END TrierParFusion;
 
PROCEDURE Pr�parerEntr�e(FichierEntr�e: IN OUT Ada.Text_IO.File_Type;
                         F: IN OUT VecteurFichiers;
                         NombreArticles: IN OUT Natural) IS
-- Lire le fichier d'entr�e FichierEntr�e et le diviser en deux fichiers
-- temporaires F(0) et F(1) avec des s�ries de longueur 2.
-- Compter le nombre total d'�l�ments dans le fichier d'entr�e. 
Article1, Article2: �l�ments;
Sortie: Natural;
BEGIN
  Ada.Text_IO.Open(File => FichierEntr�e, Mode => Ada.Text_IO.IN_FILE, Name => "Merge.dat");
  Ada.Text_IO.Open(File => F(0), Mode => Ada.Text_IO.OUT_FILE, Name => "MFile1.dat");
  Ada.Text_IO.Open(File => F(1), Mode => Ada.Text_IO.OUT_FILE, Name => "MFile2.dat");
  NombreArticles := 0;
  Sortie := 0;
  WHILE NOT Ada.Text_IO.End_Of_File(FichierEntr�e) LOOP
    LireArticle(FichierEntr�e, Article1);
    NombreArticles := NombreArticles + 1;
    IF Ada.Text_IO.End_Of_File(FichierEntr�e) THEN
      �crireArticle(F(Sortie), Article1);
      EXIT;
    END IF; 
    LireArticle(FichierEntr�e, Article2);
    NombreArticles := NombreArticles + 1;
    IF Article1.Clef > Article2.Clef THEN -- Cr�er s�rie de longueur 2 
      �crireArticle(F(Sortie), Article2);
      �crireArticle(F(Sortie), Article1);
    ELSE
      �crireArticle(F(Sortie), Article1);
      �crireArticle(F(Sortie), Article2);
    END IF;
    Sortie := (Sortie + 1) MOD 2;         -- Alterner les fichiers de sortie 
  END LOOP; 
  Ada.Text_IO.Close(F(0));
  Ada.Text_IO.Close(F(1));
  Ada.Text_IO.Close(FichierEntr�e);
  EXCEPTION
    WHEN Ada.Text_IO.Status_Error => Ada.Text_IO.Put(Item => "Pr�parerEntr�e: erreur statut");
    WHEN Ada.Text_IO.Name_Error => Ada.Text_IO.Put(Item => "Pr�parerEntr�e: erreur nom");
END Pr�parerEntr�e;
 
FichierEntr�e: Ada.Text_IO.File_Type;
F: VecteurFichiers;
NombreArticles, R�sultat: Natural;
 
BEGIN      -- Tri par fusion 
  Ada.Text_IO.Create(File => F(0), Name => "MFile1.dat");
  Ada.Text_IO.Create(File => F(1), Name => "MFile2.dat");
  Ada.Text_IO.Create(File => F(2), Name => "MFile3.dat");
  Ada.Text_IO.Create(File => F(3), Name => "MFile4.dat");
  Ada.Text_IO.Close(F(0));
  Ada.Text_IO.Close(F(1));
  Ada.Text_IO.Close(F(2));
  Ada.Text_IO.Close(F(3));
  Pr�parerEntr�e(FichierEntr�e, F, NombreArticles);
  Ada.Text_IO.Put_Line(Item => "Fin Pr�parerEntr�e");
  TrierParFusion(F, R�sultat, NombreArticles);
  Ada.Text_IO.Put(Item => "R�sultats dans fichier: ");
  Ada.Integer_Text_IO.Put(Item => R�sultat, Width => 1); Ada.Text_IO.New_Line;
END TriFusion;
