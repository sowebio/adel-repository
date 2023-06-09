--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PROCEDURE TriFusion IS
-- Application de l'algorithme de fusion à un fichier d'enregistrements
--             P. Gabrini     Février 1989

LongueurClef: CONSTANT Natural := 2;
LongueurInfo: CONSTANT Natural := 8;
LongueurEnregistrement: CONSTANT Natural := LongueurClef + LongueurInfo;
 
TYPE Éléments IS RECORD
                   Clef: String(1..LongueurClef);
                   Info: String(1..LongueurInfo);
                 END RECORD;
TYPE VecteurFichiers IS ARRAY(0..3) OF Ada.Text_IO.File_Type;
 
PROCEDURE LireArticle(F: IN OUT Ada.Text_IO.File_Type; Élt: IN OUT Éléments) IS
-- Lire un article à partir du fichier F. Terminer si fin de fichier. 
BEGIN
  IF NOT Ada.Text_IO.End_Of_File(F) THEN
    FOR i IN 1..LongueurClef LOOP
      Ada.Text_IO.Get(File => F, Item => Élt.Clef(i));
    END LOOP;
    FOR i IN 1..LongueurInfo LOOP
      Ada.Text_IO.Get(File => F, Item => Élt.Info(i));
    END LOOP;
  END IF;
END LireArticle;
 
PROCEDURE ÉcrireArticle(F: IN OUT Ada.Text_IO.File_Type; Élt: IN Éléments) IS
-- Écrire un article dans le fichier F 
BEGIN
  FOR i IN 1..LongueurClef LOOP
    Ada.Text_IO.Put(File => F, Item => Élt.Clef(i));
  END LOOP; 
  FOR i IN 1..LongueurInfo LOOP
    Ada.Text_IO.Put(File => F, Item => Élt.Info(i));
  END LOOP; 
END ÉcrireArticle;
 
PROCEDURE TrierParFusion(F: IN OUT VecteurFichiers; Résultant: OUT Natural;
                         NombreArticles: IN Natural) IS
-- Tri d'un fichier d'enregistrements sur le champ Clef.  Le tri est
-- fait en combinant (fusionnant) des séries de longueur 2, 4, 8 ...
-- en utilisant 4 fichiers temporaires F[0],F[1],F[2],F[3].
-- Initialement on a des séries de longueur 2 dans les fichiers F[0]
-- et F[1]. Le tri est fait en plusieurs passes, chaque passe 
-- combinant des séries d'une longueur donnée jusqu'à ce que la 
-- longueur des séries dépasse la taille du fichier.  À la fin du tri 
-- les enregistrements triés seront dans le fichier temporaire 
-- F[Résultant].
 
Entrée, Sortie,                -- Designe les fichiers d'entrée et de sortie 
Limite,                        -- Nombre d'enregistrements qui restent à trier 
Longueur,                      -- Longueur de la série en cours 
Longueur1, Longueur2: Natural; -- Longueurs de série1 et série2 
TicTac: Boolean;               -- Indique quels fichiers sont utilisés 
Article1, Article2: Éléments;  -- Enregistrements temporaraires
 
BEGIN
  -- Ouvrir les fichiers pour qu'ils puissent être échangés tout de suite 
  Ada.Text_IO.Open(File => F(0), Mode => Ada.Text_IO.In_File, Name => "MFile1.dat");
  Ada.Text_IO.Open(File => F(1), Mode => Ada.Text_IO.In_File, Name => "MFile2.dat");
  Ada.Text_IO.Open(File => F(2), Mode => Ada.Text_IO.In_File, Name => "MFile3.dat");
  Ada.Text_IO.Open(File => F(3), Mode => Ada.Text_IO.In_File, Name => "MFile4.dat");
  TicTac := True;
  Longueur := 2;              -- Commencer avec des séries de longueur 2 
  LOOP                        -- Trier les séries 
    Limite := NombreArticles;
    IF TicTac THEN  -- Premiers fichiers à trier, seconds pour rangement 
      Entrée := 0;
      Sortie := 3;
      Ada.Text_IO.Reset(F(0), Ada.Text_IO.In_File);
	    Ada.Text_IO.Reset(F(1), Ada.Text_IO.In_File);
      Ada.Text_IO.Reset(F(2), Ada.Text_IO.Out_File);
	    Ada.Text_IO.Reset(F(3), Ada.Text_IO.Out_File);
    ELSE   -- Seconds fichiers à trier, premiers fichiers pour rangement 
      Entrée := 2;
      Sortie := 1;
      Ada.Text_IO.Reset(F(2), Ada.Text_IO.In_File);
	    Ada.Text_IO.Reset(F(3), Ada.Text_IO.In_File);
      Ada.Text_IO.Reset(F(0), Ada.Text_IO.Out_File);
	    Ada.Text_IO.Reset(F(1), Ada.Text_IO.Out_File);
    END IF; 
    LireArticle(F(Entrée), Article1);
    LireArticle(F(Entrée+1), Article2);
    LOOP -- Fusionner les séries des fichiers d'entrée aux fichiers de sortie 
      IF (Sortie MOD 2 /= 0) THEN  -- Distribuer sur deux fichiers de sortie 
        Sortie := Sortie - 1;      -- Passer de 1 à 0, ou de 3 à 2 
      ELSE
        Sortie := Sortie + 1;      -- Passer de 0 à 1, ou de 2 à 3 
      END IF; 
      IF Limite >= Longueur THEN   -- Calculer les longueurs des fichiers 
        Longueur1 := Longueur;
      ELSE
        Longueur1 := Limite;
      END IF; 
      Limite := Limite - Longueur1;-- Mise à jour limite 
      IF Limite >= Longueur THEN
        Longueur2 := Longueur;
      ELSE
        Longueur2 := Limite;
      END IF; 
      Limite := Limite - Longueur2;
      Résultant := Sortie;         -- Dernier fichier écrit 
      WHILE (Longueur1 /= 0) AND (Longueur2 /= 0) LOOP -- Fusion 
        IF Article2.Clef > Article1.Clef THEN
          ÉcrireArticle(F(Sortie), Article1);
          LireArticle(F(Entrée), Article1);
          Longueur1 := Longueur1 - 1;
        ELSE
          ÉcrireArticle(F(Sortie), Article2);
          LireArticle(F(Entrée+1), Article2);
          Longueur2 := Longueur2 - 1;
        END IF; 
      END LOOP;
      -- Copier la fin du second fichier 
      WHILE Longueur2 /= 0 LOOP
        ÉcrireArticle(F(Sortie), Article2);
        LireArticle(F(Entrée+1), Article2);
        Longueur2 := Longueur2 - 1;
      END LOOP; 
      -- Copier la fin du premier fichier 
      WHILE Longueur1 /= 0 LOOP
        ÉcrireArticle(F(Sortie), Article1);
        LireArticle(F(Entrée), Article1);
        Longueur1 := Longueur1 - 1;
      END LOOP; 
      EXIT WHEN Limite=0;
	  END LOOP;
    TicTac := NOT TicTac; -- Échanger fichiers d'entrée et de sortie 
    Longueur := 2 * Longueur;   -- Augmenter la longueur des séries 
    EXIT WHEN Longueur >= NombreArticles;
  END LOOP;
  Ada.Text_IO.Close(F(Sortie));
  Ada.Text_IO.Close(F(Sortie+1));
  Ada.Text_IO.Close(F(Entrée));
  Ada.Text_IO.Close(F(Entrée+1));
END TrierParFusion;
 
PROCEDURE PréparerEntrée(FichierEntrée: IN OUT Ada.Text_IO.File_Type;
                         F: IN OUT VecteurFichiers;
                         NombreArticles: IN OUT Natural) IS
-- Lire le fichier d'entrée FichierEntrée et le diviser en deux fichiers
-- temporaires F(0) et F(1) avec des séries de longueur 2.
-- Compter le nombre total d'éléments dans le fichier d'entrée. 
Article1, Article2: Éléments;
Sortie: Natural;
BEGIN
  Ada.Text_IO.Open(File => FichierEntrée, Mode => Ada.Text_IO.IN_FILE, Name => "Merge.dat");
  Ada.Text_IO.Open(File => F(0), Mode => Ada.Text_IO.OUT_FILE, Name => "MFile1.dat");
  Ada.Text_IO.Open(File => F(1), Mode => Ada.Text_IO.OUT_FILE, Name => "MFile2.dat");
  NombreArticles := 0;
  Sortie := 0;
  WHILE NOT Ada.Text_IO.End_Of_File(FichierEntrée) LOOP
    LireArticle(FichierEntrée, Article1);
    NombreArticles := NombreArticles + 1;
    IF Ada.Text_IO.End_Of_File(FichierEntrée) THEN
      ÉcrireArticle(F(Sortie), Article1);
      EXIT;
    END IF; 
    LireArticle(FichierEntrée, Article2);
    NombreArticles := NombreArticles + 1;
    IF Article1.Clef > Article2.Clef THEN -- Créer série de longueur 2 
      ÉcrireArticle(F(Sortie), Article2);
      ÉcrireArticle(F(Sortie), Article1);
    ELSE
      ÉcrireArticle(F(Sortie), Article1);
      ÉcrireArticle(F(Sortie), Article2);
    END IF;
    Sortie := (Sortie + 1) MOD 2;         -- Alterner les fichiers de sortie 
  END LOOP; 
  Ada.Text_IO.Close(F(0));
  Ada.Text_IO.Close(F(1));
  Ada.Text_IO.Close(FichierEntrée);
  EXCEPTION
    WHEN Ada.Text_IO.Status_Error => Ada.Text_IO.Put(Item => "PréparerEntrée: erreur statut");
    WHEN Ada.Text_IO.Name_Error => Ada.Text_IO.Put(Item => "PréparerEntrée: erreur nom");
END PréparerEntrée;
 
FichierEntrée: Ada.Text_IO.File_Type;
F: VecteurFichiers;
NombreArticles, Résultat: Natural;
 
BEGIN      -- Tri par fusion 
  Ada.Text_IO.Create(File => F(0), Name => "MFile1.dat");
  Ada.Text_IO.Create(File => F(1), Name => "MFile2.dat");
  Ada.Text_IO.Create(File => F(2), Name => "MFile3.dat");
  Ada.Text_IO.Create(File => F(3), Name => "MFile4.dat");
  Ada.Text_IO.Close(F(0));
  Ada.Text_IO.Close(F(1));
  Ada.Text_IO.Close(F(2));
  Ada.Text_IO.Close(F(3));
  PréparerEntrée(FichierEntrée, F, NombreArticles);
  Ada.Text_IO.Put_Line(Item => "Fin PréparerEntrée");
  TrierParFusion(F, Résultat, NombreArticles);
  Ada.Text_IO.Put(Item => "Résultats dans fichier: ");
  Ada.Integer_Text_IO.Put(Item => Résultat, Width => 1); Ada.Text_IO.New_Line;
END TriFusion;
