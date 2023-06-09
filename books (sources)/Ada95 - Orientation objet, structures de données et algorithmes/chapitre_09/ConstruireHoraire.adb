WITH Ada.Text_IO, Ada.Integer_Text_IO, EnsemblesDiscrets;
PROCEDURE ConstruireHoraire IS
-- Construire un horaire tel que les �tudiants puissent prendre tous les cours
-- qu'ils d�sirent sans avoir � �tre dans deux cours au m�me moment.
Nombred�tudiants: CONSTANT Natural :=  5;
NombredeCours: CONSTANT Natural :=  6;
TailleHoraireMax: CONSTANT Natural := 10;
SUBTYPE TypeCours IS Natural RANGE 1..NombredeCours;
PACKAGE Ensembles IS NEW EnsemblesDiscrets(TypeCours);
SUBTYPE Type�tudiant IS Natural RANGE 1..Nombred�tudiants;
SUBTYPE TypeHoraire IS Natural RANGE 1..TailleHoraireMax;
SUBTYPE TypeHoraireZ�ro IS Natural RANGE 0..TailleHoraireMax;
SUBTYPE EnsembleCours IS Ensembles.TypEnsemble;
TYPE HoraireSession IS ARRAY (TypeHoraire) OF EnsembleCours;
TYPE Tableau�tudiants IS ARRAY (Type�tudiant) OF EnsembleCours;
TYPE TableauCours IS ARRAY (TypeCours) OF EnsembleCours;
TYPE TableauSession IS RECORD
                         NombreSessions: TypeHoraireZ�ro;
                         ListeSessions: HoraireSession;
                       END RECORD;
      
PROCEDURE ListerEnsembleCours(EnsembleDonn�: IN EnsembleCours) IS
-- Afficher un ensemble de cours donn�. 
D�j�Sorti: Boolean := False;
BEGIN
  Ada.Text_IO.Put('{');
  FOR Cours IN 1..NombredeCours LOOP
    IF Ensembles.Membre(Cours, EnsembleDonn�) THEN
      IF D�j�Sorti THEN
        Ada.Text_IO.Put(", ");
      END IF;
      Ada.Integer_Text_IO.Put(Cours, 1);
      D�j�Sorti := True;
    END IF;
  END LOOP;
  Ada.Text_IO.Put('}');
END ListerEnsembleCours;
   
PROCEDURE D�terminerConflits(Inscription: IN Tableau�tudiants; 
                             ListeConflits: OUT TableauCours) IS
-- Construire ensembles des conflits montrant pour chaque cours quels 
-- cours ne peuvent voir lieu en m�me temps.
BEGIN
  FOR Num�roCours IN 1..NombredeCours LOOP
    Ensembles.Vider(ListeConflits(Num�roCours));
  END LOOP;
  FOR �tudiant IN 1..Nombred�tudiants LOOP
    FOR Num�roCours IN 1..NombredeCours LOOP
      IF Ensembles.Membre(Num�roCours, Inscription(�tudiant)) THEN
        ListeConflits(Num�roCours) := 
            Ensembles."+"(ListeConflits(Num�roCours), Inscription(�tudiant));
      END IF;
    END LOOP;
  END LOOP;
END D�terminerConflits;
   
PROCEDURE SessionSuivante(Restant: IN EnsembleCours;
                          ListeConflits: IN TableauCours;
                          Session: OUT EnsembleCours) IS
-- Trouver la prochaine session possible qui contient autant de classes
-- que possible qui ne sont pas en conflit.
Num�roCours: TypeCours;
Essai: EnsembleCours;
BEGIN
  Ensembles.Vider(Session);
  Num�roCours := 1;
  WHILE NOT Ensembles.Membre(Num�roCours, Restant) LOOP
    Num�roCours := Num�roCours + 1;
  END LOOP;
  Ensembles.Inclure(Session, Num�roCours);
  Essai := Ensembles."-"(Restant, ListeConflits(Num�roCours));
  FOR CoursEssai IN 1..NombredeCours LOOP
    IF Ensembles.Membre(CoursEssai, Essai) THEN
      IF Ensembles.Vide(Ensembles."*"(ListeConflits(CoursEssai), Session)) THEN
        Ensembles.Inclure(Session, CoursEssai);
      END IF;
    END IF;
  END LOOP;
END SessionSuivante;
   
PROCEDURE ConstruireHoraire(ListeConflits: IN TableauCours;
                            Horaire: OUT TableauSession) IS
-- Construire l'horaire � partir des ensembles de conflits.
Restant, Session: EnsembleCours;
BEGIN
  Horaire.NombreSessions := 0;
  FOR Cours IN 1..NombredeCours LOOP
    Ensembles.Inclure(Restant, Cours);
  END LOOP;
  WHILE NOT Ensembles.Vide(Restant) LOOP
    SessionSuivante(Restant, ListeConflits, Session);
    Restant := Ensembles."-"(Restant, Session);
    Horaire.NombreSessions := Horaire.NombreSessions + 1;
    Horaire.ListeSessions(Horaire.NombreSessions) := Session;
  END LOOP;
END ConstruireHoraire;
    
�tudiants: Tableau�tudiants;
Conflits: TableauCours;
Horaire: TableauSession;
BEGIN
  -- D�finir donn�es inscription
  Ensembles.Inclure(�tudiants(1), 1);
  Ensembles.Inclure(�tudiants(1), 2);
  Ensembles.Inclure(�tudiants(2), 2);
  Ensembles.Inclure(�tudiants(2), 3);
  Ensembles.Inclure(�tudiants(3), 2);
  Ensembles.Inclure(�tudiants(3), 3);
  Ensembles.Inclure(�tudiants(3), 4);
  Ensembles.Inclure(�tudiants(4), 1);
  Ensembles.Inclure(�tudiants(4), 5);
  Ensembles.Inclure(�tudiants(4), 6);
  Ensembles.Inclure(�tudiants(5), 3);
  Ensembles.Inclure(�tudiants(5), 6);
   
  -- Construire et afficher liste conflits
  D�terminerConflits(�tudiants, Conflits);
  FOR Num�roCours IN 1..NombredeCours LOOP
    Ada.Text_IO.Put("Conflits(");
    Ada.Integer_Text_IO.Put(Num�roCours, 1);
    Ada.Text_IO.Put(") = ");
    ListerEnsembleCours(Conflits(Num�roCours));
    Ada.Text_IO.New_Line;
  END LOOP;
  Ada.Text_IO.New_Line;
   
  -- Construire et afficher horaire
  ConstruireHoraire(Conflits, Horaire);
  FOR Num�roSession IN 1..Horaire.NombreSessions LOOP
    Ada.Text_IO.Put("Session(");
    Ada.Integer_Text_IO.Put(Num�roSession, 2);
    Ada.Text_IO.Put(") = ");
    ListerEnsembleCours(Horaire.ListeSessions(Num�roSession));
    Ada.Text_IO.New_Line;
   END LOOP;
END ConstruireHoraire;



