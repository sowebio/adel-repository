WITH Ada.Text_IO, Ada.Integer_Text_IO, EnsemblesDiscrets;
PROCEDURE ConstruireHoraire IS
-- Construire un horaire tel que les étudiants puissent prendre tous les cours
-- qu'ils désirent sans avoir à être dans deux cours au même moment.
NombredÉtudiants: CONSTANT Natural :=  5;
NombredeCours: CONSTANT Natural :=  6;
TailleHoraireMax: CONSTANT Natural := 10;
SUBTYPE TypeCours IS Natural RANGE 1..NombredeCours;
PACKAGE Ensembles IS NEW EnsemblesDiscrets(TypeCours);
SUBTYPE TypeÉtudiant IS Natural RANGE 1..NombredÉtudiants;
SUBTYPE TypeHoraire IS Natural RANGE 1..TailleHoraireMax;
SUBTYPE TypeHoraireZéro IS Natural RANGE 0..TailleHoraireMax;
SUBTYPE EnsembleCours IS Ensembles.TypEnsemble;
TYPE HoraireSession IS ARRAY (TypeHoraire) OF EnsembleCours;
TYPE TableauÉtudiants IS ARRAY (TypeÉtudiant) OF EnsembleCours;
TYPE TableauCours IS ARRAY (TypeCours) OF EnsembleCours;
TYPE TableauSession IS RECORD
                         NombreSessions: TypeHoraireZéro;
                         ListeSessions: HoraireSession;
                       END RECORD;
      
PROCEDURE ListerEnsembleCours(EnsembleDonné: IN EnsembleCours) IS
-- Afficher un ensemble de cours donné. 
DéjàSorti: Boolean := False;
BEGIN
  Ada.Text_IO.Put('{');
  FOR Cours IN 1..NombredeCours LOOP
    IF Ensembles.Membre(Cours, EnsembleDonné) THEN
      IF DéjàSorti THEN
        Ada.Text_IO.Put(", ");
      END IF;
      Ada.Integer_Text_IO.Put(Cours, 1);
      DéjàSorti := True;
    END IF;
  END LOOP;
  Ada.Text_IO.Put('}');
END ListerEnsembleCours;
   
PROCEDURE DéterminerConflits(Inscription: IN TableauÉtudiants; 
                             ListeConflits: OUT TableauCours) IS
-- Construire ensembles des conflits montrant pour chaque cours quels 
-- cours ne peuvent voir lieu en même temps.
BEGIN
  FOR NuméroCours IN 1..NombredeCours LOOP
    Ensembles.Vider(ListeConflits(NuméroCours));
  END LOOP;
  FOR Étudiant IN 1..NombredÉtudiants LOOP
    FOR NuméroCours IN 1..NombredeCours LOOP
      IF Ensembles.Membre(NuméroCours, Inscription(Étudiant)) THEN
        ListeConflits(NuméroCours) := 
            Ensembles."+"(ListeConflits(NuméroCours), Inscription(Étudiant));
      END IF;
    END LOOP;
  END LOOP;
END DéterminerConflits;
   
PROCEDURE SessionSuivante(Restant: IN EnsembleCours;
                          ListeConflits: IN TableauCours;
                          Session: OUT EnsembleCours) IS
-- Trouver la prochaine session possible qui contient autant de classes
-- que possible qui ne sont pas en conflit.
NuméroCours: TypeCours;
Essai: EnsembleCours;
BEGIN
  Ensembles.Vider(Session);
  NuméroCours := 1;
  WHILE NOT Ensembles.Membre(NuméroCours, Restant) LOOP
    NuméroCours := NuméroCours + 1;
  END LOOP;
  Ensembles.Inclure(Session, NuméroCours);
  Essai := Ensembles."-"(Restant, ListeConflits(NuméroCours));
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
-- Construire l'horaire à partir des ensembles de conflits.
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
    
Étudiants: TableauÉtudiants;
Conflits: TableauCours;
Horaire: TableauSession;
BEGIN
  -- Définir données inscription
  Ensembles.Inclure(Étudiants(1), 1);
  Ensembles.Inclure(Étudiants(1), 2);
  Ensembles.Inclure(Étudiants(2), 2);
  Ensembles.Inclure(Étudiants(2), 3);
  Ensembles.Inclure(Étudiants(3), 2);
  Ensembles.Inclure(Étudiants(3), 3);
  Ensembles.Inclure(Étudiants(3), 4);
  Ensembles.Inclure(Étudiants(4), 1);
  Ensembles.Inclure(Étudiants(4), 5);
  Ensembles.Inclure(Étudiants(4), 6);
  Ensembles.Inclure(Étudiants(5), 3);
  Ensembles.Inclure(Étudiants(5), 6);
   
  -- Construire et afficher liste conflits
  DéterminerConflits(Étudiants, Conflits);
  FOR NuméroCours IN 1..NombredeCours LOOP
    Ada.Text_IO.Put("Conflits(");
    Ada.Integer_Text_IO.Put(NuméroCours, 1);
    Ada.Text_IO.Put(") = ");
    ListerEnsembleCours(Conflits(NuméroCours));
    Ada.Text_IO.New_Line;
  END LOOP;
  Ada.Text_IO.New_Line;
   
  -- Construire et afficher horaire
  ConstruireHoraire(Conflits, Horaire);
  FOR NuméroSession IN 1..Horaire.NombreSessions LOOP
    Ada.Text_IO.Put("Session(");
    Ada.Integer_Text_IO.Put(NuméroSession, 2);
    Ada.Text_IO.Put(") = ");
    ListerEnsembleCours(Horaire.ListeSessions(NuméroSession));
    Ada.Text_IO.New_Line;
   END LOOP;
END ConstruireHoraire;



