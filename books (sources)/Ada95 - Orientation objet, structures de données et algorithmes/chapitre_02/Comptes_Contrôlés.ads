WITH Ada.Finalization, Ada.Text_IO;
PACKAGE Comptes_Contrôlés IS

TYPE Compte_Contrôlé IS NEW Ada.Finalization.Controlled WITH PRIVATE;

PROCEDURE Déposer(Dans_Compte: IN OUT Compte_Contrôlé; Montant: IN Float);
PROCEDURE Retirer(Du_Compte: IN OUT Compte_Contrôlé; Montant: IN Float);
FUNCTION Intérêt(Compte: IN Compte_Contrôlé) RETURN Float;
FUNCTION Solde(Compte: IN Compte_Contrôlé) RETURN Float;

PRIVATE
TYPE Transaction;
TYPE Pointeur IS ACCESS Transaction;
TYPE Transaction IS RECORD
                      Valeur: Float;
                      Lien: Pointeur;
                    END RECORD;

TYPE Compte_Contrôlé IS NEW Ada.Finalization.Controlled WITH
  RECORD
    PremièreTransaction: Pointeur;
    DernièreTransaction: Pointeur;
    Solde: Float := 0.0;
  END RECORD;
  
Journal: Ada.Text_IO.File_Type; -- fichier des audits
Comptes_actifs: Natural := 0;   -- nombre de comptes actifs
    
PROCEDURE Initialize(Compte: IN OUT Compte_Contrôlé);
PROCEDURE Adjust(Compte: IN OUT Compte_Contrôlé);
PROCEDURE Finalize(Compte: IN OUT Compte_Contrôlé);

END Comptes_Contrôlés;
