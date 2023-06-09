WITH Ada.Finalization, Ada.Text_IO;
PACKAGE Comptes_Contr�l�s IS

TYPE Compte_Contr�l� IS NEW Ada.Finalization.Controlled WITH PRIVATE;

PROCEDURE D�poser(Dans_Compte: IN OUT Compte_Contr�l�; Montant: IN Float);
PROCEDURE Retirer(Du_Compte: IN OUT Compte_Contr�l�; Montant: IN Float);
FUNCTION Int�r�t(Compte: IN Compte_Contr�l�) RETURN Float;
FUNCTION Solde(Compte: IN Compte_Contr�l�) RETURN Float;

PRIVATE
TYPE Transaction;
TYPE Pointeur IS ACCESS Transaction;
TYPE Transaction IS RECORD
                      Valeur: Float;
                      Lien: Pointeur;
                    END RECORD;

TYPE Compte_Contr�l� IS NEW Ada.Finalization.Controlled WITH
  RECORD
    Premi�reTransaction: Pointeur;
    Derni�reTransaction: Pointeur;
    Solde: Float := 0.0;
  END RECORD;
  
Journal: Ada.Text_IO.File_Type; -- fichier des audits
Comptes_actifs: Natural := 0;   -- nombre de comptes actifs
    
PROCEDURE Initialize(Compte: IN OUT Compte_Contr�l�);
PROCEDURE Adjust(Compte: IN OUT Compte_Contr�l�);
PROCEDURE Finalize(Compte: IN OUT Compte_Contr�l�);

END Comptes_Contr�l�s;
