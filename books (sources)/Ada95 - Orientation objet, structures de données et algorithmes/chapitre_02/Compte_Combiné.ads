WITH Compte_Épargne;
WITH Compte_Chèque;
PACKAGE Compte_Combiné IS

TYPE Objet IS TAGGED LIMITED PRIVATE;

TYPE Épargne(Identifié: ACCESS Objet'Class) IS
     NEW Compte_Épargne.C_Épargne WITH NULL RECORD;

-- on doit redéfinir les sous-programmes ci-dessous pour appel par "Identifie"
PROCEDURE Déposer(Dans_Compte: IN OUT Épargne; Montant: IN Float);
PROCEDURE Retirer(Du_Compte: IN OUT Épargne; Montant: IN Float);
FUNCTION Intérêt(Compte: IN Épargne) RETURN Float;
FUNCTION Solde(Compte: IN Épargne) RETURN Float;

TYPE Chèque(Identifié: ACCESS Objet'Class) IS
     NEW Compte_Chèque.Compte_à_chèques WITH NULL RECORD;

PROCEDURE Déposer(Dans_Compte: IN OUT Chèque; Montant: IN Float);
PROCEDURE Retirer(Du_Compte: IN OUT Chèque; Montant: IN Float);
FUNCTION Intérêt(Compte: IN Chèque) RETURN Float;
FUNCTION Solde(Compte: IN Chèque) RETURN Float;

-- ces opérations appelleront soit les opérations de Compte_Épargne, soit
-- les opérations de Compte_Chèque.  On hérite ainsi de toutes les opérations
-- des comptes chèques et d'épargne

PROCEDURE Déposer(Dans_Compte: IN OUT Objet; Montant: IN Float);
PROCEDURE Retirer(Du_Compte: IN OUT Objet; Montant: IN Float);
FUNCTION Intérêt(Compte: IN Objet) RETURN Float;
FUNCTION Solde(Compte: IN Objet) RETURN Float;

PRIVATE
  TYPE Objet IS TAGGED LIMITED
    RECORD
      Vue_Épargne: Épargne(Objet'ACCESS);
      Vue_Chèque:  Chèque(Objet'ACCESS);
    END RECORD;
END Compte_Combiné;
