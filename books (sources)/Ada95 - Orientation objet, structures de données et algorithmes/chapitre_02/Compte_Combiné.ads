WITH Compte_�pargne;
WITH Compte_Ch�que;
PACKAGE Compte_Combin� IS

TYPE Objet IS TAGGED LIMITED PRIVATE;

TYPE �pargne(Identifi�: ACCESS Objet'Class) IS
     NEW Compte_�pargne.C_�pargne WITH NULL RECORD;

-- on doit red�finir les sous-programmes ci-dessous pour appel par "Identifie"
PROCEDURE D�poser(Dans_Compte: IN OUT �pargne; Montant: IN Float);
PROCEDURE Retirer(Du_Compte: IN OUT �pargne; Montant: IN Float);
FUNCTION Int�r�t(Compte: IN �pargne) RETURN Float;
FUNCTION Solde(Compte: IN �pargne) RETURN Float;

TYPE Ch�que(Identifi�: ACCESS Objet'Class) IS
     NEW Compte_Ch�que.Compte_�_ch�ques WITH NULL RECORD;

PROCEDURE D�poser(Dans_Compte: IN OUT Ch�que; Montant: IN Float);
PROCEDURE Retirer(Du_Compte: IN OUT Ch�que; Montant: IN Float);
FUNCTION Int�r�t(Compte: IN Ch�que) RETURN Float;
FUNCTION Solde(Compte: IN Ch�que) RETURN Float;

-- ces op�rations appelleront soit les op�rations de Compte_�pargne, soit
-- les op�rations de Compte_Ch�que.  On h�rite ainsi de toutes les op�rations
-- des comptes ch�ques et d'�pargne

PROCEDURE D�poser(Dans_Compte: IN OUT Objet; Montant: IN Float);
PROCEDURE Retirer(Du_Compte: IN OUT Objet; Montant: IN Float);
FUNCTION Int�r�t(Compte: IN Objet) RETURN Float;
FUNCTION Solde(Compte: IN Objet) RETURN Float;

PRIVATE
  TYPE Objet IS TAGGED LIMITED
    RECORD
      Vue_�pargne: �pargne(Objet'ACCESS);
      Vue_Ch�que:  Ch�que(Objet'ACCESS);
    END RECORD;
END Compte_Combin�;
