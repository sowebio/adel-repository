PACKAGE Base_Compte IS

TYPE Compte_de_base IS ABSTRACT TAGGED LIMITED NULL RECORD;

PROCEDURE Déposer(Dans_Compte: IN OUT Compte_de_base; Montant: IN Float)
                  IS ABSTRACT;
PROCEDURE Retirer(Du_Compte: IN OUT Compte_de_base; Montant: IN Float)
                  IS ABSTRACT;
FUNCTION Intérêt(Compte: IN Compte_de_base) RETURN Float IS ABSTRACT;
FUNCTION Solde(Compte: IN Compte_de_base) RETURN Float IS ABSTRACT;

END Base_Compte;
