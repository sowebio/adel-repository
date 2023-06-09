PACKAGE Base_Compte IS

TYPE Compte_de_base IS ABSTRACT TAGGED LIMITED NULL RECORD;

PROCEDURE D�poser(Dans_Compte: IN OUT Compte_de_base; Montant: IN Float)
                  IS ABSTRACT;
PROCEDURE Retirer(Du_Compte: IN OUT Compte_de_base; Montant: IN Float)
                  IS ABSTRACT;
FUNCTION Int�r�t(Compte: IN Compte_de_base) RETURN Float IS ABSTRACT;
FUNCTION Solde(Compte: IN Compte_de_base) RETURN Float IS ABSTRACT;

END Base_Compte;
