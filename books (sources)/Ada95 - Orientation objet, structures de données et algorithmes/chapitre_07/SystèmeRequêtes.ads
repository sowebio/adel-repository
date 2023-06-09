--          Copyright � 1998 Philippe J. Gabrini
WITH Cha�nes;
PACKAGE Syst�meRequ�tes IS

SUBTYPE TypeClefs IS Cha�nes.TypCha�ne;
TYPE Type�l�ments IS PRIVATE;
Secret: CONSTANT String; -- mot de passe administratif


FUNCTION Pr�sent(Identit�: TypeClefs) RETURN Boolean;
-- Retourne True si Identit� identifie un membre.

PROCEDURE AjouterMembre(Identit�: IN TypeClefs);
-- Ajouter un nouveau membre � la table des membres.

PROCEDURE TrouverNom(Identit�: IN TypeClefs; Nom: OUT Cha�nes.TypCha�ne;
                     Trouv�: IN OUT Boolean);
-- Si Identit� identifie un membre Trouv�' = True
-- et Nom' = nom du membre, sinon Trouv�' = False.

FUNCTION MotPasseValide(Identit�: TypeClefs;
                        MotPasse: Cha�nes.TypCha�ne) RETURN Boolean;
-- Retourne True si Identit� identifie un membre avec le mot de passe MotPasse.

PROCEDURE TrouverT�l�phone(Identit�: IN TypeClefs;
                           T�l�phone: OUT Cha�nes.TypCha�ne;
                           Trouv�: IN OUT Boolean);
-- Si Identit� identifie un membre Trouv�' = True
-- et T�l�phone' = num�ro de t�l�phone du membre sinon Trouv�' = False.

PROCEDURE TrouverAdresse(Identit�: IN TypeClefs;
                         Adresse: OUT Cha�nes.TypCha�ne;
                         Trouv�: IN OUT Boolean);
-- Si Identit� identifie un membre Trouv�' = True
-- et Adresse' = adresse du membre sinon Trouv�' = False.

PROCEDURE �liminer(Identit�: IN TypeClefs; Succ�s: OUT Boolean);
-- �liminer le membre identifi� par Identit� de la table des membres.
-- Retourne True si le membre a �t� �limin�, False autrement.

PROCEDURE ChangerMotPasse(Identit�: IN TypeClefs;
                          MotDePasse: IN Cha�nes.TypCha�ne;
                          Succ�s: OUT Boolean);
-- Changer le mot de passe d'un membre existant. Retourne True
-- si le mot de passe a �t� chang�, False autrement.

PROCEDURE ListerTous;
-- Afficher les clefs de tout le monde.

PROCEDURE Sauvegarder;
-- Sauvegarder toute l'information dans un fichier.

PRIVATE
Secret: CONSTANT String := "Fahrenheit451"; -- mot de passe administratif
TYPE Type�l�ments IS RECORD
                       Clef: TypeClefs;
                       Nom: TypeClefs;
                       Adresse: TypeClefs;
                       MotPasse: TypeClefs;
                       T�l�phone: TypeClefs;
                     END RECORD;
                   
END Syst�meRequ�tes;
