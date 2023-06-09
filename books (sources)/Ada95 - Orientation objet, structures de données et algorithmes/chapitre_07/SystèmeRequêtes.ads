--          Copyright © 1998 Philippe J. Gabrini
WITH Chaînes;
PACKAGE SystèmeRequêtes IS

SUBTYPE TypeClefs IS Chaînes.TypChaîne;
TYPE TypeÉléments IS PRIVATE;
Secret: CONSTANT String; -- mot de passe administratif


FUNCTION Présent(Identité: TypeClefs) RETURN Boolean;
-- Retourne True si Identité identifie un membre.

PROCEDURE AjouterMembre(Identité: IN TypeClefs);
-- Ajouter un nouveau membre ˆ la table des membres.

PROCEDURE TrouverNom(Identité: IN TypeClefs; Nom: OUT Chaînes.TypChaîne;
                     Trouvé: IN OUT Boolean);
-- Si Identité identifie un membre Trouvé' = True
-- et Nom' = nom du membre, sinon Trouvé' = False.

FUNCTION MotPasseValide(Identité: TypeClefs;
                        MotPasse: Chaînes.TypChaîne) RETURN Boolean;
-- Retourne True si Identité identifie un membre avec le mot de passe MotPasse.

PROCEDURE TrouverTéléphone(Identité: IN TypeClefs;
                           Téléphone: OUT Chaînes.TypChaîne;
                           Trouvé: IN OUT Boolean);
-- Si Identité identifie un membre Trouvé' = True
-- et Téléphone' = numŽro de tŽlŽphone du membre sinon Trouvé' = False.

PROCEDURE TrouverAdresse(Identité: IN TypeClefs;
                         Adresse: OUT Chaînes.TypChaîne;
                         Trouvé: IN OUT Boolean);
-- Si Identité identifie un membre Trouvé' = True
-- et Adresse' = adresse du membre sinon Trouvé' = False.

PROCEDURE Éliminer(Identité: IN TypeClefs; Succès: OUT Boolean);
-- ƒliminer le membre identifiŽ par Identité de la table des membres.
-- Retourne True si le membre a ŽtŽ ŽliminŽ, False autrement.

PROCEDURE ChangerMotPasse(Identité: IN TypeClefs;
                          MotDePasse: IN Chaînes.TypChaîne;
                          Succès: OUT Boolean);
-- Changer le mot de passe d'un membre existant. Retourne True
-- si le mot de passe a ŽtŽ changŽ, False autrement.

PROCEDURE ListerTous;
-- Afficher les clefs de tout le monde.

PROCEDURE Sauvegarder;
-- Sauvegarder toute l'information dans un fichier.

PRIVATE
Secret: CONSTANT String := "Fahrenheit451"; -- mot de passe administratif
TYPE TypeÉléments IS RECORD
                       Clef: TypeClefs;
                       Nom: TypeClefs;
                       Adresse: TypeClefs;
                       MotPasse: TypeClefs;
                       Téléphone: TypeClefs;
                     END RECORD;
                   
END SystèmeRequêtes;
