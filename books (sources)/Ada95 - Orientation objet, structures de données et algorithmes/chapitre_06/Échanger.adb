PROCEDURE �changer(X, Y: IN OUT Quelconque) IS
Copie: Quelconque;
BEGIN
	 Copie := X;
	 X := Y;
	 Y := Copie;
END �changer;
