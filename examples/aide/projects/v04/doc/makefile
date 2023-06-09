file = v04
visu = "/d/Program Files/Adobe/Acrobat 4.0/Acrobat/acrobat" 

$(file) :
	cp $(file).texi $(file).texinfo
#	recode -v l1..texi $(file).texinfo
	recode -v -f ISO-8859-15..texi $(file).texinfo
	pdftex $(file).texinfo
#	tex  $(file).texinfo
#	dvipdfm  $(file).dvi
	rm $(file).texinfo
	$(visu) $(file).pdf 

.PHONY : clean
clean :
	-rm *.aux
	-rm *.cp
	-rm *.dvi
	-rm *.fn
	-rm *.ky
	-rm *.log
	-rm *.pg
	-rm *.toc
	-rm *.tp
	-rm *.vr

