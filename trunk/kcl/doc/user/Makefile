USERSGUIDE=kcluser

all:
	latex $(USERSGUIDE)
	latex $(USERSGUIDE)
pdf:
	pdflatex $(USERSGUIDE)
	pdflatex $(USERSGUIDE)
	thumbpdf $(USERSGUIDE)
	pdflatex $(USERSGUIDE)

smallpdf:
	pslatex $(USERSGUIDE)
	dvips $(USERSGUIDE)
	ps2pdf $(USERSGUIDE).ps $(USERSGUIDE)_small.pdf
	rm -f $(USERSGUIDE).ps $(USERSGUIDE).dvi

clean:
	rm -f *.aux *.log *.png *.toc *.out thumb*.*
mrproper:
	rm -f *.aux *.log *.png *.toc *.out thumb*.* *.pdf *.dvi
