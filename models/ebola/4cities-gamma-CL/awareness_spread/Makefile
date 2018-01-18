MVF = /bin/mv -f
MV = /bin/mv
define run-R =
Rscript simPlots.R
mv Rplots.pdf $@
endef

time:
	time mpirun -n 2 PISKa -i information_transmission.cka -p 100 -t 400 -sync-t 1
	if [ -d "base.simdir" ]; then rm -Rf "base.simdir"; fi
	mkdir base.simdir
	mv *.out base.simdir

