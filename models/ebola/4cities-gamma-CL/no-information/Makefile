MVF = /bin/mv -f
MV = /bin/mv
define run-R =
Rscript simPlots.R
mv Rplots.pdf $@
endef



time:
	time mpirun -n 4 PISKa -i ebola-model-gamma.cka -i ebola-init-CL-4.cka  -i ebola-all_obs.cka -p 100 -t 400 -sync-t 1

base.simdir: base.model.cka base.city.cka base.obs.cka

#base.simdir: %.model.cka %.city.cka %.obs.cka

new.simdir: new.model.cka new.city.cka base.obs.cka

#base.plots.Rout: 

%.simdir: 
	- $(MVF) $@ .$@
	time mpirun -n 4 PISKa \
		-i $(filter %.model.cka, $^) \
		-i $(filter %.city.cka, $^) \
		-i $(filter %.obs.cka, $^) \
		-p 100 -t 400 -sync-t 1
	mkdir $@
	$(MV) *.out $@

%.plots.Rout: %.simdir simPlots.R
	#$(run-R)
	Rscript simPlots.R
