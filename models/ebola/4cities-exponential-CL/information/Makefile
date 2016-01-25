MVF = /bin/mv -f
MV = /bin/mv

time:
	time mpirun -n 4 PISKa -i ebola-model-info-exponential.cka -i ebola-init-CL-info-4.cka  -i ebola-all_obs.cka -p 100 -t 100 -sync-t 1

base.simdir: base.model.cka base.city.cka base.obs.cka

new.simdir: new.model.cka new.city.cka base.obs.cka

%.simdir: 
	- $(MVF) $@ .$@
	time mpirun -n 4 PISKa \
		-i $(filter %.model.cka, $^) \
		-i $(filter %.city.cka, $^) \
		-i $(filter %.obs.cka, $^) \
		-p 100 -t 100 -sync-t 1
	mkdir $@
	$(MV) *.out $@

%.plots.Rout: %.simdir simPlots.R
	$(run-R)
