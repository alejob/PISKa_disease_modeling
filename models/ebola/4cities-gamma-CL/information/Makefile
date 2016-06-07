MVF = /bin/mv -f
MV = /bin/mv
define run-R =
Rscript simPlots.R
mv Rplots.pdf $@
endef

time:
	time mpirun -n 4 PISKa -i ebola-model-info-gamma.cka -i ebola-init-CL-info-4.cka -i ebola-all_obs.cka -p 100 -t 400 -sync-t 1

mod-info:
	time mpirun -n 4 PISKa -i ebola-model-info-gamma.cka -i ebola-init-CL-info-4.cka -i ebola-all_obs.cka -i ebola-info_pert-fast.cka -p 100 -t 500 -sync-t 1
	mv *.out base.simdir

make-precompile:
	time mpirun -n 4 PISKa -i ebola-model-info-gamma.cka -i ebola-init-CL-info-4.cka -i ebola-all_obs.cka  -make-sim precomp.bout

load-precompile:
	time mpirun -n 4 PISKa -p 100 -t 500 -sync-t 1 -load-sim precomp.bout -i parameters.cka
	mv *.out base.simdir

load-precompile-10:
	for i in `seq 1 1000`; do\
		rm -r simdir/$$i;\
		time mpirun -n 4 PISKa -p 50 -t 200 -sync-t 1 -load-sim precomp.bout -i parameters.cka;\
		mkdir simdir/$$i;\
		mv *.out simdir/$$i;\
	done

test:
	for i in `seq 1 10`; do\
		rm -r $$i.kaka;\
		#echo $$i, primero;\
		#touch hola;\
		#mkdir $$i.kaka;\
		#mv hola $$i.kaka;\
	done 


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
