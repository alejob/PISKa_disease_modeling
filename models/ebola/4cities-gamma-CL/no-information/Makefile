MVF = /bin/mv -f
MV = /bin/mv
define run-R =
Rscript simPlots.R
mv Rplots.pdf $@
endef

time:
	time mpirun -n 4 PISKa -i model-gamma.cka -i CL-4cities.cka -i observables.cka -p 100 -t 400 -sync-t 1
	if [ -d "base.simdir" ]; then rm -Rf "base.simdir"; fi
	mkdir base.simdir
	mv *.out base.simdir

mod-info:
	time mpirun -n 4 PISKa -i model-gamma.cka -i CL-4cities.cka -i observables.cka -i perturbation.cka -p 100 -t 500 -sync-t 1
	mv *.out base.simdir

make-precompile:
	time mpirun -n 4 PISKa -i model-gamma.cka -i CL-4cities.cka -i observables.cka -make-sim precomp.bout

load-precompile:
	time mpirun -n 4 PISKa -p 100 -t 200 -sync-t 1 -load-sim precomp.bout -i parameters.cka
	mv *.out base.simdir
	cp parameters.cka base.simdir
	cp model-gamma.cka base.simdir

load-precompile-adjust:
	time mpirun -n 4 PISKa -p 100 -t 400 -sync-t 1 -load-sim precomp.bout -i parameters_test.cka -seed 3
	mv *.out base.simdir

load-precompile-100:
	if [ -d "simdir_info" ]; then rm -Rf "simdir_info";fi
	mkdir simdir_info
	for i in `seq 1 4`; do\
		mkdir simdir_info/$$i;\
		mpirun -n 4 PISKa -p 100 -t 200 -sync-t 1 -load-sim precomp.bout -i parameters.cka -d simdir_info/$$i &\
                #mv *.out simdir_info/$$i;\
	done
	if [ -d "simdir_info/model" ]; then rm -Rf "simdir_info/model";fi
	mkdir simdir_info/model
	cp *.cka simdir_info/model

load-precompile-1000:
	for i in `seq 1 1000`; do\
		rm -r simdir_info/$$i;\
		mpirun -n 4 PISKa -p 50 -t 400 -sync-t 1 -load-sim precomp.bout -i parameters.cka;\
		mkdir simdir_info/$$i;\
		mv *.out simdir_info/$$i;\
	done

test:
	for i in `seq 1 10`; do\
                #rm -r $$i.kaka;\
		echo $$i, primero;\
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
