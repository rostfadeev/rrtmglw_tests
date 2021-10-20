# Sample Makefile for compilation and linking of the SLM benchmark
ALLINC = -I.

CFLAGS =  -O3 -xSSE4.2 

EXEC=rrtmglwtest

SLAV_CONFIGURATION = 

#IPO =-ipo
IPO=
INC_DIR=inc
OBJ_DIR=obj
RUN_DIR=../run

NETCDF_DIR=/RHM-Lustre3.2/users/wg-slmod/rfadeev/NWPlab/soft/2021/fin_pnetcdf
#NETCDF_DIR=../../soft/2021/
NETCDF_DIR=

#MKL_HOME=/home/fadeev/soft/linux/mkl
#MPI_HOME=/home/fadeev/soft/linux/mpi/intel64/lib/release
#MPI_HOME=/home/fadeev/soft/lib
#MPI_HOME=/opt/intel/compilers_and_libraries_2020.4.304/linux/mpi/intel64/lib


CHECK=-CB -CU -CA -init=snan -init=arrays -ftrapuv 
#CHECK=

ADDFL = -I $(NETCDF_DIR)/include $(CHECK) -fpe0 -mp1 -qopenmp -ftz -fpp  -static-intel -warn interfaces -module $(INC_DIR)
#ADDFL= -mp1 -qopenmp  -I $(NETCDF_DIR)/inc -I $(INC_DIR) -ftz -fpp  -static-intel -module $(INC_DIR)
OPTFL =   -fp-model fast=1 
FFLAGS  =  ${OPTFL} -DMPI -DRHMC -DIA64 -fpp -traceback -r8 ${ADDFL}
FFLAGS1  =  -r8 -fp-model precise -traceback -DMPI ${ADDFL}

# attention: compilation of rrtmg_lw_k_g.f90 absolutely requires -O0!
FFLAGS3  = -O2 -r8 -no-fma -fp-model precise -traceback -DMPI ${ADDFL}
FFLAGS4  = -O0 -r8 -no-fma -fp-model precise -traceback -assume byterecl -DMPI ${ADDFL}
FFLAGS5  = -O2 -r8 -no-fma -fp-model precise -traceback  -DMPI ${ADDFL}

#FFLAGS =  -DMPI -DRHMC -DIA64 -r8 -no-fma -fp-model precise -traceback  -fpp2 ${ADDFL}
FFLAGS2 = -O3 -DMPI -DRHMC -DIA64 -r8 -no-fma -fp-model strict -traceback ${ADDFL}


# Loader flags
LDFLAGS  = -qopenmp ${OPTFL} ${ADDFL}

# Name of MPI compiler driver
F77 = ftn
# where to find BLAS subprogram DGEMM and MPI subprograms:
#LIBS = -L $(PARIO_DIR) -lpario -L${MKL_HOME}/lib/intel64 -Wl,--start-group -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -Wl,--end-group -L${NETCDF_DIR}/lib  -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lz -lm  
#LIBIO_DIR=/RHM-Lustre3.2/users/wg-slmod/mtolstykh/LLC/LIBS

# LIBS = xrd18xlarge.a -L$(MPI_HOME)  -Wl,--start-group -lmkl_sequential -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -Wl,--end-group  -L${NETCDF_DIR}/lib  -Wl,--start-group -lnetcdff -lnetcdf -lpnetcdf -lhdf5_hl  -lhdf5 -lz -ldl -lsz -lz -lcurl -Wl,--end-group 
#LIBS = /RHM-Lustre3.2/users/wg-slmod/mtolstykh/LLC/LIBS/xrd18xlarge.a -L${MKL_HOME}/lib/intel64 -Wl,--start-group -lmkl_sequential -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -Wl,--end-group  -L${NETCDF_DIR}/lib  -Wl,--start-group -lnetcdff -lnetcdf -lpnetcdf -lhdf5_hl  -lhdf5 -lsz -ldl -lz -lcurl -Wl,--end-group 
LIBS = /RHM-Lustre3.2/users/wg-slmod/mtolstykh/LLC/LIBS/xrd18xlarge.a -L${MKL_HOME}/lib/intel64 -Wl,--start-group -lmkl_sequential -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -Wl,--end-group 



OBJ = \
      slavmods.o \
      parkind.o \
      parrrtm.o \
      mod_rrtmglw.o \
      rrlwkgall.o \
      rrtmg_lw_setcoef.o \
      rrtmg_lw_init.o \
      rrtmg_lw_taumol.o \
      rrtmg_lw_k_g.o \
      rrtmg_lw_cldprop.o \
      rrtmg_lw_cldprmc.o \
      rrtmg_lw_rtrnmr.o \
      rrtmglwmain.o \
      head.o \

.SUFFIXES:
.SUFFIXES: .o .c .f .f90
OBJs=$(patsubst %.o, $(OBJ_DIR)/%.o, $(OBJ))


all:	exec


exec: $(OBJs)
	$(F77) $(LDFLAGS) -o $(EXEC) $(FFLAGS) $(ALLINC) $(OBJs) $(LIBS)
	@if [[ -f ${EXEC} && -d ${RUN_DIR} ]]; then echo "cp ${EXEC} ${RUN_DIR}"; cp ${EXEC} ${RUN_DIR}; else echo "not found"; fi;

$(OBJs): | inc_dir obj_dir

inc_dir:
	@[ -d $(OBJ_DIR) ] || (mkdir $(OBJ_DIR) && echo "Created obj dir "$(OBJ_DIR)) || (echo "cannot create obj dir:" $(OBJ_DIR) ; exit 1)
obj_dir:
	@[ -d $(INC_DIR) ] || (mkdir $(INC_DIR) && echo "Created inc dir "$(INC_DIR)) || (echo "cannot create inc dir:" $(INC_DIR) ; exit 1)
                                                                                                             
                                                                                                             
lib:
	ar cr libslm.a $(OBJs)

clean:
	rm *.o *.mod *.lst  *.L $(EXEC) $(INC_DIR)/*.mod $(INC_DIR)/*_genmod.f90 $(OBJs) $(OBJ_DIR)/*.mod



#
$(OBJ_DIR)/modprmt.o : modprmt.f90
	$(F77) $(FFLAGS) $(DEFINES) $(ALLINC) $(SLAV_CONFIGURATION) $(IPO) -c -o  $@ $<

$(OBJ_DIR)/mpt36hvsm.o:  mpt36hvsm.f
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) $(IPO) -c -extend-source -o  $@ $<

$(OBJ_DIR)/fpsd36sm.o:  fpsd36sm.f
	$(F77) $(FFLAGS5) $(DEFINES) $(ALLINC) -c -extend-source -o $@ $<

$(OBJ_DIR)/mptio.o:  mptio.f90
	$(F77) $(FFLAGS4) -hfp1 -hvector1 $(DEFINES) $(ALLINC) -c -extend-source -o $@ $<

$(OBJ_DIR)/mtmtxv.o:  mtmtxv.f
	$(F77) $(FFLAGS5) $(DEFINES) $(ALLINC) $(IPO) -hnofma -c -r8 -o  $@ $<

$(OBJ_DIR)/mptclim.o: mptclim.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o  $@ $<

$(OBJ_DIR)/mpthead.o: mpthead.f90 FORCE_COMPILE
	$(F77) $(FFLAGS1) $(DEFINES) $(ALLINC) $(SLAV_CONFIGURATION) $(IPO) -c -r8 -o  $@ $<

$(OBJ_DIR)/mptio_msp.o: mptio_msp.f
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o  $@ $<

$(OBJ_DIR)/sisl_driver_mod.o: sisl_driver_mod.f90
	$(F77) $(FFLAGS5) $(DEFINES) $(ALLINC) -c -r8 -o  $@ $<

#$(OBJ_DIR)/accvimpnew.o: accvimpnew.f90
#	$(F77) $(FFLAGS2) $(DEFINES) $(ALLINC) -c -r8 -o  $@ $<

#$(OBJ_DIR)/acclouds.o: acclouds.f90
#	$(F77) $(FFLAGS2) $(DEFINES) $(ALLINC) -c -r8 -o  $@ $<

$(OBJ_DIR)/actends.o: actends.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o  $@ $<

$(OBJ_DIR)/slm_rad_driver.o: slm_rad_driver.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/mfph3.o: mfph3.f
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/interTandRH.o: interTandRH.f
	$(F77) $(FFLAGS1) $(DEFINES) $(ALLINC) -c -r8 -o $@  $<

$(OBJ_DIR)/intvrv.o: intvrv.f
	$(F77) $(FFLAGS5) $(DEFINES) $(ALLINC) -c -r8 -o $@  $<
#$(OBJ_DIR)/mfph1.o: mfph1.f
#	$(F77) $(FFLAGS2) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o $@ $<

#$(OBJ_DIR)/aplpar5.o: aplpar5.f90
#	$(F77) $(FFLAGS2) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o $@ $<

$(OBJ_DIR)/clirad.o: clirad.f
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o $@ $< 

$(OBJ_DIR)/aplmphys2.o: aplmphys2.f90
	$(F77) $(FFLAGS5) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o $@ $<

$(OBJ_DIR)/actke_surf.o: actke_surf.f90
	$(F77) $(FFLAGS5) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrtmg_lw_setcoef.o: rrtmg_lw_setcoef.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrtmg_lw_init.o: rrtmg_lw_init.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrtmg_lw_taumol.o: rrtmg_lw_taumol.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrtmg_lw_k_g.o: rrtmg_lw_k_g.f90
	$(F77) $(FFLAGS4) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrtmg_lw_cldprop.o: rrtmg_lw_cldprop.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrtmg_lw_rtrn.o: rrtmg_lw_rtrn.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrtmg_lw_rtrnmr.o: rrtmg_lw_rtrnmr.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrtmglwmain.o: rrtmglwmain.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/mod_rrtmglw.o: mod_rrtmglw.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/rrlwkgall.o: rrlwkgall.f90
	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/init_aerosols.o: init_aerosols.f90
	$(F77) $(FFLAGS4) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/pardsp.o: pardsp.f90
	$(F77) $(FFLAGS5) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o $@ $<

$(OBJ_DIR)/M_diag_movave.o: M_diag_movave.f90
	$(F77) $(FFLAGS4) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o $@ $<

#$(OBJ_DIR)/mfph23.o: mfph23.f90
#	$(F77) $(FFLAGS2) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o $@ $<

#$(OBJ_DIR)/sltdc.o: sltdc.f90
#	$(F77) $(FFLAGS2) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

$(OBJ_DIR)/achmtnew.o: achmtnew.f90
	$(F77) $(FFLAGS1) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o $@ $<

#$(OBJ_DIR)/acmrip_all.o: acmrip_all.f90
#	$(F77) $(FFLAGS2) $(DEFINES) $(ALLINC) -c -r8 -o $@ $<

#$(OBJ_DIR)/acdifusnew.o: acdifusnew.f90
#	$(F77) $(FFLAGS3) $(DEFINES) $(ALLINC) $(IPO) -c -r8 -o  $@ $<

$(OBJ_DIR)/%.o: %.f
	$(F77) $(FFLAGS) $(DEFINES) $(ALLINC) $(IPO) -c -o $@ $<

$(OBJ_DIR)/%.o: %.f90
	$(F77) $(FFLAGS) $(DEFINES) $(ALLINC) $(IPO) -c -o  $@ $<

FORCE_COMPILE: #force compile of smth, currently used for mpthead.f90 to force-set of correct SLAV-configuration flags

acdrag2.o: yomcst.o yomphy0.o yomphy2.o acdrag2.f90
bpfvrv.o: modgem.o modver.o modprmt.o yomcst.o bpfvrv.f
mfph1.o: moddyn.o modver.o modprmt.o yomcst.o yomphy0.o yomphy1.o yomphy2.o yomphy3.o yomphy.o yomrip.o yomtoph.o mfph1.f
mfph23.o: yomcst.o yomphy0.o yomphy1.o yomphy2.o yomphy.o mfph23.f90
mfph2nc.o: yomcst.o yomphy0.o yomphy1.o yomphy2.o yomphy.o mfph2nc.f
head.o: mpt36hvsm.o
mpt36hvsm.o: mptio.o mainloop.o
# ------------------------------------------------------------
