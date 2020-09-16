PROG =	ConsHouse

SRCS =	AllocateArrays.f90 ConsHouse.f90 cumnor.f90 Decisions.f90 \
	DecisionsRet.f90 funcs.f90 globals.f90 golden.f90 Grids.f90 \
	mnbrak.f90 params.f90 Procedures.f90 random.f90 \
	SaveOutput.f90 SetParameters.f90 SimulateEarnings.f90 \
	zbrak.f90 FnVFR.f90 FnVFB.f90 FnVFF.f90 FnVFN.f90 FnVFP.f90 \
	FnVFRRet.f90 FnVFBRet.f90 FnVFFRet.f90 FnVFNRet.f90 FnVFPRet.f90 \
	FnVFRRetJ.f90 FnVFFRetJ.f90 FnVFPRetJ.f90 \
	ComputeLOM.f90 updateforecast.f90 FnXSH.f90 rtbis.f90 invertmatrix.f90 \
	AggGrids.f90  \
	SimulateStochGEDist.f90 \
	SimulateBPP.f90 ComputeBPPCoef.f90 \

OBJS =	AllocateArrays.o ConsHouse.o cumnor.o Decisions.o DecisionsRet.o \
	funcs.o globals.o golden.o Grids.o mnbrak.o params.o \
	Procedures.o random.o SaveOutput.o SetParameters.o \
	SimulateEarnings.o zbrak.o FnVFR.o FnVFB.o FnVFF.o FnVFN.o \
	FnVFRRet.o FnVFBRet.o FnVFFRet.o FnVFNRet.o FnVFPRet.o FnVFP.o \
	FnVFRRetJ.o FnVFFRetJ.o FnVFPRetJ.o \
	ComputeLOM.o updateforecast.o FnXSH.o rtbis.o invertmatrix.o \
	AggGrids.o \
	SimulateStochGEDist.o \
	SimulateBPP.o ComputeBPPCoef.o \

LIBS =
CC = cc
CFLAGS = -O
FC = ifort
FFLAGS = -O
F90 = ifort
F90FLAGS = -O3 -axcore-avx2 -shared-intel -implicitnone -mcmodel=medium -traceback

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

AllocateArrays.o: globals.o params.o
ConsHouse.o: funcs.o globals.o params.o Procedures.o
Decisions.o: funcs.o globals.o params.o
DecisionsRet.o: Procedures.o funcs.o globals.o params.o
funcs.o: Procedures.o globals.o params.o
globals.o: params.o
Grids.o: Procedures.o funcs.o globals.o params.o random.o
AggGrids.o: Procedures.o funcs.o globals.o params.o
Procedures.o: globals.o params.o
SaveOutput.o: Procedures.o globals.o params.o
SetParameters.o: Procedures.o funcs.o globals.o params.o
SimulateStochGEDist.o: Procedures.o funcs.o globals.o params.o
SimulateEarnings.o: Procedures.o funcs.o globals.o params.o random.o
zbrak.o: globals.o params.o
FnVFR.o: Procedures.o funcs.o globals.o params.o
FnVFP.o: Procedures.o funcs.o globals.o params.o
FnVFB.o: Procedures.o funcs.o globals.o params.o
FnVFF.o: Procedures.o funcs.o globals.o params.o
FnVFN.o: Procedures.o funcs.o globals.o params.o
FnVFRRet.o: Procedures.o funcs.o globals.o params.o
FnVFPRet.o: Procedures.o funcs.o globals.o params.o
FnVFBRet.o: Procedures.o funcs.o globals.o params.o
FnVFFRet.o: Procedures.o funcs.o globals.o params.o
FnVFNRet.o: Procedures.o funcs.o globals.o params.o
FnVFRRetJ.o: Procedures.o funcs.o globals.o params.o
FnVFFRetJ.o: Procedures.o funcs.o globals.o params.o
FnVFPRetJ.o: Procedures.o funcs.o globals.o params.o
updateforecast.o: Procedures.o funcs.o globals.o params.o
ComputeLOM.o: Procedures.o funcs.o globals.o params.o
FnXSH.o: Procedures.o funcs.o globals.o params.o
SimulateBPP.o: Procedures.o funcs.o globals.o params.o
ComputeBPPCoef.o: Procedures.o funcs.o globals.o params.o
