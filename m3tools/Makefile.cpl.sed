#
#.........................................................................
# Version "$Id: Makefile.cpl.sed 238 2023-03-13 16:54:33Z coats $"
# EDSS/Models-3 M3TOOLS
#    (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
#    (C) 2003-2004 by Baron Advanced Meteorological Systems,
#    (C) 2005-2014, 2017, 2021 Carlie J. Coats, Jr., and
#    (C) 2014-2017 UNC Institute for the Environment
# Distributed under the GNU GENERAL PUBLIC LICENSE version 2
# See file "GPL.txt" for conditions of use.
#.........................................................................
#   Makefile for Coupling-Mode version of M3Tools
#   Note that I/O API must also have been built for Coupling-Mode
#.........................................................................
#  Environment Variables:
#       BIN     machine/OS/compiler/mode type. Shows up as suffix
#               for "Makeinclude.${BIN}" to determine compilation
#               flags, and in ${OBJDIR} and $(INSTALL) to determine
#               binary directories
#       INSTALL installation-directory root, used for "make install":
#               "libioapi.a" and the tool executables will be installed
#               in $(INSTALL)/${BIN}
#.........................................................................
#  Directories:
#       ${BASEDIR}  is the root directory for the I/O API library source,
#                   the M3Tools and M3Test source,the  HTML documentation,
#                   and the (machine/compiler/flag-specific) binary
#                   object/library/executable directories.
#       $(SRCDIR)   is the source directory for the M3TOOLS
#       $(IODIR)    is the source directory for the I/O API library
#       ${OBJDIR}   is the current machine/compiler/flag-specific
#                   build-directory
#       $(F90DIR)   is the current machine/compiler/flag-specific
#                   build-directory for F90-based programs (SGI & Sun)
#.........................................................................
#
#       ---------------     Definitions:   -------------------------

.SUFFIXES: .m4 .c .F .f .f90 .F90

BASEDIR = IOAPI_BASE
SRCDIR  = ${BASEDIR}/m3tools
IODIR   = ${BASEDIR}/ioapi
OBJDIR  = ${BASEDIR}/${BIN}
INSTDIR = BININSTALL

# Architecture dependent stuff
# Assumes FC is an f90

MAKEINCLUDE.${BIN}

PVMINCLUDE

FFLAGS = -I$(IODIR) IOAPI_DEFS $(ARCHFLAGS) $(PARFLAGS) $(FOPTFLAGS) $(ARCHFLAGS)

LDFLAGS = -I$(IODIR) -DIOAPICPL $(DEFINEFLAGS) $(ARCHFLAGS)

#  Incompatibility between netCDF versions before / after v4.1.1:
#  For netCDF v4 and later, you may also need the extra libraries
#  given by netCDF commands
#
#          nc-config --libs
#          nf-config --libs
#
#  Cygwin libraries need "-lnetcdff.dll -lnetcdf.dll" below
#
#LIBS = -L${OBJDIR} -lioapi -lnetcdf  $(PVMLIBS) $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)
#LIBS = -L${OBJDIR} -lioapi `nf-config --libs` `nc-config --libs`  $(PVMLIBS) $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)

LIBS  = -L${OBJDIR} -lioapi NCFLIBS $(PVMLIBS) $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)

VPATH = ${OBJDIR}

f90SRC = \
aggvars.f90     agmask.f90      agmax.f90       airnow2m3.f90   \
airs2m3.f90     bcwndw.f90      camxtom3.f90    cdiffstep.f90   \
datshift.f90    dayagg.f90      diffstep.f90    factor.f90      \
fakestep.f90    fills.f90       findwndw.f90    greg2jul.f90    \
gregdate.f90    gridprobe.f90   insertgrid.f90  jul2greg.f90    \
juldate.f90     juldiff.f90     julshift.f90    kfxtract.f90    \
latlon.f90      m3agmask.f90    m3agmax.f90     m3combo.f90     \
m3cple.f90      m3diff.f90      m3edhdr.f90     m3fake.f90      \
m3hdr.f90       m3interp.f90    m3mask.f90      m3merge.f90     \
m3pair.f90      m3probe.f90     m3stat.f90      m3totxt.f90     \
m3tproc.f90     m3tshift.f90    m3wndw.f90      m3xtract.f90    \
mpasdiff.f90    mpasstat.f90    mpastom3.f90    mtxblend.f90    \
mtxbuild.f90    mtxcalc.f90     mtxcple.f90     pairstep.f90    \
presterp.f90    presz.f90       projtool.f90    randomstat.f90  \
selmrg2d.f90    statb.f90       statbdry.f90    statc.f90       \
statcust.f90    statg.f90       statgrid.f90    stati.f90       \
statiddat.f90   statm.f90       statspars.f90   timediff.f90    \
timeshift.f90   vertimeproc.f90 vertintegral.f90    vertot.f90  \
wndwdesc.f90    wndwpoints.f90  wndwptdata.f90  wrfgriddesc.f90 \
wrftom3.f90     wrfwndw.f90

OBJ = $(f90SRC:.f90=.o)

EXE = \
airs2m3         bcwndw          camxtom3        datshift        dayagg          \
factor          findwndw        greg2jul        gregdate        gridprobe       \
insertgrid      jul2greg        juldate         juldiff         julshift        \
kfxtract        latlon          m3agmax         m3agmask        m3cple          \
m3combo         m3diff          m3edhdr         m3fake          m3hdr           \
m3interp        m3mask          m3merge         m3pair          m3probe         \
m3stat          m3totxt         m3tproc         m3tshift        m3wndw          \
m3xtract        mtxblend        mtxbuild        mtxcalc         mtxcple         \
mpasdiff        mpasstat        mpastom3        presterp        presz           \
projtool        selmrg2d        timediff        timeshift       vertot          \
vertimeproc     vertintegral    wndwdesc        wndwpoints      wndwptdata      \
wrfgriddesc     wrftom3         randomstat


#      ----------------------   TOP-LEVEL TARGETS:   ------------------

all: $(EXE)

clean:
	cd ${OBJDIR}; rm $(EXE) $(OBJ)

install: BININSTALL
	echo "Installing M3TOOLS in ${INSTDIR}"
	cd ${OBJDIR}; cp $(EXE) $(INSTDIR)

rmexe:
	cd ${OBJDIR}; rm ${EXE}

relink:
	make BIN=${BIN} -i rmexe ; make BIN=${BIN} all

bins:
	make BIN=Linux2_x86_64
	make BIN=Linux2_x86_64sun
	make BIN=Linux2_x86_64ifort
	make BIN=Linux2_x86_64dbg
	make BIN=Linux2_x86_64sundbg
	make BIN=Linux2_x86_64ifortdbg

binclean:
	make -i BIN=Linux2_x86_64          clean
	make -i BIN=Linux2_x86_64sun       clean
	make -i BIN=Linux2_x86_64ifort     clean
	make -i BIN=Linux2_x86_64dbg       clean
	make -i BIN=Linux2_x86_64sundbg    clean
	make -i BIN=Linux2_x86_64ifortdbg  clean


binrelink:
	make BIN=Linux2_x86_64         relink
	make BIN=Linux2_x86_64sun      relink
	make BIN=Linux2_x86_64ifort    relink
	make BIN=Linux2_x86_64dbg      relink
	make BIN=Linux2_x86_64sundbg   relink
	make BIN=Linux2_x86_64ifortdbg relink

flags:
	echo "BIN=${BIN}"
	echo "FFLAGS=$(FFLAGS)"
	echo "LDFLAGS=$(LDFLAGS)"
	echo "LIBS=$(LIBS)"
	echo "ARCHFLAGS=$(ARCHFLAGS)"
	echo "ARCHLIB=$(ARCHLIB)"
	echo "ARCHLIBS=$(ARCHLIBS)"
	echo "OMPFLAGS=$(OMPFLAGS)"
	echo "OMPLIBS=$(OMPLIBS)"
	echo "FOPTFLAGS=$(FOPTFLAGS)"
	echo "COPTFLAGS=$(COPTFLAGS)"
	echo "PARFLAGS=$(PARFLAGS)"
	echo "PVM_ROOT=$(PVM_ROOT)"
	echo "PVMLIBS=$(PVMLIBS)"


#      -----------------------   RULES:   -------------------------

%.o : %.mod        #  Disable "gmake"s obnoxious implicit Modula-2 rule !!
%.f : %.F          #  Hack for some versions of  "gmake" + "gfortran"

.F.o:
	cd ${OBJDIR}; $(FC) $(FPPFLAGS) $(FFLAGS) -c $(SRCDIR)/$<


.f.o:
	cd ${OBJDIR}; $(FC) $(FFLAGS) -c $(SRCDIR)/$<


.f90.o:
	cd ${OBJDIR}; $(FC) $(FFLAGS) -c $(SRCDIR)/$<

#  ---------------------------  Dependencies:  --------------------

aggvars.o       : m3utilio.mod
agmask.o        : m3utilio.mod
agmax.o         : m3utilio.mod
airnow2m3.o     : m3utilio.mod
airs2m3.o       : m3utilio.mod
cdiffstep.o     : m3utilio.mod
dayagg.o        : m3utilio.mod
diffstep.o      : m3utilio.mod
aggvars.o       : m3utilio.mod
agmask.o        : m3utilio.mod
agmax.o         : m3utilio.mod
airnow2m3.o     : m3utilio.mod
airs2m3.o       : m3utilio.mod
cdiffstep.o     : m3utilio.mod
dayagg.o        : m3utilio.mod
diffstep.o      : m3utilio.mod
gridprobe.o     : m3utilio.mod  modgctp.mod
insertgrid.o    : m3utilio.mod  modgctp.mod
latlon.o        : m3utilio.mod  modgctp.mod
m3agmask.o      : m3utilio.mod
m3agmax.o       : m3utilio.mod
m3combo.o       : m3utilio.mod  modatts3.mod
m3cple.o        : m3utilio.mod  modgctp.mod modatts3.mod
m3diff.o        : m3utilio.mod
m3edhdr.o       : m3utilio.mod
m3hdr.o         : m3utilio.mod  modatts3.mod
m3interp.o      : m3utilio.mod  modgctp.mod modatts3.mod
m3mask.o        : m3utilio.mod
m3merge.o       : m3utilio.mod  modgctp.mod
m3stat.o        : m3utilio.mod
m3tproc.o       : m3utilio.mod  modatts3.mod
m3tshift.o      : m3utilio.mod  modatts3.mod
m3xtract.o      : m3utilio.mod  modatts3.mod
m3wndw.o        : m3utilio.mod  modatts3.mod
mpasdiff.o      : m3utilio.mod  modmpasfio.mod
mpasstat.o      : m3utilio.mod  modmpasfio.mod
mpastom3.o      : m3utilio.mod  modmpasfio.mod
mtxbuild.o      : m3utilio.mod  modatts3.mod
mtxcalc.o       : m3utilio.mod  modatts3.mod modgctp.mod
mtxcple.o       : m3utilio.mod  modatts3.mod
projtool.o      : m3utilio.mod  modgctp.mod
selmrg2d.o      : m3utilio.mod
statbdry.o      : m3utilio.mod
statcust.o      : m3utilio.mod
statgrid.o      : m3utilio.mod
statiddat.o     : m3utilio.mod
statspars.o     : m3utilio.mod
wndwdesc.o      : m3utilio.mod  modgctp.mod
wndwpoints.o    : m3utilio.mod  modgctp.mod
wndwptdata.o    : m3utilio.mod  modgctp.mod
wrfgriddesc.o   : m3utilio.mod  modwrfio.mod
wrftom3.o       : m3utilio.mod  modwrfio.mod
wrfwndw.o       : m3utilio.mod  modwrfio.mod modncfio.mod modgctp.mod


#  ---------------------------  $(EXE) Program builds:  -----------------


airs2m3:  airs2m3.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

bcwndw: bcwndw.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

camxtom3:  camxtom3.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

datshift:  datshift.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

dayagg: dayagg.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

factor:  factor.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

findwndw:  findwndw.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

greg2jul: greg2jul.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

gregdate: gregdate.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

gridprobe: gridprobe.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

insertgrid:  insertgrid.o
	cd ${OBJDIR}; ${FC} ${LFLAGS} $^ ${LIBS} -o $@

jul2greg:  jul2greg.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

juldate:  juldate.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

juldiff:  juldiff.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

julshift:  julshift.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

kfxtract: kfxtract.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

latlon:  latlon.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3agmask:  m3agmask.o agmask.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3agmax:  m3agmax.o agmax.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3combo: m3combo.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3cple: m3cple.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3diff:  m3diff.o diffstep.o cdiffstep.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3edhdr:  m3edhdr.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3fake: m3fake.o fakestep.o fills.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3hdr:  m3hdr.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3interp: m3interp.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3mask: m3mask.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3merge: m3merge.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3pair:  m3pair.o pairstep.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3probe: m3probe.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3stat: m3stat.o statbdry.o statcust.o statgrid.o statiddat.o statspars.o \
        statb.o  statc.o statg.o stati.o statm.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3totxt:  m3totxt.o
	cd ${OBJDIR}; ${FC} ${LFLAGS} $^ ${LIBS} -o $@

m3tproc:  m3tproc.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3tshift:  m3tshift.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3wndw: m3wndw.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m3xtract:  m3xtract.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

m4cple: m4cple.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

mpasdiff: mpasdiff.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

mpasstat: mpasstat.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

mpastom3: mpastom3.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

mtxblend: mtxblend.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

mtxbuild: mtxbuild.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

mtxcalc: mtxcalc.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

mtxcple: mtxcple.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

presterp: presterp.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

presz:  presz.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

projtool: projtool.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

randomstat:  randomstat.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

selmrg2d:  selmrg2d.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

sfcmet:  sfcmet.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

timediff: timediff.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

timeshift: timeshift.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

vertimeproc:  vertimeproc.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

vertintegral:  vertintegral.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

vertot:  vertot.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

wndwdesc:  wndwdesc.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

wndwpoints:  wndwpoints.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

wndwptdata:  wndwptdata.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

wrfgriddesc:  wrfgriddesc.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

wrftom3:  wrftom3.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

wrfwndw:  wrfwndw.o
	cd ${OBJDIR}; $(FC) ${LFLAGS} $^ ${LIBS} -o $@

