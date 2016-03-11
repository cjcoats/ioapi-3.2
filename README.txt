
                        THE EDSS/MODELS-3
        INPUT/OUTPUT APPLICATIONS PROGRAMMING INTERFACE


I/O API DOCUMENTS:
 
    The EDSS Documentation for the EDSS/Models-3 I/O API is available
    only as HTML -- it was designed from the start as a heavily
    cross-linked hyperdocument, and is not available in linear
    dead-tree document-forms such as PDF. The EDSS Documentation for
    the EDSS/Models-3 I/O API is copyright (C) 1992-2002 MCNC,
    (C) 1992-2002 and 2005-2013 Carlie J. Coats Jr, and 
    (C) 2003-2011 Baron Advanced Meteorological Systems.
    Documentation may be found at URL
    
        https://www.cmascenter.org/ioapi/documentation/3.1/html/index.html

CURRENT VERSION AVAILABILITY

    The current version of the I/O API is Version 3.2.  It is available
    in source code form from URL

        https://www.cmascenter.org/ioapi/documentation/3.1/html/AVAIL.html
        https://github.com/cjcoats/ioapi-3.2 via
        
            git clone https://github.com/cjcoats/ioapi-3.2

    New features of this release are documented at URL

        https://www.cmascenter.org/ioapi/documentation/3.1/html/NEWSTUFF.html


I/O API INSTALLATION

    The build-process for the I/O API uses the standard UNIX "make"
    command, and is driven by compiler/linker-compatibility
    considerations implemented in terms of "Makeinclude" files and an
    environment variable "BIN" that characterizes this compatibility
    type.  "Makeinclude.$BIN" files are available for many platforms
    (including some debug-, profiling, distributed-I/O and other
    variants, denoted by dbg, pg, mpi, i8, and r8 suffixes)

    From these examples and from a knowledge of the compiler user
    manual, it should be relatively easy to build Makeinclude files for
    most UNIX or UNIX-like platforms and compilers.  Instructions for
    building the I/O API library libioapi.a  and the I/O API tool
    executable programs are as follows:

    Build instructions for I/O API 3.1 match those for I/O API 3.2
    except that they use ioapi-3.1.tar.gz, which is available only
    from the CMAS web-site but not presently from GitHub.

       1. Download the gzipped tar-file ioapi-3.2.tar.gz from the
          CMAS web-site. It contains directories "ioapi" for the I/O API
          library source code, and "m3tools" for the related tool
          programs.

       2. Choose the directory under which you wish to build the
          I/O API. Let's call it $BASEDIR for the following.
          cd to BASEDIR gunzip and untar the "ioapi-3.1.tar.gz"
          (with Gnu tar,

              tar xvfz ioapi-3.2.tar.gz

          does unzip-untar all in one step).

       Alternative 1. & 2.:

          cd $BASEDIR.  Issue the command
        
            git clone https://github.com/cjcoats/ioapi-3.2

       3. setenv BIN <machinetype> where <machinetype> matches the
          extension on one of the "make"-configuration files
          "ioapi/Makeinclude.*" (writing your own Makeinclude if yours
          is not one of the supported systems). The usual pattern for
          generating BIN is

              setenv BIN `uname -s``uname -r | cut -d. -f1` 

          although there are exceptions where more work is needed for
          Cray, SGI and Linux systems, and F90, profiling, or DEBUG compiles.
          For 64-bit Linux, BIN will be of the form "Linux2_x86_64*"
          (e.g., "Linux2_x86_64ifort" for 64-bit builds using Intel
          "ifort" and "icc" compilers).  For 32-bit Linux, BIN will be
          of the form "Linux2_x86*" (e.g., "Linux2_x86ifort"...).

       4. mkdir $BASEDIR/$BIN.  This will be the build-and-install
          directory that will hold object-files, libraries, and
          "m3tools" executable programs.

       5. There are a number of "Makefile"s:  "Makefile.cpl" for
          PVM Coupling Mode, "Makefile.nocpl" for no-PVM,
          "Makefile.pncf" for PnetCDF/MPI distributed I/O Mode, etc. 
          Copy the appropriate one of the "Makefile.*" to "Makefile".
          Note that PnetCDF/MPI distributed I/O Mode requires BIN
          matching one of the "mpi" Makeinclude-files.

       6. Customize "Makefile" for your system:
          The default directory for both executables and object
          libraries is in directory "../$BIN" relative to the source
          code directories for the I/O API and tools. Edit the
          "Makefile" to put "BASEDIR", "SRCDIR", and "OBJDIR" wherever
          you want it (if you want somewhere other than the default
          "./$BIN" location).  Look at the "Makefile" header-comment
          for further customization options.

          NOTE 1:  Different compilers generate linker-visible object
          names in different ways (some with multiple options...).
          It is important that all of the compiles (including those
          for netCDF, PVM, and other libraries) for an entire
          executable program use the same scheme; this is controlled by
          various parts of the ARCHFLAGS variable in the
          "Makeinclude.$BIN" files.

          NOTE 2:  By default on most systems, OpenMP parallelism is
          enabled; see the OMPFLAGS variable in "Makeinclude.$BIN". 
          Enabling OpenMP does allow the activation of critical sections
          making the I/O&nbsp;API  thread-safe for OpenMP-parallel
          programs (like the MAQSIP-RT air quality model, the WRF or
          MCPL-enabled MM5 meteorology models, research versions of
          SMOKE, and others.  It also enables OpenMP parallelism in
          a few (mostly coordinate-transform or interpolation related)
          I/O API routines.

       7. In the I/O API library source directory "ioapi", type "make"
          to build the object library. The current build process will
          generate "$OBJDIR/libioapi.a".  For the non-Standard-compliant
          "fixed-132" INCLUDE-files required by CMAQ and some versions
          of SMOKE, type "make fixed_src"

       8. If necessary, get netCDF (for netCDF-4, get both
          netCDF-Fortran and netCDF-C) and build "libnetcdf.a" (and
          "libnetcdff.a" for netCDF-4); if you're building with Coupling
          Mode active, do the same for PVM, or if you're building with
          Distributed I/O active, do the same for PnetCDF.  Use the same
          compiler-set you used for the I/O API (and for your models). 
          Copy or link ("ln -s ...")  the "libnetcdf.a" (and 
          "libnetcdff.a", libpnetcdf.a, and "libpvm3.a" if you built
          them) to your $BASEDIR/$BIN.

       9. In the I/O API tool source directory "m3tools", create a
          customized "Makefile" as in (6) above, and type "make".

    


