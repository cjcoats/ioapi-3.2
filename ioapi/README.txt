
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
    It may be found at URL
    
        http://www.baronams.com/products/ioapi/index.html

CURRENT VERSION AVAILABILITY

    The current version of the I/O API is Version 3.1.  It is available
    in source code form from URL

        http://www.baronams.com/products/ioapi/AVAIL.html

    New features of this release are documented at URL

        http://www.baronams.com/products/ioapi/NEWSTUFF.html


I/O API INSTALLATION

    The build-process for the I/O API uses the standard UNIX
    "make" command, and is driven by compiler/linker-compatibility
    considerations implemented in terms of "Makeinclude" files
    and an environment variable "BIN" that characterizes this
    compatibility type.  "Makeinclude.$BIN" files are available
    for many platforms (including some debug- and other
    variants, denoted by dbg i8, and r8 suffixes)

    From these examples and from a knowledge of the compiler user
    manual, it should be relatively easy to build Makeinclude files for
    most UNIX or UNIX-like platforms and compilers.  Instructions for
    building the I/O API library libioapi_v2.1.a  and the I/O API tool
    executable programs are as follows:



       1. Download the gzipped tar-file ioapi-3.1.tar.gz. It contains
          directories "ioapi" for the I/O API library source code,
          and "m3tools" for the related tool programs.

       2. cd to the directory under which you wish to build the
          I/O API. gunzip and untar the "ioapi-3.1.tar.gz" (with
          Gnu tar,

              tar xvfz ioapi-3.1.tar.gz

          does unzip-untar all in one step).

       3. setenv BIN <machinetype> where <machinetype> matches the
          extension on one of the "Makeinclude.*" (writing your own
          Makeinclude if yours is not one of the supported systems).
          The usual pattern for generating BIN is

              setenv BIN `uname -s``uname -r | cut -d. -f1` 

          although there are exceptions where more work is needed for
          Cray, SGI and Linux systems, and F90, profiling, or DEBUG compiles.

       4. There are three "Makefile"s:  "Makefile", "Makefile.cpl",
          and "Makefile.nocpl".  The default  "Makefile" is the same
          as "Makefile.cpl", and builds the library with Coupling Mode
          enabled.  To build with Coupling Mode disabled, copy
          "Makefile.nocpl" to "Makefile".

       5. The default directory for both executables and object
          libraries is in directory "../$BIN" relative to the source
          code directories for the I/O API and tools. Edit the
          "Makefile" to put "SRCDIR" "OBJDIR" wherever you want it
          (if you want somewhere other than the default "./$BIN"
          location).

          NOTE 1:  Different compilers generate linker-visible object
          names in different ways (some with multiple options...).
          It is important that all of the compiles (including those
          for netCDF, PVM, and other libraries) for an entire
          executable program use the same scheme; this is controlled by
          various parts of the ARCHFLAGS variable in the
          "Makeinclude.$BIN" files.

          NOTE 2:  By default on most systems, OpenMP parallelism is
          enabled; see the OMPFLAGS variable in "Makeinclude.$BIN".
          The I/O API does not have parallel sections of its own;
          however, enabling OpenMP does allow the activation of
          critical sections allowing the I/O&nbsp;API to be
          thread-safe for OpenMP-parallel programs (like the
          MAQSIP-RT air quality model, the WRF or MCPL-enabled MM5
          meteorology models, research versions of SMOKE, and others.

       6. In the I/O API library source directory "ioapi", type
          "make" to build the object library. The current build process
          will generate "$OBJDIR/libioapi.a"; use "mv" to change it to
          "libm3io.a", if desired.

       7. If necessary, get netCDF and build "libnetcdf.a"; if you're
          building with Coupling Mode active, do the same for PVM.
       
       8. Copy or link ("ln -s ...")  the "libnetcdf.a" (and "libpvm3.a"
          if you built it) to your OBJDIR.

       9. In the I/O API tool source directory ioapi_tools, type "make".

    


