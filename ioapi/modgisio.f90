
MODULE MODGISIO

    !!***************************************************************
    !!  Version "$Id: modgisio.f90 279 2025-04-12 15:33:31Z coats $"
    !!  Copyright (c) 2008-2010 Baron Advanced Meteorological Systems
    !!  and (c) 2025 Carlie J. Coats, Jr.
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!  See file "LGPL.txt" for conditions of use.
    !!..............................................................
    !!  DESCRIPTION:
    !!      Routines and INTERFACEs for reading and writing
    !!      "ARC" and "ASC" ASCII and  optionally-GZIPped GRIDFLOAT and
    !!      BIL1 (uint8_t), BIL2 (uint16_t), and BIL4 (uint32_t)
    !!      GIS-style files, with optional on-the-fly transpose from GIS
    !!      "scan-line" (dy<0) row-order to "normal model" (dy>0) row-order.
    !!
    !!      "ARC" files have header lines with grid parameters
    !!
    !!          "north", "south" "east", "west", "rows", and "cols"
    !!
    !!      while "ASC" files have header lines with grid parameters
    !!
    !!          "ncols", "nrows", "xllcorner", "yllcorner", "cellsize"
    !!          and optionally "NODATA_value"
    !!
    !!      "[Z]BIL" and "[Z]GRIDFLOAT" have separate ASCII header files.
    !!
    !!      NOTE that "gridbin.c" read-routines cannot easily transpose
    !!      from "left-handed" scan-line data order to "right-handed" order
    !!      the way the ARC-ASCII routines do, since C does not support
    !!      arrays as "first-class citizens" the way that Fortran does
    !!
    !!      NOTE however that for Fortran binary reads, byte-sex for READs
    !!      is a compiler-flag option for many compilers (and typically
    !!      is set to BIG-ENDIAN because of MM5), which is awkward for
    !!      GIS files, so that the binary routines all factor through
    !!      "gridbin.c"
    !!
    !!      "ARC" files have header lines with grid parameters
    !!      for the ARC-ASCII routines.
    !!
    !!      NOTE also that  "gisio.c" routines have an extra argument for
    !!      controlling whether to perform Big-end/Little-end byteswapping.
    !!      Fortran wrappers use the byte-order set by routine SETSWAP.
    !!
    !!  PUBLIC SUBROUTINES:
    !!      SETSWAP:     set byte-swapping flag:  0 for native
    !!                                            1 for network-to/from-native
    !!      BINVERBOSE:  set verbose logging
    !!      BINTERSE:    set non-verbose "terse" logging
    !!
    !!  ARGUMENTS for public logical functions:
    !!      GFIL        path-name for the file
    !!      COLS, NROWS array dimension
    !!      SWAP        0 for no byte-swap
    !!                  1 for byte-swap to/from network (big-endian) byte order
    !!      GBUF( NCOLS,NROWS ) array for input or output.
    !!
    !!  PUBLIC LOGICAL FUNCTIONS:
    !!      RDGFHDR, WRGFHDR    read / write a GRIDFLOAT header file
    !!      BILHDR      write a standard BIL ASCII header file
    !!      VARSCALE    Read per-variable BIL-scale-factor file
    !!
    !!      RDAFILE     read and transpose real or integer ASC-file data
    !!      RDARFILE    read and transpose real ASC-file data
    !!      RAIFILE     read and transpose integer ASC-file data
    !!      WRAFILE     transpose and write real or integer ASC-file data
    !!      WRARFILE    transpose and write real ASC-file data
    !!      WRAEFILE    transpose and write real ASC-file data using E-format
    !!      WRAIFILE    transpose and write integer ASC-file data
    !!
    !!      RDARC       read and transpose real or integer ARC-file data
    !!      RDARCR      read and transpose real ARC-file data
    !!      RDARCI      read and transpose integer ARC-file data
    !!      WRARC       transpose and write real or integer ARC-file data
    !!      WRARCR      transpose and write real ARC-file data
    !!      WRARCI      transpose and write integer ARC-file data
    !!
    !!      RDBIFILE    read and transpose INTEGER*1 BIL data
    !!      RDBI2FILE   read and transpose INTEGER*2 BIL data
    !!      RDBI4FILE   read and transpose INTEGER*4 BIL data
    !!      RDBRFILE    read and transpose REAL GRIDFLOAT data
    !!      RDZBIFILE   read and transpose GZIPped INTEGER*1 BIL data
    !!      RDZBI2FILE  read and transpose GZIPped INTEGER*2 BIL data
    !!      RDZBI4FILE  read and transpose GZIPped INTEGER*4 BIL data
    !!      RDBRFILE    read and transpose GZIPped REAL GRIDFLOAT data
    !!
    !!      WRBIFILE    transpose and write INTEGER*1 BIL data
    !!      WRBI2FILE   transpose and write INTEGER*2 BIL data
    !!      WRBI4FILE   transpose and write INTEGER*4 BIL data
    !!      WRBRFILE    transpose and write REAL GRIDFLOAT data
    !!      WRZBIFILE   transpose and write GZIPped INTEGER*1 BIL data
    !!      WRZBI2FILE  transpose and write GZIPped INTEGER*2 BIL data
    !!      WRZBI4FILE  transpose and write GZIPped INTEGER*4 BIL data
    !!      WRBRFILE    transpose and write GZIPped REAL GRIDFLOAT data
    !!
    !!      PRIVATE DBLERR     double precision "definitely not equal"
    !!      PRIVATE FLTSAME    single precision "approx equal"
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  1/2008 by Carlie J. Coats, Jr., BAMS --
    !!      builds on "rttools" BIL codes
    !!
    !!      Version   2/2010 by CJC:  add WRAEFILE()
    !!
    !!      Version   2/2010 by CJC:  add (Z)BIL/GFLT wrappers
    !!
    !!      Version   7/2010 by CJC:   [Z]BIL[2,4] and transposing
    !!      wrappers;  enhanced error messages and style changes.
    !!
    !!      Version   8/2010 by CJC:  factor binary routines through
    !!      "gridbin.c" routines, so as to get around compiler-flags
    !!      byte-sex problems.
    !!      byte-sex problems.
    !!
    !!      Version   4/2025 by CJC for I/O API 4.0
    !!***************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    PRIVATE


    !!--------  Routines in this module:  ----------------------

    PUBLIC  SETSWAP, VARSCALE, BINVERBOSE, BINTERSE

    PUBLIC  RDGFHDR, WRGFHDR, BILHDR, RDAFILE, WRAFILE,         &
            RDARFILE, RDAIFILE, WRARFILE, WRAEFILE, WRAIFILE,   &
            RDARC, WRARC, RDARCR, RDARCI,   WRARCR, WRARCI,     &
            RDBIFILE,   WRBIFILE,   RDBI2FILE,  WRBI2FILE,      &
            RDBI4FILE,  WRBI4FILE,  RDBRFILE,   WRBRFILE,       &
            RDZBIFILE,  WRZBIFILE,  RDZBI2FILE, WRZBI2FILE,     &
            RDZBI4FILE, WRZBI4FILE, RDZBRFILE,  WRZBRFILE

    PUBLIC  GFREAD,   GFZREAD,   GFWRITE,   GFZWRITE,           &
            BILREAD,  ZBILREAD,  BILWRITE,  ZBILWRITE,          &
            BIL2READ, ZBIL2READ, BIL2WRITE, ZBIL2WRITE,         &
            BIL4READ, ZBIL4READ, BIL4WRITE, ZBIL4WRITE


    !!--------  Generic Interfaces:  ----------------------

    INTERFACE RDAFILE
        MODULE PROCEDURE  RDARFILE, RDAIFILE
    END INTERFACE

    INTERFACE WRAFILE
        MODULE PROCEDURE  WRARFILE, WRAIFILE
    END INTERFACE

    INTERFACE RDARC
        MODULE PROCEDURE  RDARCR, RDARCI
    END INTERFACE

    INTERFACE WRARC
        MODULE PROCEDURE  WRARCR, WRARCI
    END INTERFACE

    INTERFACE RDBIFILE
        MODULE PROCEDURE  RDBIFILE1, RDBIFILE2, RDBIFILE4
    END INTERFACE

    INTERFACE RDBI2FILE
        MODULE PROCEDURE  RDBI2FILE2, RDBI2FILE4
    END INTERFACE

    INTERFACE WRBIFILE
        MODULE PROCEDURE  WRBIFILE1, WRBIFILE2, WRBI2FILE4
    END INTERFACE

    INTERFACE WRBI2FILE
        MODULE PROCEDURE  WRBI2FILE2, WRBI2FILE4
    END INTERFACE

    INTERFACE RDZBIFILE
        MODULE PROCEDURE  RDZBIFILE1, RDZBIFILE2, RDZBIFILE4
    END INTERFACE

    INTERFACE RDZBI2FILE
        MODULE PROCEDURE  RDZBI2FILE2, RDZBI2FILE4
    END INTERFACE

    INTERFACE WRZBIFILE
        MODULE PROCEDURE  WRZBIFILE1, WRZBIFILE2, WRZBIFILE4
    END INTERFACE

    INTERFACE WRZBI2FILE
        MODULE PROCEDURE  WRZBI2FILE2, WRZBI2FILE4
    END INTERFACE


    !!--------  Prototypes for routines in "gridbin.c"  -----------
    !!--------  SWAPB:  0 for native byte order; else call "htonl()"

    INTERFACE
        INTEGER FUNCTION GFREAD( GFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   GFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS
        INTEGER,          INTENT( IN ) ::   SWAPB
        REAL   ,          INTENT( OUT) ::   GBUF( NCOLS*NROWS )
        END FUNCTION GFREAD
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION GFZREAD( GFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   GFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS
        INTEGER,          INTENT( IN ) ::   SWAPB
        REAL   ,          INTENT( OUT) ::   GBUF( NCOLS*NROWS )
        END FUNCTION GFZREAD
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION GFWRITE( GFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   GFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        REAL   ,          INTENT( IN ) ::   GBUF( NCOLS*NROWS )
        END FUNCTION GFWRITE
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION GFZWRITE( GFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   GFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        REAL   ,          INTENT( IN ) ::   GBUF( NCOLS*NROWS )
        END FUNCTION GFZWRITE
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION ZBILWRITE( BFILE, NCOLS, NROWS, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS
        INTEGER*1,        INTENT( IN ) ::   GBUF( NCOLS*NROWS )
        END FUNCTION ZBILWRITE
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION BILWRITE( BFILE, NCOLS, NROWS, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS
        INTEGER*1,        INTENT( IN ) ::   GBUF( NCOLS*NROWS )
        END FUNCTION BILWRITE
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION ZBILREAD( BFILE, NCOLS, NROWS, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS
        INTEGER*1,        INTENT( OUT) ::   GBUF( NCOLS*NROWS )
        END FUNCTION ZBILREAD
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION BILREAD( BFILE, NCOLS, NROWS, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS
        INTEGER*1,        INTENT( OUT) ::   GBUF( NCOLS*NROWS )
        END FUNCTION BILREAD
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION ZBIL2WRITE( BFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        INTEGER*2,        INTENT( IN ) ::   GBUF( NCOLS*NROWS )
        END FUNCTION ZBIL2WRITE
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION BIL2WRITE( BFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        INTEGER*2,        INTENT( IN ) ::   GBUF( NCOLS*NROWS )
        END FUNCTION BIL2WRITE
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION ZBIL2READ( BFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        INTEGER*2,        INTENT( OUT) ::   GBUF( NCOLS*NROWS )
        END FUNCTION ZBIL2READ
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION BIL2READ( BFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        INTEGER*2,        INTENT( OUT) ::   GBUF( NCOLS*NROWS )
        END FUNCTION BIL2READ
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION ZBIL4WRITE( BFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        INTEGER*4,        INTENT( IN ) ::   GBUF( NCOLS*NROWS )
        END FUNCTION ZBIL4WRITE
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION BIL4WRITE( BFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        INTEGER*4,        INTENT( IN ) ::   GBUF( NCOLS*NROWS )
        END FUNCTION BIL4WRITE
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION ZBIL4READ( BFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        INTEGER*4,        INTENT( OUT) ::   GBUF( NCOLS*NROWS )
        END FUNCTION ZBIL4READ
    END INTERFACE

    INTERFACE
        INTEGER FUNCTION BIL4READ( BFILE, NCOLS, NROWS, SWAPB, GBUF )
        CHARACTER(LEN=*), INTENT( IN ) ::   BFILE
        INTEGER,          INTENT( IN ) ::   NCOLS, NROWS, SWAPB
        INTEGER*4,        INTENT( OUT) ::   GBUF( NCOLS*NROWS )
        END FUNCTION BIL4READ
    END INTERFACE


    PRIVATE  RDARCHDR

    LOGICAL, PRIVATE, SAVE :: VERBOSE = .TRUE.
    INTEGER, PRIVATE, SAVE :: BYTESWAP = 0



    !!...........   PARAMETERs and their descriptions:

    CHARACTER(LEN=24), PARAMETER, PRIVATE ::  PROJTYPE( 0:10 ) = (/   &
         'UNKNOWN              ',                           &
         'LATLON               ',                           &
         'LAMBERT              ',                           &
         'GENERAL_MERCATOR     ',                           &
         'GENERAL_STEREOGRAPHIC',                           &
         'UTM                  ',                           &
         'POLAR_STEREOGRAPHIC  ',                           &
         'EQUATORIAL__MERCATOR ',                           &
         'TRANSVERSE__MERCATOR ',                           &
         'ALBERS_EQUAL_AREA    ',                           &
         'UNKNOWN              '         /)


CONTAINS    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



     SUBROUTINE SETSWAP( SWAPB )

         INTEGER, INTENT( IN ) :: SWAPB

         BYTESWAP = SWAPB
         RETURN

     END SUBROUTINE SETSWAP



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-




     SUBROUTINE BINTERSE

         VERBOSE = .FALSE.
         RETURN

     END SUBROUTINE BINTERSE



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



     SUBROUTINE BINVERBOSE

         VERBOSE = .TRUE.
         RETURN

     END SUBROUTINE BINVERBOSE



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRGFHDR( GFROOT, VNAME, UNITS, DESCS, VMISS,       &
                              GDTYP, NCOLS, NROWS,                      &
                              P_ALP, P_BET, P_GAM, XCENT, YCENT,        &
                              XORIG, YORIG, XCELL, YCELL )

        !!***********************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open, write, and close a BIL "header" ASCII file,
        !!       containing metadata for binary BIL output files for
        !!       this variable.  Output header file name will be
        !!
        !!               ${GFROOT}/${VNAME}.bil.hdr
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       path GFROOT exists and is writable
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       SETENVVAR()
        !!       GETEFILE()
        !!
        !!  REVISION  HISTORY:
        !!       Prototype 4/2003 by Carlie J. Coats, Jr., BAMS
        !!       Version  12/2005 by CJC: single-layer version
        !!       Version   9/2006 by CJC: BIL-header grid description
        !!       is cell-center based, not grid-corner; F formats
        !!       for Lat-Lon.
        !!***********************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN )::     GFROOT     !  output path
        CHARACTER(LEN=*), INTENT( IN )::     VNAME      !  variable name
        CHARACTER(LEN=*), INTENT( IN )::     UNITS      !  units
        CHARACTER(LEN=*), INTENT( IN )::     DESCS      !  description
        REAL            , INTENT( IN )::     VMISS      !
        INTEGER,          INTENT( IN )::     GDTYP
        INTEGER,          INTENT( IN )::     NCOLS
        INTEGER,          INTENT( IN )::     NROWS
        REAL*8,           INTENT( IN )::     P_ALP
        REAL*8,           INTENT( IN )::     P_BET
        REAL*8,           INTENT( IN )::     P_GAM
        REAL*8,           INTENT( IN )::     XCENT
        REAL*8,           INTENT( IN )::     YCENT
        REAL*8,           INTENT( IN )::     XORIG
        REAL*8,           INTENT( IN )::     YORIG
        REAL*8,           INTENT( IN )::     XCELL
        REAL*8,           INTENT( IN )::     YCELL


        !!...........   PARAMETERs and their descriptions:

        CHARACTER(LEN=16), PARAMETER::  PNAME = 'MODGISIO/WRGFHDR'
        CHARACTER(LEN=NAMLEN3), PARAMETER::  FOO = 'gf_wr_hdr'    !  dummy logical name

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*256   MESG
        CHARACTER*512   EQNAME
        INTEGER         ISTAT
        INTEGER         HDRDEV
        INTEGER         L
        REAL*8          X1, Y1


        !!***************************************************************
        !!   begin body of function  WRGFHDR

        WRITE( EQNAME, '( 4 A )' ) TRIM( GFROOT ), '/', TRIM( VNAME ), '.bil.hdr'

        IF ( .NOT.SETENVVAR( FOO, EQNAME ) ) THEN
            MESG = TRIM(PNAME) // ': Error setting WRGFHDR-file env for vble "' // TRIM( VNAME ) // '"'
            CALL M3WARN( 'WRGFHDR', 0, 0, MESG )
            WRGFHDR = .FALSE.
            RETURN
        END IF

        HDRDEV = GETEFILE( FOO, .FALSE., .TRUE., 'WRGFHDR' )
         IF ( HDRDEV .LT. 0 ) THEN
            MESG = TRIM(PNAME) // ': Error opening WRGFHDR-file for vble"' // TRIM( VNAME ) // '"'
            CALL M3WARN( 'WRGFHDR', 0, 0, MESG )
            WRGFHDR = .FALSE.
            RETURN
        END IF

        WRITE( HDRDEV, '( A, T16, A   )'      ) 'LAYOUT',       'GRIDFLOAT'
        WRITE( HDRDEV, '( A, T16, I10 )'      ) 'NROWS',        NROWS
        WRITE( HDRDEV, '( A, T16, I10 )'      ) 'NCOLS',        NCOLS
        WRITE( HDRDEV, '( A, T16, 1PE14.6  )' ) 'NODATA_value', VMISS

        X1 = XORIG + 0.5D0 * XCELL
        Y1 = YORIG + 0.5D0 * YCELL
        L  = MAX( MIN( GDTYP, 10 ), 0 )
        IF ( GDTYP .EQ. LATGRD3 ) THEN
            WRITE( HDRDEV, '( A, T16, F20.14 )' ) 'ULXMAP', X1
            WRITE( HDRDEV, '( A, T16, F20.14 )' ) 'ULYMAP', Y1
            WRITE( HDRDEV, '( A, T16, F20.14 )' ) 'XDIM',   XCELL
            WRITE( HDRDEV, '( A, T16, F20.14 )' ) 'YDIM',  -YCELL
        ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN
            WRITE( HDRDEV, '( A, T16, A   )' )      'MAP_PROJECTION', PROJTYPE( L )
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'UTM_ZONE', NINT( P_ALP )
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'UTM_XOFF',XCENT
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'UTM_YOFF',YCENT
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'X_1',     X1
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'Y_1',     Y1
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'DX',      XCELL
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'DY',     -YCELL
        ELSE
            WRITE( HDRDEV, '( A, T16, A   )' )      'MAP_PROJECTION', PROJTYPE( L )
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'P_ALPHA', P_ALP
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'P_BETA',  P_BET
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'P_GAMMA', P_GAM
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'X_CENTER',XCENT
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'Y_CENTER',YCENT
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'X_1',     X1
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'Y_1',     Y1
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'DX',      XCELL
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'DY',     -YCELL
        END IF

        CLOSE( HDRDEV, IOSTAT = ISTAT )
        IF ( ISTAT .EQ. 0 ) THEN
            MESG = 'GRIDFLOAT-header file written for vble "' // TRIM( VNAME ) // '"'
            CALL M3MESG( MESG )
        ELSE
            MESG = TRIM(PNAME) // ': Error closing WRGFHDR-file for vble "' // TRIM( VNAME ) // '"'
            CALL M3WARN( 'WRGFHDR', 0, 0, MESG )
            WRGFHDR = .FALSE.
            RETURN
        END IF

        WRGFHDR = .TRUE.

        RETURN

    END FUNCTION WRGFHDR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION RDGFHDR( EQNAME,                           &
                              NCOLS, NROWS, BYTEORDER,          &
                              XLL, YLL, XCELL, YCELL, AMISS )


        !!...........   PARAMETERs, EXTERNALs their descriptions:

        CHARACTER(LEN=16),      PARAMETER::  PNAME = 'MODGISIO/RDGFHDR'
        CHARACTER(LEN=NAMLEN3), PARAMETER::  FOO   = 'gf_rd_hdr'      !  dummy logical name

        INTEGER, PARAMETER :: I8   = SELECTED_INT_KIND( 16 )        !  will be integer*8
        REAL*8 , PARAMETER :: IFAC = 64.0D0 * 81.0D0 * 25.0D0
        REAL*8 , PARAMETER :: DFAC = 1.0D0 / IFAC


        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN  ) :: EQNAME    !!  input header-file
        INTEGER,          INTENT( OUT ) :: NCOLS     !!  grid dimensions
        INTEGER,          INTENT( OUT ) :: NROWS     !!
        INTEGER,          INTENT( OUT ) :: BYTEORDER !!
        REAL*8 ,          INTENT( OUT ) :: XLL       !!  lower-left cell-center coords
        REAL*8 ,          INTENT( OUT ) :: YLL       !!
        REAL*8 ,          INTENT( OUT ) :: XCELL     !!  cell-size
        REAL*8 ,          INTENT( OUT ) :: YCELL     !!
        REAL   ,          INTENT( OUT ) :: AMISS     !!  missing-data value


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*256   MESG, LINE
        CHARACTER*64    FIELD( 2 )
        INTEGER         ISTAT
        INTEGER         HDRDEV
        INTEGER         L, N
        REAL*8          X1, Y1
        LOGICAL         EFLAG, AFLAG, CFLAG, UFLAG
        LOGICAL         NCFLAG, NRFLAG, BOFLAG, XOFLAG, YOFLAG, XCFLAG, YCFLAG, AMFLAG


        !!...........   STATEMENT FUNCTION:  double-precision fixup

        REAL*8          ZZ, DFIX
        DFIX( ZZ ) = DFAC * DBLE( NINT( IFAC*ZZ, I8 ) )


        !!***************************************************************
        !!   begin body of function  RDGFHDR

        EFLAG  = .FALSE.
        CFLAG  = .FALSE.
        UFLAG  = .FALSE.
        NCFLAG = .FALSE.
        NRFLAG = .FALSE.
        BOFLAG = .FALSE.
        XOFLAG = .FALSE.
        YOFLAG = .FALSE.
        XCFLAG = .FALSE.
        YCFLAG = .FALSE.
        AMFLAG = .FALSE.

        IF ( .NOT.SETENVVAR( FOO, EQNAME ) ) THEN
            MESG = TRIM(PNAME) // ': Error setting RDGFHDR-file env for "' // TRIM( EQNAME ) // '"'
            CALL M3WARN( 'RDGFHDR', 0, 0, MESG )
            RDGFHDR = .FALSE.
            RETURN
        END IF

        HDRDEV = GETEFILE( FOO, .TRUE., .TRUE., 'RDGFHDR' )
         IF ( HDRDEV .LT. 0 ) THEN
            CALL M3WARN( 'RDGFHDR', 0, 0, 'Error opening RDGFHDR-file' )
            RDGFHDR = .FALSE.
            RETURN
        END IF

        DO L = 1, 1999999999

            READ( HDRDEV, '( A )', END=99, IOSTAT=ISTAT ) LINE

            IF ( ISTAT .NE. 0 ) THEN

                WRITE( MESG, '( 2 ( A, I10, :, 2X ) )' )            &
                    TRIM(PNAME) // ': Error reading header at line', L, 'I/O STATUS=', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.

            ELSE

                CALL SPLITLINE( LINE, 2, N, FIELD, AFLAG )
                CALL UPCASE( FIELD(1) )

                IF      ( AFLAG ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I10 )' ) 'Bad formatting at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( N .LT. 2 ) THEN
                    MESG = 'Line="' // TRIM( LINE ) // '"'
                    CALL M3MESG( MESG )
                    WRITE( MESG, '( A, I10 )' ) 'Possibly missing field at line', L
                    CALL M3MESG( MESG )
                ELSE IF ( FIELD(1) .EQ. 'NCOLS' .OR. FIELD(1) .EQ. 'SAMPLES' ) THEN
                    NCOLS  = STR2INT( FIELD(2) )
                    NCFLAG = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'NROWS' .OR. FIELD(1) .EQ. 'LINES'  ) THEN
                    NROWS  = STR2INT( FIELD(2) )
                    NRFLAG = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'XLLCORNER' ) THEN
                    XLL    = DFIX( STR2DBLE( FIELD(2) ) )
                    XOFLAG = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'YLLCORNER' ) THEN
                    YLL    = DFIX( STR2DBLE( FIELD(2) ) )
                    YOFLAG = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'XLLCENTER' ) THEN
                    XLL    = DFIX( STR2DBLE( FIELD(2) ) )
                    XOFLAG = .TRUE.
                    CFLAG  = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'YLLCENTER' ) THEN
                    YLL    = DFIX( STR2DBLE( FIELD(2) ) )
                    YOFLAG = .TRUE.
                    CFLAG  = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'ULXMAP' ) THEN
                    XLL    = DFIX( STR2DBLE( FIELD(2) ) )
                    XOFLAG = .TRUE.
                    UFLAG  = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'ULYMAP' ) THEN
                    YLL    = DFIX( STR2DBLE( FIELD(2) ) )
                    YOFLAG = .TRUE.
                    UFLAG  = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'CELLSIZE' ) THEN
                    XCELL  = DFIX( STR2DBLE( FIELD(2) ) )
                    YCELL  = XCELL
                    XCFLAG = .TRUE.
                    YCFLAG = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'XDIM' .OR. FIELD(1)( 1:2 ) .EQ. 'DX' ) THEN
                    XCELL  = DFIX( STR2DBLE( FIELD(2) ) )
                    XCFLAG = .TRUE.
6               ELSE IF ( FIELD(1) .EQ. 'YDIM' .OR. FIELD(1)( 1:2 ) .EQ. 'DY' ) THEN
                    YCELL  = DFIX( STR2DBLE( FIELD(2) ) )
                    YCFLAG = .TRUE.
                ELSE IF ( FIELD(1)( 1:6 ) .EQ. 'NODATA' ) THEN
                    AMISS  = STR2REAL( FIELD(2) )
                    AMFLAG = .TRUE.
                ELSE IF ( FIELD(1) .EQ. 'BYTEORDER' ) THEN
                    CALL UPCASE( FIELD(2) )
                    BOFLAG = .TRUE.
                    IF      ( FIELD(2)( 1:3 ) .EQ. 'LSB' ) THEN
                        BYTEORDER = LITTLE_ENDIAN
                    ELSE IF ( FIELD(2)( 1:3 ) .EQ. 'MSB' ) THEN
                        BYTEORDER = BIG_ENDIAN
                    ELSE IF ( FIELD(2)( 1:3 ) .EQ. 'PDP' ) THEN
                        BYTEORDER = PDP_ENDIAN
                    ELSE IF ( FIELD(2)( 1:3 ) .EQ. 'I' ) THEN
                        BYTEORDER = 0
                    ELSE
                        EFLAG = .TRUE.
                        MESG  = 'Unrecognized BYTEORDER= '//FIELD(2)
                        CALL M3MESG( MESG )
                    END IF
                END IF

            END IF      !  if read-error, or not

        END DO          !  end loop reading lines from HDRDEV

99      CONTINUE        !  exit from loop


        CLOSE( HDRDEV )
        IF      ( .NOT.NCFLAG ) THEN
            CALL M3MESG( 'Bad header NCOLS' )
            EFLAG = .TRUE.
        END IF
        IF ( .NOT.NRFLAG ) THEN
            CALL M3MESG( 'Bad header NROWS' )
            EFLAG = .TRUE.
        END IF
        IF ( .NOT.BOFLAG ) THEN
            CALL M3MESG( 'Bad header BYTE-ORDER' )
            EFLAG = .TRUE.
        END IF
        IF ( .NOT.XOFLAG ) THEN
            CALL M3MESG( 'Bad header XLL' )
            EFLAG = .TRUE.
        END IF
        IF ( .NOT.YOFLAG ) THEN
            CALL M3MESG( 'Bad header YLL' )
            EFLAG = .TRUE.
        END IF
        IF ( .NOT.XCFLAG ) THEN
            CALL M3MESG( 'Bad header XCELL' )
            EFLAG = .TRUE.
        END IF
        IF ( .NOT.YCFLAG ) THEN
            CALL M3MESG( 'Bad header YCELL' )
            EFLAG = .TRUE.
        END IF
        IF ( .NOT.AMFLAG ) THEN
            CALL M3MESG( 'Bad header NODATA' )
            EFLAG = .TRUE.
        END IF

        IF ( EFLAG ) THEN
            RDGFHDR = .FALSE.
        ELSE
            RDGFHDR = .TRUE.
            IF ( CFLAG ) THEN
                XLL = XLL - 0.5D0 * XCELL
                YLL = YLL - 0.5D0 * YCELL
            END IF
            IF ( UFLAG ) THEN
                YLL = YLL - DBLE( NROWS ) * YCELL
            END IF
            IF ( VERBOSE ) THEN
                WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) 'Origin   <', XLL,   ':', YLL, '>'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) 'CELLSIZE <', XCELL, ':', YCELL, '>'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 5( A, :, I6      ) )' ) '<NC:NR>  <', NCOLS, ':', NROWS, '>'
                CALL M3MESG( MESG )
                WRITE( MESG, '( 5( A, :, 1PE14.7 ) )' ) 'MISSING_DATA= <', AMISS, '>'
                CALL M3MESG( MESG )
           END IF
        END IF

        RETURN

    END FUNCTION RDGFHDR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION BILHDR( BILROOT,                               &
                             VNAME, UNITS, DESCS, SCALE, OFFSET,    &
                             IMISS, NBITS, GDTYP, NCOLS, NROWS,     &
                             P_ALP, P_BET, P_GAM, XCENT, YCENT,     &
                             XORIG, YORIG, XCELL, YCELL )

        !!***********************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open, write, and close a BIL "header" ASCII file,
        !!       containing metadata for binary BIL output files for
        !!       this variable.  Output header file name will be
        !!
        !!               ${BILROOT}/${VNAME}.bil.hdr
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       path BILROOT exists and is writable
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       SETENVVAR()
        !!       GETEFILE()
        !!
        !!  REVISION  HISTORY:
        !!       Prototype 4/2003 by Carlie J. Coats, Jr., BAMS
        !!       Version  12/2005 by CJC: single-layer version
        !!***********************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN )::     BILROOT    !  output path
        CHARACTER(LEN=*), INTENT( IN )::     VNAME      !  variablename
        REAL,             INTENT( IN )::     SCALE
        REAL,             INTENT( IN )::     OFFSET
        INTEGER,          INTENT( IN )::     IMISS
        INTEGER,          INTENT( IN )::     NBITS
        CHARACTER(LEN=*), INTENT( IN )::     UNITS      !  units
        CHARACTER(LEN=*), INTENT( IN )::     DESCS      !  description
        INTEGER,          INTENT( IN )::     GDTYP
        INTEGER,          INTENT( IN )::     NCOLS
        INTEGER,          INTENT( IN )::     NROWS
        REAL*8,           INTENT( IN )::     P_ALP
        REAL*8,           INTENT( IN )::     P_BET
        REAL*8,           INTENT( IN )::     P_GAM
        REAL*8,           INTENT( IN )::     XCENT
        REAL*8,           INTENT( IN )::     YCENT
        REAL*8,           INTENT( IN )::     XORIG
        REAL*8,           INTENT( IN )::     YORIG
        REAL*8,           INTENT( IN )::     XCELL
        REAL*8,           INTENT( IN )::     YCELL



        !!...........   PARAMETERs, EXTERNALs their descriptions:

        CHARACTER(LEN=16), PARAMETER::  PNAME = 'MODGISIO/BILHDR'
        CHARACTER(LEN=16), PARAMETER::  FOO   = 'bil_scr'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*256   MESG
        CHARACTER*512   EQNAME
        INTEGER         ISTAT
        INTEGER         HDRDEV
        INTEGER         L
        REAL*8          X1, Y1


        !!***************************************************************
        !!   begin body of function  BILHDR

        WRITE( EQNAME, '( 4 A )' ) TRIM( BILROOT ), '/', TRIM( VNAME ), '.bil.hdr'

        IF ( .NOT.SETENVVAR( FOO, EQNAME ) ) THEN
            MESG = 'Error setting bilhdr-file env for vble "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            BILHDR = .FALSE.
            RETURN
        END IF

        HDRDEV = GETEFILE( FOO, .FALSE., .TRUE., 'BILHDR' )
         IF ( HDRDEV .LT. 0 ) THEN
            MESG = 'Error opening bilhdr-file for vble"' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            BILHDR = .FALSE.
            RETURN
        END IF

        WRITE( HDRDEV, '( A, T16, A   )' ) 'LAYOUT', 'BIL'
        WRITE( HDRDEV, '( A, T16, I10 )' ) 'NROWS',  NROWS
        WRITE( HDRDEV, '( A, T16, I10 )' ) 'NCOLS',  NCOLS
        WRITE( HDRDEV, '( A, T16, I10 )' ) 'NBITS',  NBITS
        WRITE( HDRDEV, '( A, T16, I10 )' ) 'NODATA', IMISS

        WRITE( HDRDEV, '( A, T16, 1PE14.6 )' ) 'SCALE',   SCALE
        WRITE( HDRDEV, '( A, T16, 1PE14.6 )' ) 'OFFSET',  OFFSET

        X1 = XORIG + 0.5D0 * XCELL
        Y1 = YORIG + 0.5D0 * YCELL
        L  = MAX( MIN( GDTYP, 10 ), 0 )
        IF ( GDTYP .EQ. LATGRD3 ) THEN
            WRITE( HDRDEV, '( A, T16, 1F23.16 )' ) 'ULXMAP', X1
            WRITE( HDRDEV, '( A, T16, 1F23.16 )' ) 'ULYMAP', Y1
            WRITE( HDRDEV, '( A, T16, 1F23.16 )' ) 'XDIM',   XCELL
            WRITE( HDRDEV, '( A, T16, 1F23.16 )' ) 'YDIM',  -YCELL
        ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN
            WRITE( HDRDEV, '( A, T16, A   )' ) 'MAP_PROJECTION', PROJTYPE( L )
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'UTM_ZONE', NINT( P_ALP )
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'UTM_XOFF',XCENT
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'UTM_YOFF',YCENT
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'X_1',     X1
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'Y_1',     Y1
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'DX',      XCELL
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'DY',      YCELL
        ELSE
            WRITE( HDRDEV, '( A, T16, A   )' ) 'MAP_PROJECTION', PROJTYPE( L )
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'P_ALPHA', P_ALP
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'P_BETA',  P_BET
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'P_GAMMA', P_GAM
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'X_CENTER',XCENT
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'Y_CENTER',YCENT
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'X_1',     X1
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'Y_1',     Y1
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'DX',      XCELL
            WRITE( HDRDEV, '( A, T16, 1PD23.16 )' ) 'DY',      YCELL
        END IF

        CLOSE( HDRDEV, IOSTAT = ISTAT )
        IF ( ISTAT .EQ. 0 ) THEN
            MESG = 'BIL-header file written for vble "' // TRIM( VNAME ) // '"'
            CALL M3MESG( MESG )
        ELSE
            MESG = 'Error closing bilhdr-file for vble "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            BILHDR = .FALSE.
            RETURN
        END IF

        BILHDR = .TRUE.

        RETURN

    END FUNCTION BILHDR



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION VARSCALE( NMAX, NCNT, ANAM, AFAC, AOFF )

        !!***********************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Read per-variable BIL-scaleing file BILSCALE
        !!       BIL-scaling will be linear, of the form:
        !!
        !!          IB = INT( ( V - AOFF(v) ) / AFAC(v) )
        !!          Y  ~ AFAC(V) * FLOAT( IB ) + AOFF(v)
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv BILSCALE  <path name for scale file>
        !!       BILSCALE is list-formatted:
        !!          <vname>  <scale-factor>  <scale-offset>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       SETENVVAR()
        !!       GETEFILE()
        !!
        !!  REVISION  HISTORY:
        !!       Prototype 4/2003 by Carlie J. Coats, Jr., BAMS
        !!       Version  12/2005 by CJC: single-layer version
        !!***********************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER,          INTENT( IN  ) :: NMAX
        INTEGER,          INTENT( OUT ) :: NCNT
        CHARACTER(LEN=*), INTENT( OUT ) :: ANAM( NMAX )
        REAL,             INTENT( OUT ) :: AFAC( NMAX )
        REAL,             INTENT( OUT ) :: AOFF( NMAX )

        !!...........   PARAMETERs and their descriptions:

        CHARACTER(LEN=16), PARAMETER::  PNAME = 'MODGISIO/VARSCALE'

        !!...........   Local Variables and their descriptions:

        INTEGER         BDEV
        INTEGER         N
        REAL            A, B
        INTEGER         ISTAT
        LOGICAL         EFLAG
        CHARACTER*16    SCRBUF
        CHARACTER*256   MESG

        !!...........   function body   ...........................

        EFLAG = .FALSE.

        BDEV = GETEFILE( 'BILSCALE', .TRUE., .TRUE., PNAME )

        IF ( BDEV .LT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = TRIM( PNAME ) // ':  Could not open "BILSCALE"'
            CALL M3MESG( MESG )
        ELSE

            N = 0
11              CONTINUE        !  head of loop reading BDEV

                READ( BDEV,*,END=22,IOSTAT=ISTAT ) SCRBUF, A, B
                N = N + 1

                IF ( ISTAT .NE. 0 ) THEN

                    WRITE( MESG, '( A, I10, 2X, A, I10 )' )     &
                       TRIM(PNAME) // ': Error', ISTAT, 'reading BILSCALE at line', N
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )

                ELSE IF ( N .LE. NMAX ) THEN

                   ANAM( N ) = SCRBUF
                   AFAC( N ) = A
                   AOFF( N ) = B

                ELSE

                    WRITE( MESG, '( A, I10  )' ) 'Overflow reading BILSCALE at line', N
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )

                END IF

            GO TO  11       !  to head of loop reading BDEV

22              CONTINUE        !  exit from loop reading BDEV

            NCNT = N

        END IF              !  if BDEV < o, or not


        IF ( EFLAG ) THEN
            MESG   = 'Bad BILSCALE file'
            CALL M3WARN( PNAME, 0, 0, MESG )
        END IF

        VARSCALE = ( .NOT.EFLAG )
        RETURN

    END FUNCTION VARSCALE



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDARFILE( FNAME, NCOLS, NROWS,             &
                               XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Read its header and check for consistency with the
        !!       supplied argument grid description.
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        REAL*8          , INTENT(  IN ) :: XORIG, YORIG, ACELL
        REAL            , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDARFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     AFLAG, EFLAG
        LOGICAL     CFLAG, RFLAG, XFLAG, YFLAG, DFLAG, MFLAG
        INTEGER     NC, NR, C, R, L, N, ISTAT
        REAL*8      XLL, YLL, CELL
        REAL        AMISS

        CHARACTER*1         CHBUF
        CHARACTER*16        FIELDS( 2 )
        CHARACTER*256       MESG, LINE


        !!***************************************************************
        !!   begin body of function  RDARFILE

        FDEV = GETEFILE( FNAME, .TRUE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) // ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            RDARFILE = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

        !!  Read file header:

        CFLAG = .FALSE.
        RFLAG = .FALSE.
        XFLAG = .FALSE.
        YFLAG = .FALSE.
        DFLAG = .FALSE.
        MFLAG = .FALSE.
        N     =  0

        DO  L = 1, 7

            READ( FDEV, '( A )', IOSTAT = ISTAT ) LINE
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )                &
                     TRIM(PNAME) // ': Error reading ', FNAME, ' at line', L, 'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            CALL SPLITLINE( LINE, 2, N, FIELDS, AFLAG )
            IF ( AFLAG ) THEN
                WRITE( MESG, '( 3 A, I6 )' ) 'Bad line in ', FNAME, ' at line', L
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            CALL UPCASE( FIELDS( 1 ) )
            IF      ( FIELDS( 1 ) .EQ. 'NCOLS' ) THEN
                CFLAG = .TRUE.
                NC    = STR2INT( FIELDS( 2 ) )
                IF ( NC .LT. 0 ) THEN
                    MESG = 'Bad NCOLS in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( NC .NE. NCOLS ) THEN
                    MESG = 'Inconsistent NCOLS in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'NROWS' ) THEN
                RFLAG = .TRUE.
                NR = STR2INT( FIELDS( 2 ) )
                IF ( NR .LT. 0 ) THEN
                    MESG = 'Bad NROWS in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( NR .NE. NROWS ) THEN
                    MESG = 'Inconsistent NROWS in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'XLLCORNER' ) THEN
                XFLAG = .TRUE.
                XLL = STR2DBLE( FIELDS( 2 ) )
                IF ( XLL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad XLL in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( XLL, XORIG ) ) THEN
                    MESG = 'Inconsistent XORIG in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'YLLCORNER' ) THEN
                YFLAG = .TRUE.
                YLL = STR2DBLE( FIELDS( 2 ) )
                IF ( YLL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad YLL in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( YLL, YORIG ) ) THEN
                    MESG = 'Inconsistent YORIG in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'CELLSIZE' ) THEN
                DFLAG = .TRUE.
                CELL = STR2DBLE( FIELDS( 2 ) )
                IF ( CELL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad CELLSIZE in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( CELL, ACELL ) ) THEN
                    MESG = 'Inconsistent CELLSIZE in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'NODATA_VALUE' ) THEN
                AMISS = STR2REAL( FIELDS( 2 ) )
            ELSE
                N = L-1
                EXIT
            END IF

        END DO

        IF ( .NOT.( CFLAG.AND.RFLAG.AND.XFLAG.AND.YFLAG.AND.DFLAG ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Missing header field(s)'
            CALL M3MESG ( MESG )
            RETURN
        END IF

        IF ( EFLAG ) THEN
            CALL M3MESG( 'Header-input error' )
            RDARFILE = .FALSE.
            RETURN
        END IF

        REWIND( FDEV )

        DO  L = 1, N

            READ( FDEV, '( A )', IOSTAT = ISTAT ) CHBUF

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )        &
                      TRIM(PNAME) // ': Error rereading ', FNAME, ' at line', L, 'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END DO

        IF ( EFLAG ) THEN
            CALL M3MESG( 'Header-skip error' )
            RDARFILE = .FALSE.
            RETURN
        END IF

        L = N
        DO R = NROWS, 1, -1

            L = L + 1
            READ( FDEV, *, IOSTAT = ISTAT ) ( VBUF( C,R ), C = 1, NCOLS )

             IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )                &
                      TRIM(PNAME) // ': Error reading ', FNAME, ' at record', L, 'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                DO C = 1, NCOLS
                    IF ( FLTSAME( AMISS, VBUF( C,R ) ) ) THEN
                        VBUF( C,R ) = BADVAL3
                    END IF
                END DO
            END IF

        END DO

        CLOSE( FDEV )

        RDARFILE = ( .NOT. EFLAG )
        RETURN

    END FUNCTION RDARFILE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDAIFILE( FNAME, NCOLS, NROWS,             &
                               XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Read its header and check for consistency with the
        !!       supplied argument grid description.
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        REAL*8          , INTENT(  IN ) :: XORIG, YORIG, ACELL
        INTEGER         , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDAIFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     AFLAG, EFLAG
        LOGICAL     CFLAG, RFLAG, XFLAG, YFLAG, DFLAG, MFLAG
        INTEGER     NC, NR, C, R, L, N, ISTAT
        REAL*8      XLL, YLL, CELL
        INTEGER     IMISS

        CHARACTER*1         CHBUF
        CHARACTER*16        FIELDS( 2 )
        CHARACTER*256       MESG, LINE


        !!***************************************************************
        !!   begin body of function  RDAIFILE

        FDEV = GETEFILE( FNAME, .TRUE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) //  ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            RDAIFILE = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

        !!  Read file header:

        CFLAG = .FALSE.
        RFLAG = .FALSE.
        XFLAG = .FALSE.
        YFLAG = .FALSE.
        DFLAG = .FALSE.
        MFLAG = .FALSE.
        N     =  0

        DO  L = 1, 7

            READ( FDEV, '( A )', IOSTAT = ISTAT ) LINE
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )                &
                     TRIM(PNAME) // ': Error reading ', FNAME, ' at line', L, 'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            CALL SPLITLINE( LINE, 2, N, FIELDS, AFLAG )
            IF ( AFLAG ) THEN
                WRITE( MESG, '( 3 A, I6 )' ) 'Bad line in ', FNAME, ' at line', L
                    CALL M3MESG( MESG )
                    EFLAG = .TRUE.
                    CYCLE
            END IF

            CALL UPCASE( FIELDS( 1 ) )
            IF      ( FIELDS( 1 ) .EQ. 'NCOLS' ) THEN
                CFLAG = .TRUE.
                NC    = STR2INT( FIELDS( 2 ) )
                IF ( NC .LT. 0 ) THEN
                    MESG = 'Bad NCOLS in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( NC .NE. NCOLS ) THEN
                    MESG = 'Inconsistent NCOLS in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'NROWS' ) THEN
                RFLAG = .TRUE.
                NR = STR2INT( FIELDS( 2 ) )
                IF ( NR .LT. 0 ) THEN
                    MESG = 'Bad NROWS in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( NR .NE. NROWS ) THEN
                    MESG = 'Inconsistent NROWS in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'XLLCORNER' ) THEN
                XFLAG = .TRUE.
                XLL = STR2DBLE( FIELDS( 2 ) )
                IF ( XLL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad XLL in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( XLL, XORIG ) ) THEN
                    MESG = 'Inconsistent XORIG in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'YLLCORNER' ) THEN
                YFLAG = .TRUE.
                YLL = STR2DBLE( FIELDS( 2 ) )
                IF ( YLL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad YLL in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( YLL, YORIG ) ) THEN
                    MESG = 'Inconsistent YORIG in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'CELLSIZE' ) THEN
                DFLAG = .TRUE.
                CELL = STR2DBLE( FIELDS( 2 ) )
                IF ( CELL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad CELLSIZE in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( CELL, ACELL ) ) THEN
                    MESG = 'Inconsistent CELLSIZE in ' // FNAME
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'NODATA_VALUE' ) THEN
                IMISS = STR2INT( FIELDS( 2 ) )
            ELSE
                N = L-1
                EXIT
            END IF

        END DO

        IF ( .NOT.( CFLAG.AND.RFLAG.AND.XFLAG.AND.YFLAG.AND.DFLAG ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Missing header field(s)'
            CALL M3MESG ( MESG )
        END IF

        IF ( EFLAG ) THEN
            CALL M3MESG( 'Header-input error' )
            RDAIFILE = .FALSE.
            RETURN
        END IF

        REWIND( FDEV )

        DO  L = 1, N

            READ( FDEV, '( A )', IOSTAT = ISTAT ) CHBUF

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )                &
                      TRIM(PNAME) // ': Error rereading ', FNAME, ' at line', L,  'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END DO

        IF ( EFLAG ) THEN
            CALL M3MESG( 'Header-skip error' )
            RDAIFILE = .FALSE.
            RETURN
        END IF

        L = N
        DO R = NROWS, 1, -1

            L = L + 1
            READ( FDEV, *, IOSTAT = ISTAT ) ( VBUF( C,R ), C = 1, NCOLS )

             IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )                &
                      TRIM(PNAME) // ': Error reading ', FNAME, ' at record', L, 'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                DO C = 1, NCOLS
                    IF ( IMISS .EQ. VBUF( C,R ) ) THEN
                        VBUF( C,R ) = IMISS3
                    END IF
                END DO
            END IF

        END DO

        CLOSE( FDEV )

        RDAIFILE = ( .NOT. EFLAG )
        RETURN

    END FUNCTION RDAIFILE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRARFILE( FNAME, NCOLS, NROWS,             &
                               XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Write its header using the supplied grid description
        !!       arguments.
        !!       Write the REAL data from VBUF.
        !!       Close the file
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        REAL*8          , INTENT( IN ) :: XORIG, YORIG, ACELL
        REAL            , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERs and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRARFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     EFLAG
        INTEGER     C, R, ISTAT
        REAL        AMISS
        REAL        AROW( NCOLS )

        CHARACTER*256 FMT, MESG


        !!***************************************************************
        !!   begin body of function  WRARFILE

        FDEV = GETEFILE( FNAME, .FALSE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) //  ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            WRARFILE = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

        FMT = '( 2( A16, I16, / ), 3( A16, F24.7, / ), A16, I16 )'
        WRITE( FDEV, FMT, IOSTAT=ISTAT )        &
             'ncols',        NCOLS,             &
             'nrows',        NROWS,             &
             'xllcorner',    XORIG,             &
             'yllcorner',    YORIG,             &
             'cellsize',     ACELL,             &
             'NODATA_value', IMISS3

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( 3 A, I10 )' )               &
                TRIM(PNAME) // ': Error writing header to', FNAME, 'STATUS = ', ISTAT
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        AMISS = FLOAT( IMISS3 )
        FMT   = '( 99999999 ( 10 F12.5, :, / ) )'
        DO R = NROWS, 1, -1

            DO C = 1, NCOLS
                IF ( VBUF( C,R ) .GT. AMISS3 ) THEN
                    AROW( C ) = VBUF( C,R )
                ELSE
                    AROW( C ) = AMISS
                END IF
            END DO

            WRITE( FDEV, FMT, IOSTAT=ISTAT ) ( AROW( C ), C = 1, NCOLS )

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I10, 2X, 3 A, I10 )' )               &
                    TRIM(PNAME) // ': Error writing record', NROWS-R+1, 'to ', FNAME, ' STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END DO

        CLOSE( FDEV )

        WRARFILE = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRARFILE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRAEFILE( FNAME, NCOLS, NROWS,             &
                               XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Write its header using the supplied grid description
        !!       arguments.
        !!       Write the REAL data from VBUF.
        !!       Close the file
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        REAL*8          , INTENT( IN ) :: XORIG, YORIG, ACELL
        REAL            , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERs and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRAEFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     EFLAG
        INTEGER     C, R, ISTAT
        REAL        AMISS
        REAL        AROW( NCOLS )

        CHARACTER*256 FMT, MESG


        !!***************************************************************
        !!   begin body of function  WRAEFILE

        FDEV = GETEFILE( FNAME, .FALSE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) //  ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            WRAEFILE = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

        FMT = '( 2( A16, I16, / ), 3( A16, F24.7, / ), A16, I16 )'
        WRITE( FDEV, FMT, IOSTAT=ISTAT )        &
             'ncols',        NCOLS,             &
             'nrows',        NROWS,             &
             'xllcorner',    XORIG,             &
             'yllcorner',    YORIG,             &
             'cellsize',     ACELL,             &
             'NODATA_value', IMISS3

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( 3 A, I10 )' )               &
                TRIM(PNAME) // ': Error writing header to', FNAME, 'STATUS = ', ISTAT
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        AMISS = FLOAT( IMISS3 )
        FMT   = '( 99999999 ( 10 ( 1PE14.6, : ), / ) )'
        DO R = NROWS, 1, -1

            DO C = 1, NCOLS
                IF ( VBUF( C,R ) .GT. AMISS3 ) THEN
                    AROW( C ) = VBUF( C,R )
                ELSE
                    AROW( C ) = AMISS
                END IF
            END DO

            WRITE( FDEV, FMT, IOSTAT=ISTAT ) ( AROW( C ), C = 1, NCOLS )

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I10, 2X, 3 A, I10 )' )               &
                TRIM(PNAME) // ': Error writing record', NROWS-R+1, 'to ', FNAME, ' STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END DO

        CLOSE( FDEV )

        WRAEFILE = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRAEFILE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRAIFILE( FNAME, NCOLS, NROWS,             &
                               XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Write its header using the supplied grid description
        !!       arguments.
        !!       Write the INTEGER data from VBUF.
        !!       Close the file
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        REAL*8          , INTENT( IN ) :: XORIG, YORIG, ACELL
        INTEGER         , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERs and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRAIFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     EFLAG
        INTEGER     C, R, ISTAT

        CHARACTER*256 FMT, MESG


        !!***************************************************************
        !!   begin body of function  WRAIFILE

        FDEV = GETEFILE( FNAME, .FALSE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) //  ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            WRAIFILE = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

        FMT = '( 2( A16, I16, / ), 3( A16, F24.7, / ), A16, I16 )'
        WRITE( FDEV, FMT, IOSTAT=ISTAT )        &
             'ncols',        NCOLS,             &
             'nrows',        NROWS,             &
             'xllcorner',    XORIG,             &
             'yllcorner',    YORIG,             &
             'cellsize',     ACELL,             &
             'NODATA_value', IMISS3

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( 3 A, I10 )' )               &
               TRIM(PNAME) // ': Error writing header to', FNAME, 'STATUS = ', ISTAT
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        FMT = '( 99999999 ( 10 I8, :, / ) )'
        DO R = NROWS, 1, -1

            WRITE( FDEV, FMT, IOSTAT=ISTAT ) ( VBUF( C,R ), C = 1, NCOLS )

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I10, 2X, 3 A, I10 )' )               &
                    TRIM(PNAME) // ': Error writing record', NROWS-R+1, 'to ', FNAME, ' STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END DO

        CLOSE( FDEV )

        WRAIFILE = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRAIFILE



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDBRFILE( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the GRIDFLOAT GIS input file with logical name FNAME
        !!       Read and transpose the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        REAL            , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDBRFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, RR
        REAL            IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  RDBRFILE

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. GFREAD( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) ) THEN
            RDBRFILE = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )
            END DO
        END DO

        RDBRFILE = .TRUE.
        RETURN

    END FUNCTION RDBRFILE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDBIFILE1( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GIS-output file with logical name FNAME
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER*1       , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDBIFILE1'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDBIFILE1

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. BILREAD( EQNAME, NCOLS, NROWS, IBUF ) ) THEN
            RDBIFILE1 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer*1
            END DO
        END DO

        RDBIFILE1 = .TRUE.
        RETURN

    END FUNCTION RDBIFILE1



    LOGICAL FUNCTION RDBIFILE2( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GIS-output file with logical name FNAME
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER*2       , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        INTEGER*2        , PARAMETER :: N8 = 2**8
        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDBIFILE2'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER *2  I
        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDBIFILE1

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. BILREAD( EQNAME, NCOLS, NROWS, IBUF ) ) THEN
            RDBIFILE2 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                I = MOD( IBUF( C,R ) + N8, N8 ) !!  "C" uint_8t ~~> integer*2
                VBUF( C,RR ) = I
            END DO
        END DO

        RDBIFILE2 = .TRUE.
        RETURN

    END FUNCTION RDBIFILE2



    LOGICAL FUNCTION RDBIFILE4( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GIS-output file with logical name FNAME
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDBIFILE4'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR, I
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDBIFILE4

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. BILREAD( EQNAME, NCOLS, NROWS, IBUF ) ) THEN
            RDBIFILE4 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                I = MOD( IBUF( C,R ) + 256, 256 )       !!  "C" uint_8t ~~> integer
                VBUF( C,RR ) = IBUF( C,R )
            END DO
        END DO

        RDBIFILE4 = .TRUE.
        RETURN

    END FUNCTION RDBIFILE4



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDBI2FILE2( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*2-BIL GIS-output file with logical name FNAME
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER*2       , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDBIFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*2   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDBI2FILE

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. BIL2READ( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) ) THEN
            RDBI2FILE2 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer*2
            END DO
        END DO

        RDBI2FILE2 = .TRUE.
        RETURN

    END FUNCTION RDBI2FILE2



    LOGICAL FUNCTION RDBI2FILE4( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*2-BIL GIS-output file with logical name FNAME
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        INTEGER          , PARAMETER :: N16   = 2 ** 16
        CHARACTER(LEN=16), PARAMETER :: PNAME = 'MODGISIO/RDBIFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR, I
        INTEGER*2   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDBI2FILE4

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. BIL2READ( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) ) THEN
            RDBI2FILE4 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                I = MOD( IBUF( C,R ) + N16, N16 )   !!  "C" uint_16t ~~> integer
                VBUF( C,RR ) = I
            END DO
        END DO

        RDBI2FILE4 = .TRUE.
        RETURN

    END FUNCTION RDBI2FILE4



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDBI4FILE( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*4-BIL GIS-output file with logical name FNAME
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDBI4FILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*4   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDBI4FILE

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. BIL4READ( EQNAME, NCOLS, NROWS, BYTESWAP, IBUF ) ) THEN
            RDBI4FILE = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer
            END DO
        END DO

        RDBI4FILE = .TRUE.
        RETURN

    END FUNCTION RDBI4FILE



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRBRFILE( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the GRIDFLOAT GIS-output file with logical name FNAME
        !!       Write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        REAL            , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRBRFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        REAL        IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME

        !!***************************************************************
        !!   begin body of function  WRZBRFIL

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRBRFILE = ( 0 .NE. GFWRITE( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) )
        RETURN

    END FUNCTION WRBRFILE



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRBIFILE1( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GIS-output file with logical name FNAME
        !!       Write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER*1       , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRBIFIL1'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRBIFILE1

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRBIFILE1 = ( 0 .NE. BILWRITE( EQNAME, NCOLS, NROWS, IBUF ) )
        RETURN

    END FUNCTION WRBIFILE1



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRBIFILE2( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GIS-output file with logical name FNAME
        !!       Write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER*2       , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRBIFIL2'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRBIFILE2

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRBIFILE2 = ( 0 .NE. BILWRITE( EQNAME, NCOLS, NROWS, IBUF ) )
        RETURN

    END FUNCTION WRBIFILE2



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRBIFILE4( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GIS-output file with logical name FNAME
        !!       Write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRBIFIL4'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRBIFILE4

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRBIFILE4 = ( 0 .NE. BILWRITE( EQNAME, NCOLS, NROWS, IBUF ) )
        RETURN

    END FUNCTION WRBIFILE4



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRBI2FILE2( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*2-BIL GIS-output file with logical name FNAME
        !!       Write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER*2       , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRBI2FILE2'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*2   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRBI2FILE2

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRBI2FILE2 = ( 0 .NE. BIL2WRITE( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) )
        RETURN

    END FUNCTION WRBI2FILE2



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRBI2FILE4( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*2-BIL GIS-output file with logical name FNAME
        !!       Write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRBI2FILE4'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*2   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRBI2FILE4

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRBI2FILE4 = ( 0 .NE. BIL2WRITE( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) )
        RETURN

    END FUNCTION WRBI2FILE4



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRBI4FILE( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*4-BIL GIS-output file with logical name FNAME
        !!       Write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETDFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRBI4FILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*4   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRBI4FILE

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRBI4FILE = ( 0 .NE. BIL4WRITE( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) )
        RETURN

    END FUNCTION WRBI4FILE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDARCR( FNAME, NCOLS, NROWS, XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Read its header and check for consistency with the
        !!       supplied argument grid description.
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        REAL*8          , INTENT(  IN ) :: XORIG, YORIG, ACELL
        REAL            , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDARCR'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     AFLAG, EFLAG
        LOGICAL     CFLAG, RFLAG, XFLAG, YFLAG, DFLAG, MFLAG
        INTEGER     NC, NR, C, R, L, N, ISTAT
        REAL*8      XLL, YLL, CELL
        REAL        AMISS

        CHARACTER*1         CHBUF
        CHARACTER*16        FIELDS( 2 )
        CHARACTER*256       MESG, LINE


        !!***************************************************************
        !!   begin body of function  RDARCR

        FDEV = GETEFILE( FNAME, .TRUE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) //  ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            RDARCR = .FALSE.
            RETURN
        END IF

        !!  Read file header:

        IF ( .NOT.RDARCHDR( FDEV, NCOLS, NROWS, XORIG, YORIG, ACELL, AMISS, N ) ) THEN
            CALL M3MESG( 'Header-input error for ' // FNAME )
            RDARCR = .FALSE.
            RETURN
        END IF

        L = 0
        DO R = NROWS, 1, -1

            L = L + 1
            READ( FDEV, *, IOSTAT = ISTAT ) ( VBUF( C,R ), C = 1, NCOLS )

             IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )                &
                      '***Error reading ', FNAME, ' at record', L, 'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                DO C = 1, NCOLS
                    IF ( FLTSAME( AMISS, VBUF( C,R ) ) ) THEN
                        VBUF( C,R ) = BADVAL3
                    END IF
                END DO
            END IF

        END DO

        CLOSE( FDEV )

        RDARCR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION RDARCR



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDARCI( FNAME, NCOLS, NROWS, XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Read its header and check for consistency with the
        !!       supplied argument grid description.
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        REAL*8          , INTENT(  IN ) :: XORIG, YORIG, ACELL
        INTEGER         , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDARCI'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     AFLAG, EFLAG
        INTEGER     NC, NR, C, R, L, N, ISTAT
        REAL*8      XLL, YLL, CELL
        REAL        AMISS
        INTEGER     IMISS

        CHARACTER*1         CHBUF
        CHARACTER*16        FIELDS( 2 )
        CHARACTER*256       MESG, LINE


        !!***************************************************************
        !!   begin body of function  RDARCI

        FDEV = GETEFILE( FNAME, .TRUE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) //  ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            RDARCI = .FALSE.
            RETURN
        END IF


        !!  Read file header:
        IF ( .NOT.RDARCHDR( FDEV, NCOLS, NROWS, XORIG, YORIG, ACELL, AMISS, N ) ) THEN
            CALL M3MESG( 'Header-input error for ' // FNAME )
            RDARCI = .FALSE.
            RETURN
        END IF
        IMISS = NINT( AMISS )

        L = 0
        DO R = NROWS, 1, -1

            L = L + 1
            READ( FDEV, *, IOSTAT = ISTAT ) ( VBUF( C,R ), C = 1, NCOLS )

             IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )                &
                      TRIM(PNAME) // ': Error reading ', FNAME, ' at record', L, 'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                DO C = 1, NCOLS
                    IF ( IMISS .EQ. VBUF( C,R ) ) THEN
                        VBUF( C,R ) = IMISS3
                    END IF
                END DO
            END IF

        END DO

        CLOSE( FDEV )

        RDARCI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION RDARCI



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRARCR( FNAME, NCOLS, NROWS, XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Write its header using the supplied grid description
        !!       arguments.
        !!       Write the REAL data from VBUF.
        !!       Close the file
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        REAL*8          , INTENT( IN ) :: XORIG, YORIG, ACELL
        REAL            , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERs and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRARCR'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     EFLAG
        INTEGER     C, R, ISTAT
        REAL        AMISS
        REAL        AROW( NCOLS )

        CHARACTER*256 FMT, MESG


        !!***************************************************************
        !!   begin body of function  RDARCR

        FDEV = GETEFILE( FNAME, .TRUE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) //  ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            WRARCR = .FALSE.
            RETURN
        END IF

        !!  write file header:

        FMT = '( 2( A16, I16, / ), 3( A16, F16.0, / ), A16, I16 )'
        WRITE( FDEV, FMT, IOSTAT=ISTAT )        &
              'ncols',        NCOLS,            &
              'nrows',        NROWS,            &
              'xllcorner',    XORIG,            &
              'yllcorner',    YORIG,            &
              'cellsize',     ACELL,            &
              'NODATA_value', IMISS3

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( 3 A, I10 )' )               &
                TRIM(PNAME) // ': Error writing header to', FNAME, 'STATUS = ', ISTAT
            CALL M3MESG( MESG )
            WRARCR = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.
        AMISS = FLOAT( IMISS3 )
        FMT   = '( 99999999 ( 10 F12.5, :, / ) )'
        DO R = NROWS, 1, -1

            DO C = 1, NCOLS
                IF ( VBUF( C,R ) .GT. AMISS3 ) THEN
                    AROW( C ) = VBUF( C,R )
                ELSE
                    AROW( C ) = AMISS
                END IF
            END DO

            WRITE( FDEV, FMT, IOSTAT=ISTAT ) ( AROW( C ), C = 1, NCOLS )

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I10, 2X, 3 A, I10 )' )               &
                TRIM(PNAME) // ': Error writing record', NROWS-R+1, 'to ', FNAME, ' STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END DO

        CLOSE( FDEV )

        WRARCR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRARCR



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRARCI( FNAME, NCOLS, NROWS, XORIG, YORIG, ACELL, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Write its header using the supplied grid description
        !!       arguments.
        !!       Write the INTEGER data from VBUF.
        !!       Close the file
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        REAL*8          , INTENT( IN ) :: XORIG, YORIG, ACELL
        INTEGER         , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERs and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRARCI'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     FDEV
        LOGICAL     EFLAG
        INTEGER     C, R, ISTAT

        CHARACTER*256 FMT, MESG


        !!***************************************************************
        !!   begin body of function  WRARCI

        FDEV = GETEFILE( FNAME, .FALSE., .TRUE., PNAME )
        IF ( FDEV .LT. 0 ) THEN
            MESG = TRIM( PNAME ) // ':  Could not open ' // FNAME
            CALL M3MESG( MESG )
            WRARCI = .FALSE.
            RETURN
        END IF

        !!  write file header:

        EFLAG = .FALSE.

        FMT = '( 2( A16, I16, / ), 3( A16, F16.0, / ), A16, I16 )'
        WRITE( FDEV, FMT, IOSTAT=ISTAT )        &
              'ncols',        NCOLS,            &
              'nrows',        NROWS,            &
              'xllcorner',    XORIG,            &
              'yllcorner',    YORIG,            &
              'cellsize',     ACELL,            &
              'NODATA_value', IMISS3

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( 3 A, I10 )' )               &
                TRIM(PNAME) // ': Error writing header to', FNAME, 'STATUS = ', ISTAT
            CALL M3MESG( MESG )
            WRARCI = .FALSE.
            RETURN
        END IF

        FMT   = '( 99999999 ( 10 I8, :, / ) )'
        EFLAG = .FALSE.
        DO R = NROWS, 1, -1

            WRITE( FDEV, FMT, IOSTAT=ISTAT ) ( VBUF( C,R ), C = 1, NCOLS )

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I10, 2X, 3 A, I10 )' )               &
                TRIM(PNAME) // ': Error writing record', NROWS-R+1, 'to ', FNAME, ' STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END DO

        CLOSE( FDEV )

        WRARCI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRARCI



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-




    LOGICAL FUNCTION RDARCHDR( FDEV, NCOLS, NROWS, XORIG, YORIG,        &
                               ACELL, AMISS, N )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the ASCII GIS-output file with logical name FNAME
        !!       Read its header and check for consistency with the
        !!       supplied argument grid description.
        !!       Read the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GETEFILE
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(  IN ) :: FDEV
        INTEGER, INTENT(  IN ) :: NCOLS, NROWS
        REAL*8 , INTENT(  IN ) :: XORIG, YORIG, ACELL
        REAL   , INTENT( OUT ) :: AMISS
        INTEGER, INTENT( OUT ) :: N


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        LOGICAL     AFLAG, EFLAG
        LOGICAL     CFLAG, RFLAG, XFLAG, YFLAG, DFLAG, MFLAG
        INTEGER     NC, NR, ISTAT, L
        REAL*8      XLL, YLL, CELL

        CHARACTER*1         CHBUF
        CHARACTER*16        FIELDS( 2 )
        CHARACTER*256       MESG, LINE


        !!***************************************************************
        !!   begin body of function  RDARCR

        EFLAG = .FALSE.

        !!  Read file header:

        CFLAG = .FALSE.
        RFLAG = .FALSE.
        XFLAG = .FALSE.
        YFLAG = .FALSE.
        DFLAG = .FALSE.
        MFLAG = .FALSE.
        N     =  0

        DO  L = 1, 7

            READ( FDEV, '( A )', IOSTAT = ISTAT ) LINE
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 3 A, I6, 2X, A, I10 )' )                &
                      '*** Error reading header at line', L, 'STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            CALL SPLITLINE( LINE, 2, N, FIELDS, AFLAG )
            IF ( AFLAG ) THEN
                WRITE( MESG, '( 3 A, I6 )' ) '*** Bad line in header at line', L
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            CALL UPCASE( FIELDS( 1 ) )
            IF      ( FIELDS( 1 ) .EQ. 'NCOLS' ) THEN
                CFLAG = .TRUE.
                NC    = STR2INT( FIELDS( 2 ) )
                IF ( NC .LT. 0 ) THEN
                    MESG = 'Bad NCOLS in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( NC .NE. NCOLS ) THEN
                    MESG = 'Inconsistent NCOLS in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'NROWS' ) THEN
                RFLAG = .TRUE.
                NR = STR2INT( FIELDS( 2 ) )
                IF ( NR .LT. 0 ) THEN
                    MESG = 'Bad NROWS in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( NR .NE. NROWS ) THEN
                    MESG = 'Inconsistent NROWS in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'XLLCORNER' ) THEN
                XFLAG = .TRUE.
                XLL = STR2DBLE( FIELDS( 2 ) )
                IF ( XLL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad XLL in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( XLL, XORIG ) ) THEN
                    MESG = 'Inconsistent XORIG in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'YLLCORNER' ) THEN
                YFLAG = .TRUE.
                YLL = STR2DBLE( FIELDS( 2 ) )
                IF ( YLL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad YLL in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( YLL, YORIG ) ) THEN
                    MESG = 'Inconsistent YORIG in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'CELLSIZE' ) THEN
                DFLAG = .TRUE.
                CELL = STR2DBLE( FIELDS( 2 ) )
                IF ( CELL .LT. DBLE( AMISS3 ) ) THEN
                    MESG = 'Bad CELLSIZE in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                ELSE IF ( DBLERR( CELL, ACELL ) ) THEN
                    MESG = 'Inconsistent CELLSIZE in header'
                    CALL M3MESG ( MESG )
                    EFLAG = .TRUE.
                END IF
            ELSE IF ( FIELDS( 1 ) .EQ. 'NODATA_VALUE' ) THEN
                AMISS = STR2REAL( FIELDS( 2 ) )
            ELSE
                N = L-1
                EXIT
            END IF

        END DO

        REWIND( FDEV )

        DO  L = 1, N

            READ( FDEV, '( A )', IOSTAT = ISTAT ) CHBUF

            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I4, A, I6, 2X, A, I10 )' )       &
                      'MODGISIO/RDARCHDR: Error reading ', FDEV,    &
                      ' at line', L, ' STATUS = ', ISTAT
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END DO

        IF ( EFLAG ) THEN
            CALL M3MESG( '*** Header-skip error' )
            RDARCHDR = .FALSE.
        ELSE
            RDARCHDR = ( CFLAG .AND. RFLAG .AND. XFLAG .AND. YFLAG .AND. DFLAG )
        END IF

        RETURN

    END FUNCTION RDARCHDR


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDZBRFILE( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the GRIDFLOAT GZipped GIS input file with logical name FNAME
        !!       Read and transpose the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GFZREAD from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        REAL            , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDZBRFILE'

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, RR
        REAL            IBUF( NCOLS, NROWS )

        CHARACTER*256   MESG
        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  RDZBIFILE

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. GFZREAD( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) ) THEN
            RDZBRFILE = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )
            END DO
        END DO

        RDZBRFILE = .TRUE.
        RETURN

    END FUNCTION RDZBRFILE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDZBIFILE1( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GZipped GIS input file with logical name FNAME
        !!       Read and transpose the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBILREAD from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER*1       , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDZBIFILE1'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDZBIFILE1

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. ZBILREAD( EQNAME, NCOLS, NROWS, IBUF ) ) THEN
            RDZBIFILE1 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer*1
            END DO
        END DO

        RDZBIFILE1 = .TRUE.
        RETURN

    END FUNCTION RDZBIFILE1



    LOGICAL FUNCTION RDZBIFILE2( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GZipped GIS input file with logical name FNAME
        !!       Read and transpose the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBILREAD from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER*2       , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDZBIFILE2'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDZBIFILE2

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. ZBILREAD( EQNAME, NCOLS, NROWS, IBUF ) ) THEN
            RDZBIFILE2 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer*1
            END DO
        END DO

        RDZBIFILE2 = .TRUE.
        RETURN

    END FUNCTION RDZBIFILE2



    LOGICAL FUNCTION RDZBIFILE4( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GZipped GIS input file with logical name FNAME
        !!       Read and transpose the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBILREAD from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDZBIFILE4'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDZBIFILE4

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. ZBILREAD( EQNAME, NCOLS, NROWS, IBUF ) ) THEN
            RDZBIFILE4 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer*1
            END DO
        END DO

        RDZBIFILE4 = .TRUE.
        RETURN

    END FUNCTION RDZBIFILE4



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDZBI2FILE2( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*2-BIL GZipped GIS input file with logical name FNAME
        !!       Read and transpose  the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBIL2READ from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER*2       , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDZBI2FILE2'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*2   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDZBIFILE2

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. ZBIL2READ( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) ) THEN
            RDZBI2FILE2 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer*2
            END DO
        END DO

        RDZBI2FILE2 = .TRUE.
        RETURN

    END FUNCTION RDZBI2FILE2



    LOGICAL FUNCTION RDZBI2FILE4( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*2-BIL GZipped GIS input file with logical name FNAME
        !!       Read and transpose  the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBIL2READ from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDZBI2FILE4'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*2   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDZBIFILE2

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. ZBIL2READ( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) ) THEN
            RDZBI2FILE4 = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer*2
            END DO
        END DO

        RDZBI2FILE4 = .TRUE.
        RETURN

    END FUNCTION RDZBI2FILE4



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDZBI4FILE( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*4-BIL GZipped GIS input file with logical name FNAME
        !!       Read and transpose the data into VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBIL4READ from"gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(  IN ) :: FNAME
        INTEGER         , INTENT(  IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( OUT ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/RDZBI4FILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*4   IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME


        !!***************************************************************
        !!   begin body of function  RDZBIFILE

        CALL NAMEVAL( FNAME, EQNAME )
        IF ( 0 .EQ. ZBIL4READ( EQNAME, NCOLS, NROWS, BYTESWAP, IBUF ) ) THEN
            RDZBI4FILE = .FALSE.
            RETURN
        END IF

        DO R = 1, NROWS
            RR = NROWS - R + 1
            DO C = 1, NCOLS
                VBUF( C,RR ) = IBUF( C,R )    ! integer ~~> integer*
            END DO
        END DO

        RDZBI4FILE = .TRUE.
        RETURN

    END FUNCTION RDZBI4FILE



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRZBRFILE( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the GRIDFLOAT GZipped GIS-output file with logical name FNAME
        !!       Transpose and write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       GFZWRITE from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        REAL            , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRZBRFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        REAL        IBUF( NCOLS, NROWS )

        CHARACTER*512       EQNAME

        !!***************************************************************
        !!   begin body of function  WRZBRFIL

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRZBRFILE = ( 0 .NE. GFZWRITE( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) )
        RETURN

    END FUNCTION WRZBRFILE



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRZBIFILE1( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GZipped GIS-output file with logical name FNAME
        !!       Transpose and write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBILWRITE from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER*1       , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRZBIFILE'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRZBIFILE

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRZBIFILE1 = ( 0 .NE. ZBILWRITE( EQNAME, NCOLS, NROWS, IBUF ) )
        RETURN

    END FUNCTION WRZBIFILE1



    LOGICAL FUNCTION WRZBIFILE2( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GZipped GIS-output file with logical name FNAME
        !!       Transpose and write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBILWRITE from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER*2       , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRZBIFILE2'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRZBIFILE

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRZBIFILE2 = ( 0 .NE. ZBILWRITE( EQNAME, NCOLS, NROWS, IBUF ) )
        RETURN

    END FUNCTION WRZBIFILE2



    LOGICAL FUNCTION WRZBIFILE4( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*1-BIL GZipped GIS-output file with logical name FNAME
        !!       Transpose and write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBILWRITE from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRZBIFILE4'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*1   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRZBIFILE4

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRZBIFILE4 = ( 0 .NE. ZBILWRITE( EQNAME, NCOLS, NROWS, IBUF ) )
        RETURN

    END FUNCTION WRZBIFILE4



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRZBI2FILE2( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*2-BIL GZipped GIS-output file with logical name FNAME
        !!       Transpose and write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBIL2WRITE from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER*2       , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRZBI2FILE2'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*2   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRZBI2FILE2

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRZBI2FILE2 = ( 0 .NE. ZBIL2WRITE( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) )
        RETURN

    END FUNCTION WRZBI2FILE2



    LOGICAL FUNCTION WRZBI2FILE4( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*2-BIL GZipped GIS-output file with logical name FNAME
        !!       Transpose and write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBIL2WRITE from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRZBI2FILE4'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*2   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRZBI2FILE4

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRZBI2FILE4 = ( 0 .NE. ZBIL2WRITE( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) )
        RETURN

    END FUNCTION WRZBI2FILE4



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRZBI4FILE( FNAME, NCOLS, NROWS, VBUF )

        !!***************************************************************
        !!  RETURNS:
        !!       TRUE iff the operation succeeds
        !!
        !!  DESCRIPTION:
        !!       Open the INT*4-BIL GZipped GIS-output file with logical name FNAME
        !!       Transpose and write the data from VBUF.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       setenv FNAME <path name>
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       ZBIL4WRITE from "gridbin.c"
        !!***************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        INTEGER         , INTENT( IN ) :: NCOLS, NROWS
        INTEGER         , INTENT( IN ) :: VBUF( NCOLS, NROWS )


        !!...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=16), PARAMETER ::  PNAME = 'MODGISIO/WRZBI4FIL'


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     C, R, RR
        INTEGER*4   IBUF( NCOLS, NROWS )

        CHARACTER*512   EQNAME


        !!***************************************************************
        !!   begin body of function  WRZBI4FILE

        DO R = 1, NROWS

            RR = NROWS - R + 1
            DO C = 1, NCOLS
                IBUF( C,R ) = VBUF( C,RR )
            END DO
        END DO

        CALL NAMEVAL( FNAME, EQNAME )
        WRZBI4FILE = ( 0 .NE. ZBIL4WRITE( EQNAME, NCOLS, NROWS, BYTESWAP,IBUF ) )
        RETURN

    END FUNCTION WRZBI4FILE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION DBLERR( P, Q )
        REAL*8, INTENT( IN ) :: P, Q
        DBLERR = ( (P - Q)**2  .GT.  1.0D-10*( P*P + Q*Q + 1.0D-5 ) )
    END FUNCTION DBLERR



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION FLTSAME( P, Q )
        REAL, INTENT( IN ) :: P, Q
        FLTSAME = ( (P - Q)**2  .LE.  1.0E-9*( P*P + Q*Q + 1.0E-5 ) )
    END FUNCTION FLTSAME



END MODULE MODGISIO
