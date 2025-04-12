
PROGRAM GISTOM3

    !!***************************************************************
    !!  Version "$Id: gistom3.f90 280 2025-04-12 15:34:39Z coats $"
    !!  Copyright (c) 2008 Baron Advanced Meteorological Systems
    !!  and (C) 2021-2025 Carlie J. Coats, Jr.,
    !!  Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!  See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  program body starts at line  85
    !!
    !!  DESCRIPTION:
    !!      Read  GZIPped GRIDFLOAT and BIL NLCD variables
    !!      and write the result to M3IO GRIDFILE
    !!
    !!  PRECONDITIONS:
    !!      setenv FLIST      <path-name for input ASCII file-list>
    !!
    !!      setenv LLFILE     <path name for  input M3IO Lat-Lon file>
    !!      setenv GRIDFILE   <path name for output M3IO gridded file>
    !!
    !!      ${FLIST} is list-formatted (quoted strings) with one
    !!      input file per line:
    !!
    !!          <file-name> <vble-name> <units> <description> <type>
    !!          where <type> is one of { 'BIL' 'BIL2' 'BIL4' 'FLT' } and
    !!          LEN( file-name | vble-name | units ) at most 16
    !!
    !!      for each file in ${FLIST}
    !!
    !!          setenv <file> <path-name>
    !!          path-names are of the form <root>.[ flt | bil | hdr ]
    !!
    !!      Input files already on the output map-projection and grid.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  6/2008 by CJC
    !!      Version    4/2025 by CJC for I/O API M3Tools version 4.0
    !!***************************************************************

    USE M3UTILIO
    USE MODGISIO

    IMPLICIT NONE


    !!......  PARAMETERS and their descriptions:

    INTEGER     , PARAMETER :: NTYPES = 12
    CHARACTER*16, PARAMETER ::  ATYPES( NTYPES ) = (/    &
      'BIL  ', 'BIL2 ', 'BIL4 ', 'FLT  ', 'ZBIL ', 'ZBIL2', 'ZBIL4', 'ZFLT ', 'ARCI ', 'ARCR ', 'ASCI ', 'ASCR ' /)
    INTEGER     , PARAMETER ::  VTYPES( NTYPES ) = (/    &
      M3INT,   M3INT,   M3INT,   M3REAL,   M3INT,   M3INT,   M3INT,   M3REAL,  M3INT,   M3REAL,  M3INT,   M3REAL /)

    CHARACTER*16, PARAMETER ::  PNAME = 'GISTOM3'
    CHARACTER*16, PARAMETER::   BLANK = ' '
    CHARACTER*64, PARAMETER::   BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    CHARACTER(LEN=80), PARAMETER :: PROGVER = '$Id: gistom3.f90 280 2025-04-12 15:34:39Z coats $'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT, IMISS
    INTEGER         FDEV                !  for ${FLIST}
    INTEGER         V, L
    LOGICAL         EFLAG
    CHARACTER*256   MESG
    CHARACTER*512   EQBUF

    !!      variables-list

    INTEGER             NVARS
    CHARACTER*16        FNAME( MXVARS3 )
    CHARACTER*16        VNAME( MXVARS3 )
    CHARACTER*16        VUNIT( MXVARS3 )
    CHARACTER*16        ATYPE( MXVARS3 )
    INTEGER             VTYPE( MXVARS3 )
    CHARACTER*80        VDESC( MXVARS3 )

    !!      Output buffers

    REAL,    ALLOCATABLE ::  RBUF( :,: )
    INTEGER, ALLOCATABLE ::  IBUF( :,: )

    !!--------------------------------------------------------------
    !!   begin body of program GISTOM3

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )   BLANK, BAR,                            &
'Program GISTOM3 to read multiple ASC- and ARC-format ASCII files,',    &
'and binary GRIDFLOAT and BIL files, and write the contents to a',      &
'gridded M3IO file',                                                    &
'',                                                                     &
'PRECONDITIONS REQUIRED:',                                              &
'    setenv LLFILE     <path name for input M3IO Lat-Lon file',         &
'                      (used to specify the grid)',                     &
'',                                                                     &
'    setenv GRIDFILE   <path name for output gridded file>',            &
'    setenv FILELIST   <path-name for input ASCII file-list>',          &
'',                                                                     &
'    ${FILELIST} is list-formatted (quoted strings) with one input',    &
'    file per line:',                                                   &
'',                                                                     &
'        <file-name> <vble-name> <units> <description> <type>',         &
'        where <type> is one of:',                                      &
'        { "BIL" "BIL2" "BIL4" "FLT",  "ZBIL" "ZBIL2" "ZBIL4" "ZFLT",', &
'        "ARCI", "ARCR", "ASCI", "ASCR" }',                             &
'        and LEN( file-name | vble-name | units ) at most 16',          &
'',                                                                     &
'    for each file in ${FILELIST}:',                                    &
'        setenv <file> <path-name>',                                    &
'        path-names are of the form <root>.[ flt | bil | hdr ]',        &
'',                                                                     &
'    Input files already on the output map-projection and grid.',       &
'',                                                                     &
'NOTE:  ARCI, ARCR have header lines with grid parameters',             &
'',                                                                     &
'            "north", "south" "east", "west", "rows", and "cols",',     &
'',                                                                     &
'while ASCI, ASCR have header lines with',                              &
'',                                                                     &
'            "ncols", "nrows", "xllcorner", "yllcorner", "cellsize"',   &
'            and optionally "NODATA_value"',                            &
'',                                                                     &
'Copyright (C) 2008 Baron Advanced Meteorological Systems, LLC.',       &
'and (c) 2021-2025 Carlie J. Coats, Jr.',                               &
'Released under Version 2 of the GNU General Public License.',          &
'See enclosed GPL.txt, or URL',                                         &
'https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html',            &
' ',                                                                    &
'Comments and questions are welcome and can be sent to',                &
' ',                                                                    &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                   &
'',                                                                     &
'Program version:',                                                     &
'$Id: gistom3.f90 280 2025-04-12 15:34:39Z coats $',&
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF
    
    CALL BINVERBOSE()


    !!...............  Open LL file and get its description:

    IF ( .NOT.OPEN3( 'LLFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "LLFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'LLFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "LLFILE"'
        CALL M3MESG( MESG )
    ELSE
        ALLOCATE( RBUF( NCOLS3D,NROWS3D ), &
                  IBUF( NCOLS3D,NROWS3D ),  STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'Allocation failure for work arrays:  STATUS=', ISTAT
            EFLAG = .TRUE.
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF
    END IF


    !!...............  Open and read ASCII file-list

    FDEV = GETEFILE( 'FILELIST', .TRUE., .TRUE., PNAME )
    IF ( FDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Could not open file "FILELIST"' )
    ELSE

        DO L = 1, MXVARS3

            READ( FDEV, *, END=12, IOSTAT=ISTAT )       &
                FNAME( L ), VNAME( L ), VUNIT( L ), VDESC( L ), ATYPE( L )

            IF ( ISTAT .NE. 0 ) THEN

                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10, 2X, A, I3 )' )  &
                    'Error=', ISTAT, 'reading "FILELIST" at line', L 
                CALL M3MESG( MESG )

            ELSE

                V = INDEX1( ATYPE( L ), NTYPES, ATYPES )
                IF ( V .LE. 0 ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( 3A, I3 )' )         &
                       'Invalid type="', TRIM( ATYPE( L ) ), '" in "FILELIST" at line', L
                    CALL M3MESG( MESG )
                ELSE
                    VTYPE( L ) = VTYPES( V )
                END IF

            END IF

            NVARS = L

        END DO          !  end of loop reading FILELIST

12      CONTINUE        !  EOF-exit from loop reading FILELIST

    END IF


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal setup error(s)', 2 )
    END IF


    !!...............  Open output file (borrowing most of LLFILE description):

    NVARS3D = NVARS
    DO V = 1, NVARS
        VNAME3D( V ) = VNAME( V )
        UNITS3D( V ) = VUNIT( V )
        VDESC3D( V ) = VDESC( V )
        VTYPE3D( V ) = VTYPE( V )
    END DO
    FDESC3D( : ) = BLANK
    FDESC3D( 1 ) = 'Converted GIS gridded variables'

    IF ( OPEN3( 'GRIDFILE', FSUNKN3, PNAME ) ) THEN

        DO V = 1, NVARS3D

            CALL M3MESG( 'Processing ' // FNAME( V ) )

            IF      ( ATYPE( V ) .EQ. 'BIL' ) THEN

                IF ( .NOT.RDBIFILE( FNAME( V ), NCOLS3D, NROWS3D, IBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, IBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'BIL2' ) THEN

                IF ( .NOT.RDBI2FILE( FNAME( V ), NCOLS3D, NROWS3D, IBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, IBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'BIL4' ) THEN

                IF ( .NOT.RDBI4FILE( FNAME( V ), NCOLS3D, NROWS3D, IBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, IBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'FLT' )  THEN

                IF ( .NOT.RDBRFILE( FNAME( V ), NCOLS3D, NROWS3D, RBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, RBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF


            ELSE IF ( ATYPE( V ) .EQ. 'ZBIL' ) THEN

                IF ( .NOT.RDZBIFILE( FNAME( V ), NCOLS3D, NROWS3D, IBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, IBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'ZBIL2' ) THEN

                IF ( .NOT.RDZBI2FILE( FNAME( V ), NCOLS3D, NROWS3D, IBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, IBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'ZBIL4' ) THEN

                IF ( .NOT.RDZBI4FILE( FNAME( V ), NCOLS3D, NROWS3D, IBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, IBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'ZFLT' )  THEN

                IF ( .NOT.RDZBRFILE( FNAME( V ), NCOLS3D, NROWS3D, RBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, RBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'ARCI' )  THEN

                IF ( .NOT.RDARCI( FNAME( V ), NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, IBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, IBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'ARCR' )  THEN

                IF ( .NOT.RDARCR( FNAME( V ), NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, RBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, RBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'ASCI' )  THEN

                IF ( .NOT.RDAIFILE( FNAME( V ), NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, IBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, IBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            ELSE IF ( ATYPE( V ) .EQ. 'ASCR' )  THEN

                IF ( .NOT.RDARFILE( FNAME( V ), NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, RBUF ) ) THEN
                    EFLAG = .TRUE.
                ELSE IF ( .NOT. WRITE3( 'GRIDFILE', VNAME( V ), 0,0, RBUF ) ) THEN
                    EFLAG = .TRUE.
                END IF

            END IF              !  if atype(l) = ...

        END DO

    END IF                !  if open3(GRIDFILE) succeeded, or failed


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )



END PROGRAM GISTOM3










