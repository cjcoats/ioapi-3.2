
PROGRAM GISTOGIS

    !!***************************************************************
    !!  Version "$Id: gistogis.f90 280 2025-04-12 15:34:39Z coats $"
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
    !!
    !!      setenv LLFILE     <path name for input M3IO Lat-Lon file>
    !!      setenv FLIST      <path-name for input ASCII file-list>
    !!
    !!      ${FLIST} is list-formatted (quoted strings) with one
    !!      input file per line:
    !!
    !!          <infile-name> <intype> <outfile-name> <outtype>
    !!          where <type> is one of { 'BIL' 'BIL2' 'BIL4' 'FLT',
    !!          'ARCI', 'ARCR', 'ASCI', 'ASCR' }
    !!          LEN( file-name ) at most 16.
    !!          Types are compatible:  only int-to-int or real-to-real conversions
    !!
    !!      for each file in ${FLIST}
    !!
    !!          setenv <file> <path-name>
    !!          path-names are of the form <root>.[ flt | bil | hdr ]
    !!          All the files are on a common grid
    !!
    !!      Input files already on the output map-projection and grid.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  9/2008 by CJC
    !!      Version    4/2025 by CJC for I/O API M3Tools version 4.0
    !!***************************************************************

    USE M3UTILIO
    USE MODGISIO

    IMPLICIT NONE


    !!......  PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::  ATYPES( 12 ) = (/ 'BIL  ', 'BIL2 ', 'BIL4 ', 'FLT  ',       &
                                                  'ZBIL ', 'ZBIL2', 'ZBIL4', 'ZFLT ',        &
                                                  'ARCI ', 'ARCR ', 'ASCI ', 'ASCR ' /)
    INTEGER     , PARAMETER ::  VTYPES( 12 ) = (/  M3INT,   M3INT,   M3INT,  M3REAL,        &
                                                   M3INT,   M3INT,   M3INT,  M3REAL,        &
                                                   M3INT,  M3REAL,   M3INT,  M3REAL /)

    CHARACTER*16, PARAMETER ::  PNAME = 'GISTOGIS'
    CHARACTER*16, PARAMETER::   BLANK = ' '
    CHARACTER*64, PARAMETER::   BAR   = &
  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT, IMISS
    INTEGER         FDEV                !  for ${FLIST}
    INTEGER         J, K, L
    LOGICAL         EFLAG
    CHARACTER*16    INPNAME, INPTYPE, OUTNAME, OUTTYPE
    CHARACTER*256   MESG
    CHARACTER*512   EQBUF

    !!      Output buffers

    REAL,    ALLOCATABLE ::  RBUF( :,: )
    INTEGER, ALLOCATABLE ::  IBUF( :,: )

    !!--------------------------------------------------------------
    !!   begin body of program GISTOGIS

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                             &
'Program GIS2GIS to read multiple GIS-formatted files, and write',      &
'the contents to a GIS file with possibly different-format.',           &
'',                                                                     &
'PRECONDITIONS REQUIRED:',                                              &
'    setenv LLFILE     <path name for input M3IO Lat-Lon file',         &
'                      (used to specify the grid)',                     &
'    setenv FILELIST   <path-name for input ASCII file-list>',          &
'',                                                                     &
'    ${FILELIST} is list-formatted (quoted strings) with one input',    &
'    file per line:',                                                   &
'',                                                                     &
'        <infile-name> <intype> <outfile-name> <outtype>',              &
'        where <*type> is one of { "BIL" "BIL2" "BIL4" "FLT", ',        &
'        "ZBIL" "ZBIL2" "ZBIL4" "ZFLT", "ARCI", "ARCR",',               &
'        "ASCI", "ASCR" } for output.',                                 &
'        LEN( *filename ) at most 16',                                  &
'        Types are compatible:  only int-to-int or real-to-real',       &
'        conversions/',                                                 &
'',                                                                     &
'    for each IN- OR OUT-file in ${FILELIST}:',                         &
'        setenv <file> <path-name>',                                    &
'        path-names are of the form <root>.[ flt | bil | hdr ]',        &
'',                                                                     &
'    Input files already on the output map-projection and grid.',       &
'',                                                                     &
'NOTE:  ARCI, ARCR, and ARCE have header lines with grid parameters',   &
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
'$Id: gistogis.f90 280 2025-04-12 15:34:39Z coats $',&
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


    !!...............  Open ASCII file-list:


    FDEV = GETEFILE( 'FILELIST', .TRUE., .TRUE., PNAME )
    IF ( FDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3EXIT( PNAME, 0, 0, 'Could not open file "FILELIST"', 2 )
    END IF


    !!...............  Read and process ASCII file-list:

    DO L = 1, 99999999

        READ( FDEV, *, END=99, IOSTAT=ISTAT ) INPNAME, INPTYPE, OUTNAME, OUTTYPE

        IF ( ISTAT .NE. 0 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( A, I10, 2X, A, I3 )' ) 'Error=', ISTAT, 'reading "FILELIST" at line', L 
            CALL M3MESG( MESG )
            CYCLE
        END IF

        CALL M3MESG( 'Processing input "' // TRIM( INPNAME ) // '" and output "' // TRIM( OUTNAME ) // '"' )

        J = INDEX1( INPTYPE, 12, ATYPES )
        IF ( J .LE. 0 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( 3A, I3 )' ) 'Invalid input type="', TRIM( INPTYPE ), '" in "FILELIST" at line', L
            CALL M3MESG( MESG )
            CYCLE
        END IF

        K = INDEX1( OUTTYPE, 12, ATYPES )
        IF ( K .LE. 0 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( 3A, I3 )' ) 'Invalid output type="', TRIM( OUTTYPE ), '" in "FILELIST" at line', L
            CALL M3MESG( MESG )
            CYCLE
        ELSE IF ( VTYPES( J ) .NE. VTYPES( K ) ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( 5A, I3 )' )         &
               'Incompatible types "', TRIM( INPTYPE ), '" and "', TRIM( OUTTYPE ), '" in "FILELIST" at line', L
            CALL M3MESG( MESG )
            CYCLE
        END IF

        IF      ( INPTYPE .EQ. 'BIL' ) THEN

            IF ( .NOT.RDBIFILE( INPNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'BIL2' ) THEN

            IF ( .NOT.RDBI2FILE( INPNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'BIL4' ) THEN

            IF ( .NOT.RDBI4FILE( INPNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'FLT' )  THEN

            IF ( .NOT.RDBRFILE( INPNAME, NCOLS3D, NROWS3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'ZBIL' ) THEN

            IF ( .NOT.RDZBIFILE( INPNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'ZBIL2' ) THEN

            IF ( .NOT.RDZBI2FILE( INPNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'ZBIL4' ) THEN

            IF ( .NOT.RDZBI4FILE( INPNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'ZFLT' )  THEN

            IF ( .NOT.RDZBRFILE( INPNAME, NCOLS3D, NROWS3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'ARCI' )  THEN

            IF ( .NOT.RDARCI( INPNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'ARCR' )  THEN

            IF ( .NOT.RDARCR( INPNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'ASCI' )  THEN

            IF ( .NOT.RDAIFILE( INPNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( INPTYPE .EQ. 'ASCR' )  THEN

            IF ( .NOT.RDARFILE( INPNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        END IF              !  if inptype = ...


        IF      ( OUTTYPE .EQ. 'BIL' ) THEN

            IF ( .NOT.WRBIFILE( OUTNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'BIL2' ) THEN

            IF ( .NOT.WRBI2FILE( OUTNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'BIL4' ) THEN

            IF ( .NOT.WRBI4FILE( OUTNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'FLT' )  THEN

            IF ( .NOT.WRBRFILE( OUTNAME, NCOLS3D, NROWS3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ZBIL' ) THEN

            IF ( .NOT.WRZBIFILE( OUTNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ZBIL2' ) THEN

            IF ( .NOT.WRZBI2FILE( OUTNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ZBIL4' ) THEN

            IF ( .NOT.WRZBI4FILE( OUTNAME, NCOLS3D, NROWS3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ZFLT' )  THEN

            IF ( .NOT.WRZBRFILE( OUTNAME, NCOLS3D, NROWS3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ARCI' )  THEN

            IF ( .NOT.WRARCI( OUTNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ARCR' )  THEN

            IF ( .NOT.WRARCR( OUTNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ASCI' )  THEN

            IF ( .NOT.WRAIFILE( OUTNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ASCR' )  THEN

            IF ( .NOT.WRARFILE( OUTNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( OUTTYPE .EQ. 'ASCE' )  THEN

            IF ( .NOT.WRAEFILE( OUTNAME, NCOLS3D, NROWS3D, XORIG3D, YORIG3D, XCELL3D, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        END IF              !  if outtype = ...

    END DO          !  end of loop reading FILELIST

99  CONTINUE        !  EOF-exit from loop reading FILELIST


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )



END PROGRAM GISTOGIS

