
PROGRAM M3TOGIS

    !!***************************************************************
    !!  Version "$Id: m3togis.f90 280 2025-04-12 15:34:39Z coats $"
    !!  Copyright (c) 2009-2013 Baron Advanced Meteorological Systems
    !!  and (C) 2021-2025 Carlie J. Coats, Jr.,
    !!  Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!  See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  program body starts at line  110
    !!
    !!  DESCRIPTION:
    !!      Read time step of variable from M3IO file and write
    !!      the result as BIL, BIL2, or GRIDFLOAT
    !!
    !!  PRECONDITIONS:
    !!      setenv GRIDFILE   <path name for input M3IO gridded file>
    !!      setenv FLIST      <path-name for input ASCII file-list>
    !!
    !!      for each file in ${FLIST}:
    !!
    !!          setenv <file> <path-name>
    !!
    !!          path-names are of the form <root>.[ flt | bil | hdr ]
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  5/2009 by CJC
    !!      Version    2/2010 by CJC:  add ARCE
    !!      Version    2/2010 by CJC:  add ZBIL, ZBIL2, ZBIL4, ZFLT
    !!      Version    1/2013 by CJC:  Use logical names in FILELIST
    !!      and then call NAMEVAL()
    !!      Version   10/2013 by CJC:  environment variables IMISS, RMISS
    !!      for BIL-header "NODATA"
    !!      Version   4/2025 by CJC for I/O API M3Tools version 4.0
    !!      Support for more file types
    !!***************************************************************

    USE M3UTILIO
    USE MODGISIO

    IMPLICIT NONE


    !!......  PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::  ATYPES( 13 ) =  &
        (/ 'BIL  ', 'BIL2 ', 'BIL4 ', 'FLT  ', 'ZBIL ', 'ZBIL2', 'ZBIL4', 'ZFLT ', 'ARCI ', 'ARCR ', 'ARCE ', 'ASCI ', 'ASCR ' /)
    INTEGER     , PARAMETER ::  VTYPES( 13 ) =  &
        (/  M3INT,  M3INT,   M3INT,   M3REAL,  M3INT,   M3INT,   M3INT,   M3REAL,   M3INT,   M3REAL,  M3REAL,  M3INT,   M3REAL /)
    INTEGER     , PARAMETER ::  NBITS( 11 ) =  &
        (/     8,      16,      32,       32,      8,      16,      32,       32,      32,       32,       32 /)

    CHARACTER*16, PARAMETER ::  PNAME = 'M3TOGIS'
    CHARACTER*16, PARAMETER::   BLANK = ' '
    CHARACTER*64, PARAMETER::   BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT
    INTEGER         FDEV                !  for ${FLIST}
    INTEGER         V, J, K, L
    INTEGER         VBITS
    INTEGER         IMISS
    REAL            RMISS
    LOGICAL         EFLAG
    CHARACTER*1     CBUF
    CHARACTER*16    LNAME, FN, VN, AT
    CHARACTER*256   MESG
    CHARACTER*512   EQBUF
    CHARACTER*512   FROOT
    CHARACTER*512   HROOT

    !!     GRIDDESC name, parameters for output grid

    CHARACTER*16    GDNAM2
    INTEGER         GDTYP2
    INTEGER         NCOLS2
    INTEGER         NROWS2
    INTEGER         NTHIK2
    REAL*8          P_ALP2
    REAL*8          P_BET2
    REAL*8          P_GAM2
    REAL*8          XCENT2
    REAL*8          YCENT2
    REAL*8          XORIG2
    REAL*8          YORIG2
    REAL*8          XCELL2
    REAL*8          YCELL2

    !!      file/variable/timestep-list

    INTEGER             NVARS
    CHARACTER*16,  ALLOCATABLE :: FNAME( : )
    CHARACTER*16 , ALLOCATABLE :: VNAME( : )
    INTEGER      , ALLOCATABLE :: VDATE( : )
    INTEGER      , ALLOCATABLE :: VTIME( : )
    INTEGER      , ALLOCATABLE :: VLAYR( : )
    INTEGER      , ALLOCATABLE :: VTYPE( : )
    CHARACTER*16 , ALLOCATABLE :: ATYPE( : )
    CHARACTER*16 , ALLOCATABLE :: UNITS( : )
    CHARACTER*80 , ALLOCATABLE :: VDESC( : )

    INTEGER, ALLOCATABLE :: IBUF( :,: )
    REAL   , ALLOCATABLE :: RBUF( :,: )

    !!--------------------------------------------------------------
    !!   begin body of program M3TOGIS

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                                     &
'Program M3TOGIS to read time steps of variables from a gridded M3IO file',     &
'and write them to multiple ARC-ASCII, GRIDFLOAT and/or BIL files.',            &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'    setenv GRIDFILE   <path name for input gridded file>',                     &
'    setenv FILELIST   <path-name for input ASCII file-list>',                  &
'    setenv HDRDIR     <path for header files>',                                &
'',                                                                             &
'    setenv IMISS      <missing value for INTEGER variables> [IMISS3]',         &
'    setenv RMISS      <missing value for  REAL   variables> [BADVAL3]',        &
'',                                                                             &
'    ${FILELIST} is list-formatted (quoted strings) with one input',            &
'    file per line:',                                                           &
'',                                                                             &
'        <file-name> <vble-name> <date> <time> <layer> <type>',                 &
'        where <type> is one of',                                               &
'           { "BIL"  "BIL2"  "BIL4"  "FLT",  "ARCI", "ARCR", "ARCE"',           &
'            "ZBIL" "ZBIL2" "ZBIL4" "ZFLT" }',                                  &
'        and LEN( file-name ) at most 16',                                      &
'',                                                                             &
'    for each file in ${FILELIST} and for the corresponding header-files:',     &
'        setenv <file> <path-name>',                                            &
'        path-names are of the form <root>.[ flt | bil | bil2 | bil4  | hdr ]', &
'',                                                                             &
'    Output header files are ${HDRDIR}/<vname>.bil.hdr',                        &
'',                                                                             &
'Copyright (C) 2009-2013 Baron Advanced Meteorological Systems, LLC.',          &
'and (c) 2021-2025 Carlie J. Coats, Jr.',                                       &
'Released under Version 2 of the GNU General Public License.',                  &
'See enclosed GPL.txt, or URL',                                                 &
'https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html',                    &
' ',                                                                            &
'Comments and questions are welcome and can be sent to',                        &
' ',                                                                            &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                           &
'',                                                                             &
'Program version:',                                                             &
'$Id: m3togis.f90 280 2025-04-12 15:34:39Z coats $',&
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    CALL BINVERBOSE()


    !!...............  Open LL file and get its description:

    IF ( .NOT.OPEN3( 'GRIDFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "GRIDFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'GRIDFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "GRIDFILE"'
        CALL M3MESG( MESG )
    ELSE

        GDNAM2 = GDNAM3D
        GDTYP2 = GDTYP3D
        NCOLS2 = NCOLS3D
        NROWS2 = NROWS3D
        GDTYP2 = GDTYP3D
        P_ALP2 = P_ALP3D
        P_BET2 = P_BET3D
        P_GAM2 = P_GAM3D
        XCENT2 = XCENT3D
        YCENT2 = YCENT3D
        XORIG2 = XORIG3D
        YORIG2 = YORIG3D
        XCELL2 = XCELL3D
        YCELL2 = YCELL3D

        ALLOCATE( RBUF( NCOLS3D,NROWS3D ), &
                  IBUF( NCOLS3D,NROWS3D ),  STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'Allocation failure for work arrays:  STATUS=', ISTAT
            EFLAG = .TRUE.
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

    END IF              !  if not.open3(gridfile...); else...



    !!...............  Open and read ASCII file-list

    FDEV = GETEFILE( 'FILELIST', .TRUE., .TRUE., PNAME )
    IF ( FDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Could not open file "FILELIST"' )
    ELSE

        NVARS = 0

        DO              !!  loop counting FILELIST

            READ( FDEV,  *, END=11, IOSTAT=ISTAT ) FN, VN, J, K, L, AT
            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10, 2X, A, I3 )' )  &
                    'Error=', ISTAT, 'reading "FILELIST" at line', NVARS+1
                CALL M3MESG( MESG )
                EXIT
            END IF
            NVARS = NVARS + 1

        END DO

11      CONTINUE        !!  EOF-exit from loop counting FILELIST

        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Error(s) counting ${FILELIST}', 2 )
        END IF
        REWIND( FDEV )
        WRITE ( MESG, '( A, I10 )' ) 'Number of input variables/records:', NVARS
        CALL M3MESG( MESG )
        CALL M3MESG( BAR )


        ALLOCATE( FNAME( NVARS ), &
                  VNAME( NVARS ), &
                  VDATE( NVARS ), &
                  VTIME( NVARS ), &
                  VLAYR( NVARS ), &
                  VTYPE( NVARS ), &
                  ATYPE( NVARS ), &
                  UNITS( NVARS ), &
                  VDESC( NVARS ),  STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'Allocation failure for FILELIST arrays:  STATUS=', ISTAT
            EFLAG = .TRUE.
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        DO L = 1, NVARS

            READ( FDEV, *, IOSTAT=ISTAT )       &
                FNAME( L ), VNAME( L ), VDATE( L ), VTIME( L ), VLAYR( L ), ATYPE( L )

            IF ( ISTAT .NE. 0 ) THEN

                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10, 2X, A, I3 )' )  &
                    'Error=', ISTAT, 'reading "FILELIST" at line', L
                CALL M3MESG( MESG )
                EXIT

            ELSE

                V = INDEX1( ATYPE( L ), 13, ATYPES )
                IF ( V .LE. 0 ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( 3A, I3 )' )         &
                       'Invalid type="', TRIM( ATYPE( L ) ), '" in "FILELIST" at line', L
                    CALL M3MESG( MESG )
                END IF
                V = INDEX1( VNAME( L ), NVARS3D, VNAME3D )
                IF ( V .LE. 0 ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Requested variable not available: ' // VNAME( L )
                    CALL M3MESG( MESG )
                ELSE
                    UNITS( L ) = UNITS3D( V )
                    VDESC( L ) = VDESC3D( V )
                    VTYPE( L ) = VTYPE3D( V )
                END IF

            END IF

        END DO          !  end of loop reading FILELIST

    END IF                      !  if fdev<0; else...

    IMISS = ENVINT( 'IMISS', 'Missing-value for INTEGER variables [0]', 0, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "IMISS"' )
    END IF

    RMISS = ENVREAL( 'RMISS', 'Missing-value for REAL variables [-9.999e36]', BADVAL3, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "RMISS"' )
    END IF

    CALL ENVSTR( 'HDRDIR', 'Directory for output header-files, or "NONE"', 'NONE', HROOT, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "HDRDIR"' )
    ELSE IF ( HROOT .EQ. 'NONE' ) THEN
        CALL M3MESG( 'Header production turned off' )
    END IF



    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal setup error(s)', 2 )
    END IF


    !!...............  Loop on L:  output files/variables/time-steps

    DO L = 1, NVARS

        IF ( ATYPE( L ) .EQ. 'BIL'  ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                IBUF( :,: ) = NINT( RBUF( :,: ) )
            END IF

            IF ( .NOT. WRBIFILE( FNAME(L), NCOLS2, NROWS2, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 8

        ELSE IF ( ATYPE( L ) .EQ. 'ZBIL'  ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                IBUF( :,: ) = NINT( RBUF( :,: ) )
            END IF

            IF ( .NOT. WRZBIFILE( FNAME(L), NCOLS2, NROWS2, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 8

        ELSE IF ( ATYPE( L ) .EQ. 'BIL2' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                IBUF( :,: ) = NINT( RBUF( :,: ) )
            END IF

            IF ( .NOT. WRBI2FILE( FNAME(L), NCOLS2, NROWS2, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 16

        ELSE IF ( ATYPE( L ) .EQ. 'ZBIL2' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                IBUF( :,: ) = NINT( RBUF( :,: ) )
            END IF

            IF ( .NOT. WRZBI2FILE( FNAME(L), NCOLS2, NROWS2, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 16

        ELSE IF ( ATYPE( L ) .EQ. 'BIL4' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                IBUF( :,: ) = NINT( RBUF( :,: ) )
            END IF

            IF ( .NOT. WRBI4FILE( FNAME(L), NCOLS2, NROWS2, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 32

        ELSE IF ( ATYPE( L ) .EQ. 'ZBIL4' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                IBUF( :,: ) = NINT( RBUF( :,: ) )
            END IF

            IF ( .NOT. WRZBI4FILE( FNAME(L), NCOLS2, NROWS2, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 32

        ELSE IF ( ATYPE( L ) .EQ. 'FLT'  ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                RBUF( :,: ) = FLOAT( IBUF( :,: ) )
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            END IF
            VBITS = 32

            IF ( .NOT. WRBRFILE( FNAME(L), NCOLS2, NROWS2, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

        ELSE IF ( ATYPE( L ) .EQ. 'ZFLT'  ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                RBUF( :,: ) = FLOAT( IBUF( :,: ) )
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            END IF

            IF ( .NOT. WRZBRFILE( FNAME(L), NCOLS2, NROWS2, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 32

        ELSE IF ( ATYPE( L ) .EQ. 'ARCI' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                IBUF( :,: ) = NINT( RBUF( :,: ) )
            END IF

            IF ( .NOT. WRARC( FNAME(L), NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 32

        ELSE IF ( ATYPE( L ) .EQ. 'ARCR' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                RBUF( :,: ) = FLOAT( IBUF( :,: ) )
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            END IF

            IF ( .NOT. WRARC( FNAME(L), NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 32

        ELSE IF ( ATYPE( L ) .EQ. 'ARCE' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                RBUF( :,: ) = FLOAT( IBUF( :,: ) )
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            END IF

            IF ( .NOT. WRAEFILE( FNAME(L), NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 32

        ELSE IF ( ATYPE( L ) .EQ. 'ASCI' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                IBUF( :,: ) = NINT( RBUF( :,: ) )
            END IF

            IF ( .NOT. WRAFILE( FNAME(L), NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, IBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 32

        ELSE IF ( ATYPE( L ) .EQ. 'ASCR' ) THEN

            IF (      VTYPE( L ) .EQ. M3INT  ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), IBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
                RBUF( :,: ) = FLOAT( IBUF( :,: ) )
            ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN
                IF ( .NOT.READ3( 'GRIDFILE', VNAME(L), VLAYR(L), VDATE(L), VTIME(L), RBUF )  ) THEN
                    EFLAG = .TRUE.
                    CYCLE
                END IF
            END IF

            IF ( .NOT. WRAFILE( FNAME(L), NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, RBUF ) ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF
            VBITS = 32

        ELSE

            EFLAG = .TRUE.
            MESG = 'Unrsupported type ' // ATYPE( L )
            CALL M3MESG( MESG )
            CYCLE

        END IF          !  if atype(l) is 'BIL', 'BIL2', ...

        IF ( HROOT .EQ. 'NONE' .OR. L .GT. 8 ) THEN

            CONTINUE

        ELSE IF ( VTYPE( L ) .EQ. M3INT ) THEN

            IF ( .NOT.BILHDR( HROOT,                                    &
                              VNAME(L), UNITS(L), VDESC(L), 1.0, 0.0,   &
                              IMISS, VBITS, GDTYP2, NCOLS2, NROWS2,     &
                              P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,   &
                              XORIG2, YORIG2, XCELL2, YCELL2 ) ) THEN

                EFLAG = .TRUE.

            END IF              !  if not bilhdr

        ELSE IF ( VTYPE( L ) .EQ. M3REAL ) THEN

            IF ( .NOT.WRGFHDR( HROOT,  VNAME(L), UNITS(L), VDESC(L),    &
                               RMISS, GDTYP2, NCOLS2, NROWS2,           &
                               P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,  &
                               XORIG2, YORIG2, XCELL2, YCELL2 ) ) THEN

                EFLAG = .TRUE.

            END IF              !  if not wrgfhdr

        END IF                  !  if vtype(L) is m3int; else is m3real

    END DO                      !  end loop on outputs L


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )



END PROGRAM M3TOGIS

