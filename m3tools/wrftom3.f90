
PROGRAM WRFTOM3

    !!***************************************************************
    !! Version "$Id: wrftom3.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (c) 2010 Baron Advanced Meteorological Systems
    !! and (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!..............................................................
    !!  Program body starts at line  88
    !!
    !!  DESCRIPTION:
    !!      Convert a set of variables from a WRF-netCDF file to M3IO
    !!      
    !!  PRECONDITIONS:
    !!      see below
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  8/2010 by Carlie J. Coats, Jr., BAMS
    !!
    !!      Version  11/2015 by CJC for I/O API v3.2 M3TOOLS  .
    !!      support for M3DBLE.
    !!***************************************************************

    USE M3UTILIO
    USE MODWRFIO

    IMPLICIT NONE

    !!......  PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::  TYPES( 0:7 ) =  &
                    (/  'invalid        ',      &
                        'byte           ',      &
                        'character      ',      &
                        'int16          ',      &
                        'integer        ',      &
                        'real           ',      & 
                        'double         ',      &
                        'invalid        '       /)      
    CHARACTER*16, PARAMETER ::  PNAME = 'WRFTOM3'
    CHARACTER*16, PARAMETER ::  BLANK = ' '
    CHARACTER*64, PARAMETER ::  BAR   = &
  '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER :: LDEV, IDEV, ISTAT
    LOGICAL :: EFLAG = .FALSE.
    
    INTEGER     C, CC, R, RR, I, L, N, V
    INTEGER     NCOLS, NROWS, NLAYS, NVARS, NDIMS
    INTEGER     YEAR, DAY, TIME
    INTEGER     SDATE, STIME, TSTEP, NRECS, JDATE, JTIME, JSTEP
    
    REAL*8      SCR
    
    CHARACTER*16 :: OUTGRID
    CHARACTER*4  :: STAGGER
    CHARACTER*32 :: WNAMES( MXVARS3 )
    CHARACTER*16 :: VNAMES( MXVARS3 )
    CHARACTER*16 :: VUNITS( MXVARS3 )
    CHARACTER*80 :: VDESCS( MXVARS3 )
    INTEGER      :: VTYPES( MXVARS3 )
    
    CHARACTER*512   MESG

    INTEGER, ALLOCATABLE :: JBUF1( :,: )
    INTEGER, ALLOCATABLE :: JBUF2( :,: )
    REAL*8 , ALLOCATABLE :: EBUF1( :,: )
    REAL*8 , ALLOCATABLE :: EBUF2( :,: )
    REAL   , ALLOCATABLE :: SBUF1( :,: )
    REAL   , ALLOCATABLE :: SBUF2( :,: )

    INTEGER, ALLOCATABLE :: KBUF1( :,:,: )
    INTEGER, ALLOCATABLE :: KBUF2( :,:,: )
    REAL*8 , ALLOCATABLE :: DBUF1( :,:,: )
    REAL*8 , ALLOCATABLE :: DBUF2( :,:,: )
    REAL   , ALLOCATABLE :: TBUF1( :,:,: )
    REAL   , ALLOCATABLE :: TBUF2( :,:,: )


    !!--------------------------------------------------------------
    !!   begin body of program WRFTOM3

    LDEV  = INIT3()

    WRITE( LDEV, '( 5X, A )' )                                              &
'Program WRFTOM3 to read and window a set of variables from WRF netCDF',    &
'output data, and write it out to an I/O API file, as specified in',        &
'configuration file ${CONFIG}.',                                            &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'   setenv GRIDDESC   <path name>',                                         &
'   setenv OUTGRID    <GRIDDESC name for output grid>',                     &
'   setenv WRFFILE    <path name>',                                         &
'   setenv OUTFILE    <path name>',                                         &
'   setenv CONFIG     <path name>',                                         &
'',                                                                         &
'   ${CONFIG} lists one variable per line.  Each line has two fields',      &
'   list-formatted (names are quoted strings) as follows:',                 &
'',                                                                         &
'       <(input) WRF name>   <(output) M3IO name>',                         &
'',                                                                         &
'   WRF names have length at most 32; M3IO names have length at most 16.',  &
'   All variables listed in a single ${CONFIG} have the same "stagger"',    &
'   and vertical structure, and are of type DOUBLE, REAL or INTEGER.',      &
'',                                                                         &
'   Use ${OUTGRID} = " " (i.e., BLANK) to use the native WRF grid',         &
'   as the output grid.',                                                   &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for starting date time, time step, and',       &
'number of time steps to process.',                                         &
'',                                                                         &
'Copyright (C) 2010-2013 Baron Advanced Meteorological Systems, LLC.,',     &
'and (C) 2015 UNC Institute for the Environment.',                          &
'Released under Version 2 of the GNU General Public License.',              &
'See enclosed GPL.txt, or URL',                                             &
''  ,                                                                       &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',               &
''  ,                                                                       &
'Comments and questions are welcome and can be sent to'  ,                  &
'',                                                                         &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                       &
'or',                                                                       &
'    UNC Institute for the Environment',                                    &
'    100 Europa Dr., Suite 490 Rm 405',                                     &
'    Campus Box 1105',                                                      &
'    Chapel Hill, NC 27599-1105',                                           &
'',                                                                         &
'Program version:',                                                         &
'$Id: wrftom3.f90 1 2017-06-10 18:05:20Z coats $',&
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Get output grid

    CALL ENVSTR( 'OUTGRID', 'GRIDDESC name for output grid', 'WRF_FOO_CRO', OUTGRID, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "OUTGRID"' )
    END IF

    !!...............  Get WRF file data:

    IF ( .NOT.OPENWRF( 'WRFFILE', OUTGRID, FSREAD3 ) ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Error(s) processing WRF file  "WRFFILE"' )
    ELSE
        JDATE = SDATEW
        JTIME = STIMEW
        JSTEP = TSTEPW
    END IF


    !!...............  Read config file:

    IDEV = GETEFILE( 'CONFIG', .TRUE., .TRUE., PNAME )
    IF ( IDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Error(s) opening  "CONFIG"' )
    ELSE
        CALL M3MESG( BAR )
        DO V = 1, MXVARS3

            READ( IDEV, *, END=99, IOSTAT=ISTAT ) WNAMES( V ), VNAMES( V )

            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '(A, I4, 2X, A, I10 )' ) 'Error reading line', V, 'from "CONFIG":  IOSTAT=', ISTAT
                CALL M3MESG( MESG )
                CYCLE
            END IF

            N = INDEX1( WNAMES(V), NVARSW, VNAMEW )
            IF ( N .LE. 0 ) THEN
                EFLAG = .TRUE.
                MESG  = 'WRF variable "' // TRIM( WNAMES(V) ) // '" not found in "WRFFILE"'
                CALL M3MESG( MESG )
                CYCLE
            END IF

            NVARS = V
            VUNITS( V ) = UNITSW( N )
            VDESCS( V ) = VDESCW( N )
            VTYPES( V ) = VTYPEW( N )
            WRITE( MESG, '( 7A )' ) 'WRF Variable "', WNAMES(V), '" M3IO variable "', VNAMES(V), '" (', TRIM( VUNITS(V) ), ')'
            CALL M3MESG( MESG )
            I = MAX( 0, MIN( 7, VTYPES( V ) ) )
            WRITE( MESG, '( 5A  )' ) 'TYPE ', TYPES(I), '; DESC "', TRIM( VDESCS(V) ), '"'
            CALL M3MESG( MESG )
            
            IF ( VTYPES(V) .NE. M3DBLE  .AND.   &
                 VTYPES(V) .NE. M3REAL  .AND.   &
                 VTYPES(V) .NE. M3INT  ) THEN
                EFLAG = .TRUE.
                MESG  = 'WRF variable "' // TRIM( WNAMES(V) ) // '" has non-DBLE | REAL | INT type'
                CALL M3MESG( MESG )
            END IF

            IF ( V .EQ. 1 ) THEN
                NDIMS   = DIMCNT( N )           !  includes time-dimension
                STAGGER = VSTAGR( N )
                NCOLS   = VARDIM( 1,N )
                NROWS   = VARDIM( 2,N )
                NLAYS   = VARDIM( 3,N )
            ELSE
                IF ( STAGGER .NE. VSTAGR( N ) ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Bad STAGGER for variable "' // TRIM( WNAMES(V) ) // '"'
                    CALL M3MESG( MESG )
                END IF
                IF ( NDIMS .NE. DIMCNT( N ) ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Bad NDIMS for variable "' // TRIM( WNAMES(V) ) // '"'
                    CALL M3MESG( MESG )
                END IF
                IF ( NCOLS .NE. VARDIM( 1,N ) ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Bad NCOLS for variable "' // TRIM( WNAMES(V) ) // '"'
                    CALL M3MESG( MESG )
                END IF
                IF ( NROWS .NE. VARDIM( 2,N ) ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Bad NROWS for variable "' // TRIM( WNAMES(V) ) // '"'
                    CALL M3MESG( MESG )
                END IF
                IF ( NLAYS .NE. VARDIM( 3,N ) ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'Bad NLAYS for variable "' // TRIM( WNAMES(V) ) // '"'
                    CALL M3MESG( MESG )
                END IF
            END IF
            CALL M3MESG( ' ' )

        END DO

99      CONTINUE
        CALL M3MESG( BAR )

    END IF


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Setup error(s)', 2 )
    END IF


    !!...............  Allocate buffers:

    NDIMS = NDIMS-1             !  spatial dimension (split out time)

    IF ( NDIMS .EQ. 2 ) THEN            !!  2D + time
        NLAYS = 1
        ALLOCATE( JBUF1( NCOLS1,NROWS1 ),       &
                  EBUF1( NCOLS1,NROWS1 ),       &
                  SBUF1( NCOLS1,NROWS1 ),       &
                  JBUF2( NCOLS ,NROWS  ),       &
                  EBUF2( NCOLS ,NROWS  ),       &
                  SBUF2( NCOLS ,NROWS  ),       STAT = ISTAT )
    ELSE IF ( NDIMS .EQ. 3 ) THEN       !!  3D + time
        ALLOCATE( KBUF1( NCOLS1,NROWS1,NLAYS ),       &
                  DBUF1( NCOLS1,NROWS1,NLAYS ),       &
                  TBUF1( NCOLS1,NROWS1,NLAYS ),       &
                  KBUF2( NCOLS ,NROWS ,NLAYS ),       &
                  DBUF2( NCOLS ,NROWS ,NLAYS ),       &
                  TBUF2( NCOLS ,NROWS ,NLAYS ),       STAT = ISTAT )
    ELSE
         CALL M3EXIT( PNAME, 0, 0, 'Bad spatial NDIMS (not 2,3)', 2 )
    END IF

    IF ( ISTAT .NE. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Allocation error', 2 )
    END IF


    !!...............  Get timestep sequence

    SDATE = GETNUM( JDATE, 9999999, JDATE,  'Enter starting date' )
    STIME = GETNUM( 0,     9999999, JTIME,  'Enter starting time' )
    TSTEP = GETNUM( 0,     9999999, JSTEP,  'Enter     time step' )
    IF ( NTIMEW .GT. 0 ) THEN
        NRECS = GETNUM( 1,     9999999, NTIMEW, 'Enter  # of records' )
    ELSE
        NRECS = 1
    END IF


    !!...............  Open output file

    FTYPE3D = GRDDED3
    GDTYP3D = GDTYP1
    SDATE3D = SDATE
    STIME3D = STIME
    TSTEP3D = TSTEP
    NCOLS3D = NCOLS1
    NROWS3D = NROWS1
    NLAYS3D = NLAYS
    NTHIK3D = NTHIK1
    P_ALP3D = P_ALP1
    P_BET3D = P_BET1
    P_GAM3D = P_GAM1
    XCENT3D = XCENT1
    YCENT3D = YCENT1
    XORIG3D = XORIG1
    YORIG3D = YORIG1
    XCELL3D = XCELL1
    YCELL3D = YCELL1
    GDNAM3D = OUTGRID

    IF ( NLAYS .EQ. 1 .OR. .NOT.ZSTAGR ) THEN
        VGTOP3D = 0.0
        VGTYP3D = IMISS3
        VGLVS3D( 1 ) =  0.0
        VGLVS3D( 2 ) =  1.0
    ELSE IF ( NLAYS .EQ. NSOILW ) THEN
        VGTOP3D = 0.0
        VGTYP3D = VGHVAL3
        VGLVS3D( 1:NSOILW ) =  ZSOILW( 1:NSOILW )
    ELSE IF ( NLAYS .EQ. NLAYSW .OR. NLAYS .EQ. NLAYSW+1 ) THEN
        VGTOP3D = VGTOPW
        VGTYP3D = VGWRFEM
        VGLVS3D( 1:NLAYSW+1 ) =  VGLVSW( 1:NLAYSW+1 )
    ELSE
        CALL M3EXIT( PNAME, 0, 0, 'Unsupported layer structure', 2 )
    END IF

    NVARS3D = NVARS
    VNAME3D( 1:NVARS ) = VNAMES( 1:NVARS )
    UNITS3D( 1:NVARS ) = VUNITS( 1:NVARS )
    VTYPE3D( 1:NVARS ) = VTYPES( 1:NVARS )
    VDESC3D( 1:NVARS ) = VDESCS( 1:NVARS )

    IF ( .NOT.OPEN3( 'OUTFILE', FSUNKN3, PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Failure opening "OUTFILE"', 2 )
    END IF


    !!...............  Process time step sequence

    CALL M3MESG( BAR )
    JDATE = SDATE
    JTIME = STIME
    
    CALL NEXTIME( JDATE, JTIME, -TSTEP )

    DO N = 1, NRECS
    
        CALL NEXTIME( JDATE, JTIME, TSTEP )
        WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'Processing ', JDATE, ':', JTIME
        CALL M3MESG( MESG )

        IF ( NDIMS .EQ. 2 ) THEN

            DO V = 1, NVARS

                IF ( VTYPES( V ) .EQ. M3INT ) THEN

                    IF ( .NOT.READWRF( WNAMES(V), JDATE, JTIME, NCOLS, NROWS, JBUF2 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF              !!  if read-failure

                    DO R = 1, NROWS1
                    DO C = 1, NCOLS1
                        JBUF1( C,R ) = JBUF2( C+XOFFS1,R+YOFFS1 )
                    END DO
                    END DO

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES(V), JDATE, JTIME, JBUF1 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                ELSE IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                    IF ( .NOT.READWRF( WNAMES(V), JDATE, JTIME, NCOLS, NROWS, SBUF2 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF              !!  if read-failure

                    DO R = 1, NROWS1
                    DO C = 1, NCOLS1
                        SBUF1( C,R ) = SBUF2( C+XOFFS1,R+YOFFS1 )
                    END DO
                    END DO

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES(V), JDATE, JTIME, SBUF1 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                    IF ( .NOT.READWRF( WNAMES(V), JDATE, JTIME, NCOLS, NROWS, EBUF2 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF              !!  if read-failure

                    DO R = 1, NROWS1
                    DO C = 1, NCOLS1
                        EBUF1( C,R ) = EBUF2( C+XOFFS1,R+YOFFS1 )
                    END DO
                    END DO

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES(V), JDATE, JTIME, EBUF1 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                END IF          !  if m3int, or m3real

            END DO              !  end loop on variables V

        ELSE IF ( NDIMS .EQ. 3 ) THEN

            DO V = 1, NVARS

                IF ( VTYPES( V ) .EQ. M3INT ) THEN

                    IF ( .NOT.READWRF( WNAMES(V), JDATE, JTIME, NCOLS, NROWS, NLAYS, KBUF2 ) ) THEN
                        EFLAG = .TRUE.
                         CYCLE
                    END IF              !!  if read-failure

                    DO L = 1, NLAYS
                    DO R = 1, NROWS1
                    DO C = 1, NCOLS1
                        KBUF1( C,R,L ) = KBUF2( C+XOFFS1,R+YOFFS1,L )
                    END DO
                    END DO
                    END DO

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES(V), JDATE, JTIME, KBUF1 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                ELSE IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                    IF ( .NOT.READWRF( WNAMES(V), JDATE, JTIME, NCOLS, NROWS, NLAYS, TBUF2 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF              !!  if read-failure

                    DO L = 1, NLAYS
                    DO R = 1, NROWS1
                    DO C = 1, NCOLS1
                        TBUF1( C,R,L ) = TBUF2( C+XOFFS1,R+YOFFS1,L )
                    END DO
                    END DO
                    END DO

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES(V), JDATE, JTIME, TBUF1 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                    IF ( .NOT.READWRF( WNAMES(V), JDATE, JTIME, NCOLS, NROWS, NLAYS, DBUF2 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF              !!  if read-failure

                    DO L = 1, NLAYS
                    DO R = 1, NROWS1
                    DO C = 1, NCOLS1
                        DBUF1( C,R,L ) = DBUF2( C+XOFFS1,R+YOFFS1,L )
                    END DO
                    END DO
                    END DO

                    IF ( .NOT.WRITE3( 'OUTFILE', VNAMES(V), JDATE, JTIME, DBUF1 ) ) THEN
                        EFLAG = .TRUE.
                        CYCLE
                    END IF

                END IF          !  if m3int, or m3real

            END DO              !  end loop on variables V

        END IF                  !  if ndims = 2, or = 3
    
    END DO


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


END PROGRAM WRFTOM3

    
