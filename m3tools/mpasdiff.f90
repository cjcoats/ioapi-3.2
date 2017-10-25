
PROGRAM MPASDIFF

    !!***********************************************************************
    !!  Copyright (c) 2017 Carlie J. Coats, Jr.
    !!  Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!  See file "GPL.txt" for conditions of use.
    !!.......................................................................
    !!
    !!  DESCRIPTION:
    !!      Compute statistics for a specified variable from an MPAS file,
    !!      optionally for a specified time period and/or layer-range.
    !!
    !!      Difference-ops borrowed from I/O API "gridops.f"
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  10/13/2017 by Carlie J. Coats, Jr., UNC IE
    !!.......................................................................

    USE MODMPASFIO
    USE MODNCFIO
    USE M3UTILIO

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    INTEGER, PARAMETER :: OPCOUNT = 19         !  dimension for OP(*); number of ops

    CHARACTER*72, PARAMETER :: DIFMNU ( OPCOUNT ) = (/                      &
       '(pointwise) difference                            A - B        ',   &       !  1
       '(pointwise) difference                            B - A        ',   &       !  2
       '(pointwise) ratio                                 A / B        ',   &       !  3
       '(pointwise) ratio                                 B / A        ',   &       !  4
       '(pointwise) absolute value of difference         |A - B|       ',   &       !  5
       'difference normalized by first grid              (A - B)/A     ',   &       !  6
       'difference normalized by second grid             (B - A)/B     ',   &       !  7
       'difference normalized by second grid             (A - B)/B     ',   &       !  8
       'absolute value of difference normalized by A     |A - B|/A     ',   &       !  9
       'absolute value of difference normalized by B     |A - B|/B     ',   &       ! 10
       'difference normalized by pointwise mean         2(A-B)/(A + B) ',   &       ! 11
       'difference normalized by pointwise mean         2(B-A)/(A + B) ',   &       ! 12
       'difference normalized by joint root mean square (A-B)/RMS(A  B)',   &       ! 13
       'difference normalized by joint root mean square (B-A)/RMS(A  B)',   &       ! 14
       '(pointwise) sum                                   A + B        ',   &       ! 15
       '(pointwise) maximum                               MAX( A,B )   ',   &       ! 16
       '(pointwise) minimum                               min( A,B )   ',   &       ! 17
       'value from grid A                                 A            ',   &       ! 18
       'value from grid B                                 B            '    /)      ! 19

    CHARACTER*16, PARAMETER :: OP ( OPCOUNT ) = (/  &
                 '(A - B)         ',   &        !  1
                 '(B - A)         ',   &        !  2
                 'A / B           ',   &        !  3
                 'B / A           ',   &        !  4
                 '|(A - B)|       ',   &        !  5
                 '(A - B)/A       ',   &        !  6
                 '(B - A)/B       ',   &        !  7
                 '(A - B)/B       ',   &        !  8
                 '|(A - B)/A|     ',   &        !  9
                 '|(B - A)/B|     ',   &        ! 10
                 '(A-B)/((A+B)/2) ',   &        ! 11
                 '(B-A)/((A+B)/2) ',   &        ! 12
                 '(A - B)/RMS     ',   &        ! 13
                 '(B - A)/RMS     ',   &        ! 14
                 '(A + B)         ',   &        ! 15
                 'MAX(A, B)       ',   &        ! 16
                 'min(a, B)       ',   &        ! 17
                 'Grid A          ',   &        ! 18
                 'Grid B          '   /)        ! 19

    CHARACTER*1,  PARAMETER :: BLANK = ' '
    CHARACTER*80, PARAMETER :: BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
    CHARACTER*16, PARAMETER :: PNAME = 'MPASDIFF'

    !!...........   LOCAL VARIABLES and their descriptions:

    LOGICAL         EFLAG

    CHARACTER*256   MESG, RNAME

    INTEGER         ISTAT, LDEV, RDEV

    INTEGER         K, K1, K2, L, M, N, P, Q, V, T, T1, T2

    INTEGER         SDATEA, STIMEA, EDATEA, ETIMEA, NRECSA
    INTEGER         SDATEB, STIMEB, EDATEB, ETIMEB, NRECSB
    INTEGER         JDATEA, JTIMEA, JDATEB, JTIMEB

    INTEGER         NCELLS
    INTEGER         LAY0A, LAY1A, NLAYSA, LAY0B, LAY1B, NLAYSB, NLAYSD
    INTEGER         REC0A, REC1A, REC0B, REC1B, NRECS

    CHARACTER*512   EQNAME

    CHARACTER*32    FNAMEA
    INTEGER         FVARSA
    CHARACTER*32    VNAMESA( MXVARS3 )         !!  data structures for DESCNCVAR()
    INTEGER         VTYPESA( MXVARS3 )
    INTEGER         VNDIMSA( MXVARS3 )
    INTEGER          VDIMSA( 7,MXVARS3 )
    CHARACTER*32     DNAMEA( 7,MXVARS3 )
    CHARACTER*32     TNAMEA
    CHARACTER*32     VNAMEA
    LOGICAL          TFLAGA

    INTEGER              :: FSTEPA
    INTEGER, ALLOCATABLE :: FDATEA( : )
    INTEGER, ALLOCATABLE :: FTIMEA( : )

    CHARACTER*32    FNAMEB
    INTEGER         FVARSB
    CHARACTER*32    VNAMESB( MXVARS3 )         !!  data structures for DESCNCVAR()
    INTEGER         VTYPESB( MXVARS3 )
    INTEGER         VNDIMSB( MXVARS3 )
    INTEGER          VDIMSB( 7,MXVARS3 )
    CHARACTER*32     DNAMEB( 7,MXVARS3 )
    CHARACTER*32     VNAMEB
    LOGICAL          TFLAGB

    INTEGER              :: FSTEPB
    INTEGER, ALLOCATABLE :: FDATEB( : )
    INTEGER, ALLOCATABLE :: FTIMEB( : )

    REAL   , ALLOCATABLE :: AGRID1D( : ),   BGRID1D( : ),   DGRID1D( : )
    REAL   , ALLOCATABLE :: AGRID2D( :,: ), BGRID2D( :,: ), DGRID2D( :,: )

    REAL*8      FSUMA, FSSQA
    REAL        FMAXA, FMINA
    INTEGER     KMAXA, KMINA, LMAXA, LMINA, TMAXA, TMINA

    REAL*8      FSUMB, FSSQB
    REAL        FMAXB, FMINB
    INTEGER     KMAXB, KMINB, LMAXB, LMINB, TMAXB, TMINB

    REAL*8      FSUMD, FSSQD
    REAL        FMAXD, FMIND
    INTEGER     KMAXD, KMIND, LMAXD, LMIND, TMAXD, TMIND, FCNTD

    INTEGER         DIFMODE
    CHARACTER*16    OPNAME      !  difference-operation name

    !!***********************************************************************
    !!.......   First:  Initialize the I/O API:

    LDEV = INIT3()	!  initialization returns unit # for log
    EFLAG = .FALSE.

    WRITE( *, '( 5X, A )' ) BAR,                                                &
'Program "MPASDIFF" to compute differences for the specified variables,',       &
'for the for the specified layer range (if appropriate), and for the',          &
'specified time period (if appropriate), and write the statistical result',     &
'to the program-log or to a specified ASCII REPORT file, and/or the',           &
'MPAS-gridded differences to a specified MPAS-netCDF file.',                    &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'    setenv  INFILEA   <path name for first input MPAS-gridded file>',          &
'    setenv  INFILEB   <path name for second input MPAS-gridded file>',         &
'                      or "SAME"',                                              &
'    setenv  REPORT    <path name for output ASCII REPORT file>',               &
'                      or "LOG"',                                               &
'',                                                                             &
'    The requested variables must be of type REAL.',                            &
'',                                                                             &
'    The requested variables must be subscripted by CELL, VERTEX, or',          &
'    EDGE (N), and optionally by TIME (T) and/or LAYER (L), in (Fortran)',      &
'    subscript order',                                                          &
'',                                                                             &
'        V( [L,] N [, T] )',                                                    &
'',                                                                             &
'    The requested dimension-ranges for the variables must be consistent.',     &
'',                                                                             &
'    The requested time-range (if any) must be available in both files.',       &
'',                                                                             &
'THE PROGRAM WILL PROMPT YOU for the variable-names, and (as appropriate)',     &
'for the starting and ending date&time for the report period, and for the',     &
'layer ranges.',                                                                &
'',                                                                             &
'Program version: ',                                                            &
'$Id:: mpasdiff.f90 38 2017-10-25 19:49:48Z coats                       $',     &
'',                                                                             &
'Program copyright (C) 2017 Carlie J. Coats, Jr.',                              &
'Released under Version 2 of the GNU General Public License.',                  &
'See enclosed GPL.txt, or URL',                                                 &
''  ,                                                                           &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',                   &
'', BAR, ''

    FNAMEA = 'INFILEA'
    IF ( .NOT.INITMPGRID( FNAMEA ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILEA"', 2 )
    ELSE IF ( .NOT.DESCMPAS( FNAMEA,  NRECSA, FVARSA, VNAMESA,               &
                             VTYPESA, VNDIMSA, VDIMSA, DNAMEA ) ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILE1"', 2 )
    END IF

    ALLOCATE( FDATEA( NRECSA ), FTIMEA( NRECSA ), STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'DATE&TIME buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    CALL M3MESG( 'The list of variables in the first file is:' )
    DO  L = 1, FVARSA
        WRITE( *, '( I3, ": ", A )' ) L, VNAMESA( L )
    END DO
    WRITE( *, '(A)' ) ''
    V     = GETNUM( 1, FVARSA, 1, 'Enter number for the first variable to analyze' )

    VNAMEA = VNAMESA( V )

    IF ( VTYPESA( V ) .NE. M3REAL  ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Variable not of type REAL', 2 )
    END IF

    N = INDEX1( 'Time', VNDIMSA( V ), DNAMEA( :,V ) )
    T = MAX( INDEX1( 'xtime', FVARSA, VNAMESA ) ,  &
             INDEX1( 'xTime', FVARSA, VNAMESA ) )

    IF ( N .LE. 0 ) THEN

        Call M3MESG( 'Variable "' // TRIM( VNAMEA ) // '" is time independent' )
        TFLAGB = .FALSE.
        SDATEA = IMISS3
        STIMEA = IMISS3
        EDATEA = IMISS3
        ETIMEA = IMISS3
        NRECS  = 1

    ELSE IF ( T .LE. 0 ) THEN

        CALL M3EXIT( PNAME, 0, 0, 'Time-variable not found', 2 )

    ELSE IF ( .NOT.READMPSTEPS( 'INFILE1', VNAMESA( T ), N, NRECSA, FDATEA, FTIMEA ) )  THEN

        CALL M3EXIT( PNAME, 0, 0, 'Error reading time-variable "'//VNAMESA( T )//'"', 2 )

    ELSE

        WRITE( MESG , '( 3A, I9.7, A, I6.6, A, I7.7, A, I6.6 )' )       &
            'Variable "', TRIM( VNAMEA ), '" has date&time range',      &
            FDATEA(1), ':', FTIMEA(1), ' -', FDATEA(NRECSA), ':', FTIMEA(NRECSA)
        Call M3MESG( MESG )

        SDATEA = GETNUM( FDATEA(1),    FDATEA(NRECSA), FDATEA(1),      'Enter starting date (YYYYDDD)' )
        STIMEA = GETNUM( 0, 999999999, FTIMEA(1),                      'Enter starting time  (HHMMSS)' )
        EDATEA = GETNUM( SDATEA,       FDATEA(NRECSA), FDATEA(NRECSA), 'Enter   ending date (YYYYDDD)' )
        ETIMEA = GETNUM( 0, 999999999, FTIMEA(NRECSA),                 'Enter   ending time  (HHMMSS)' )

        REC0A = FINDKEY( SDATEA, STIMEA, NRECSA, FDATEA, FTIMEA )
        REC1A = FINDKEY( EDATEA, ETIMEA, NRECSA, FDATEA, FTIMEA )
        NRECS = REC1A - REC0A + 1

        IF ( REC0A .LT. 1 .OR. REC1A .LT. 1 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Requested dates&times not found', 2 )
        END IF
        TFLAGA = .TRUE.

    END IF

    K1 = MAX( INDEX1( 'nCells',    VNDIMSA( V ), DNAMEA( :,V ) ),  &
              INDEX1( 'nEdges',    VNDIMSA( V ), DNAMEA( :,V ) ),  &
              INDEX1( 'nVertices', VNDIMSA( V ), DNAMEA( :,V ) ) )
    IF ( K1 .LE. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Variable 1 not subscripted by CELL, EDGE, or VERTEX', 2 )
    ELSE IF ( K1 .EQ. 1 .AND. ( N .EQ. 0 .OR. N .EQ. 2 ) ) THEN
        NCELLS = VDIMSA( K,V )
        NLAYSA= 1
        LAY0A = 1
        LAY1A = 1
    ELSE IF ( K1 .EQ. 2 .AND. ( N .EQ. 0 .OR. N .EQ. 3 ) ) THEN
        NCELLS = VDIMSA( 2,V )
        NLAYSA = VDIMSA( 1,V )
        LAY0A  = GETNUM(     1, NLAYSA,      1, 'Enter start of layer range for first-variable analysis' )
        LAY1A  = GETNUM( LAY0A, NLAYSA, NLAYSA, 'Enter  end  of layer range for first-variable analysis' )
    ELSE
        CALL M3EXIT( PNAME, 0, 0, 'Bad subscripting for variable 1:  not ([LVL,]CELL[,TIME])', 2 )
    END IF


    !!...............   Process second file:


    FNAMEB = 'INFILEB'
    CALL ENVSTR( 'INFILEB', 'Path-name for second input file, or "SAME"', 'SAME', EQNAME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN

        CALL M3EXIT( PNAME, 0, 0, 'Bad environment variable "INFILEB"', 2 )

    ELSE IF ( EQNAME .EQ. 'SAME'  ) THEN

        FNAMEB  = FNAMEA
        NRECSB  = NRECSA
        FVARSB  = FVARSA
        VNAMESB = VNAMESA
        VTYPESB = VTYPESA
        VNDIMSB = VNDIMSA
        VDIMSB  = VDIMSA
        DNAMEB  = DNAMEA
        SDATEB  = SDATEA
        STIMEB  = STIMEA
        EDATEB  = EDATEA
        ETIMEB  = ETIMEA
        REC0B   = REC0A
        REC1B   = REC1A

    ELSE IF ( .NOT. OPENMPAS( FNAMEB, FSREAD3 ) ) THEN

        CALL M3EXIT( PNAME, 0,0, 'Could not open "INFILEB"', 2 )

    ELSE IF ( .NOT. DESCMPAS( FNAMEB, NRECSB, FVARSB, VNAMESB,               &
                             VTYPESB, VNDIMSB, VDIMSB, DNAMEB ) ) THEN

        CALL M3EXIT( PNAME, 0,0, 'Could not read header for "INFILE2"', 2 )
    
    END IF

    ALLOCATE( FDATEB( NRECSB ), FTIMEB( NRECSB ), STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'DATE&TIME buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    CALL M3MESG( 'The list of variables in the second file is:' )
    DO  L = 1, FVARSB
        WRITE( *, '( I3, ": ", A )' ) L, VNAMESB( L )
    END DO
    WRITE( *, '(A)' ) ''

    V     = GETNUM( 1, FVARSB, 1, 'Enter number for the second variable to analyze' )
    VNAMEB = VNAMESB( V )

    IF ( VTYPESB( V ) .NE. M3REAL  ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Variable not of type REAL', 2 )
    END IF

    N = INDEX1( 'Time', VNDIMSB( V ), DNAMEB( :,V ) )
    T = MAX( INDEX1( 'xtime', FVARSB, VNAMESB ) ,  &
             INDEX1( 'xTime', FVARSB, VNAMESB ) )
    K2 = MAX( INDEX1( 'nCells',    VNDIMSB( V ), DNAMEB( :,V ) ),  &
              INDEX1( 'nEdges',    VNDIMSB( V ), DNAMEB( :,V ) ),  &
              INDEX1( 'nVertices', VNDIMSB( V ), DNAMEB( :,V ) ) )

    IF ( K2 .LE. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Variable 2 not subscripted by CELL, EDGE, or VERTEX', 2 )
    ELSE IF ( NCELLS .NE. VDIMSB( K2,V ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Inconsistent cell-dimensioning for second variable', 2 )
    ELSE IF ( K2 .EQ. 1 .AND. ( N .EQ. 0 .OR. N .EQ. 2 ) ) THEN
        NLAYSB = 1
        LAY0B  = 1
        LAY1B  = 1
    ELSE IF ( K2 .EQ. 2 .AND. ( N .EQ. 0 .OR. N .EQ. 3 ) ) THEN
        NCELLS = VDIMSB( 2,V )
        NLAYSB = VDIMSB( 1,V )
        LAY0B  = GETNUM(     1, NLAYSB,      1, 'Enter start of layer range for second-variable analysis' )
        LAY1B  = GETNUM( LAY0B, NLAYSB, NLAYSB, 'Enter  end  of layer range for second-variable analysis' )
    ELSE
        CALL M3EXIT( PNAME, 0, 0, 'Bad subscripting for variable:  not ([LVL,]CELL[,TIME])', 2 )
    END IF

    IF ( LAY1A - LAY0A .NE. LAY1B - LAY0B ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Inconsistent vertical dimensioning for analysis', 2 )
    ELSE
        NLAYSD = LAY1A - LAY0A + 1
    END IF

    IF ( N .LE. 0 .AND. NRECS .GT. 1 ) THEN

        CALL M3EXIT( PNAME, 0, 0, 'Inconsistent time dimensioning for analysis', 2 )

    ELSE IF ( N .LE. 0 ) THEN

        CALL M3MESG( 'Variable "' // TRIM( VNAMEB ) // '" is time independent' )
        TFLAGB = .FALSE.

    ELSE IF ( .NOT.READMPSTEPS( FNAMEB, VNAMESB( T ), N, NRECSB, FDATEB, FTIMEB ) )  THEN

        CALL M3EXIT( PNAME, 0, 0, 'Error reading time-variable "'//VNAMESA( T )//'"', 2 )

    ELSE

        WRITE( MESG , '( 3A, I9.7, A, I6.6, A, I7.7, A, I6.6 )' )       &
            'Variable "', TRIM( VNAMEB ), '" has date&time range',      &
            FDATEB(1), ':', FTIMEB(1), ' -', FDATEB(NRECSB), ':', FTIMEB(NRECSB)
        Call M3MESG( MESG )

        SDATEB = GETNUM( FDATEB(1),   FDATEB(NRECSB), FDATEB(1),      'Enter starting date (YYYYDDD)' )
        STIMEB = GETNUM( 0, 999999999, FTIMEB(1),                        'Enter starting time  (HHMMSS)' )
        EDATEB = GETNUM( SDATEB,       FDATEB(NRECSB), FDATEB(NRECSB),'Enter   ending date (YYYYDDD)' )
        ETIMEB = GETNUM( 0, 999999999, FTIMEB(NRECSB),                  'Enter   ending time  (HHMMSS)' )

        REC0B = FINDKEY( SDATEB, STIMEB, NRECSB, FDATEB, FTIMEB )
        REC1B = FINDKEY( EDATEA, ETIMEB, NRECSB, FDATEB, FTIMEB )

        IF ( REC0B .LT. 1 .OR. REC1B .LT. 1 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Requested dates&times not found', 2 )
        ELSE IF ( NRECS .NE. REC1B - REC0B + 1 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Inconsistent dates&times for analysis', 2 )
        END IF

        TFLAGB = .TRUE.

    END IF


    !!...............   Allocate arrays:

    IF ( NLAYSA .GT. 1 ) THEN
        ALLOCATE( AGRID2D( NLAYSA, NCELLS ), STAT = ISTAT )
    ELSE
        ALLOCATE( AGRID1D( NCELLS ), STAT = ISTAT )
    END IF
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Buffer A allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    IF ( NLAYSB .GT. 1 ) THEN
        ALLOCATE( BGRID2D( NLAYSB, NCELLS ), STAT = ISTAT )
    ELSE
        ALLOCATE( BGRID1D( NCELLS ), STAT = ISTAT )
    END IF
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Buffer B allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    IF ( NLAYSD .GT. 1 ) THEN
        ALLOCATE( DGRID2D( NLAYSB, NCELLS ), STAT = ISTAT )
    ELSE
        ALLOCATE( BGRID1D( NCELLS ), STAT = ISTAT )
    END IF
    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Buffer B allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............   Open REPORT file:

    CALL ENVSTR( 'REPORT', 'Path-name for ASCII REPORT file, or "LOG"', 'LOG', RNAME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Bad environment variable "REPORT"', 2 )
    ELSE IF ( RNAME .EQ. 'LOG' ) THEN
        RDEV = LDEV
    ELSE
        RDEV = GETEFILE( 'REPORT', .FALSE., .TRUE., PNAME )
        IF ( RDEV .LT. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Failure opening REPORT file', 2 )
        END IF
    END IF


    !!...............   Process stats:  time independent or time dependent:

    CALL PICKOPS( OPNAME )

    FMAXA =  BADVAL3     !!  is very negative
    FMINA = -BADVAL3     !!  is huge
    FSUMA = 0.0D0
    FSSQA = 0.0D0
    FMAXB =  BADVAL3
    FMINB = -BADVAL3
    FSUMB = 0.0D0
    FSSQB = 0.0D0
    FMAXD =  BADVAL3
    FMIND = -BADVAL3
    FSUMD = 0.0D0
    FSSQD = 0.0D0

    JDATEA = 0
    JTIMEA = 0
    IF ( TFLAGA ) THEN
        CONTINUE
    ELSE IF ( NLAYSA .EQ. 1 ) THEN
        IF ( .NOT.READMPAS( FNAMEA, VNAMEA, NCELLS, AGRID1D ) ) THEN
            EFLAG = .TRUE.
        END IF
    ELSE
        JDATEA = 0
        JTIMEA = 0
        IF ( .NOT.READMPAS( FNAMEA, VNAMEA, NLAYSA, NCELLS, AGRID2D ) ) THEN
            EFLAG = .TRUE.
        END IF
    END IF

    JDATEB = 0
    JTIMEB = 0
    IF ( TFLAGB ) THEN
        CONTINUE
    ELSE IF ( NLAYSB .EQ. 1 ) THEN
        IF ( .NOT.READMPAS( FNAMEB, VNAMEB, NCELLS, BGRID1D ) ) THEN
            EFLAG = .TRUE.
        END IF
    ELSE
        IF ( .NOT.READMPAS( FNAMEB, VNAMEB, NLAYSB, NCELLS, BGRID2D ) ) THEN
            EFLAG = .TRUE.
        END IF
    END IF
    
    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Error(s) reading time independent data', 2 )
    END IF


    DO N = 1, NRECS

        IF ( TFLAGA ) THEN
            M = N + REC0A - 1
            JDATEA = FDATEA( N )
            JTIMEA = FTIMEA( N )
            IF ( NLAYSA .EQ. 1 ) THEN
                IF ( .NOT.READMPAS( FNAMEA, M, VNAMEA, NCELLS, AGRID1D ) ) THEN
                    EFLAG = .TRUE.
                END IF
            ELSE
                IF ( .NOT.READMPAS( FNAMEA, M, VNAMEA, NLAYSA, NCELLS, AGRID2D ) ) THEN
                    EFLAG = .TRUE.
                END IF
            END IF
        END IF


        IF ( TFLAGB ) THEN
            M = N + REC0B - 1
            JDATEB = FDATEB( N )
            JTIMEB = FTIMEB( N )
            IF ( NLAYSB .EQ. 1 ) THEN
                IF ( .NOT.READMPAS( FNAMEB, M, VNAMEB, NCELLS, BGRID1D ) ) THEN
                    EFLAG = .TRUE.
                END IF
            ELSE
                IF ( .NOT.READMPAS( FNAMEB, M, VNAMEB, NLAYSB, NCELLS, BGRID2D ) ) THEN
                    EFLAG = .TRUE.
                END IF
            END IF
        END IF
        
        IF      ( NLAYSA .EQ. 1 .AND. NLAYSB .EQ. 1 ) THEN
            CALL STAT11()
        ELSE IF ( NLAYSA .EQ. 1 ) THEN
            CALL STAT12()
        ELSE IF ( NLAYSB .EQ. 1 ) THEN
            CALL STAT21()
        ELSE
            CALL STAT22()
        END IF

    END DO

    IF ( NLAYSD .EQ. 1 ) THEN
        CALL STAT1DF()
    ELSE
        CALL STAT22F()
    END IF


    !!.......   Clean up and exit

    ! CALL SHUTMPGRID()

    IF ( EFLAG ) THEN
        MESG  = 'Failure(s) in program'
        ISTAT = 2
    ELSE
        MESG  = 'Successful completion of program'
        ISTAT = 0
    END IF
    CALL M3EXIT( PNAME, 0, 0, MESG, 0 )


CONTAINS


    SUBROUTINE STAT22( )

        !!--------------------------------------------------------------
        !!    Both variables A and B are 2-D (layered)
        !!--------------------------------------------------------------
       
        REAL*8      VASUM, VASSQ, VABAR, VASIG, DIV
        REAL        VAMAX, VAMIN
        INTEGER     IAMIN, IAMAX, LAMIN, LAMAX
        REAL*8      VBSUM, VBSSQ, VBBAR, VBSIG
        REAL        VBMAX, VBMIN
        INTEGER     IBMIN, IBMAX, LBMIN, LBMAX
        REAL*8      VDSUM, VDSSQ, VDBAR, VDSIG
        REAL        VDMAX, VDMIN
        INTEGER     IDMIN, IDMAX, LDMIN, LDMAX

        INTEGER     I, L, LA, LB, MM
        REAL        TA, TB, TD, TT
        REAL*8      RMS

        MM = 0      !!  "missing" count


        VASUM = 0.0D0
        VASSQ = 0.0D0
        VAMAX = AGRID2D( LAY0A,1 )
        VAMIN = AGRID2D( LAY0A,1 )
        IAMAX = 1
        IAMIN = 1
        LAMAX = LAY0A
        LAMIN = LAY0A
        VBSUM = 0.0D0
        VBSSQ = 0.0D0
        VBMAX = BGRID2D( LAY0B,1 )
        VBMIN = BGRID2D( LAY0B,1 )
        IBMAX = 1
        IBMIN = 1
        LBMAX = LAY0B
        LBMIN = LAY0B
        VDSUM = 0.0D0
        VDSSQ = 0.0D0
        VDMAX =-BADVAL3
        VDMIN = BADVAL3
        IDMAX = IMISS3
        IDMIN = IMISS3
        LDMAX = IMISS3
        LDMIN = IMISS3

        IF      ( DIFMODE .EQ. 1 )  THEN        !  diff


            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                TD = TA - TB
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                    LDMAX = L
                END IF
                IF ( TB .LT. VBMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                    LDMIN = L
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 2 )  THEN        !  diff B-A

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                TD = TB - TA
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                    LDMAX = L
                END IF
                IF ( TB .LT. VBMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                    LDMIN = L
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 3 )  THEN        !  ratio A/B

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = TA / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 4 )  THEN        !  ratio B/A

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = TB / TA
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TB .LT. VBMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 5 )  THEN        !  diff | A-B |

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                TD = ABS( TA - TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                    LDMAX = L
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                    LDMIN = L
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 6 )  THEN        !  diffn A-B/A

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = ( TA - TB ) / TA
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 7 )  THEN        !  diffn B-A/B

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ( TB - TA ) / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 8 )  THEN        !  diffn A-B/B

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ( TA - TB ) / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 9 )  THEN        !  diffna |A - B|/A

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = ABS( ( TA - TB ) / TA )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 10 )  THEN        !  diffna |A - B|/B

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ABS( ( TA - TB ) / TB )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 11 )  THEN        !  diffnm 2(A - B)/(A + B)

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                TT = TA + TB
                IF ( TT .NE. 0.0 ) THEN
                    TD = 2.0*( TA - TB ) / TT
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 12 )  THEN        !  diffnm 2(B - A)/(A + B)

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                TT = TA + TB
                IF ( TT .NE. 0.0 ) THEN
                    TD = 2.0*( TB - TA ) / TT
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 13 )  THEN        !  diffnrms (a-b)/rms

            RMS = 0.0D0

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                RMS = TA**2 + TB**2

            END DO
            END DO
            
            TT = SQRT( RMS / DBLE( NCELLS*NLAYSD ) )
            
            IF ( TT .GT. 0.0 ) THEN
            
                TT = 1.0 / TT

                DO I = 1, NCELLS
                DO L = 1, NLAYSD

                    TA = AGRID2D( L+LAY0A-1,I )
                    TB = BGRID2D( L+LAY0B-1,I )
                    TD = TT * ( TA - TB )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF

                END DO
                END DO

            ELSE
                MM = NCELLS * NLAYSD
            END IF

        ELSE IF ( DIFMODE .EQ. 14 )  THEN        !   diffnrms (b-a)/rms

            RMS = 0.0D0

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                RMS = TA**2 + TB**2

            END DO
            END DO
            
            TT = SQRT( RMS / DBLE( NCELLS*NLAYSD ) )
            
            IF ( TT .GT. 0.0 ) THEN
            
                TT = 1.0 / TT

                DO I = 1, NCELLS
                DO L = 1, NLAYSD

                    TA = AGRID2D( L+LAY0A-1,I )
                    TB = BGRID2D( L+LAY0B-1,I )
                    TD = TT * ( TB - TA )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                        LDMAX = L
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                        LDMIN = L
                    END IF

                END DO
                END DO

            ELSE
                MM = NCELLS * NLAYSD
            END IF

        ELSE IF ( DIFMODE .EQ. 15 )  THEN        !  add

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                TD = TA + TB
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                    LDMAX = L
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                    LDMIN = L
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 16 ) THEN         !  max

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                TD = MAX( TA , TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                    LDMAX = L
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                    LDMIN = L
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 17 ) THEN         !  min

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

                TD = MIN( TA , TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                    LDMAX = L
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                    LDMIN = L
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 18 ) THEN         !  grid A

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

            END DO
            END DO
            
            VDSUM = VASUM
            VDSSQ = VASSQ
            VDMAX = VAMAX
            VDMIN = VAMIN
            IDMAX = IAMAX
            IDMIN = IAMIN
            LDMAX = LAMAX
            LDMIN = LAMIN

        ELSE IF ( DIFMODE .EQ. 19 ) THEN         !  grid B

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID2D( L+LAY0A-1,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                    LAMAX = L+LAY0A-1
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                    LAMIN = L+LAY0A-1
                END IF

                TB = BGRID2D( L+LAY0B-1,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                    LBMAX = L+LAY0B-1
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                    LBMIN = L+LAY0B-1
                END IF

            END DO
            END DO
            
            VDSUM = VBSUM
            VDSSQ = VBSSQ
            VDMAX = VBMAX
            VDMIN = VBMIN
            IDMAX = IBMAX
            IDMIN = IBMIN
            LDMAX = LBMAX
            LDMIN = LBMIN

        END IF

        IF ( N .EQ. 1 ) THEN

            FSUMA = VASUM
            FSSQA = VASSQ
            FMAXA = VAMAX
            KMAXA = IAMAX
            LMAXA = LAMAX
            TMAXA = N
            FMINA = VAMIN
            KMINA = IAMIN
            LMINA = LAMIN
            TMINA = N
            FSUMB = VBSUM
            FSSQB = VBSSQ
            FMAXB = VBMAX
            KMAXB = IBMAX
            LMAXB = LBMAX
            TMAXB = N
            FMINB = VBMIN
            KMINB = IBMIN
            LMINB = LBMIN
            TMINB = N
            FSUMD = VDSUM
            FSSQD = VDSSQ
            FMAXD = VDMAX
            KMAXD = IDMAX
            LMAXD = LDMAX
            TMAXD = N
            FMIND = VDMIN
            KMIND = IDMIN
            LMIND = LDMIN
            TMIND = N
            FCNTD = NCELLS * ( LAY1A - LAY0A + 1 ) - MM

            WRITE( RDEV, '( 3( /, 2( 2X, A ), I5, A, I5 ) )' )              &
                'Variable:', TRIM( VNAMEA ),  'layers', LAY0A, ':', LAY1A,  &
                'Variable:', TRIM( VNAMEB ),  'layers', LAY0B, ':', LAY1B,  &
                'Difference:', OP( DIFMODE),  'layers',     1, ':', NLAYSD
            IF ( TFLAGA .OR. TFLAGB ) THEN
                WRITE( RDEV, '( 3 A16, 2 A10, A16, 2 A10)' )                &
                    'Date&Time', 'MEAN', 'SIGMA', 'MAX @', 'CELL', 'LAYER', &
                    'MIN @', 'CELL', 'LAYER'
            ELSE
                WRITE( RDEV, '( 2 A16, 2 A10, A16, 2 A10)' )                &
                    'MEAN', 'SIGMA', 'MAX @', 'CELL', 'LAYER',              &
                    'MIN @', 'CELL', 'LAYER'
            END IF

        ELSE

            FSUMA = FSUMA + VASUM
            FSSQA = FSSQA + VASSQ
            FSUMB = FSUMB + VBSUM
            FSSQB = FSSQB + VBSSQ
            FSUMD = FSUMD + VDSUM
            FSSQD = FSSQD + VDSSQ
            FCNTD = FCNTD + NCELLS * ( LAY1A - LAY0A + 1 ) - MM
            IF ( VAMAX .GT. FMAXA ) THEN
                FMAXA = VAMAX
                LMAXA = LAMAX
                KMAXA = IAMAX
                TMAXA = N
            END IF
            IF ( VAMIN .LT. FMINA ) THEN
                FMINA = VAMIN
                LMINA = LAMIN
                KMINA = IAMIN
                TMINA = N
            END IF
            IF ( VBMAX .GT. FMAXB ) THEN
                FMAXB = VBMAX
                LMAXB = LBMAX
                KMAXB = IBMAX
                TMAXB = N
            END IF
            IF ( VBMIN .LT. FMINB ) THEN
                FMINB = VBMIN
                LMINB = LBMIN
                KMINB = IBMIN
                TMINB = N
            END IF
            IF ( VDMAX .GT. FMAXD ) THEN
                FMAXD = VDMAX
                LMAXD = LDMAX
                KMAXD = IDMAX
                TMAXD = N
            END IF
            IF ( VDMIN .LT. FMIND ) THEN
                FMIND = VDMIN
                LMIND = LDMIN
                KMIND = IDMIN
                TMIND = N
            END IF

        END IF      !!  if n==1, or not

        DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1A - LAY0A + 1 ) )
        VASUM = VASUM * DIV
        VASSQ = SQRT( MAX( VASSQ * DIV - VASUM**2, 0.0D0 ) )
        VBSUM = VBSUM * DIV
        VBSSQ = SQRT( MAX( VBSSQ * DIV - VBSUM**2, 0.0D0 ) )
        IF ( NCELLS * ( LAY1A - LAY0A + 1 ) - MM .GT. 0 ) THEN
           DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1A - LAY0A + 1 ) - MM )
           VDSUM = VDSUM * DIV
           VDSSQ = SQRT( MAX( VDSSQ * DIV - VDSUM**2, 0.0D0 ) )
        ELSE
           VDSUM = BADVAL3
           VDSSQ = BADVAL3
        END IF
        IF ( TFLAGA .AND. TFLAGB ) THEN
            WRITE( RDEV, '( "A: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, LAMAX, VAMIN, IAMIN, LAMIN
            WRITE( RDEV, '( "B: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, LBMAX, VBMIN, IBMIN, LBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, LAMAX, VDMIN, IAMIN, LAMIN
        ELSE IF ( TFLAGA ) THEN
            WRITE( RDEV, '( "A: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, LAMAX, VAMIN, IAMIN, LAMIN
            WRITE( RDEV, '( "B: ", 16X          , 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, LBMAX, VBMIN, IBMIN, LBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, LAMAX, VDMIN, IAMIN, LAMIN
        ELSE IF ( TFLAGB ) THEN
            WRITE( RDEV, '( "A: ", 16X          , 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, LAMAX, VAMIN, IAMIN, LAMIN
            WRITE( RDEV, '( "B: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, LBMAX, VBMIN, IBMIN, LBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, LAMAX, VDMIN, IAMIN, LAMIN
        ELSE
            WRITE( RDEV, '( "A: ", 3X, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )         &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, LAMAX, VAMIN, IAMIN, LAMIN
            WRITE( RDEV, '( "B: ", 3X, 3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )         &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, LBMAX, VBMIN, IBMIN, LBMIN
            WRITE( RDEV, '( "Diff: ",  3( 2X, 1PD14.6 ), 2 I10, 2X, 1PD14.6, 2 I10 )' )         &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, LAMAX, VDMIN, IAMIN, LAMIN
        END IF
        
        RETURN

    END SUBROUTINE STAT22


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-




    SUBROUTINE STAT21( )

        !!--------------------------------------------------------------
        !!    Variables A is 2-D (layered), B is 1-D
        !!--------------------------------------------------------------
       
        REAL*8      VASUM, VASSQ, VABAR, VASIG, DIV
        REAL        VAMAX, VAMIN
        INTEGER     IAMIN, IAMAX
        REAL*8      VBSUM, VBSSQ, VBBAR, VBSIG
        REAL        VBMAX, VBMIN
        INTEGER     IBMIN, IBMAX
        REAL*8      VDSUM, VDSSQ, VDBAR, VDSIG
        REAL        VDMAX, VDMIN
        INTEGER     IDMIN, IDMAX

        INTEGER     I, L, LA, LB, MM
        REAL        TA, TB, TD, TT
        REAL*8      RMS

        MM = 0      !!  "missing" count
        L  = LAY0A

        VASUM = 0.0D0
        VASSQ = 0.0D0
        VAMAX = AGRID2D( L,1 )
        VAMIN = AGRID2D( L,1 )
        IAMAX = 1
        IAMIN = 1
        VBSUM = 0.0D0
        VBSSQ = 0.0D0
        VBMAX = BGRID1D( 1 )
        VBMIN = BGRID1D( 1 )
        IBMAX = 1
        IBMIN = 1
        VDSUM = 0.0D0
        VDSSQ = 0.0D0
        VDMAX =-BADVAL3
        VDMIN = BADVAL3
        IDMAX = IMISS3
        IDMIN = IMISS3

        IF      ( DIFMODE .EQ. 1 )  THEN        !  diff


            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TA - TB
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 2 )  THEN        !  diff B-A

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TD - TA
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 3 )  THEN        !  ratio A/B

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = TA / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 4 )  THEN        !  ratio B/A

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = TB / TA
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 5 )  THEN        !  diff | A-B |

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = ABS( TA - TD )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 6 )  THEN        !  diffn A-B/A

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = ( TA - TB ) / TA
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 7 )  THEN        !  diffn B-A/B

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ( TB - TA ) / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 8 )  THEN        !  diffn A-B/B

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ( TA - TB ) / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 9 )  THEN        !  diffna |A - B|/A

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = ABS( ( TA - TB ) / TA )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 10 )  THEN        !  diffna |A - B|/B

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ABS( ( TA - TB ) / TB )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 11 )  THEN        !  diffnm 2(A - B)/(A + B)

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TT = TA + TB
                IF ( TT .NE. 0.0 ) THEN
                    TD = 2.0*( TA - TB ) / TT
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 12 )  THEN        !  diffnm 2(B - A)/(A + B)

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TT = TA + TB
                IF ( TT .NE. 0.0 ) THEN
                    TD = 2.0*( TB - TA ) / TT
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 13 )  THEN        !  diffnrms (a-b)/rms

            RMS = 0.0D0

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                RMS = TA**2 + TB**2

            END DO
            
            TT = SQRT( RMS / DBLE( NCELLS*NLAYSD ) )
            
            IF ( TT .GT. 0.0 ) THEN
            
                TT = 1.0 / TT

                DO I = 1, NCELLS

                    TA = AGRID2D( L,I )
                    TB = BGRID1D( I )
                    TD = TT * ( TA - TB )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF

                END DO

            ELSE
                MM = NCELLS * NLAYSD
            END IF

        ELSE IF ( DIFMODE .EQ. 14 )  THEN        !   diffnrms (b-a)/rms

            RMS = 0.0D0

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                RMS = TA**2 + TB**2

            END DO
            
            TT = SQRT( RMS / DBLE( NCELLS*NLAYSD ) )
            
            IF ( TT .GT. 0.0 ) THEN
            
                TT = 1.0 / TT

                DO I = 1, NCELLS

                    TA = AGRID2D( L,I )
                    TB = BGRID1D( I )
                    TD = TT * ( TB - TA )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF

                END DO

            ELSE
                MM = NCELLS * NLAYSD
            END IF

        ELSE IF ( DIFMODE .EQ. 15 )  THEN        !  add

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TA + TB
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 16 ) THEN         !  max

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = MAX( TA , TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 17 ) THEN         !  min

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = MIN( TA , TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 18 ) THEN         !  grid A

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

            END DO
            
            VDSUM = VASUM
            VDSSQ = VASSQ
            VDMAX = VAMAX
            VDMIN = VAMIN
            IDMAX = IAMAX
            IDMIN = IAMIN

        ELSE IF ( DIFMODE .EQ. 19 ) THEN         !  grid B

            DO I = 1, NCELLS

                TA = AGRID2D( L,I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

            END DO
            
            VDSUM = VBSUM
            VDSSQ = VBSSQ
            VDMAX = VBMAX
            VDMIN = VBMIN
            IDMAX = IBMAX
            IDMIN = IBMIN

        END IF

        IF ( N .EQ. 1 ) THEN

            FSUMA = VASUM
            FSSQA = VASSQ
            FMAXA = VAMAX
            KMAXA = IAMAX
            TMAXA = N
            FMINA = VAMIN
            KMINA = IAMIN
            TMINA = N
            FSUMB = VBSUM
            FSSQB = VBSSQ
            FMAXB = VBMAX
            KMAXB = IBMAX
            TMAXB = N
            FMINB = VBMIN
            KMINB = IBMIN
            TMINB = N
            FSUMD = VDSUM
            FSSQD = VDSSQ
            FMAXD = VDMAX
            KMAXD = IDMAX
            TMAXD = N
            FMIND = VDMIN
            KMIND = IDMIN
            TMIND = N
            FCNTD = NCELLS - MM

            WRITE( RDEV, '( /, 2( 2X, A ), I5, A, I5, 2( /, 2( 2X, A ) ) )' )   &
                'Variable:', TRIM( VNAMEA ),  'layers', LAY0A, ':', LAY1A,      &
                'Variable:', TRIM( VNAMEB ),                                    &
                'Difference:', OP( DIFMODE)
            IF ( TFLAGA .OR. TFLAGB ) THEN
                WRITE( RDEV, '( 3 A16, A10, A16, A10) ' )           &
                    'Date&Time', 'MEAN', 'SIGMA', 'MAX @', 'CELL',  &
                    'MIN @', 'CELL'
            ELSE
                WRITE( RDEV, '( 2 A16, A10, A16, A10 )' )            &
                    'MEAN', 'SIGMA', 'MAX @', 'CELL', 'MIN @', 'CELL'
            END IF

        ELSE

            FSUMA = FSUMA + VASUM
            FSSQA = FSSQA + VASSQ
            FSUMB = FSUMB + VBSUM
            FSSQB = FSSQB + VBSSQ
            FSUMD = FSUMD + VDSUM
            FSSQD = FSSQD + VDSSQ
            FCNTD = FCNTD + NCELLS - MM
            IF ( VAMAX .GT. FMAXA ) THEN
                FMAXA = VAMAX
                KMAXA = IAMAX
                TMAXA = N
            END IF
            IF ( VAMIN .LT. FMINA ) THEN
                FMINA = VAMIN
                KMINA = IAMIN
                TMINA = N
            END IF
            IF ( VBMAX .GT. FMAXB ) THEN
                FMAXB = VBMAX
                KMAXB = IBMAX
                TMAXB = N
            END IF
            IF ( VBMIN .LT. FMINB ) THEN
                FMINB = VBMIN
                KMINB = IBMIN
                TMINB = N
            END IF
            IF ( VDMAX .GT. FMAXD ) THEN
                FMAXD = VDMAX
                KMAXD = IDMAX
                TMAXD = N
            END IF
            IF ( VDMIN .LT. FMIND ) THEN
                FMIND = VDMIN
                KMIND = IDMIN
                TMIND = N
            END IF

        END IF      !!  if n==1, or not

        DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1A - LAY0A + 1 ) )
        VASUM = VASUM * DIV
        VASSQ = SQRT( MAX( VASSQ * DIV - VASUM**2, 0.0D0 ) )
        VBSUM = VBSUM * DIV
        VBSSQ = SQRT( MAX( VBSSQ * DIV - VBSUM**2, 0.0D0 ) )
        IF ( NCELLS * ( LAY1A - LAY0A + 1 ) - MM .GT. 0 ) THEN
           DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1A - LAY0A + 1 ) - MM )
           VDSUM = VDSUM * DIV
           VDSSQ = SQRT( MAX( VDSSQ * DIV - VDSUM**2, 0.0D0 ) )
        ELSE
           VDSUM = BADVAL3
           VDSSQ = BADVAL3
        END IF
        IF ( TFLAGA .AND. TFLAGB ) THEN
            WRITE( RDEV, '( "A: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE IF ( TFLAGA ) THEN
            WRITE( RDEV, '( "A: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", 16X          , 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE IF ( TFLAGB ) THEN
            WRITE( RDEV, '( "A: ", 16X          , 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE
            WRITE( RDEV, '( "A: ", 3X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", 3X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ",  3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        END IF
        
        RETURN

    END SUBROUTINE STAT21



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    SUBROUTINE STAT12( )

        !!--------------------------------------------------------------
        !!    Variables A is 1-D (non-layered), and B is 2-D (layered)
        !!--------------------------------------------------------------
       
        REAL*8      VASUM, VASSQ, VABAR, VASIG, DIV
        REAL        VAMAX, VAMIN
        INTEGER     IAMIN, IAMAX
        REAL*8      VBSUM, VBSSQ, VBBAR, VBSIG
        REAL        VBMAX, VBMIN
        INTEGER     IBMIN, IBMAX
        REAL*8      VDSUM, VDSSQ, VDBAR, VDSIG
        REAL        VDMAX, VDMIN
        INTEGER     IDMIN, IDMAX

        INTEGER     I, L, LA, LB, MM
        REAL        TA, TB, TD, TT
        REAL*8      RMS

        MM = 0      !!  "missing" count
        L  = LAY0B

        VASUM = 0.0D0
        VASSQ = 0.0D0
        VAMAX = AGRID1D( 1 )
        VAMIN = AGRID1D( 1 )
        IAMAX = 1
        IAMIN = 1
        VBSUM = 0.0D0
        VBSSQ = 0.0D0
        VBMAX = BGRID2D( L,1 )
        VBMIN = BGRID2D( L,1 )
        IBMAX = 1
        IBMIN = 1
        VDSUM = 0.0D0
        VDSSQ = 0.0D0
        VDMAX =-BADVAL3
        VDMIN = BADVAL3
        IDMAX = IMISS3
        IDMIN = IMISS3

        IF      ( DIFMODE .EQ. 1 )  THEN        !  diff

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TA - TB
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 2 )  THEN        !  diff B-A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TB - TA
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 3 )  THEN        !  ratio A/B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = TA / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 4 )  THEN        !  ratio B/A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = TB / TA
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 5 )  THEN        !  diff | A-B |

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = ABS( TA - TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 6 )  THEN        !  diffn A-B/A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = ( TA - TB ) / TA
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 7 )  THEN        !  diffn B-A/B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ( TB - TA ) / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 8 )  THEN        !  diffn A-B/B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ( TA - TB ) / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 9 )  THEN        !  diffna |A - B|/A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = ABS( ( TA - TB ) / TA )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 10 )  THEN        !  diffna |A - B|/B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ABS( ( TA - TB ) / TB )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 11 )  THEN        !  diffnm 2(A - B)/(A + B)

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TT = TA + TB
                IF ( TT .NE. 0.0 ) THEN
                    TD = 2.0*( TA - TB ) / TT
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 12 )  THEN        !  diffnm 2(B - A)/(A + B)

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TT = TA + TB
                IF ( TT .NE. 0.0 ) THEN
                    TD = 2.0*( TB - TA ) / TT
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 13 )  THEN        !  diffnrms (a-b)/rms

            RMS = 0.0D0

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                RMS = TA**2 + TB**2

            END DO
            
            TT = SQRT( RMS / DBLE( NCELLS*NLAYSD ) )
            
            IF ( TT .GT. 0.0 ) THEN
            
                TT = 1.0 / TT

                DO I = 1, NCELLS

                    TA = AGRID1D( I )
                    TB = BGRID2D( L,I )
                    TD = TT * ( TA - TB )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF

                END DO

            ELSE
                MM = NCELLS * NLAYSD
            END IF

        ELSE IF ( DIFMODE .EQ. 14 )  THEN        !   diffnrms (b-a)/rms

            RMS = 0.0D0

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                RMS = TA**2 + TB**2

            END DO
            
            TT = SQRT( RMS / DBLE( NCELLS*NLAYSD ) )
            
            IF ( TT .GT. 0.0 ) THEN
            
                TT = 1.0 / TT

                DO I = 1, NCELLS

                    TA = AGRID1D( I )
                    TB = BGRID2D( L,I )
                    TD = TT * ( TB - TA )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF

                END DO

            ELSE
                MM = NCELLS * NLAYSD
            END IF

        ELSE IF ( DIFMODE .EQ. 15 )  THEN        !  add

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TA + TB
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 16 ) THEN         !  max

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = MAX( TA , TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 17 ) THEN         !  min

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = MIN( TA , TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 18 ) THEN         !  grid A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

            END DO
            
            VDSUM = VASUM
            VDSSQ = VASSQ
            VDMAX = VAMAX
            VDMIN = VAMIN
            IDMAX = IAMAX
            IDMIN = IAMIN

        ELSE IF ( DIFMODE .EQ. 19 ) THEN         !  grid B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID2D( L,I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

            END DO
            
            VDSUM = VBSUM
            VDSSQ = VBSSQ
            VDMAX = VBMAX
            VDMIN = VBMIN
            IDMAX = IBMAX
            IDMIN = IBMIN

        END IF

        IF ( N .EQ. 1 ) THEN

            FSUMA = VASUM
            FSSQA = VASSQ
            FMAXA = VAMAX
            KMAXA = IAMAX
            TMAXA = N
            FMINA = VAMIN
            KMINA = IAMIN
            TMINA = N
            FSUMB = VBSUM
            FSSQB = VBSSQ
            FMAXB = VBMAX
            KMAXB = IBMAX
            TMAXB = N
            FMINB = VBMIN
            KMINB = IBMIN
            TMINB = N
            FSUMD = VDSUM
            FSSQD = VDSSQ
            FMAXD = VDMAX
            KMAXD = IDMAX
            TMAXD = N
            FMIND = VDMIN
            KMIND = IDMIN
            TMIND = N
            FCNTD = NCELLS - MM

            WRITE( RDEV, '( /, 2X, 2 A, /2X, 2 ( A, I5 ), /, 2X, 2A )' )    &
                'Variable:', TRIM( VNAMEA ),                                &
                'Variable:', TRIM( VNAMEB ),  'layers', LAY0B, ':', LAY1B,  &
                'Difference:', OP( DIFMODE)
            IF ( TFLAGA .OR. TFLAGB ) THEN
                WRITE( RDEV, '( 3 A16, A10, A16, A10) ' )           &
                    'Date&Time', 'MEAN', 'SIGMA', 'MAX @', 'CELL',  &
                    'MIN @', 'CELL'
            ELSE
                WRITE( RDEV, '( 2 A16, A10, A16, A10 )' )            &
                    'MEAN', 'SIGMA', 'MAX @', 'CELL', 'MIN @', 'CELL'
            END IF

        ELSE

            FSUMA = FSUMA + VASUM
            FSSQA = FSSQA + VASSQ
            FSUMB = FSUMB + VBSUM
            FSSQB = FSSQB + VBSSQ
            FSUMD = FSUMD + VDSUM
            FSSQD = FSSQD + VDSSQ
            FCNTD = FCNTD + NCELLS - MM
            IF ( VAMAX .GT. FMAXA ) THEN
                FMAXA = VAMAX
                KMAXA = IAMAX
                TMAXA = N
            END IF
            IF ( VAMIN .LT. FMINA ) THEN
                FMINA = VAMIN
                KMINA = IAMIN
                TMINA = N
            END IF
            IF ( VBMAX .GT. FMAXB ) THEN
                FMAXB = VBMAX
                KMAXB = IBMAX
                TMAXB = N
            END IF
            IF ( VBMIN .LT. FMINB ) THEN
                FMINB = VBMIN
                KMINB = IBMIN
                TMINB = N
            END IF
            IF ( VDMAX .GT. FMAXD ) THEN
                FMAXD = VDMAX
                KMAXD = IDMAX
                TMAXD = N
            END IF
            IF ( VDMIN .LT. FMIND ) THEN
                FMIND = VDMIN
                KMIND = IDMIN
                TMIND = N
            END IF

        END IF      !!  if n==1, or not

        DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1A - LAY0A + 1 ) )
        VASUM = VASUM * DIV
        VASSQ = SQRT( MAX( VASSQ * DIV - VASUM**2, 0.0D0 ) )
        VBSUM = VBSUM * DIV
        VBSSQ = SQRT( MAX( VBSSQ * DIV - VBSUM**2, 0.0D0 ) )
        IF ( NCELLS * ( LAY1A - LAY0A + 1 ) - MM .GT. 0 ) THEN
           DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1A - LAY0A + 1 ) - MM )
           VDSUM = VDSUM * DIV
           VDSSQ = SQRT( MAX( VDSSQ * DIV - VDSUM**2, 0.0D0 ) )
        ELSE
           VDSUM = BADVAL3
           VDSSQ = BADVAL3
        END IF
        IF ( TFLAGA .AND. TFLAGB ) THEN
            WRITE( RDEV, '( "A: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE IF ( TFLAGA ) THEN
            WRITE( RDEV, '( "A: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", 16X          , 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE IF ( TFLAGB ) THEN
            WRITE( RDEV, '( "A: ", 16X          , 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE
            WRITE( RDEV, '( "A: ", 3X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", 3X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ",  3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        END IF
        
        RETURN

    END SUBROUTINE STAT12


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    SUBROUTINE STAT11( )

        !!--------------------------------------------------------------
        !!    Variables A is 1-D (non-layered), and B is 2-D (layered)
        !!--------------------------------------------------------------
       
        REAL*8      VASUM, VASSQ, VABAR, VASIG, DIV
        REAL        VAMAX, VAMIN
        INTEGER     IAMIN, IAMAX
        REAL*8      VBSUM, VBSSQ, VBBAR, VBSIG
        REAL        VBMAX, VBMIN
        INTEGER     IBMIN, IBMAX
        REAL*8      VDSUM, VDSSQ, VDBAR, VDSIG
        REAL        VDMAX, VDMIN
        INTEGER     IDMIN, IDMAX

        INTEGER     I, L, LA, LB, MM
        REAL        TA, TB, TD, TT
        REAL*8      RMS

        MM = 0      !!  "missing" count

        VASUM = 0.0D0
        VASSQ = 0.0D0
        VAMAX = AGRID1D( 1 )
        VAMIN = AGRID1D( 1 )
        IAMAX = 1
        IAMIN = 1
        VBSUM = 0.0D0
        VBSSQ = 0.0D0
        VBMAX = BGRID1D( 1 )
        VBMIN = BGRID1D( 1 )
        IBMAX = 1
        IBMIN = 1
        VDSUM = 0.0D0
        VDSSQ = 0.0D0
        VDMAX =-BADVAL3
        VDMIN = BADVAL3
        IDMAX = IMISS3
        IDMIN = IMISS3

        IF      ( DIFMODE .EQ. 1 )  THEN        !  diff

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TA - TB
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 2 )  THEN        !  diff B-A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TB - TA
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 3 )  THEN        !  ratio A/B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = TA / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 4 )  THEN        !  ratio B/A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = TB / TA
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 5 )  THEN        !  diff | A-B |

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = ABS( TA - TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 6 )  THEN        !  diffn A-B/A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = ( TA - TB ) / TA
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 7 )  THEN        !  diffn B-A/B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ( TB - TA ) / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 8 )  THEN        !  diffn A-B/B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ( TA - TB ) / TB
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 9 )  THEN        !  diffna |A - B|/A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TA .NE. 0.0 ) THEN
                    TD = ABS( ( TA - TB ) / TA )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 10 )  THEN        !  diffna |A - B|/B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                IF ( TB .NE. 0.0 ) THEN
                    TD = ABS( ( TA - TB ) / TB )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 11 )  THEN        !  diffnm 2(A - B)/(A + B)

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TT = TA + TB
                IF ( TT .NE. 0.0 ) THEN
                    TD = 2.0*( TA - TB ) / TT
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 12 )  THEN        !  diffnm 2(B - A)/(A + B)

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TT = TA + TB
                IF ( TT .NE. 0.0 ) THEN
                    TD = 2.0*( TB - TA ) / TT
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF
                ELSE
                    MM = MM + 1
                    TD = BADVAL3
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 13 )  THEN        !  diffnrms (a-b)/rms

            RMS = 0.0D0

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                RMS = TA**2 + TB**2

            END DO
            
            TT = SQRT( RMS / DBLE( NCELLS*NLAYSD ) )
            
            IF ( TT .GT. 0.0 ) THEN
            
                TT = 1.0 / TT

                DO I = 1, NCELLS

                    TA = AGRID1D( I )
                    TB = BGRID1D( I )
                    TD = TT * ( TA - TB )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF

                END DO

            ELSE
                MM = NCELLS * NLAYSD
            END IF

        ELSE IF ( DIFMODE .EQ. 14 )  THEN        !   diffnrms (b-a)/rms

            RMS = 0.0D0

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                RMS = TA**2 + TB**2

            END DO
            
            TT = SQRT( RMS / DBLE( NCELLS*NLAYSD ) )
            
            IF ( TT .GT. 0.0 ) THEN
            
                TT = 1.0 / TT

                DO I = 1, NCELLS

                    TA = AGRID1D( I )
                    TB = BGRID1D( I )
                    TD = TT * ( TB - TA )
                    VDSUM = VDSUM + TD
                    VDSSQ = VDSSQ + TD**2
                    IF ( TD .GT. VDMAX ) THEN
                        VDMAX = TD
                        IDMAX = I
                    END IF
                    IF ( TD .LT. VDMIN ) THEN
                        VDMIN = TD
                        IDMIN = I
                    END IF

                END DO

            ELSE
                MM = NCELLS * NLAYSD
            END IF

        ELSE IF ( DIFMODE .EQ. 15 )  THEN        !  add

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = TA + TB
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 16 ) THEN         !  max

            DO I = 1, NCELLS
            DO L = 1, NLAYSD

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = MAX( TA , TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO
            END DO

        ELSE IF ( DIFMODE .EQ. 17 ) THEN         !  min

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

                TD = MIN( TA , TB )
                VDSUM = VDSUM + TD
                VDSSQ = VDSSQ + TD**2
                IF ( TD .GT. VDMAX ) THEN
                    VDMAX = TD
                    IDMAX = I
                END IF
                IF ( TD .LT. VDMIN ) THEN
                    VDMIN = TD
                    IDMIN = I
                END IF

            END DO

        ELSE IF ( DIFMODE .EQ. 18 ) THEN         !  grid A

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

            END DO
            
            VDSUM = VASUM
            VDSSQ = VASSQ
            VDMAX = VAMAX
            VDMIN = VAMIN
            IDMAX = IAMAX
            IDMIN = IAMIN

        ELSE IF ( DIFMODE .EQ. 19 ) THEN         !  grid B

            DO I = 1, NCELLS

                TA = AGRID1D( I )
                VASUM = VASUM + TA
                VASSQ = VASSQ + TA**2
                IF ( TA .GT. VAMAX ) THEN
                    VAMAX = TA
                    IAMAX = I
                ELSE IF ( TA .LT. VAMIN ) THEN
                    VAMIN = TA
                    IAMIN = I
                END IF

                TB = BGRID1D( I )
                VBSUM = VBSUM + TB
                VBSSQ = VBSSQ + TB**2
                IF ( TB .GT. VBMAX ) THEN
                    VBMAX = TB
                    IBMAX = I
                ELSE IF ( TB .LT. VBMIN ) THEN
                    VBMIN = TB
                    IBMIN = I
                END IF

            END DO
            
            VDSUM = VBSUM
            VDSSQ = VBSSQ
            VDMAX = VBMAX
            VDMIN = VBMIN
            IDMAX = IBMAX
            IDMIN = IBMIN

        END IF

        IF ( N .EQ. 1 ) THEN

            FSUMA = VASUM
            FSSQA = VASSQ
            FMAXA = VAMAX
            KMAXA = IAMAX
            TMAXA = N
            FMINA = VAMIN
            KMINA = IAMIN
            TMINA = N
            FSUMB = VBSUM
            FSSQB = VBSSQ
            FMAXB = VBMAX
            KMAXB = IBMAX
            TMAXB = N
            FMINB = VBMIN
            KMINB = IBMIN
            TMINB = N
            FSUMD = VDSUM
            FSSQD = VDSSQ
            FMAXD = VDMAX
            KMAXD = IDMAX
            TMAXD = N
            FMIND = VDMIN
            KMIND = IDMIN
            TMIND = N
            FCNTD = NCELLS - MM

            WRITE( RDEV, '( 3( /, 2( 2X, A ), I5, A, I5 ) )' )  &
                'Variable:', TRIM( VNAMEA ),                    &
                'Variable:', TRIM( VNAMEB ),                    &
                'Difference:', OP( DIFMODE)
            IF ( TFLAGA .OR. TFLAGB ) THEN
                WRITE( RDEV, '( 3 A16, A10, A16, A10) ' )           &
                    'Date&Time', 'MEAN', 'SIGMA', 'MAX @', 'CELL',  &
                    'MIN @', 'CELL'
            ELSE
                WRITE( RDEV, '( 2 A16, A10, A16, A10 )' )            &
                    'MEAN', 'SIGMA', 'MAX @', 'CELL', 'MIN @', 'CELL'
            END IF

        ELSE

            FSUMA = FSUMA + VASUM
            FSSQA = FSSQA + VASSQ
            FSUMB = FSUMB + VBSUM
            FSSQB = FSSQB + VBSSQ
            FSUMD = FSUMD + VDSUM
            FSSQD = FSSQD + VDSSQ
            FCNTD = FCNTD + NCELLS - MM
            IF ( VAMAX .GT. FMAXA ) THEN
                FMAXA = VAMAX
                KMAXA = IAMAX
                TMAXA = N
            END IF
            IF ( VAMIN .LT. FMINA ) THEN
                FMINA = VAMIN
                KMINA = IAMIN
                TMINA = N
            END IF
            IF ( VBMAX .GT. FMAXB ) THEN
                FMAXB = VBMAX
                KMAXB = IBMAX
                TMAXB = N
            END IF
            IF ( VBMIN .LT. FMINB ) THEN
                FMINB = VBMIN
                KMINB = IBMIN
                TMINB = N
            END IF
            IF ( VDMAX .GT. FMAXD ) THEN
                FMAXD = VDMAX
                KMAXD = IDMAX
                TMAXD = N
            END IF
            IF ( VDMIN .LT. FMIND ) THEN
                FMIND = VDMIN
                KMIND = IDMIN
                TMIND = N
            END IF

        END IF      !!  if n==1, or not

        DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1A - LAY0A + 1 ) )
        VASUM = VASUM * DIV
        VASSQ = SQRT( MAX( VASSQ * DIV - VASUM**2, 0.0D0 ) )
        VBSUM = VBSUM * DIV
        VBSSQ = SQRT( MAX( VBSSQ * DIV - VBSUM**2, 0.0D0 ) )
        IF ( NCELLS * ( LAY1A - LAY0A + 1 ) - MM .GT. 0 ) THEN
           DIV  = 1.0D0 / DBLE( NCELLS * ( LAY1A - LAY0A + 1 ) - MM )
           VDSUM = VDSUM * DIV
           VDSSQ = SQRT( MAX( VDSSQ * DIV - VDSUM**2, 0.0D0 ) )
        ELSE
           VDSUM = BADVAL3
           VDSSQ = BADVAL3
        END IF
        IF ( TFLAGA .AND. TFLAGB ) THEN
            WRITE( RDEV, '( "A: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE IF ( TFLAGA ) THEN
            WRITE( RDEV, '( "A: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", 16X          , 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE IF ( TFLAGB ) THEN
            WRITE( RDEV, '( "A: ", 16X          , 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", I9.7, A, I6.6, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )      &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ", 13X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )             &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        ELSE
            WRITE( RDEV, '( "A: ", 3X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEA, VASUM, VASSQ, VAMAX, IAMAX, VAMIN, IAMIN
            WRITE( RDEV, '( "B: ", 3X, 3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEB, VBSUM, VBSSQ, VBMAX, IBMAX, VBMIN, IBMIN
            WRITE( RDEV, '( "Diff: ",  3( 2X, 1PD14.6 ), I10, 2X, 1PD14.6, I10 )' )         &
                JDATEA, ':', JTIMEA, VDSUM, VDSSQ, VDMAX, IAMAX, VDMIN, IDMIN
        END IF
        
        RETURN

    END SUBROUTINE STAT11



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE STAT1DF

        REAL*8      FBARA, FSIGA, FBARB, FSIGB, FBARD, FSIGD, DIV
        INTEGER     IDATEA, ITIMEA, IDATEB, ITIMEB, IDATED, ITIMED
        INTEGER     JDATEA, JTIMEA, JDATEB, JTIMEB, JDATED, JTIMED

        DIV   = 1.0 / DBLE( NRECS * NCELLS )
        FBARA = FSUMA * DIV
        FSIGA = SQRT( MAX( FSSQA * DIV - FBARA**2, 0.0D0 ) )
        FBARB = FSUMB * DIV
        FSIGB = SQRT( MAX( FSSQB * DIV - FBARB**2, 0.0D0 ) )

        IF ( FCNTD .GT. 0 ) THEN
            DIV   = 1.0 / DBLE( FCNTD )
            FBARD = FSUMD * DIV
            FSIGD = SQRT( MAX( FSSQD * DIV - FBARD**2, 0.0D0 ) )
        ELSE
            FBARD = BADVAL3
            FSIGD = BADVAL3
        END IF
        
        IF ( TFLAGA ) THEN
            IDATEA = FDATEA(TMINA)
            ITIMEA = FTIMEA(TMINA)
            JDATEA = FDATEA(TMAXA)
            JTIMEA = FTIMEA(TMAXA)
        ELSE
            IDATEA = 0
            ITIMEA = 0
            JDATEA = 0
            JTIMEA = 0
        END IF
        
        IF ( TFLAGB ) THEN
            IDATEB = FDATEB(TMINB)
            ITIMEB = FTIMEB(TMINB)
            JDATEB = FDATEB(TMAXB)
            JTIMEB = FTIMEB(TMAXB)
        ELSE
            IDATEB = 0
            ITIMEB = 0
            JDATEB = 0
            JTIMEB = 0
        END IF
        
        IF ( TFLAGA .OR. TFLAGB ) THEN
            IDATED = FDATEA(TMIND)
            ITIMED = FTIMEA(TMIND)
            JDATED = FDATEA(TMAXD)
            JTIMED = FTIMEA(TMAXD)
        ELSE
            IDATED = 0
            ITIMED = 0
            JDATED = 0
            JTIMED = 0
        END IF

        WRITE( RDEV, '( /, 2X, A )' ) 'FULL layer/time-period statistics:'

        WRITE( RDEV, 91001 )        &
            'A    MEAN:', FBARA,    &
            'SIGMA:'    , FSIGA,    &
            '  MAX:'    , FMAXA, ' at', JDATEA, ':', JTIMEA, 'K=', KMAXA, &
            '  MIN:'    , FMINB, ' at', IDATEA, ':', ITIMEA, 'K=', KMINA
        WRITE( RDEV, 91001 )        &
            'B    MEAN:', FBARB,    &
            'SIGMA:'    , FSIGB,    &
            '  MAX:'    , FMAXB, ' at', JDATEB, ':', JTIMEB, 'K=', KMAXB, &
            '  MIN:'    , FMINB, ' at', IDATEB, ':', ITIMEB, 'K=', KMINB
        WRITE( RDEV, 91001 )        &
            'DIFF MEAN:', FBARD,    &
            'SIGMA:'    , FSIGD,    &
            '  MAX:'    , FMAXD, ' at', JDATED, ':', JTIMED, 'K=', KMAXD, &
            '  MIN:'    , FMIND, ' at', IDATED, ':', ITIMED, 'K=', KMIND

        RETURN

91001   FORMAT( 2( A8, 1PD14.6, / ), 2( A12, 1PD14.6, A, I9.7, A, I6.6, 2X, A, I10, / ) )

    END SUBROUTINE STAT1DF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE STAT22F

        REAL*8      FBARA, FSIGA, FBARB, FSIGB, FBARD, FSIGD, DIV
        INTEGER     IDATEA, ITIMEA, IDATEB, ITIMEB, IDATED, ITIMED
        INTEGER     JDATEA, JTIMEA, JDATEB, JTIMEB, JDATED, JTIMED

        DIV = 1.0 / DBLE( NRECS * NCELLS * NLAYSD )
        FBARA = FSUMA * DIV
        FSIGA = SQRT( MAX( FSSQA * DIV - FBARA**2, 0.0D0 ) )
        FBARB = FSUMB * DIV
        FSIGB = SQRT( MAX( FSSQB * DIV - FBARB**2, 0.0D0 ) )

        IF ( FCNTD .GT. 0 ) THEN
            DIV   = 1.0 / DBLE( FCNTD )
            FBARD = FSUMD * DIV
            FSIGD = SQRT( MAX( FSSQD * DIV - FBARD**2, 0.0D0 ) )
        ELSE
            FBARD = BADVAL3
            FSIGD = BADVAL3
        END IF
        
        IF ( TFLAGA ) THEN
            IDATEA = FDATEA(TMINA)
            ITIMEA = FTIMEA(TMINA)
            JDATEA = FDATEA(TMAXA)
            JTIMEA = FTIMEA(TMAXA)
        ELSE
            IDATEA = 0
            ITIMEA = 0
            JDATEA = 0
            JTIMEA = 0
        END IF
        
        IF ( TFLAGB ) THEN
            IDATEB = FDATEB(TMINB)
            ITIMEB = FTIMEB(TMINB)
            JDATEB = FDATEB(TMAXB)
            JTIMEB = FTIMEB(TMAXB)
        ELSE
            IDATEB = 0
            ITIMEB = 0
            JDATEB = 0
            JTIMEB = 0
        END IF
        
        IF ( TFLAGA .OR. TFLAGB ) THEN
            IDATED = FDATEA(TMIND)
            ITIMED = FTIMEA(TMIND)
            JDATED = FDATEA(TMAXD)
            JTIMED = FTIMEA(TMAXD)
        ELSE
            IDATED = 0
            ITIMED = 0
            JDATED = 0
            JTIMED = 0
        END IF

        WRITE( RDEV, '( /, 2X, A )' ) 'FULL time-period statistics:'

        WRITE( RDEV, 91011 )        &
            'A    MEAN:', FBARA,    &
            'SIGMA:'    , FSIGA,    &
            '  MAX:'    , FMAXA, ' at', JDATEA, ':', JTIMEA, 'K=', KMAXA, 'L=', LMAXA, &
            '  MIN:'    , FMINB, ' at', IDATEA, ':', ITIMEA, 'K=', KMINA, 'L=', LMINA
        WRITE( RDEV, 91011 )        &
            'B    MEAN:', FBARB,    &
            'SIGMA:'    , FSIGB,    &
            '  MAX:'    , FMAXB, ' at', JDATEB, ':', JTIMEB, 'K=', KMAXB, 'L=', LMAXB, &
            '  MIN:'    , FMINB, ' at', IDATEB, ':', ITIMEB, 'K=', KMINB, 'L=', LMINB
        WRITE( RDEV, 91011 )        &
            'DIFF MEAN:', FBARD,    &
            'SIGMA:'    , FSIGD,    &
            '  MAX:'    , FMAXD, ' at', JDATED, ':', JTIMED, 'K=', KMAXD, 'L=', LMAXD,&
            '  MIN:'    , FMIND, ' at', IDATED, ':', ITIMED, 'K=', KMIND, 'L=', LMIND

        RETURN

91011   FORMAT( 2( A8, 1PD14.6, / ), 2( A12, 1PD14.6, A, I9.7, A, I6.6, 2( 2X, A, I10 ), / ) )

    END SUBROUTINE STAT22F

END PROGRAM MPASDIFF

