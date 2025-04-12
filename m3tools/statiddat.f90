
SUBROUTINE  STATIDDAT ( NROWS, NLAYS, NVARS,            &
                       JDATE, JTIME, NTHRES, THRES,     &
                       INNAME, VNAMES, VTYPES, LOGDEV )

    !***********************************************************************
    ! Version "$Id: statiddat.f90 280 2025-04-12 15:34:39Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021-2024 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  97
    !
    !  FUNCTION:
    !       Statistics report to LOGDEV on variables VNAMES  from file
    !       INNAME.
    !       and on the results of using GRIDOPS to apply the operations
    !       OPNAME( * ) to them.
    !
    !  PRECONDITIONS REQUIRED:
    !       Valid dates and times JDATE:JTIME
    !       Stack-allocation operating environment (such as CRAY)
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
    !
    !  REVISION  HISTORY:
    !       Prototype 3/93 by CJC
    !
    !       Modified  9/99 by CJC for enhanced portability
    !
    !       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !
    !       Version 12/2013 by CJC:  INTENT for arguments
    !
    !       Version  02/2015 by CJC: Support for M3INT8 variables
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !
    !       Version  4/2025 by CJC:  in-line STATI()
    !***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER     , INTENT(IN) :: NROWS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NLAYS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NVARS   !  number of vbles to be totaled
    INTEGER     , INTENT(IN) :: JDATE   ! current model date
    INTEGER     , INTENT(IN) :: JTIME   ! current model time
    INTEGER     , INTENT(IN) :: NTHRES( NVARS )         ! number of tests per vble
    REAL        , INTENT(IN) :: THRES ( 10,NVARS )      ! thresholds for counting
    CHARACTER*16, INTENT(IN) :: INNAME                  !  input file logical name
    CHARACTER*16, INTENT(IN) :: VNAMES( NVARS )         !  list of vble names
    INTEGER     , INTENT(IN) :: VTYPES( NVARS )         ! number of tests per vble
    INTEGER     , INTENT(IN) :: LOGDEV  ! unit number for output


    !...........   LOCAL VARIABLES and their descriptions:

    REAL            VV( 0:NROWS, NLAYS, NVARS )
    REAL            GG( NROWS, NLAYS )
    INTEGER         ID( NROWS )          !  ID-list
    
    INTEGER         N, W, SIZE, GSIZ, ECNT
    INTEGER         R, L, V         !  row, level, variable, counters
    INTEGER         MI, MR, ML      !  indexes for maximum
    INTEGER         NI, NR, NL      !  indexes for minimum
    REAL            T
    REAL            AMAX
    REAL            AMIN
    REAL*8          ASUM
    REAL*8          ASSQ
    REAL*8          DNOM

    CHARACTER*20    MIBUF
    CHARACTER*20    MRBUF
    CHARACTER*20    MLBUF
    CHARACTER*20    NIBUF
    CHARACTER*20    NRBUF
    CHARACTER*20    NLBUF
    CHARACTER*120    MESG


    !***********************************************************************
    !   begin body of subroutine  STATIDDAT

    GSIZ = NROWS * NVARS * NLAYS
    SIZE = 1 + 2 * ( NROWS + GSIZ )

    IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
        WRITE( LOGDEV,92000 ) INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
    ELSE
        WRITE( LOGDEV,92000 ) INNAME
    END IF

    IF ( READ3( INNAME, ALLVAR3, ALLAYS3, JDATE, JTIME, VV ) ) THEN

        CALL REAL2INTG( GSIZ, VV( 0,:,: ), ID )
        W = 1

        DO  V = 1, NVARS

            IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                GG = VV( 1:,:,W )
                W  = W + GSIZ

            ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

                CALL INTG2REAL( GSIZ, VV( 1,1,W ), GG )
                W = W + GSIZ

            ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN

                CALL INT82REAL( GSIZ, VV( 1,1,W ), GG )
                W = W + 2 * GSIZ

            ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                CALL DBLE2REAL( GSIZ, VV( 1,1,W ), GG )
                W = W + 2 * GSIZ

            ELSE

                MESG = 'Bad type for variable ' // VNAMES( V )
                CALL M3EXIT( 'STATIDDAT', JDATE, JTIME, MESG, 2 )

            END IF

            !...........   Construct 3-D VV stats: max, min and their locations,
            !...........   mean, and sigma

            MR   = ID( 1 )
            ML   = 1
            NR   = 1
            NL   = 1
            T    = GG( 1,1 )
            AMAX = T
            AMIN = T
            ASUM = 0.0
            ASSQ = 0.0

            DO  L = 1, NLAYS   !  3-D traversal:  all other layers
            DO  R = 1, N
                T    = GG( R,L )
                ASUM = ASUM + T
                ASSQ = ASSQ + T*T
                IF ( T .GT. AMAX ) THEN
                    AMAX = T
                    MR   = R
                    MI   = ID( R )
                    ML   = L
                ELSE IF ( T .LT. AMIN ) THEN
                    AMIN = T
                    NR   = R
                    NI   = ID( R )
                    NL   = L
                END IF
            END DO
            END DO

            DNOM = 1.0 / DBLE( N * NLAYS )
            ASUM = DNOM * ASUM
            ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

            WRITE( MIBUF,94010 ) MI
            WRITE( MRBUF,94010 ) MR
            WRITE( MRBUF,94010 ) ML
            WRITE( NIBUF,94010 ) NI
            WRITE( NRBUF,94010 ) NR
            WRITE( NRBUF,94010 ) NL

            WRITE( LOGDEV,92000 )                               &
                    VNAMES( V ), ' array statistics' ,          &
                    'Max   ', AMAX, ' at site ',                &
                    MIBUF( LEN2( 1,20,MIBUF )+1 : 20 ),         &
                    ' @(r,l)=',                                 &
                    MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ), ',',    &
                    MLBUF( LEN2( 1,20,MLBUF )+1 : 20 ), ')',    &
                    'Min   ', AMIN, ' at site ',                &
                    NIBUF( LEN2( 1,20,NIBUF )+1 : 20 ),         &
                    ' @(r,l)=',                                 &
                    NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ), ',',    &
                    NLBUF( LEN2( 1,20,NLBUF )+1 : 20 ), ')',    &
                    'Mean  ', ASUM,                             &
                    'Sigma ', ASSQ


            !...........   For each threshold level, count the number of times the
            !...........   grid value exceeds the threshold, and report it:

            DO  N = 1, NTHRES( V )    !  count threshold excesses:
                ECNT = 0
                T    = THRES( N,V )
                DO L = 1, NLAYS
                DO R = 1, NROWS
                    IF ( GG( R,L ) .GE. T )  ECNT = ECNT + 1
                END DO
                END DO
            END DO

            WRITE( LOGDEV,92020 ) T, ECNT, DNOM * FLOAT( ECNT )

        END DO        !  end loop on variables


    ELSE                !  read3() failed:

        MESG = 'Read failure:  file ' // INNAME
        CALL M3EXIT( 'STATIDDAT', JDATE, JTIME, MESG, 2 )

    END IF              !  if read3() worked, or not


    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92000 FORMAT ( //5X, 'File:  ', A, :,           &
              /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

92010 FORMAT ( /5X , 'Variable:  ', A, A,           &
             2( /9X, A, 1PE12.5, 7A ),              &
             2( /9X, A, 1PE12.5 ) )

92020 FORMAT ( 9X , 'Number of times ', 1PE12.5,    &
             2X, 'exceeded:', I10,                  &
             2X, 'fraction:', F12.8 )

    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( I20 )

END SUBROUTINE  STATIDDAT

