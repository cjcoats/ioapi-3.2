
SUBROUTINE  STATSPARS( NCOLS, NROWS, NLAYS, NVARS,          &
                       JDATE, JTIME,                        &
                       INNAME, VNAMES, VTYPES, RDEV )

    !***********************************************************************
    ! Version "$Id: statspars.f90 280 2025-04-12 15:34:39Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021-2024 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  98
    !
    !  FUNCTION:
    !       Statistics report to  on variables VNAMES  from file
    !       INNAME and on the results of using GRIDOPS to apply the
    !       operations OPNAME( * ) to them.
    !
    !  PRECONDITIONS REQUIRED:  none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       Models-3 I/O:  M3EXIT(), READ3(), WRITE3()
    !
    !  REVISION  HISTORY:
    !       Prototype 3/93 by CJC
    !
    !       Modified  9/99 by CJC for enhanced portability
    !
    !       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !
    !       Version  12/2013 by CJC: INTENT for arguments
    !
    !       Version  02/2015 by CJC: Support for M3INT8 variables.
    !       Fix indexing bug
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !
    !       Version 040/2025 by CJC:  In-line STATM()
    !***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER     , INTENT(IN) :: NCOLS   ! grid dim:  number of active coeffs
    INTEGER     , INTENT(IN) :: NROWS   ! grid dim:  number of matrix rows
    INTEGER     , INTENT(IN) :: NLAYS   ! grid dim:  number of matrix layers
    INTEGER     , INTENT(IN) :: NVARS   !  number of vbles to be totaled
    INTEGER     , INTENT(IN) :: JDATE   ! current model date
    INTEGER     , INTENT(IN) :: JTIME   ! current model time
    CHARACTER*16, INTENT(IN) :: INNAME                  !  input file logical name
    CHARACTER*16, INTENT(IN) :: VNAMES( NVARS )         !  list of vble names
    INTEGER     , INTENT(IN) :: VTYPES( NVARS )         !  list of types for variables
    INTEGER     , INTENT(IN) :: RDEV                    !  unit number for output

    CHARACTER*16, PARAMETER :: PNAME = 'STATSPARS'

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         INDX( NROWS + NCOLS*(2*NLAYS+1) )
    INTEGER         CNTS( NROWS )
    REAL            GRID( NCOLS,NLAYS )                       !!  scratch arrays
    REAL            CSUM( NLAYS,NCOLS )                       !!  scratch arrays
    INTEGER         I, C, R, K, L, M, V   !  row, coef traversal counter
    INTEGER         MC, MR, ML, MRR    !  indexes for maximum
    INTEGER         NC, NR, NL, NRR    !  indexes for minimum
    REAL            S, T
    REAL            AMAX
    REAL            AMIN
    REAL*8          ASUM
    REAL*8          ASSQ
    REAL            RMAX
    REAL            RMIN
    REAL*8          RSUM
    REAL*8          RSSQ
    REAL*8          DNOM

    CHARACTER*20    MCBUF
    CHARACTER*20    MRBUF
    CHARACTER*20    MLBUF
    CHARACTER*20    NCBUF
    CHARACTER*20    NRBUF
    CHARACTER*20    NLBUF
    CHARACTER*256   MESG


    !***********************************************************************
    !   begin body of subroutine  STATSPARS

    IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
        WRITE( RDEV,92010 ) INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
    ELSE
        WRITE( RDEV,92010 ) INNAME
    END IF           !  if read3() failed

    DO  V = 1, NVARS

        IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, INDX ) ) THEN
            MESG = 'Read failure:  file "' // TRIM( INNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
        END IF
        CNTS = INDX( NROWS+1: )

        IF ( VTYPES( V ) .EQ. M3REAL ) THEN
        
            CALL REAL2REAL( NCOLS*NLAYS, INDX( NCOLS+NROWS+1: ), GRID )

        ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN
        
            CALL INTG2REAL( NCOLS*NLAYS, INDX( NCOLS+NROWS+1: ), GRID )

        ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN
        
            CALL INT82REAL( NCOLS*NLAYS, INDX( NCOLS+NROWS+1: ), GRID )

        ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN
        
            CALL DBLE2REAL( NCOLS*NLAYS, INDX( NCOLS+NROWS+1: ), GRID )

        ELSE

            MESG = 'Unknown data type for variable ' // VNAMES( V )
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )

        END IF

        !...........   Construct 3-D GRID stats: max, min and their locations,
        !...........   mean, and sigma

        CSUM = 0.0

        RMAX = GRID( 1,1 )
        RMIN = GRID( 1,1 )
        RSUM = 0.0
        RSSQ = 0.0

        M    = 0        !  high-water-mark for columns encountered

        MC   = INDX( 1 )
        MR   = 1
        ML   = 1
        MRR  = 1
        NC   = MC
        NR   = 1
        NL   = 1
        NRR  = 1
        T    = GRID( 1,1 )
        AMAX = T
        AMIN = T
        ASUM = 0.0
        ASSQ = 0.0

        K = 0
        DO  R = 1, NROWS

            S = 0.0

            DO  C = 1, CNTS( R )
                K    = K + 1
                I    = INDX( K )
                M    = MAX( M, I )
                DO  L = 1, NLAYS
                    K    = K + 1
                    T    = GRID( K,L )
                    I    = INDX( K )
                    M    = MAX( M, I )
                    CSUM( L,I ) = CSUM( L,I ) +  T
                    S         = S         +  T
                    ASUM      = ASUM      +  T
                    ASSQ      = ASSQ      +  T * T
                    IF ( T .GT. AMAX ) THEN
                        AMAX = T
                        MC   = I
                        MR   = R
                        ML   = L
                    ELSE IF ( T .LT. AMIN ) THEN
                        AMIN = T
                        NC   = I
                        NR   = R
                        NL   = L
                    END IF
                END DO
            END DO

            RSUM = RSUM  +  S
            RSSQ = RSSQ  +  S * S
            IF (S  .GT. RMAX ) THEN
                RMAX = S
                MRR  = R
            ELSE IF ( S .LT. RMIN ) THEN
                RMIN = S
                NRR  = R
            END IF

        END DO

        DNOM = 1.0 / DBLE( K )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

        WRITE( MCBUF,94010 ) MC
        WRITE( MRBUF,94010 ) MR
        WRITE( MLBUF,94010 ) ML
        WRITE( NCBUF,94010 ) NC
        WRITE( NRBUF,94010 ) NR
        WRITE( NLBUF,94010 ) NL

        WRITE( RDEV,92020 )                                 &
            VNAMES( V ),                                    &
            'Matrix coefficient statistics' ,               &
            'Max   ', AMAX, ' @(c,r,l)=(',                  &
                MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),         &
                MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ),         &
                MLBUF( LEN2( 1,20,MLBUF )+1 : 20 ), ')',    &
            'Min   ', AMIN, ' @(c,r)=(',                    &
                NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),         &
                NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ),         &
                NLBUF( LEN2( 1,20,NLBUF )+1 : 20 ), ')',    &
            'Mean  ', ASUM,                                 &
            'Sigma ', ASSQ

        DNOM = 1.0 / FLOAT( NROWS )
        RSUM = DNOM * RSUM
        RSSQ = SQRT( MAX( RSSQ * DNOM - RSUM * RSUM , 0.0D0 ) )

        WRITE( MRBUF,94010 ) MRR
        WRITE( NRBUF,94010 ) NRR

        WRITE( RDEV,92030 )                                 &
            'Matrix row-sum statistics' ,                   &
            'Max   ', RMAX, ' @ r=',                        &
                MRBUF( LEN2( 1,20,MRBUF )+1 : 20 ),         &
            'Min   ', RMIN, ' @ r=',                        &
                NRBUF( LEN2( 1,20,NRBUF )+1 : 20 ),         &
            'Mean  ', RSUM,                                 &
            'Sigma ', RSSQ

        !.......   Now compute statistics for column-sums:

        K    = M
        MC   = 1
        NC   = 1
        ML   = 1
        NL   = 1
        T    = CSUM( 1,1 )
        AMAX = T
        AMIN = T
        ASUM = 0.0
        ASSQ = 0.0

        DO  R = 1, K
        DO  L = 1, NLAYS
            T    = CSUM( L,R )
            ASUM = ASUM + T
            ASSQ = ASSQ + T*T
            IF ( T .GT. AMAX ) THEN
                AMAX = T
                MC   = R
                ML   = R
            ELSE IF ( T .LT. AMIN ) THEN
                AMIN = T
                NC   = R
                NL   = R
            END IF
        END DO
        END DO

        DNOM = 1.0 / DBLE( K )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0D0 ) )

        WRITE( MCBUF,94010 ) MC
        WRITE( NCBUF,94010 ) NC
        WRITE( MLBUF,94010 ) ML
        WRITE( NLBUF,94010 ) NL

        WRITE( RDEV,92030 )                             &
            'Matrix column-sum statistics' ,            &
            'Max   ', AMAX, ' @ c,l=',                  &
                MCBUF( LEN2( 1,20,MCBUF )+1 : 20 ),     &
                MLBUF( LEN2( 1,20,MLBUF )+1 : 20 ),     &
            'Min   ', AMIN, ' @ c=',                    &
                NCBUF( LEN2( 1,20,NCBUF )+1 : 20 ),     &
                NLBUF( LEN2( 1,20,NLBUF )+1 : 20 ),     &
            'Mean  ', ASUM,                             &
            'Sigma ', ASSQ

    END DO

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( //5X, 'File:  ', A, :,                       &
              /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

92020 FORMAT ( /5X , 'Variable:  ', A,                      &
               /9X, A,                                      &
               3( /9X, A, 1PE12.5, A, A, ',', A, ',', A ),  &
               2( /9X, A, 1PE12.5 ) )

92030 FORMAT ( 9X , A,                                      &
               2( /9X, A, 1PE12.5, A, ',', A ),             &
               2( /9X, A, 1PE12.5 ) )


    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( I20 )


END SUBROUTINE  STATSPARS

