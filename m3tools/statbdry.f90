
SUBROUTINE  STATBDRY( SIZE, NCOLS, NROWS, NLAYS, NTHIK, NVARS,  &
                      JDATE, JTIME, NTHRES, THRES,              &
                      INNAME, VNAMES, VTYPES, LOGDEV )

    !***********************************************************************
    ! Version "$Id: statbdry.f90 280 2025-04-12 15:34:39Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 Baron Advanced Meteorological Systems, LLC
    ! (C) 2021- Carlie J. Coats, Jr.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  74
    !
    !  FUNCTION:
    !       Statistics report to LOGDEV on variables VNAMES  from file
    !       INNAME.
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
    !       Modified  9/99 by CJC for enhanced portability
    !
    !       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !
    !       Version 02/2010 by CJC:  SIZE bug-fix
    !
    !       Version 12/2013 by CJC:  INTENT for arguments
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !
    !       Version  4/2025 by CJC:  in-line STATB()
    !***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER     , INTENT(IN) :: SIZE    ! horiz bdy size,  from INNAME header
    INTEGER     , INTENT(IN) :: NCOLS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NROWS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NLAYS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NTHIK   ! bdry thickness,  from INNAME header
    INTEGER     , INTENT(IN) :: NVARS   !  number of vbles to be totaled
    INTEGER     , INTENT(IN) :: JDATE   ! current model date
    INTEGER     , INTENT(IN) :: JTIME   ! current model time
    INTEGER     , INTENT(IN) :: NTHRES( NVARS )     ! number of tests per vble
    REAL        , INTENT(IN) :: THRES ( 10,NVARS )  ! thresholds for counting
    CHARACTER*16, INTENT(IN) :: INNAME              !  input file logical name
    CHARACTER*16, INTENT(IN) :: VNAMES( NVARS )     !  list of vble names
    INTEGER     , INTENT(IN) :: VTYPES( NVARS )     ! number of tests per vble
    INTEGER     , INTENT(IN) :: LOGDEV              ! unit number for output


    !...........   LOCAL VARIABLES and their descriptions:

    REAL            BDRY( SIZE, NLAYS )
    INTEGER         IGRD( SIZE, NLAYS )
    INTEGER*8       LGRD( SIZE, NLAYS )
    REAL*8          DGRD( SIZE, NLAYS )
    INTEGER         C, R, L, V, K   !  col, row, level, vble, cell counters
    INTEGER         MC, MR, ML      !  indexes for maximum
    INTEGER         NC, NR, NL      !  indexes for minimum
    INTEGER         ECNT            !  # of threshold-exceedences
    REAL            T
    REAL            AMAX
    REAL            AMIN
    REAL*8          ASUM
    REAL*8          ASSQ
    REAL*8          DNOM

    CHARACTER*120   MESG


    !***********************************************************************
    !   begin body of subroutine  STATBDRY

    IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
        WRITE( LOGDEV,92010 ) INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
    ELSE
        WRITE( LOGDEV,92010 ) INNAME
    END IF

    DO  111  V = 1, NVARS

        IF ( VTYPES( V ) .EQ. M3REAL ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, BDRY ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

        ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, IGRD ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

            CALL INTG2REAL( SIZE, IGRD, BDRY )

        ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, LGRD ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

            CALL INTG2REAL( SIZE, LGRD, BDRY )

        ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, DGRD ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

            CALL DBLE2REAL( SIZE, DGRD, BDRY )

        ELSE

            MESG = 'Unknown data type for' // ' variable ' // VNAMES( V )
            CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME, MESG, 2 )

        END IF
        !...........   Construct 3-D BDRY stats: max, min and their locations,
        !...........   mean, and sigma

        MC   = 1
        MR   = 1 - NTHIK
        ML   = 1
        NC   = 1
        NR   = MR
        NL   = 1
        T    = BDRY( 1,1 )
        AMAX = T
        AMIN = T
        ASUM = 0.0
        ASSQ = 0.0

        DO  L = 1, NLAYS   !  traversal:  all layers, all edges

            K = 0
            DO R = 1 - NTHIK, 0       !  south edge
            DO C = 1, NCOLS + NTHIK
                K    = K + 1
                T    = BDRY( K,L )
                ASUM = ASUM + T
                ASSQ = ASSQ + T*T
                IF ( T .GT. AMAX ) THEN
                    AMAX = T
                    MC   = C
                    MR   = R
                    ML   = L
                ELSE IF ( T .LT. AMIN ) THEN
                    AMIN = T
                    NC   = C
                    NR   = R
                    NL   = L
                END IF
            END DO
            END DO

            DO  R = 1, NROWS + NTHIK       ! east edge
            DO  C = NCOLS + 1, NCOLS + NTHIK
                K    = K + 1
                T    = BDRY( K,L )
                ASUM = ASUM + T
                ASSQ = ASSQ + T*T
                IF ( T .GT. AMAX ) THEN
                    AMAX = T
                    MC   = C
                    MR   = R
                    ML   = L
                ELSE IF ( T .LT. AMIN ) THEN
                    AMIN = T
                    NC   = C
                    NR   = R
                    NL   = L
                END IF
            END DO
            END DO

            DO R = NROWS + 1, NROWS + NTHIK   ! north edge
            DO C = 1 - NTHIK, 0
                K    = K + 1
                T    = BDRY( K,L )
                ASUM = ASUM + T
                ASSQ = ASSQ + T*T
                IF ( T .GT. AMAX ) THEN
                    AMAX = T
                    MC   = C
                    MR   = R
                    ML   = L
                ELSE IF ( T .LT. AMIN ) THEN
                    AMIN = T
                    NC   = C
                    NR   = R
                    NL   = L
                END IF
            END DO
            END DO

            DO R = 1 - NTHIK, NROWS       !  west edge
            DO C = 1 - NTHIK, 0
                K    = K + 1
                T    = BDRY( K,L )
                ASUM = ASUM + T
                ASSQ = ASSQ + T*T
                IF ( T .GT. AMAX ) THEN
                    AMAX = T
                    MC   = C
                    MR   = R
                    ML   = L
                ELSE IF ( T .LT. AMIN ) THEN
                    AMIN = T
                    NC   = C
                    NR   = R
                    NL   = L
                END IF
            END DO
            END DO

        END DO    !  end loop on levels

        DNOM = 1.0 / DBLE( NCOLS * NROWS * NLAYS )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )

        IF ( MAX( NCOLS, NROWS, NLAYS ) .LT. 100 - NTHIK ) THEN
            WRITE( LOGDEV,92020 )                                   &
                VNAMES( V ), ' 3-D boundary statistics' ,           &
                'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',     &
                'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',     &
                'Mean  ', ASUM,                                     &
                'Sigma ', ASSQ
        ELSE
            WRITE( LOGDEV,92021 )                                   &
                VNAMES( V ), ' 3-D boundary statistics' ,           &
                'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',     &
                'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',     &
                'Mean  ', ASUM,                                     &
                'Sigma ', ASSQ
        END IF


        !...........   For each threshold level, count the number of times the
        !...........   grid value exceeds the threshold, and report it:

        DO  C = 1, NTHRES( V )    !  count threshold excesses:
            ECNT = 0
            T    = THRES( C,V )
            DO L = 1, NLAYS
            DO K = 1, SIZE
                IF ( BDRY( K,L ) .GE. T )  ECNT = ECNT + 1
            END DO
            END DO
            WRITE( LOGDEV,92030 ) T, ECNT, DNOM * DBLE( ECNT )
        END DO

111 CONTINUE        !  end loop on variables

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( //5X, 'File:  ', A, :,           &
                /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

92020 FORMAT ( /5X , 'Variable:  ', A, A,                           &
            2( /9X, A, 1PE12.5, A, I2, ',', I2, ',', I2, A ),       &
            2( /9X, A, 1PE12.5 ) )

92021 FORMAT ( /5X , 'Variable:  ', A, A,                           &
           2( /9X, A, 1PE12.5, A, I3, ',', I3, ',', I3, A ),        &
           2( /9X, A, 1PE12.5 ) )

92030 FORMAT ( 9X , 'Number of times ', 1PE12.5,                    &
               2X, 'exceeded:', I8,                                 &
               2X, 'fraction:', F10.8 )

END SUBROUTINE  STATBDRY

