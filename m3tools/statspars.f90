
SUBROUTINE  STATSPARS( NCOLS, NROWS, NTHIK, NVARS,          &
                       JDATE, JTIME,                        &
                       INNAME, VNAMES, VTYPES, RDEV )

    !***********************************************************************
    ! Version "$Id: statspars.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  69
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
    !       Modified  9/99 by CJC for enhanced portability
    !
    !       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !       Version  12/2013 by CJC: INTENT for arguments
    !       Version  02/2015 by CJC: Support for M3INT8 variables.
    !       Fix indexing bug
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER     , INTENT(IN) :: NCOLS   ! grid dim:  number of active coeffs
    INTEGER     , INTENT(IN) :: NROWS   ! grid dim:  number of matrix rows
    INTEGER     , INTENT(IN) :: NTHIK   ! grid dim:  number of matrix cols
    INTEGER     , INTENT(IN) :: NVARS   !  number of vbles to be totaled
    INTEGER     , INTENT(IN) :: JDATE   ! current model date
    INTEGER     , INTENT(IN) :: JTIME   ! current model time
    CHARACTER*16, INTENT(IN) :: INNAME                  !  input file logical name
    CHARACTER*16, INTENT(IN) :: VNAMES( NVARS )         !  list of vble names
    INTEGER     , INTENT(IN) :: VTYPES( NVARS )         !  list of types for variables
    INTEGER     , INTENT(IN) :: RDEV                    !  unit number for output


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         CSUM( 2 * NROWS )
    INTEGER         NACT( NROWS + (2*NVARS+1)*NCOLS )   !! NACT(NROWS) // INDX(NCOLS) // COEF(NCOLS,NVARS)
    REAL            GRID( NCOLS )                       !!  scratch array
    INTEGER         V, W, K


    !***********************************************************************
    !   begin body of subroutine  STATSPARS

    IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
        WRITE( RDEV,92010 ) INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
    ELSE
        WRITE( RDEV,92010 ) INNAME
    END IF

    IF ( .NOT.READ3( INNAME, ALLVAR3, ALLAYS3, JDATE, JTIME, NACT ) ) THEN

        CALL M3EXIT( 'M3STAT:STATSPARS', JDATE, JTIME, 'Read failure:  file ' // INNAME, 2 )

    END IF              !  if read3() failed

    K = NROWS + 1   !!  start of "INDX(NCOLS")
    W = K + NCOLS   !!  start of "COEF(NCOLS,NVARS")

    DO  V = 1, NVARS

        IF ( VTYPES( V ) .EQ. M3REAL ) THEN

            CALL STATM( NCOLS, NROWS, NTHIK,            &
                        NACT, NACT(K), NACT(W), CSUM,   &
                        VNAMES( V ), RDEV )
            W = W + NCOLS

        ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

            CALL INTG2REAL( NCOLS, NACT(W), GRID )
            CALL STATM( NCOLS, NROWS, NTHIK,            &
                        NACT, NACT(K), GRID, CSUM,      &
                        VNAMES( V ), RDEV )
            W = W + NCOLS

        ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN

            CALL INT82REAL( NCOLS, NACT(W), GRID )
            CALL STATM( NCOLS, NROWS, NTHIK,            &
                        NACT, NACT(K), GRID, CSUM,      &
                        VNAMES( V ), RDEV )
            W = W + 2 * NCOLS

        ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

            CALL DBLE2REAL( NCOLS, NACT(W), GRID )
            CALL STATM( NCOLS, NROWS, NTHIK,            &
                        NACT, NACT(K), GRID, CSUM,      &
                        VNAMES( V ), RDEV )
            W = W + 2 * NCOLS

        END IF

    END DO


    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( //5X, 'File:  ', A, :,&
              /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

END SUBROUTINE  STATSPARS

