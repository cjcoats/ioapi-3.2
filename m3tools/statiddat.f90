
SUBROUTINE  STATIDDAT ( NROWS, NLAYS, NVARS,            &
                       JDATE, JTIME, NTHRES, THRES,     &
                       INNAME, VNAMES, VTYPES, LOGDEV )

    !***********************************************************************
    ! Version "$Id: statiddat.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  73
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
    !       Modified  9/99 by CJC for enhanced portability
    !
    !       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !       Version 12/2013 by CJC:  INTENT for arguments
    !       Version  02/2015 by CJC: Support for M3INT8 variables
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
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

    INTEGER          N
    INTEGER          ID( NROWS )
    REAL             VV( NROWS, NLAYS, NVARS )
    REAL             GG( NROWS, NLAYS )
    INTEGER          V, W, SIZE, GSIZ
    CHARACTER*120    MESG


    !***********************************************************************
    !   begin body of subroutine  STATIDDAT

    GSIZ = NROWS * NVARS * NLAYS
    SIZE = 1 + 2 * ( NROWS + GSIZ )

    IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
        WRITE( LOGDEV,92010 ) INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
    ELSE
        WRITE( LOGDEV,92010 ) INNAME
    END IF

    IF ( READ3( INNAME, ALLVAR3, ALLAYS3,&
                   JDATE, JTIME, N ) ) THEN

        W = 1

        DO  111  V = 1, NVARS

            IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                CALL STATI( NROWS, NLAYS, N, ID, VV( 1,1,W ),   &
                            NTHRES( V ), THRES( 1,V ),          &
                            VNAMES( V ), LOGDEV )
                W = W + GSIZ

            ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

                CALL INTG2REAL( GSIZ, VV( 1,1,W ), GG )
                CALL STATI( NROWS, NLAYS, N, ID, GG,            &
                            NTHRES( V ), THRES( 1,V ),          &
                            VNAMES( V ), LOGDEV )
                W = W + GSIZ

            ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN

                CALL INT82REAL( GSIZ, VV( 1,1,W ), GG )
                CALL STATI( NROWS, NLAYS, N, ID, GG,            &
                            NTHRES( V ), THRES( 1,V ),          &
                            VNAMES( V ), LOGDEV )
                W = W + 2 * GSIZ

            ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                CALL DBLE2REAL( GSIZ, VV( 1,1,W ), GG )
                CALL STATI( NROWS, NLAYS, N, ID, GG,            &
                            NTHRES( V ), THRES( 1,V ),          &
                            VNAMES( V ), LOGDEV )
                W = W + 2 * GSIZ

            ELSE

                MESG = 'Bad type for variable ' // VNAMES( V )
                CALL M3EXIT( 'STATIDDAT', JDATE, JTIME, MESG, 2 )

            END IF

111     CONTINUE        !  end loop on variables

    ELSE                !  read3() failed:

        MESG = 'Read failure:  file ' // INNAME
        CALL M3EXIT( 'STATIDDAT', JDATE, JTIME, MESG, 2 )

    END IF              !  if read3() worked, or not


    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( //5X, 'File:  ', A, :,           &
              /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

END SUBROUTINE  STATIDDAT

