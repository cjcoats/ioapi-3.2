
SUBROUTINE  STATGRID( NCOLS, NROWS, NLAYS, NVARS,       &
                      JDATE, JTIME, NTHRES, THRES,      &
                      INNAME, VNAMES, VTYPES, LOGDEV )

    !***********************************************************************
    ! Version "$Id: statgrid.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  82
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
    !
    !       Version 02/2010 by CJC:  SIZE bug-fix
    !
    !       Version  02/2015 by CJC: Support for M3INT8 variables
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'M3STAT:STATGRID'

    !...........   ARGUMENTS and their descriptions:

    INTEGER     , INTENT(IN) :: NCOLS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NROWS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NLAYS   ! grid dimensions, from INNAME header
    INTEGER     , INTENT(IN) :: NVARS   !  number of vbles to be totaled
    INTEGER     , INTENT(IN) :: JDATE   ! current model date
    INTEGER     , INTENT(IN) :: JTIME   ! current model time
    INTEGER     , INTENT(IN) :: NTHRES( NVARS )     ! number of tests per vble
    REAL        , INTENT(IN) :: THRES ( 10,NVARS )  ! thresholds for counting
    CHARACTER*16, INTENT(IN) :: INNAME              !  input file logical name
    CHARACTER*16, INTENT(IN) :: VNAMES( NVARS )     !  list of vble names
    INTEGER     , INTENT(IN) :: VTYPES( NVARS )     ! number of tests per vble
    INTEGER     , INTENT(IN) :: LOGDEV              ! unit number for output


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    REAL             GRID( NCOLS, NROWS, NLAYS )
    INTEGER          IGRD( NCOLS, NROWS, NLAYS )
    INTEGER*8        LGRD( NCOLS, NROWS, NLAYS )
    DOUBLE PRECISION DGRD( NCOLS, NROWS, NLAYS )
    INTEGER          V, SIZE
    CHARACTER*24     DTBUF
    CHARACTER*120    MESG


    !***********************************************************************
    !   begin body of subroutine  STATGRID

    IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
        DTBUF = DT2STR( JDATE, JTIME )
        WRITE( LOGDEV,92010 ) INNAME, JDATE, JTIME, DTBUF
    ELSE
        WRITE( LOGDEV,92010 ) INNAME
    END IF

    SIZE = NCOLS*NROWS*NLAYS

    DO  V = 1, NVARS

        IF ( VTYPES( V ) .EQ. M3REAL ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, GRID ) ) THEN

                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
                CYCLE

            END IF              !  if read3() worked, or not

        ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, IGRD ) ) THEN

                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
                CYCLE

            END IF              !  if read3() worked, or not

            CALL INTG2REAL( SIZE, IGRD, GRID )

        ELSE IF ( VTYPES( V ) .EQ. M3INT8 ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, LGRD ) ) THEN

                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
                CYCLE

            END IF              !  if read3() worked, or not

            CALL INT82REAL( SIZE, LGRD, GRID )

        ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

            IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3, JDATE, JTIME, DGRD ) ) THEN

                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAMES( V )
                CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
                CYCLE

            END IF              !  if read3() worked, or not

            CALL DBLE2REAL( SIZE, DGRD, GRID )

        ELSE

            MESG = 'Unknown data type for variable ' // VNAMES( V )
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            CYCLE

        END IF

        CALL STATG( NCOLS, NROWS, NLAYS, GRID,          &
                    NTHRES( V ), THRES( 1,V ),          &
                    VNAMES( V ), LOGDEV )

    END DO          !  end loop on variables

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Informational (LOG) message formats... 92xxx

92010 FORMAT ( //5X, 'File:  ', A, :,                   &
              /5X, 'Date and time:', I9.7, ':', I6.6,   &
               2X, '(', A, ')' )

END SUBROUTINE  STATGRID

