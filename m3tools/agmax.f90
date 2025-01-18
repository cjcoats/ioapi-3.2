
SUBROUTINE  MAXREG

    !***********************************************************************
    ! Version "$Id: agmax.f90 212 2021-11-10 20:39:53Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC,
    ! (C) 1995-2002,2005-2013,2021 Carlie J. Coats, Jr.,
    ! and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  entry INITTAG body starts at line  103
    !  entry AGGREG  body starts at line  151
    !  entry OUTAGG  body starts at line  201
    !
    !  FUNCTION:
    !       Aggregate time steps as either sum, average, or max.
    !       INITAGG: allocates memory and initializes array with first time
    !                step of data
    !       AGGREG:  Modifies gridded storage array based on current time step
    !                of data
    !       OUTAGG:  Completes gridded array values and writes to output file
    !
    !  PRECONDITIONS REQUIRED:
    !       Valid dates and times JDATE:JTIME
    !       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
    !
    !  REVISION  HISTORY:
    !       Prototype 5/1997 by M Houyoux
    !
    !       Modified  9/1999 by CJC for enhanced portability
    !
    !       Modified 11/1999 by CJC to keep hour-maxes of the aggregated result.
    !       F90 only.
    !
    !       Modified 11/2005 by CJC:  eliminate unused vbles
    !
    !       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !**********************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !...........   PARAMETERS

    INTEGER, PARAMETER :: M3SUM = 1
    INTEGER, PARAMETER :: M3AVE = 2
    INTEGER, PARAMETER :: M3MAX = 3

    CHARACTER*2, PARAMETER :: SUFFIX( 6 ) = (/ '_1', '_2', '_3', '_4', '_5', '_6' /)

    !...........   ARGUMENTS and their descriptions:

    INTEGER         NCOLS   ! column dimension for this variable
    INTEGER         NROWS   ! row    dimension for this variable
    INTEGER         NLAYS   ! layer  dimension for this variable
    INTEGER         JDATE   ! current model date
    INTEGER         JTIME   ! current model time
    CHARACTER*16    INAME   !  logical name of the  input file
    CHARACTER*16    ANAME   !  logical name of the AGG output file
    CHARACTER*16    MNAME   !  logical name of the MAX output file
    CHARACTER*16    VNAMEI  !  vble name (input)
    CHARACTER*16    VNAMEO  !  vble name (output)
    INTEGER         T       ! time step within the aggregation
    INTEGER         TYPE    ! type of aggregation to perform
    INTEGER         NSTEPS  ! number of time steps - used for average only
    INTEGER         LOGDEV  ! unit number for output
    REAL            CMAX    ! max( grid )
    INTEGER         CCOL
    INTEGER         CROW
    INTEGER         CLAY


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         J, K, C, R, L, JTOP
    REAL            X, DENOM

    CHARACTER*16    VNAME

    REAL,    ALLOCATABLE, SAVE::    GRID( :, :, : )
    REAL,    ALLOCATABLE, SAVE::    AMAX( :, :, :, : )
    REAL,    ALLOCATABLE, SAVE::    AGRD( :, :, : )
    INTEGER, ALLOCATABLE, SAVE::    EXC1( :, :, : )
    INTEGER, ALLOCATABLE, SAVE::    EXC8( :, :, : )
    LOGICAL, SAVE::              FIRSTIME = .TRUE.

    CHARACTER*256   MESG

    !***********************************************************************
    !   begin body of subroutine  MAXGREG

    !***********************************************************************
    !   begin body of entry point INITAGG

  ENTRY INITAGG( NCOLS, NROWS, NLAYS, T, JDATE, JTIME, INAME, VNAMEI, LOGDEV )


    IF ( FIRSTIME ) THEN
        ALLOCATE( GRID( NCOLS, NROWS, NLAYS ),          &
                  AMAX( NCOLS, NROWS, NLAYS, 6 ),       &
                  AGRD( NCOLS, NROWS, NLAYS ),          &
                  EXC1( NCOLS, NROWS, NLAYS ),          &
                  EXC8( NCOLS, NROWS, NLAYS ),  STAT=K )
        IF ( K .NE. 0 ) THEN
            CALL M3EXIT( 'TAGGREG', JDATE, JTIME, 'Memory allocation error', 2 )
        END IF
        FIRSTIME = .FALSE.
    END IF

    IF ( .NOT. READ3( INAME, VNAMEI, ALLAYS3, JDATE, JTIME, GRID ) ) THEN
        MESG = 'Read failure:  file "' // TRIM( INAME ) // '" variable "' // TRIM( VNAMEI ) // '"'
        CALL M3EXIT ( 'M3AGMAX:AGGREG', JDATE, JTIME, MESG, 2 )
    END IF              !  if read3() worked, or not

    IF ( T .EQ. 1 ) THEN
        DO  L = 1, NLAYS
        DO  R = 1, NROWS
        DO  C = 1, NCOLS
            X = GRID( C,R,L )
            IF ( X .GT. 0.125 ) THEN
                EXC1( C,R,L ) = 1
            ELSE
                EXC1( C,R,L ) = 0
            END IF
            EXC8( C,R,L ) = 0
        END DO
        END DO
        END DO
    END IF          !  if first time step for this variable

    RETURN


    !***********************************************************************
    !   begin body of entry point AGGREG

  ENTRY AGGREG( NCOLS, NROWS, NLAYS, JDATE, JTIME, INAME, VNAMEI, TYPE , LOGDEV   )

    IF ( .NOT. READ3( INAME, VNAMEI, ALLAYS3, JDATE, JTIME, AGRD ) ) THEN
        MESG = 'Read failure:  file "' // TRIM( INAME ) //&
              '" variable "' // TRIM( VNAMEI ) // '"'
        CALL M3EXIT ( 'M3AGMAX:AGGREG', JDATE, JTIME, MESG, 2 )
    ELSE IF( TYPE .EQ. M3SUM .OR. TYPE .EQ. M3AVE ) THEN

        DO  L = 1, NLAYS
        DO  R = 1, NROWS
        DO  C = 1, NCOLS
            X = AGRD( C,R,L )
            GRID( C,R,L ) = GRID( C,R,L ) + X
            IF ( X .GT. 0.125 ) THEN
                EXC1( C,R,L ) = EXC1( C,R,L ) + 1
            END IF
        END DO
        END DO
        END DO

    ELSE IF( TYPE .EQ. M3MAX ) THEN

        DO  L = 1, NLAYS
        DO  R = 1, NROWS
        DO  C = 1, NCOLS
            X = AGRD( C,R,L )
            GRID( C,R,L ) = MAX( GRID( C,R,L ), X )
            IF ( X .GT. 0.125 ) THEN
                EXC1( C,R,L ) = EXC1( C,R,L ) + 1
            END IF
        END DO
        END DO
        END DO

    ELSE

        WRITE( MESG, '( A, I8, 2X, A)' ) 'Aggregation type ', TYPE, 'not supported'
        CALL M3EXIT ( 'M3AGMAX:AGGREG', JDATE, JTIME, MESG, 2 )

    END IF              !  if read3() worked, or not

    RETURN


    !***********************************************************************
    !   begin body of entry point OUTAGG

  ENTRY OUTAGG( NCOLS, NROWS,  NLAYS, T, JDATE, JTIME,      &
                ANAME, VNAMEO, TYPE , NSTEPS,               &
                CMAX,  CCOL,   CROW,  CLAY, LOGDEV )

    JTOP = MIN( 6, T-1 )    !  number of AMAX(:,:,:,*) already set

    IF( TYPE .EQ. M3AVE ) THEN

        DENOM = 1.0 / REAL( NSTEPS )
        CMAX = DENOM * GRID( 1,1,1 )
        CCOL = 1
        CROW = 1
        CLAY = 1
        DO  L = 1, NLAYS
        DO  R = 1, NROWS
        DO  C = 1, NCOLS

            X = DENOM * GRID( C,R,L )
            GRID( C,R,L ) = X

            IF ( X .GT. CMAX ) THEN
                CMAX = X
                CCOL = C
                CROW = R
                CLAY = L
            END IF

            IF ( X .GT. 0.085 ) THEN
                EXC8( C,R,L ) = EXC8( C,R,L ) + 1
            END IF

            DO  J = 1, JTOP
                IF ( X .GT. AMAX( C,R,L,J ) ) THEN
                    DO  K = JTOP, J+1, -1
                        AMAX( C,R,L,K ) =  AMAX( C,R,L,K-1 )
                    END DO
                    AMAX( C,R,L,J ) = X
                    GO TO  11
                END IF
            END DO
            IF ( JTOP .LT. 6 ) THEN
                AMAX( C,R,L,JTOP+1 ) = X
            END IF          !  if jtop<6
11          CONTINUE

        END DO
        END DO
        END DO

    ELSE    !  not average:  sum or max

        CMAX = GRID( 1,1,1 )
        CCOL = 1
        CROW = 1
        CLAY = 1
        DO  L = 1, NLAYS
        DO  R = 1, NROWS
        DO  C = 1, NCOLS

            X = GRID( C,R,L )

            IF ( X .GT. CMAX ) THEN
                CMAX = X
                CCOL = C
                CROW = R
                CLAY = L
            END IF

            IF ( X .GT. 0.085 ) THEN
                EXC8( C,R,L ) = EXC8( C,R,L ) + 1
            END IF

            DO  J = 1, JTOP
                IF ( X .GT. AMAX( C,R,L,J ) ) THEN
                    DO  K = JTOP, J+1, -1
                        AMAX( C,R,L,K ) =  AMAX( C,R,L,K-1 )
                    END DO
                    AMAX( C,R,L,J ) = X
                    GO TO  22
                END IF
            END DO
            IF ( JTOP .LT. 6 ) THEN
                AMAX( C,R,L,JTOP+1 ) = X
            END IF          !  if jtop<6
22          CONTINUE

        END DO
        END DO
        END DO

    END IF          !  if averaging; else (sum or max)

    IF ( .NOT. WRITE3( ANAME, VNAMEO, JDATE, JTIME, GRID ) ) THEN
        MESG = 'Write failure:  file "' // TRIM( ANAME ) // '" variable "' // TRIM( VNAMEO ) // '"'
        CALL M3EXIT ( 'M3AGMAX:AGGREG', JDATE, JTIME, MESG, 2 )
    END IF

    RETURN

    !***********************************************************************
    !   begin body of entry point MAXAGG

  ENTRY MAXAGG( JDATE, JTIME, MNAME, VNAMEO )

    DO  J = 1, 6

        VNAME = TRIM( VNAMEO ) // SUFFIX( J )

        GRID = AMAX( :, :, :, J )

        IF ( .NOT. WRITE3( MNAME, VNAME, JDATE, JTIME, GRID ) ) THEN
            MESG = 'Write failure:  file "' // TRIM( MNAME ) // '" variable "' // TRIM( VNAME ) // '"'
            CALL M3EXIT ( 'M3AGMAX:AGGREG', JDATE, JTIME, MESG, 2 )
        END IF

    END DO

    VNAME = TRIM( VNAMEO ) // '_1_125'

    IF ( .NOT. WRITE3( MNAME, VNAME, JDATE, JTIME, EXC1 ) ) THEN
        MESG = 'Write failure:  file "' // TRIM( MNAME ) // '" variable "' // TRIM( VNAME ) // '"'
        CALL M3EXIT ( 'M3AGMAX:AGGREG', JDATE, JTIME, MESG, 2 )
    END IF

    VNAME = TRIM( VNAMEO ) // '_8_85'

    IF ( .NOT. WRITE3( MNAME, VNAME, JDATE, JTIME, EXC8 ) ) THEN
        MESG = 'Write failure:  file "' // TRIM( MNAME ) // '" variable "' // TRIM( VNAME ) // '"'
        CALL M3EXIT ( 'M3AGMAX:AGGREG', JDATE, JTIME, MESG, 2 )
    END IF

END SUBROUTINE  MAXREG

