
 SUBROUTINE  PAIRSTEP( NCOLS,  NROWS,  NLAYS,               &
                       CLO, CHI, RLO, RHI, LLO, LHI,        &
                       JDATEA, JTIMEA, JDATEB, JTIMEB,      &
                       NAMEA,  NAMEB, WNAMES, WTYPES, RDEV )

    !!***********************************************************************
    !! Version "$Id: pairstep.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line  84
    !!
    !!  FUNCTION:
    !!       Statistics report to RDEV on variables WNAMES( 1,* ) and
    !!       WNAMES( 2,* ) from files NAMEA and NAMEB, respectively,
    !!       and on the results of using GRIDOPS to apply the operations
    !!       OPNAME( * ) to them.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Valid dates and times JDATE:JTIME
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       Models-3 I/O:  M3EXIT(), READ3()
    !!       Utility routines:  DT2STR(), TRIMLEN()
    !!
    !!  REVISION  HISTORY:
    !!       Prototype 3/93 by CJC
    !!       Version  11/94 by CJC for new version of I/O API
    !!       Modified  9/99 by CJC for enhanced portability
    !!       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!       INTENT, USE M3UTILIO, and related changes.
    !!       Version  01/2015 by CJC for I/O API v3.2:  F90 free-format source
    !!***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !!...........   ARGUMENTS and their descriptions:

    INTEGER     , INTENT( IN ) :: NCOLS   ! grid dimensions, from file header
    INTEGER     , INTENT( IN ) :: NROWS   ! grid dimensions, from file header
    INTEGER     , INTENT( IN ) :: NLAYS   ! grid dimensions, from file header
    INTEGER     , INTENT( IN ) :: CLO     ! bottom of col-range
    INTEGER     , INTENT( IN ) :: CHI     ! top of    col-range
    INTEGER     , INTENT( IN ) :: RLO     ! bottom of row-range
    INTEGER     , INTENT( IN ) :: RHI     ! top of    row-range
    INTEGER     , INTENT( IN ) :: LLO     ! bottom of layer-range
    INTEGER     , INTENT( IN ) :: LHI     ! top of    layer-range
    INTEGER     , INTENT( IN ) :: JDATEA  ! current model date, file A
    INTEGER     , INTENT( IN ) :: JTIMEA  ! current model time, file A
    INTEGER     , INTENT( IN ) :: JDATEB  ! current model date, file B
    INTEGER     , INTENT( IN ) :: JTIMEB  ! current model time, file B
    CHARACTER*16, INTENT( IN ) :: NAMEA   !  logical name of the input file
    CHARACTER*16, INTENT( IN ) :: NAMEB   !  logical name of the input file
    CHARACTER*16, INTENT( IN ) :: WNAMES( 2 ) !  list of vble names
    INTEGER     , INTENT( IN ) :: WTYPES( 2 ) !  list of vble types
    INTEGER     , INTENT( IN ) :: RDEV    ! unit number for output


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    REAL             GRID1( NCOLS, NROWS, NLAYS )
    REAL             GRID2( NCOLS, NROWS, NLAYS )
    DOUBLE PRECISION DBLE1( NCOLS, NROWS, NLAYS )
    DOUBLE PRECISION DBLE2( NCOLS, NROWS, NLAYS )
    INTEGER          INTG1( NCOLS, NROWS, NLAYS )
    INTEGER          INTG2( NCOLS, NROWS, NLAYS )

    LOGICAL         FLAG1, FLAG2

    INTEGER         C, R, L      !  col, row, level, variable, counters
    CHARACTER*256   MESG

    INTEGER         SIZE


    !!***********************************************************************
    !!   begin body of subroutine  PAIRSTEP

    SIZE = NCOLS * NROWS * NLAYS

    IF ( WTYPES( 1 ) .EQ. M3REAL ) THEN

        FLAG1 = READ3( NAMEA,  WNAMES( 1 ), ALLAYS3, JDATEA, JTIMEA, GRID1 )

    ELSE IF ( WTYPES( 1 ) .EQ. M3DBLE ) THEN

        FLAG1 = READ3( NAMEA,  WNAMES( 1 ), ALLAYS3, JDATEA, JTIMEA, DBLE1 )
        CALL DBLE2REAL( SIZE, DBLE1, GRID1 )

    ELSE IF ( WTYPES( 1 ) .EQ. M3INT ) THEN

        FLAG1 = READ3( NAMEA,  WNAMES( 1 ), ALLAYS3, JDATEA, JTIMEA, INTG1 )
        CALL INTG2REAL( SIZE, INTG1, GRID1 )

    ELSE

        FLAG1 = .FALSE.

    END IF

    IF ( WTYPES( 2 ) .EQ. M3REAL ) THEN

        FLAG2 = READ3( NAMEB,  WNAMES( 2 ), ALLAYS3, JDATEA, JTIMEA, GRID2 )

    ELSE IF ( WTYPES( 2 ) .EQ. M3DBLE ) THEN

        FLAG2 = READ3( NAMEB,  WNAMES( 2 ), ALLAYS3, JDATEA, JTIMEA, DBLE2 )
        CALL DBLE2REAL( SIZE, DBLE2, GRID2 )

    ELSE IF ( WTYPES( 2 ) .EQ. M3INT ) THEN

        FLAG2 = READ3( NAMEB,  WNAMES( 2 ), ALLAYS3, JDATEA, JTIMEA, INTG2 )
        CALL INTG2REAL( SIZE, INTG2, GRID2 )

    ELSE

        FLAG2 = .FALSE.

    END IF

    IF ( .NOT. FLAG1 ) THEN

        MESG = 'Read failure:  file ' // NAMEA // ' variable ' // WNAMES( 1 )
        CALL M3WARN( 'M3PAIR:PAIRSTEP', JDATEA, JTIMEA, MESG )

    ELSE IF ( .NOT. FLAG2 ) THEN

        MESG = 'Read failure:  file ' // NAMEB // ' variable ' // WNAMES( 2 )
        CALL M3WARN( 'M3PAIR:PAIRSTEP', JDATEB, JTIMEB, MESG )

    ELSE            !  both reads OK

        DO  L = LLO, LHI   !  3-D traversal:  selected layers
        DO  R = RLO, RHI
        DO  C = CLO, CHI
            WRITE( RDEV, 93010 ) GRID1( C,R,L ), GRID2( C,R,L )
        END DO
        END DO
        END DO

    END IF          !  if !flag1, else if !flag2, else...

    RETURN

    !!******************  FORMAT  STATEMENTS   ******************************
    !!...........   Formatted-file formats... 93xxx

93010   FORMAT ( 1X, 1PE15.7, ',', 1X, 1Pe15.7 )

END  SUBROUTINE  PAIRSTEP

