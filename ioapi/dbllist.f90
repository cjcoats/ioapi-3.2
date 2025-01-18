
LOGICAL FUNCTION DBLLIST( ENAME, EDESC, NMAX, NCNT, LIST )

    !***********************************************************************
    ! Version "$Id: dbllist.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C)  2015-2016 UNC Institute for the Environment,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  57
    !
    !  RETURNS:  TRUE for success, FALSE for failure
    !            Success implies NCNT > 0 ("we actually found something")
    !
    !  PRECONDITIONS REQUIRED:
    !       setenv <logical name> <quoted, comma-delimited list of integers>
    !       string-length( <list> <= 511
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       ENVINT, M3EXIT, STR2REAL
    !
    !  REVISION  HISTORY:
    !       prototype 12/03/1998 by Carlie J. Coats, Jr., UNC IE:
    !       adapted from "reallist.f"
    !       Modified  06/2016 by CJC: bug-fixes
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: ENAME   !  environment variable for the list
    CHARACTER*(*), INTENT(IN   ) :: EDESC   !  environment variable description
    INTEGER      , INTENT(IN   ) :: NMAX    !  dimension for list
    INTEGER      , INTENT(  OUT) :: NCNT    !  actual number of entries in list
    REAL*8       , INTENT(  OUT) :: LIST( NMAX )    ! array of values found

    !...........   EXTERNAL FUNCTION:

    INTEGER, EXTERNAL :: LBLANK
    REAL*8,  EXTERNAL :: STR2DBLE

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    CHARACTER*65535 BUF     !  buffer for environment-variable value
    CHARACTER*256   MSG     !  buffer for messages
    CHARACTER*5     PREFIX    !  buffer for checking "LIST:"
    INTEGER         ISTAT   !  return status for ENVSTR
    INTEGER         L, M    !  subscript/loop counter
    INTEGER         LO, HI  !  substring bounds
    INTEGER         J, K

    !***********************************************************************
    !   begin body of function  dummy

    CALL ENVSTR( ENAME, EDESC, ' ', BUF, ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        MSG = 'Could not get environment variable "'// ENAME// '"'
        CALL M3MSG2( MSG )
        DBLLIST = .FALSE.
        RETURN
    END IF

    BUF    = ADJUSTL( BUF )
    PREFIX = BUF( 1:5 )
    CALL UPCASE( PREFIX )
    IF ( PREFIX .EQ. 'LIST:' ) THEN
        HI = 5
        LO = 6
    ELSE
        HI = 0
        LO = 1
    END IF
    DO  L = 1, NMAX
        LO = LO + LBLANK( BUF( LO: ) )
        IF ( LO .GE. 65535 )  THEN
            NCNT = L-1
            GO TO 99                !  list exhausted
        END IF
        J = INDEX( BUF(LO:), ',' )
        K = INDEX( BUF(LO:), ' ' )
        IF ( MAX( J, K ) .EQ. 0 ) THEN
            HI = 65536 - LO                   !  no more commas, blank-separators
        ELSE IF ( J .EQ. 0 ) THEN
            HI = K
        ELSE IF ( K .EQ. 0 ) THEN
            HI = J
        ELSE
            HI = MIN( J, K )
        END IF
        LIST( L ) = STR2DBLE( BUF( LO : ) )
        LO = LO + HI                !  1 col past the comma
    END DO

    IF ( LO+1 .LT. 65535 )  THEN   !  fall-through:  list done?
        IF ( BUF( LO+1: ) .NE. ' ' )  THEN
            DBLLIST = .FALSE.
            RETURN
        END IF
    END IF

99  CONTINUE        !  exit from loop
    DBLLIST = ( NCNT .GT. 0 )
    RETURN
END FUNCTION DBLLIST
