
        LOGICAL FUNCTION DBLLIST( ENAME, EDESC, NMAX, NCNT, LIST )

C***********************************************************************
C Version "$Id: DBLLIST.f 273 2015-12-03 18:05:34Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C)  2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  55
C
C  RETURNS:  TRUE for success, FALSE for failure
C            Success implies NCNT > 0 ("we actually found something")
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <quoted, comma-delimited list of integers>
C       string-length( <list> <= 511
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       ENVINT, M3EXIT, STR2REAL
C
C  REVISION  HISTORY:
C       prototype 12/03/1998 by Carlie J. Coats, Jr., UNC IE:
C       adapted from "reallist.f"
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: ENAME   !  environment variable for the list
        CHARACTER*(*), INTENT(IN   ) :: EDESC   !  environment variable description
        INTEGER      , INTENT(IN   ) :: NMAX    !  dimension for list
        INTEGER      , INTENT(  OUT) :: NCNT    !  actual number of entries in list
        REAL*8       , INTENT(  OUT) :: LIST( NMAX )    ! array of values found    

C...........   EXTERNAL FUNCTION:

        INTEGER, EXTERNAL :: LBLANK
        REAL,    EXTERNAL :: STR2DBLE

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*65535 BUF     !  buffer for environment-variable value
        CHARACTER*256   MSG     !  buffer for messages
        CHARACTER*5     PREFIX    !  buffer for checking "LIST:"
        INTEGER         ISTAT   !  return status for ENVSTR
        INTEGER         L, M    !  subscript/loop counter
        INTEGER         LO, HI  !  substring bounds

C***********************************************************************
C   begin body of function  dummy

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
                NCNT = L
                GO TO 99                !  list exhausted
            END IF
            HI = MAX( INDEX( BUF(LO:), ',' ), INDEX( BUF(LO:), ' ' ) )
            IF ( HI .EQ. 0 ) HI = 65536 - LO                   !  no more commas, blank-separators
            LIST( L ) = STR2DBLE( BUF( LO : LO + HI - 2 ) )
            LO = LO + HI                !  1 col past the comma
        END DO

        IF ( LO+HI+1 .LT. 65535 )  THEN   !  fall-through:  list done?
           IF ( BUF( LO+HI+1: ) .NE. ' ' )  THEN
               DBLLIST = .FALSE.
               RETURN
            END IF
         END IF

99      CONTINUE        !  exit from loop
        DBLLIST = ( NCNT .GT. 0 )
        RETURN
        END FUNCTION DBLLIST
