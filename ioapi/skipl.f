
       SUBROUTINE SKIPL( UNIT, NLINES )

C***********************************************************************
C Version "$Id: skipl.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  40
C
C  SUBROUTINE:  Skips NLINES number of lines in file UNIT
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 3/97 by M Houyoux for SMOKE
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C****************************************************************************

        IMPLICIT NONE

C.........  Subroutine arguments

        INTEGER, INTENT(IN   ) :: UNIT
        INTEGER, INTENT(IN   ) :: NLINES

C.........  Local variables

        INTEGER    I

C***********************************************************************
C   begin body of subroutine SKIPL


        DO I = 1, NLINES
            READ( UNIT, * )
        END DO

        RETURN

        END SUBROUTINE SKIPL
