
      INTEGER FUNCTION WKDAY( JDATE )
 
C***********************************************************************
C Version "$Id: wkday.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  58
C
C  RETURNS:
C	day (1=Monday, ..., 7=Sunday) of the week for the given Julian date
C
C  PRECONDITIONS REQUIRED:
C	JDATE integer of the form  1000 * YEAR +  DAY
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C	adapted  2/27/1995  by CJC from ROM 2.2 UTILIO function KDAY.FOR
C
C       Unification 2/2002 by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C***********************************************************************
 
      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

      INTEGER, INTENT(IN   ) :: JDATE	!  Julian date, YYYYDDD = 1000 * YEAR + DAY


C...........   PARAMETERS and their descriptions:

#ifdef IO_360
        INTEGER, PARAMETER :: YDAYS = 360
#endif
#ifndef IO_360
        INTEGER, PARAMETER :: YDAYS = 365
#endif

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER  YEAR	!  year 
      INTEGER  JDAY	!  Julian day-number 1 ... 365,366
      INTEGER  K	!  subexpression

                                                                                
C***********************************************************************
C   begin body of function  WKDAY

      YEAR  = JDATE / 1000
      JDAY  = MOD( JDATE, 1000 )
      K     = MAX( YEAR - 1, 0 )
      K     = K * YDAYS  +  K / 4  -  K / 100  +  K / 400  +  JDAY  -  1
      WKDAY = 1 + MOD (K, 7)
 
      RETURN
      END FUNCTION WKDAY

