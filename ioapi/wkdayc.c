
/**************************************************************************
ERSION "$Id: wkdayc.c 100 2015-01-16 16:52:16Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
	Return day (1-7) of the week for the given Julian date

PRECONDITIONS
	time coded HHMMSS  100 * ( 100 * hours + minutes ) + seconds

CALLS
	none

REVISION HISTORY
	prototype  3/1995 by Carlie J. Coats, Jr.,
        MCNC Environmental Modeling Center

        Unification 2/2002 with Global Climate Model IO_360 version,
        that uses a 360-day year
                    
**************************************************************************/

#include  "iodecl3.h"

#ifdef  IO_360
#define YEARDAYS 360
#else
#define YEARDAYS 365
#endif


int wkdayc ( int jdate )
    {
    int  year, jday, k ;
    year  = jdate ;
    jday  = jdate % 1000 ;
    k     = year - 1 ;
    k     = k * YEARDAYS  +  k / 4  -  k / 100  +  k / 400  +  jday  -  1 ;
    return  1  +  k % 7 ;

    }       /*  end body of wkdayc()  */

