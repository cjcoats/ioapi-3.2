/**************************************************************************
VERSION "$Id: dt2strc.c 100 2015-01-16 16:52:16Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    format and return the date and time as a character string
    "HH:MM:SS  M+ D+, YYYY"

PRECONDITIONS:
    valid Julian date YYYYDDD, time HHMMSS

CALLS:  none

REVISION HISTORY:
    Prototype 3/1995 by Carlie J Coats, Jr, MCNC Environmental Programs
    Revised   8/1999 by CJC -- uses mmddyyc(), hhmmssc()
    Revised   2/2002 by CJC -- error messages via m3msgc()
**************************************************************************/

#include  <string.h>
#include  <stdio.h>
#include  "iodecl3.h"

void   dt2strc( int         jdate ,
                int         jtime ,
                char  buffer[ 25 ] )
{

int  col ;
char mesg[256] ;

if ( jdate > 9999999 || jdate < 0 ) 
    {
    sprintf( mesg, 
             "%s %d",
             "Year-number error in dt2strc():  jdate = ", jdate ) ;
    m3mesgc( mesg ) ;
    strcpy( buffer, "<DATE&TIME ERROR>" ) ;
    return ; 
    }

if ( jtime > 999999 || jtime < 0 ) 
    {
    sprintf( mesg, 
             "%s %d",
             "Time-number error in dt2strc():  jtime = ", jtime ) ;
    m3mesgc( mesg ) ;
    strcpy( buffer, "<DATE&TIME ERROR>" ) ;
    return ; 
    }

hhmmssc( jtime, buffer ) ;
col = STRLEN( buffer ) ;
buffer[ col ] = ' ' ;
mmddyyc( jdate, buffer + col + 1 ) ;

return ; 
    
}		/** END BODY OF void dt2strc() **/

