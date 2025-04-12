
/************************************************************************
    Version "$Id: gisio.c 279 2025-04-12 15:33:31Z coats $
    LSMTOOLS.  Copyright (C) 2003 Baron Advanced Meteorological Systems
    and (c) 2025 Carlie J. Coats, Jr.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.
.........................................................................
    DESCRIPTION:
        C functions to read and write (optionally GZIPped) GRIDFLOAT
        and 8-bit-BIL files
        - called from FORTRAN:

        gfread,  gfzead
        gfwrite, gfzwrite
        bilread, bil2read, bil4read, zbilread, zbil2read, zbil4read
        bilwrite, bil2write, bil4write, zbilwrite, zbil2write, zbil4write

    PRECONDITIONS REQUIRED:
        "long" and "float" same as FINT and FREAL

    ARGUMENTS:
        gfile:         file path-name (padded Fortran string)
        ncols, nrows:  number of "float" cols/rows to read
        swapb:  0:     native byte order; else call "htonl()"

    REVISION  HISTORY:
        Prototype 2/2008 by Carlie J. Coats, Jr., BAMS

        Version   5/2010 by CJC:  add *BIL2*(), BIL4*()

        Version   4/2025 by CJC for I/O API 4.0.  Indentation fix-up;
        eliminate [scl|tbl]tobil*(), etc.
************************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/uio.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <zlib.h>

#include "iodecl3.h"

#define BUFLEN  ((FSTR_L)512)


#if defined (ABSFT)

#define gfread    GFREAD
#define gfwrite   GFWRITE

#define gfzread   GFZREAD
#define gfzwrite  GFZWRITE

#define bilread   BILREAD
#define bilwrite  BILWRITE

#define zbilread  ZBILREAD
#define zbilwrite ZBILWRITE

#define bil2read   BIL2READ
#define bil2write  BIL2WRITE

#define zbil2read  ZBIL2READ
#define zbil2write ZBIL2WRITE

#define bil4read   BIL4READ
#define bil4write  BIL4WRITE

#define zbil4read  ZBIL4READ
#define zbil4write ZBIL4WRITE

#elif defined (FLDMN) || defined (SGI) || defined (__sgi) || defined (SOLARIS) || defined (SUN) || defined (DEC) || defined (OSF1) || defined (VPP) || defined (HP) || defined (LINUX)

#define gfread    gfread_
#define gfwrite   gfwrite_

#define gfzread   gfzread_
#define gfzwrite  gfzwrite_

#define bilread   bilread_
#define bilwrite  bilwrite_

#define zbilread  zbilread_
#define zbilwrite zbilwrite_

#define bil2read   bil2read_
#define bil2write  bil2write_

#define zbil2read  zbil2read_
#define zbil2write zbil2write_

#define bil4read   bil4read_
#define bil4write  bil4write_

#define zbil4read  zbil4read_
#define zbil4write zbil4write_

#elif defined(__hpux) || defined(_AIX) || defined(MACH)

        /*  do nothing!  */
        /*  these use lower-case name mangling, Feldman-style strings */

#else

#error   "Error compiling envgets.c:  unsupported architecture"

#endif

static uint32_t * ubuf ;
static size_t     usiz = 0 ;

static uint16_t * vbuf ;
static size_t     vsiz = 0 ;


/** -------------------------------------------------------------- **/
/** Read the data record from a GRIDFLOAT file                     **/
/** -------------------------------------------------------------- **/

FINT gfread( const char * gfile,
             const FINT * ncols,
             const FINT * nrows,
             const FINT * swapb,
             uint32_t     gbuf[],
             FSTR_L       gfilelen  )
    {
    char           eqname[BUFLEN] ;
    unsigned long  nsize ;
    unsigned long  nread ;
    FILE   * fp ;
    char     mesg[ 256 ] ;
    int      i ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** GFREAD:  bad filename argument" ) ;
         return 0 ;
         }

     fp = fopen( eqname, "r" );
     if ( fp )
         {
         sprintf( mesg, "GFREAD:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** GFREAD:  fopen() failure" ) ;
         return 0 ;
         } ;

    nsize = (size_t) *ncols * (size_t) *nrows ;
    nread = fread ( (void *) gbuf, sizeof( float ), nsize, fp );
    if ( nread != nsize )
        {
        sprintf( mesg,
                 "*** GFREAD:  fread() failure:  nsize=%d, nread=%d",
                 (int)nsize, (int)nread ) ;
        m3mesgc( mesg ) ;
        fclose( fp ) ;
        return 0 ;
        }
    else if ( *swapb )
        {
        for ( i = 0 ; i < nsize ; i++ )
            {
            gbuf[i] = ntohl( gbuf[i] );
            }
        }

    if ( EOF == fclose( fp ) )
        {
        sprintf( mesg, "*** GFREAD: fclose() failure for %s", eqname ) ;
        m3mesgc( mesg ) ;
        }
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Read the data record from a GZIPped-GRIDFLOAT file             **/
/** -------------------------------------------------------------- **/

FINT gfzread( const char * gfile,
              const FINT * ncols,
              const FINT * nrows,
              const FINT * swapb,
              uint32_t     gbuf[],
              FSTR_L       gfilelen  )
    {
    char       eqname[BUFLEN] ;
    size_t     ngrid, n ;
    unsigned   nsize,  nread, i ;
    FILE     * fp ;
    char       mesg[ 640 ] ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** GFZREAD:  bad filename argument" ) ;
         return 0 ;
         }

     fp = gzopen( eqname, "rb" );
     if ( fp )
         {
         sprintf( mesg, "GFZREAD:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** GFZREAD:  gzopen() failure" ) ;
         return 0 ;
         } ;

    ngrid = (size_t) *ncols * (size_t) *nrows ;
    nsize = (unsigned) (sizeof( uint32_t ) ) * ngrid ;
    nread = gzread ( fp, (voidp) fp, nsize );
    if ( nread != nsize )
        {
        sprintf( mesg,
                 "*** GFZREAD:  gzread() failure:  nsize=%d, nread=%d",
                 (int)nsize, (int)nread ) ;
        m3mesgc( mesg ) ;
        gzclose( fp ) ;
        return 0 ;
        } ;

    if ( *swapb )
        {
        for ( n = 0 ; n < ngrid ; n++ )
            {
            gbuf[n] = ntohl( gbuf[n] );
            }
        }

    if ( EOF == gzclose( fp ) )
        {
        sprintf( mesg, "*** GFZREAD: gzclose() failure for %s", eqname ) ;
        m3mesgc( mesg ) ;
        return( 0 ) ;
        }
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Write a data record to a GRIDFLOAT file                        **/
/** -------------------------------------------------------------- **/

FINT gfwrite( const char *   gfile,
              const FINT * ncols,
              const FINT * nrows,
              const FINT *   swapb,
              const uint32_t gbuf[],
              FSTR_L         gfilelen  )
    {
    char           eqname[BUFLEN] ;
    unsigned long  nsize, ngrid ;
    unsigned long  nwrit ;
    FILE   * fp ;
    char     mesg[ 256 ] ;
    int      i, n ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** GFWRITE:  bad filename argument" ) ;
         return 0 ;
         }

     fp = fopen( eqname, "w" );
     if ( fp )
         {
         sprintf( mesg, "GFWRITE:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** GFWRITE:  fopen() failure" ) ;
         return 0 ;
         } ;

    nsize = (size_t) *ncols * (size_t) *nrows ;

    if ( *swapb )
        {

        if ( usiz == 0 )
            {
            ubuf = malloc( nsize * sizeof( float ) ) ;
            if ( ubuf )
                {
                usiz = nsize ;
                }
            else{
                 m3mesgc( "*** GFWRITE:  malloc() failure" ) ;
                 return 0 ;
                }
            }
        else if ( usiz < nsize )
            {
            ubuf = realloc( (void *)ubuf , nsize * sizeof( float ) ) ;
            if ( ubuf )
                {
                usiz = nsize ;
                }
            else{
                 m3mesgc( "*** GFWRITE:  realloc() failure" ) ;
                 return 0 ;
                }
            }

        for ( i = 0, n = nsize ; i < n ; i++ )
            {
            ubuf[i] = ntohl( gbuf[i] );
            }

        nwrit = fwrite( (void *) ubuf, sizeof( float ), nsize, fp );
        if ( nwrit != nsize )
            {
            m3mesgc( "*** GFWRITE:  fwrite() failure" ) ;
            return 0 ;
            }
        }

    else{
        nwrit = fwrite( (void *) gbuf, sizeof( float ), nsize, fp );
        if ( nwrit != nsize )
            {
            m3mesgc( "*** GFWRITE:  fwrite() failure" ) ;
            return 0 ;
            }
        }

    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Write a data record to a GZIPped-GRIDFLOAT file                **/
/** -------------------------------------------------------------- **/

FINT gfzwrite( const char   * gfile,
               const FINT   * ncols,
               const FINT   * nrows,
               const FINT   * swapb,
               const uint32_t gbuf[],
               FSTR_L         gfilelen  )
    {
    char        eqname[BUFLEN] ;
    size_t      ngrid, i, m, n ;
    unsigned    nsize, nwrit ;
    FILE      * fp ;
    char        mesg[ 512 ] ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** GFZWRITE:  bad filename argument" ) ;
         return 0 ;
         }

     fp = gzopen( eqname, "wb9" );
     if ( fp )
         {
         sprintf( mesg, "GFZWRITE:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** GFZWRITE:  gzopen() failure" ) ;
         return 0 ;
         } ;

    ngrid = (unsigned) (size_t) (size_t) *ncols * (size_t) *nrows ;
    nsize = (unsigned) ngrid * sizeof( uint32_t )  ;

    if ( *swapb )
        {
         if ( usiz == 0 )
            {
            ubuf = malloc( nsize * sizeof( uint32_t ) ) ;
            if ( ubuf )
                {
                usiz = nsize ;
                }
            else{
                 m3mesgc( "*** GFZWRITE:  malloc() failure" ) ;
                 return 0 ;
                }
            }
        else if ( usiz < nsize )
            {
            ubuf = realloc( (void *)ubuf , nsize * sizeof( uint32_t ) ) ;
            if ( ubuf )
                {
                usiz = nsize ;
                }
            else{
                 m3mesgc( "*** GFZWRITE:  realloc() failure" ) ;
                 return 0 ;
                }
            }
	for ( i = 0 ; i < nsize ; i++ )
            {
            ubuf[ i ] = ntohl( gbuf[ i ] );
            }
        nwrit = gzwrite( fp, (voidp) ubuf, nsize );
        }
    else{
        nwrit = gzwrite( fp, (voidp) gbuf, nsize );
        }

    if ( nwrit != nsize )
        {
        m3mesgc( "*** GFZWRITE:  fwrite() failure" ) ;
        gzclose( fp ) ;
        return 0 ;
        }

    if ( EOF == gzclose( fp ) )
        {
        m3mesgc( "*** GFZWRITE: gzclose() failure" ) ;
        return( 0 ) ;
        }

    return 1 ;
    }




/** -------------------------------------------------------------- **/
/** read a data record from a BIL-8 file                    **/
/** -------------------------------------------------------------- **/

FINT bilread( const char * bilfile, 
              const FINT * ncols,
              const FINT * nrows,       /* dimensions for BIL[]  */
              const FINT * ibil,        /* BIL-format data  */
              FSTR_L       billen       /* string-length for bilfile */
              )
    {
    char     eqname[BUFLEN] ;
    size_t   nsize ;
    size_t   nread ;
    FILE   * fp ;
    char     mesg[ 512 ] ;

    name2cstr( bilfile, eqname, billen, BUFLEN ) ;
    if ( ! eqname[0] )
        {
        m3mesgc( "*** BILREAD:  bad bilfile argument" ) ;
        return 0 ;
        }

     fp = fopen( eqname, "rb" );
     if ( fp )
        {
        sprintf( mesg, "BILREAD:  File opened: %s", eqname ) ;
        m3mesgc( mesg ) ;
        }
     else
        {
        sprintf( mesg, "*** BILREAD:  fopen() failure for  %s", eqname ) ;
        m3mesgc( mesg ) ;
        return 0 ;
        } ;

    nsize = (size_t) *ncols * (size_t) *nrows ;
    nread = fread( (void*) ibil, (size_t)1, nsize, fp );
    if ( nread != nsize )
        {
        sprintf( mesg, "*** BILREAD:  file %s", eqname ) ;
        m3mesgc( mesg ) ;
        sprintf( mesg, "fread()=%ld not %ld\0", (long)nread, (long)nsize ) ;
        m3mesgc( mesg ) ;
        fclose( fp ) ;
        return 0 ;
        } ;

    if ( EOF == fclose( fp ) )
        {
        sprintf( mesg, "*** BILREAD: fclose() failure for %s", eqname ) ;
        m3mesgc( mesg ) ;
        return 0 ;
        } ;
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Read ibil[(*ngrid+3)/4] from gzipped file bilfile              **/
/** -------------------------------------------------------------- **/

FINT zbilread( const char * bilfile, 
               const FINT * ncols,
               const FINT * nrows,    	/* dimensions for BIL[]  */
               const FINT * ibil,       /* BIL-format data  */
               FSTR_L       billen      /* string-length for bilfile */
               )
    {
    char            eqname[BUFLEN] ;
    size_t          ngrid, n ;
    unsigned        nsize, nread ;
    gzFile        * fp ;
    char            mesg[ 512 ] ;

    name2cstr( bilfile, eqname, billen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** ZBILREAD:  bad bilfile argument" ) ;
         return 0 ;
         }

     fp = gzopen( eqname, "rb" );
     if ( fp )
        {
        sprintf( mesg, "ZBILREAD:  File opened: %s", eqname ) ;
        m3mesgc( mesg ) ;
        }
     else
        {
        sprintf( mesg, "*** ZBILREAD:  fopen() failure for  %s", eqname ) ;
        m3mesgc( mesg ) ;
        return 0 ;
        } ;

    ngrid = (size_t) *ncols * (size_t) *nrows ;
    nsize = (unsigned) ngrid ;
    nread = gzread ( fp, (void *) ibil, nsize );
    if ( nread != nsize )
        {
        sprintf( mesg, "*** ZBILREAD:  file %s", eqname ) ;
        m3mesgc( mesg ) ;
        sprintf( mesg, "gzread()=%ld not %ld\0", (long) nread, (long) nsize ) ;
        m3mesgc( mesg ) ;
        gzclose( fp ) ;
        return 0 ;
        } ;

    if ( EOF == gzclose( fp ) )
        {
        sprintf( mesg, "*** ZBILREAD: gzclose() failure for %s", eqname ) ;
        m3mesgc( mesg ) ;
        return( 0 ) ;
        }
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** write a data record to a BIL-8 file                            **/
/** -------------------------------------------------------------- **/

FINT bilwrite( const char * bilfile,
               const FINT * ncols,
               const FINT * nrows,       /* dimensions for BIL[]  */
               const FINT   ibil[],      /* BIL-format data  */
               FSTR_L       billen       /* string-length for bilfile */
               )
    {
    char    eqname[BUFLEN] ;
    char    mesg[ 640 ] ;
    size_t  nsize ;
    size_t  nwrite ;
    FILE  * fp ;

    name2cstr( bilfile, eqname, billen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** BILWRITE:  bad bilfile argument" ) ;
         return 0 ;
         }

     fp = fopen( eqname, "wb" );
     if ( fp )
         {
         sprintf( mesg, "BILWRITE:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         sprintf( mesg, "*** BILWRITE:  fopen() failure for  %s", eqname ) ;
         m3mesgc( mesg ) ;
         return 0 ;
         } ;

    nsize  = (size_t) *ncols * (size_t) *nrows ;
    nwrite = fwrite ( (void *) ibil, (size_t)1, nsize, fp );
    if ( nwrite != nsize )
        {
        m3mesgc( "*** BILWRITE:  fwrite() failure" ) ;
        fclose( fp ) ;
        return 0 ;
        } ;

    if ( EOF == fclose( fp ) )
        {
        m3mesgc( "*** BILWRITE: fclose() failure" ) ;
        return( 0 ) ;
        }
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** write a data record to a GZIPped-BIL-8 file                    **/
/** -------------------------------------------------------------- **/

FINT zbilwrite( const char * bilfile,
                const FINT * ncols,
                const FINT * nrows,       /* dimensions for BIL[]  */
                const FINT   ibil[],      /* BIL-format data  */
                FSTR_L       billen       /* string-length for bilfile */
                )
    {
    char             eqname[BUFLEN] ;
    char             mesg[ 640 ] ;
    size_t           ngrid, n ;
    unsigned         nsize, nwrite ;
    gzFile         * fp ;
    unsigned char  * cptr ;

    name2cstr( bilfile, eqname, billen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** ZBILWRITE:  bad bilfile argument" ) ;
         return 0 ;
         }

     fp = gzopen( eqname, "wb9" );
     if ( fp )
         {
         sprintf( mesg, "ZBILWRITE:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** ZBILWRITE:  gzopen() failure" ) ;
         return 0 ;
         } ;

    ngrid  = (size_t) *ncols * (size_t) *nrows ;
    nsize  = (unsigned) ngrid ;
    nwrite = gzwrite ( fp, (void *) ibil, nsize );
    if ( nwrite != nsize )
        {
        m3mesgc( "*** ZBILWRITE:  gzwrite() failure" ) ;
        gzclose( fp ) ;
        return 0 ;
        } ;

    if ( EOF == gzclose( fp ) )
        {
        m3mesgc( "*** ZBILWRITE: gzclose() failure" ) ;
        return( 0 ) ;
        }
    return 1 ;
    }




/** -------------------------------------------------------------- **/
/** Read the data record from a INT16 file                         **/
/** -------------------------------------------------------------- **/

FINT bil2read( const char * gfile,
               const FINT * ncols,
               const FINT * nrows,
               const FINT * swapb,
               uint16_t     gbuf[],
               FSTR_L       gfilelen  )
    {
    char           eqname[BUFLEN] ;
    unsigned long  nsize ;
    unsigned long  nread ;
    FILE   * fp ;
    char     mesg[ 256 ] ;
    int      i ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** BIL2READ:  bad filename argument" ) ;
         return 0 ;
         }

     fp = fopen( eqname, "r" );
     if ( fp )
         {
         sprintf( mesg, "BIL2READ:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** BIL2READ:  fopen() failure" ) ;
         return 0 ;
         } ;

    nsize = (size_t) *ncols * (size_t) *nrows ;
    nread = fread ( (void *) gbuf, sizeof( uint16_t ), nsize, fp );
    if ( nread != nsize )
        {
        sprintf( mesg,
                 "*** BIL2READ:  fread() failure:  nsize=%d, nread=%d",
                 (int)nsize, (int)nread ) ;
        m3mesgc( mesg ) ;
        fclose( fp ) ;
        return 0 ;
        }
    else if ( *swapb )
        {
        for ( i = 0 ; i < nsize ; i++ )
            {
            gbuf[i] = ntohs( gbuf[i] );
            }
        }

    if ( EOF == fclose( fp ) )
        {
        sprintf( mesg, "*** BIL2READ: fclose() failure for %s", eqname ) ;
        m3mesgc( mesg ) ;
        }
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Read the data record from a GZIPped-INT16 file                 **/
/** -------------------------------------------------------------- **/

FINT zbil2read( const char * gfile,
        	const FINT * ncols,
        	const FINT * nrows,
        	const FINT * swapb,
        	uint16_t     gbuf[],
        	FSTR_L       gfilelen  )
    {
    char       eqname[BUFLEN] ;
    size_t     ngrid, n ;
    unsigned   nsize,  nread, i ;
    FILE     * fp ;
    char       mesg[ 640 ] ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** ZBIL2READ:  bad filename argument" ) ;
         return 0 ;
         }

     fp = gzopen( eqname, "rb" );
     if ( fp )
         {
         sprintf( mesg, "ZBIL2READ:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** ZBIL2READ:  gzopen() failure" ) ;
         return 0 ;
         } ;

    ngrid = (size_t) *ncols * (size_t) *nrows ;
    nsize = (unsigned) (sizeof( uint16_t ) ) * ngrid ;
    nread = gzread ( fp, (voidp) fp, nsize );
    if ( nread != nsize )
        {
        sprintf( mesg,
                 "*** ZBIL2READ:  gzread() failure:  nsize=%d, nread=%d",
                 (int)nsize, (int)nread ) ;
        m3mesgc( mesg ) ;
        gzclose( fp ) ;
        return 0 ;
        } ;

    if ( *swapb )
        {
        for ( n = 0 ; n < ngrid ; n++ )
            {
            gbuf[n] = ntohs( gbuf[n] );
            }
        }

    if ( EOF == gzclose( fp ) )
        {
        sprintf( mesg, "*** ZBIL2READ: gzclose() failure for %s", eqname ) ;
        m3mesgc( mesg ) ;
        return( 0 ) ;
        }
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Write a data record to a INT16 file                        **/
/** -------------------------------------------------------------- **/

FINT bil2write( const char *   gfile,
        	const FINT * ncols,
        	const FINT * nrows,
        	const FINT *   swapb,
        	const uint16_t gbuf[],
        	FSTR_L         gfilelen  )
    {
    char           eqname[BUFLEN] ;
    unsigned long  nsize, ngrid ;
    unsigned long  nwrit ;
    FILE   * fp ;
    char     mesg[ 256 ] ;
    int      i, n ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** BIL2WRITE:  bad filename argument" ) ;
         return 0 ;
         }

     fp = fopen( eqname, "w" );
     if ( fp )
         {
         sprintf( mesg, "BIL2WRITE:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** BIL2WRITE:  fopen() failure" ) ;
         return 0 ;
         } ;

    nsize = (size_t) *ncols * (size_t) *nrows ;

    if ( *swapb )
        {

        if ( vsiz == 0 )
            {
            vbuf = malloc( nsize * sizeof( uint16_t ) ) ;
            if ( vbuf )
                {
                vsiz = nsize ;
                }
            else{
                 m3mesgc( "*** BIL2WRITE:  malloc() failure" ) ;
                 return 0 ;
                }
            }
        else if ( vsiz < nsize )
            {
            vbuf = realloc( (void *)vbuf , nsize * sizeof( uint16_t ) ) ;
            if ( vbuf )
                {
                vsiz = nsize ;
                }
            else{
                 m3mesgc( "*** BIL2WRITE:  realloc() failure" ) ;
                 return 0 ;
                }
            }

        for ( i = 0, n = nsize ; i < n ; i++ )
            {
            vbuf[i] = ntohs( gbuf[i] );
            }

       nwrit = fwrite( (void *) vbuf, sizeof( uint16_t ), nsize, fp );
        if ( nwrit != nsize )
            {
            m3mesgc( "*** BIL2WRITE:  fwrite() failure" ) ;
            return 0 ;
            }
        }

    else{
        nwrit = fwrite( (void *) gbuf, sizeof( uint16_t ), nsize, fp );
        if ( nwrit != nsize )
            {
            m3mesgc( "*** BIL2WRITE:  fwrite() failure" ) ;
            return 0 ;
            }
        }

    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Write a data record to a GZIPped-INT16 file                **/
/** -------------------------------------------------------------- **/

FINT zbil2write( const char   * gfile,
        	 const FINT   * ncols,
        	 const FINT   * nrows,
        	 const FINT   * swapb,
        	 const uint16_t gbuf[],
        	 FSTR_L         gfilelen  )
    {
    char        eqname[BUFLEN] ;
    size_t      ngrid, i, m, n ;
    unsigned    nsize, nwrit ;
    FILE      * fp ;
    char        mesg[ 512 ] ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** ZBIL2WRITE:  bad filename argument" ) ;
         return 0 ;
         }

     fp = gzopen( eqname, "wb9" );
     if ( fp )
         {
         sprintf( mesg, "ZBIL2WRITE:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** ZBIL2WRITE:  gzopen() failure" ) ;
         return 0 ;
         } ;

    ngrid = (unsigned) (size_t) (size_t) *ncols * (size_t) *nrows ;
    nsize = (unsigned) ngrid * sizeof( uint16_t )  ;

    if ( *swapb )
        {
         if ( vsiz == 0 )
            {
            vbuf = malloc( nsize * sizeof( uint16_t ) ) ;
            if ( vbuf )
                {
                vsiz = nsize ;
                }
            else{
                 m3mesgc( "*** ZBIL2WRITE:  malloc() failure" ) ;
                 return 0 ;
                }
            }
        else if ( vsiz < nsize )
            {
            vbuf = realloc( (void *)vbuf , nsize * sizeof( uint16_t ) ) ;
            if ( vbuf )
                {
                vsiz = nsize ;
                }
            else{
                 m3mesgc( "*** ZBIL2WRITE:  realloc() failure" ) ;
                 return 0 ;
                }
            }
	for ( i = 0 ; i < nsize ; i++ )
            {
            vbuf[ i ] = ntohs( gbuf[ i ] );
            }
        nwrit = gzwrite( fp, (voidp) vbuf, nsize );
        }
    else{
        nwrit = gzwrite( fp, (voidp) gbuf, nsize );
        }

    if ( nwrit != nsize )
        {
        m3mesgc( "*** ZBIL2WRITE:  fwrite() failure" ) ;
        gzclose( fp ) ;
        return 0 ;
        }

    if ( EOF == gzclose( fp ) )
        {
        m3mesgc( "*** ZBIL2WRITE: gzclose() failure" ) ;
        return( 0 ) ;
        }

    return 1 ;
    }





/** -------------------------------------------------------------- **/
/** Read the data record from a INT32 file                         **/
/** -------------------------------------------------------------- **/

FINT bil4read( const char * gfile,
               const FINT * ncols,
               const FINT * nrows,
               const FINT * swapb,
               uint32_t     gbuf[],
               FSTR_L       gfilelen  )
    {
    char           eqname[BUFLEN] ;
    unsigned long  nsize ;
    unsigned long  nread ;
    FILE   * fp ;
    char     mesg[ 512 ] ;
    int      i ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** BIL4READ:  bad filename argument" ) ;
         return 0 ;
         }

     fp = fopen( eqname, "r" );
     if ( fp )
         {
         sprintf( mesg, "BIL4READ:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** BIL4READ:  fopen() failure" ) ;
         return 0 ;
         } ;

    nsize = (size_t) *ncols * (size_t) *nrows ;
    nread = fread ( (void *) gbuf, sizeof( uint32_t ), nsize, fp );
    if ( nread != nsize )
        {
        sprintf( mesg,
                 "*** BIL4READ:  fread() failure:  nsize=%d, nread=%d",
                 (int)nsize, (int)nread ) ;
        m3mesgc( mesg ) ;
        fclose( fp ) ;
        return 0 ;
        }
    else if ( *swapb )
        {
        for ( i = 0 ; i < nsize ; i++ )
            {
            gbuf[i] = ntohl( gbuf[i] );
            }
        }

    if ( EOF == fclose( fp ) )
        {
        sprintf( mesg, "*** BIL4READ: fclose() failure for %s", eqname ) ;
        m3mesgc( mesg ) ;
        }
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Read the data record from a GZIPped-INT32 file                 **/
/** -------------------------------------------------------------- **/

FINT zbil4read( const char * gfile,
        	const FINT * ncols,
        	const FINT * nrows,
        	const FINT * swapb,
        	uint32_t     gbuf[],
        	FSTR_L       gfilelen  )
    {
    char       eqname[BUFLEN] ;
    size_t     ngrid, n ;
    unsigned   nsize,  nread, i ;
    FILE     * fp ;
    char       mesg[ 640 ] ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** ZBIL4READ:  bad filename argument" ) ;
         return 0 ;
         }

     fp = gzopen( eqname, "rb" );
     if ( fp )
         {
         sprintf( mesg, "ZBIL4READ:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** ZBIL4READ:  gzopen() failure" ) ;
         return 0 ;
         } ;

    ngrid = (size_t) *ncols * (size_t) *nrows ;
    nsize = (unsigned) (sizeof( uint32_t ) ) * ngrid ;
    nread = gzread ( fp, (voidp) fp, nsize );
    if ( nread != nsize )
        {
        sprintf( mesg,
                 "*** ZBIL4READ:  gzread() failure:  nsize=%d, nread=%d",
                 (int)nsize, (int)nread ) ;
        m3mesgc( mesg ) ;
        gzclose( fp ) ;
        return 0 ;
        } ;

    if ( *swapb )
        {
        for ( n = 0 ; n < ngrid ; n++ )
            {
            gbuf[n] = ntohl( gbuf[n] );
            }
        }

    if ( EOF == gzclose( fp ) )
        {
        sprintf( mesg, "*** ZBIL4READ: gzclose() failure for %s", eqname ) ;
        m3mesgc( mesg ) ;
        return( 0 ) ;
        }
    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Write a data record to a INT32 file                        **/
/** -------------------------------------------------------------- **/

FINT bil4write( const char *   gfile,
        	const FINT *   ncols,
        	const FINT *   nrows,
        	const FINT *   swapb,
        	const uint32_t gbuf[],
        	FSTR_L         gfilelen  )
    {
    char           eqname[BUFLEN] ;
    unsigned long  nsize, ngrid ;
    unsigned long  nwrit ;
    FILE   * fp ;
    char     mesg[ 256 ] ;
    int      i, n ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** BIL4WRITE:  bad filename argument" ) ;
         return 0 ;
         }

     fp = fopen( eqname, "w" );
     if ( fp )
         {
         sprintf( mesg, "BIL4WRITE:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** BIL4WRITE:  fopen() failure" ) ;
         return 0 ;
         } ;

    nsize = (size_t) *ncols * (size_t) *nrows ;

    if ( *swapb )
        {

        if ( vsiz == 0 )
            {
            vbuf = malloc( nsize * sizeof( uint32_t ) ) ;
            if ( vbuf )
                {
                vsiz = nsize ;
                }
            else{
                 m3mesgc( "*** BIL4WRITE:  malloc() failure" ) ;
                 return 0 ;
                }
            }
        else if ( vsiz < nsize )
            {
            vbuf = realloc( (void *)vbuf , nsize * sizeof( uint32_t ) ) ;
            if ( vbuf )
                {
                vsiz = nsize ;
                }
            else{
                 m3mesgc( "*** BIL4WRITE:  realloc() failure" ) ;
                 return 0 ;
                }
            }

        for ( i = 0, n = nsize ; i < n ; i++ )
            {
            vbuf[i] = ntohl( gbuf[i] );
            }

       nwrit = fwrite( (void *) vbuf, sizeof( uint32_t ), nsize, fp );
        if ( nwrit != nsize )
            {
            m3mesgc( "*** BIL4WRITE:  fwrite() failure" ) ;
            return 0 ;
            }
        }

    else{
        nwrit = fwrite( (void *) gbuf, sizeof( uint32_t ), nsize, fp );
        if ( nwrit != nsize )
            {
            m3mesgc( "*** BIL2WRITE:  fwrite() failure" ) ;
            return 0 ;
            }
        }

    return 1 ;
    }


/** -------------------------------------------------------------- **/
/** Write a data record to a GZIPped-INT32 file                **/
/** -------------------------------------------------------------- **/

FINT zbil4write( const char   * gfile,
        	 const FINT   * ncols,
        	 const FINT   * nrows,
        	 const FINT   * swapb,
        	 const uint32_t gbuf[],
        	 FSTR_L         gfilelen  )
    {
    char        eqname[BUFLEN] ;
    size_t      ngrid, i, m, n ;
    unsigned    nsize, nwrit ;
    FILE      * fp ;
    char        mesg[ 512 ] ;

    name2cstr( gfile, eqname, gfilelen, BUFLEN ) ;
    if ( ! eqname[0] )
         {
         m3mesgc( "*** ZBIL4WRITE:  bad filename argument" ) ;
         return 0 ;
         }

     fp = gzopen( eqname, "wb9" );
     if ( fp )
         {
         sprintf( mesg, "ZBIL4WRITE:  File opened: %s", eqname ) ;
         m3mesgc( mesg ) ;
         }
     else{
         m3mesgc( "*** ZBIL4WRITE:  gzopen() failure" ) ;
         return 0 ;
         } ;

    ngrid = (unsigned) (size_t) (size_t) *ncols * (size_t) *nrows ;
    nsize = (unsigned) ngrid * sizeof( uint32_t )  ;

    if ( *swapb )
        {
         if ( vsiz == 0 )
            {
            vbuf = malloc( nsize * sizeof( uint32_t ) ) ;
            if ( vbuf )
                {
                vsiz = nsize ;
                }
            else{
                 m3mesgc( "*** ZBIL2WRITE:  malloc() failure" ) ;
                 return 0 ;
                }
            }
        else if ( vsiz < nsize )
            {
            vbuf = realloc( (void *)vbuf , nsize * sizeof( uint32_t ) ) ;
            if ( vbuf )
                {
                vsiz = nsize ;
                }
            else{
                 m3mesgc( "*** ZBIL4WRITE:  realloc() failure" ) ;
                 return 0 ;
                }
            }
	for ( i = 0 ; i < nsize ; i++ )
            {
            vbuf[ i ] = ntohl( gbuf[ i ] );
            }
        nwrit = gzwrite( fp, (voidp) vbuf, nsize );
        }
    else{
        nwrit = gzwrite( fp, (voidp) gbuf, nsize );
        }

    if ( nwrit != nsize )
        {
        m3mesgc( "*** ZBIL4WRITE:  fwrite() failure" ) ;
        gzclose( fp ) ;
        return 0 ;
        }

    if ( EOF == gzclose( fp ) )
        {
        m3mesgc( "*** ZBIL4WRITE: gzclose() failure" ) ;
        return( 0 ) ;
        }

    return 1 ;
    }



