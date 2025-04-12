/***********************************************************************
FILE: gdplot.c

        Version "$Id: gdplot.c 280 2025-04-12 15:34:39Z coats $"

        Copyright (c) 2010-2012 Baron Advanced Meteorological Systems, LLC.
        and (C) 2013-2025 Carlie J. Coats, Jr.,
        Distributed under the GNU GENERAL PUBLIC LICENSE version 2
        See file "GPL.txt" for conditions of use.
        All rights reserved.

        Fortran wrappers around "libgd v2.x" calls for creating images
        using PNG, JPEG, or GIF.

PROVIDES:
        state varibles and routines for constructing tile-plot images
        for a given M3IO grid-window.

        INITPLOT():  initialize plot system for given image size,
        map projection, grid, and color-scales.

        INITMAP():  Initialize a map from McIDAS format map-file

        LEGEND():  Draw titles and subtitles, grid-dimension labels,
        and scale bars and their labels.

        IMGCLR():   Clear image:  call at start of every new plot.

        IMGTILE():  Draw tile plot for indicated data and scale factors.

        IMGSMTH():  Draw "smooth" color-filled plot for indicated data and scale factors.

        IMGVECT():  Draw wind vectors overlays (on top of map and tile plot)

        IMGCONT():  Draw contour plot ovelays (on top of map and tile plot)

        IMGOBS():   Draw scalar-observation ovelays (on top of map and tile plot)

        IMGOVEC():  Draw wind-vector observation ovelays (on top of map and tile plot)

        IMGMAP():   Draw map on top of tile plot (etc.).

        IMGWRITE():  Save image to GIF, JPG, or PNG file.

PRECONDITIONS:
        Parameter-consistency with "m3plot.f90", "mtxplot.f90", "gisplot.f90"

REFERENCE FOR "libgd":  "http://www.libgd.org/"

REVISION  HISTORY:
        Prototype 10/2009 by Carlie J. Coats, Jr., BAMS.
        Version    7/2010 by CJC:  vector and obs overlays.
        Version   11/2010 by CJC:  contour overlays.  obs-vector overlays.
        Version   03/2012 by CJC:  bug-fix in vector legend
        Version   09/2012 by CJC:  add map-width control structure
        Version   09/2012 by CJC:  add "wmohack" to deal with Lon-wraparound
                                   issues on WMO-"standard" grids
        Version   4/2025 by CJC for I/O API M3Tools version 4.0
                                    Obs coor-transforms now done in caller.
***********************************************************************/

#include "gd.h"
#include "gdfonts.h"
#include "gdfontl.h"
#include "gdfontmb.h"
#include "math.h"

#include "iodecl3.h"
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <endian.h>
#include <arpa/inet.h>

#include <byteswap.h>

#if defined(__sgi)    || defined(__sun) || defined(__osf__) || \
    defined(__mips__) || defined(__OPENNT)
#define FLDMN 1
#endif

#if BYTE_ORDER == BIG_ENDIAN
#define   bswap_32( X )  ( X )
#endif

#if FLDMN

#define INITPLOT initplot_
#define INITMAP  initmap_
#define LEGEND   legend_
#define IMGCLR   imgclr_
#define IMGTILE  imgtile_
#define IMGSMTH  imgsmth_
#define IMGVECT  imgvect_
#define IMGCONT  imgcont_
#define IMGOBS   imgobs_
#define IMGOVEC  imgovec_
#define IMGWRITE imgwrite_
#define IMGMAP   imgmap_

#define INITXY   initxy_
#define LLXY     llxy_
#define LLBOX    llbox_

#elif defined(__hpux) || defined(_AIX)

#define INITPLOT initplotfllxy
#define INITMAP  initmap
#define LEGEND   legend
#define IMGCLR   imgclr
#define IMGTILE  imgtile
#define IMGSMTH  imgsmth
#define IMGVECT  imgvect
#define IMGCONT  imgcont
#define IMGOBS   imgobs
#define IMGOVEC  imgovec
#define IMGWRITE imgwrite
#define IMGMAP   imgmap

#define INITXY   initxy
#define LLXY     llxy
#define LLBOX    llbox

#else

#error   "Error compiling:  unsupported architecture"

#endif

#define MAXHUE     (250)
#define MAXSTR     (512)
#define MAXSEG   (65536)
#define GIF          (1)
#define JPG          (2)
#define PNG          (3)

#define VAL( V,I,J )  ( (V)[ (size_t)(I) + (size_t)(J)*(size_t)nc ] )

#define  PI180   ( M_PI / 180.0 )


typedef struct {
               int y ;
               int x ;
               }             llPoint ;          /*  MacIdas map point  */

extern void LLXY(   FREAL*, FREAL*,  FREAL*,  FREAL* ) ;
extern void LLBOX(  FREAL*, FREAL*,  FREAL*,  FREAL*,  FREAL*,  FREAL*,  FREAL*,  FREAL* ) ;
extern void INITXY( FINT*,  double*, double*, double*, double*, double* ) ;

static gdImagePtr img ;

static int       pltonly = 0 ;          /*  pltonly==0:  plot everything        */
                                        /*  pltonly==1:  tile-only,  width nx   */
                                        /*  pltonly==2:  tile-only, height ny   */

static int       npoly1  ;
static int       npoint1 ;
static int     * mapcnt1 ;
static int     * mapdex1 ;
static gdPoint * mappnt1 ;

static int       npoly2  ;
static int       npoint2 ;
static int     * mapcnt2 ;
static int     * mapdex2 ;
static gdPoint * mappnt2 ;

static int       npoly3  ;
static int       npoint3 ;
static int     * mapcnt3 ;
static int     * mapdex3 ;
static gdPoint * mappnt3 ;

static int    nx,  ny  ;                /* whole-image pixel dimensions         */
static int    nc,  nr ;                 /*   data-grid dimensions               */
static int    nc0, nr0, nc1, nr1 ;      /*   data | countour-grid window extent */
static int    vc0, vr0, vc1, vr1 ;      /* vector-grid window extent            */
static int    nxg,  nyg ;               /* tiled-grid pixel dimensions          */
static int    nx0, nx1 ;                /* left, right edge of tiled grid       */
static int    ny0, ny1 ;                /* top, bottom edge of tiled grid       */
static int    fillx, filly ;            /* extra space in X, Y directions       */
static int    mx0, mx1, my0, my1 ;      /* left, right, top, bottom margin      */
static float  ft1, ft2, ft3 ;           /* title, header/footer, label fonts    */

static int    wmap1, wmap2, wmap3 ;     /* map width (pixels)                   */
static int    cmap1, cmap2, cmap3 ;     /* map color index                      */

static int    wmohack = 0 ;             /* 1 for WMO-"standard" Lat-Lon grids   */

static int    black, white, cmiss ;
static int    ncolors ;
static int    colors[ MAXHUE ] ;
static int    nvcolors ;
static int    vcolors[ MAXHUE ] ;
static int    nzcolors ;
static int    zcolors[ MAXHUE ] ;

static int    mapok ;                           /*  map-valid:  0: no, 1: LL, 2... */
static FINT   mzone ;                           /*  UTM zone, or (-1) for non-UTM  */
static int    mproj ;                           /*  map projection LATGRD3, etc.   */
static double p_alp, p_bet, p_gam, xcent, ycent ;
static double xorig, yorig, xcell, ycell ;
static float  lonmin, latmin, lonmax, latmax ;

static int   * ic2x ;           /*  tile-plot grid-col/row to pixel x/y conversion  */
static int   * ir2y ;

static int   * kc2x ;           /* smooth-plot grid-col/row to pixel x/y conversion */
static int   * kr2y ;
static float * xtrp ;           /* smooth-plot grid-col/row to pixel coeffs         */
static float * ytrp ;

static int   * vc2x ;           /*  vector-plot dotgrid-col/row to pixel x/y conversion  */
static int   * vr2y ;
static float   xcellpix, ycellpix ;


/*--------------------------------------------------------------- -------*/
/*  Initialize image, grid, and map projection state variables          */
/*  for plotting.                                                       */
/*  Call INITPLOT() once, before using the rest of the routines         */
/*  in this file.                                                       */
/*----------------------------------------------------------------------*/

void INITPLOT(
             FINT   * plotonly,       /* > 0:  no legend;               */
                                      /*   1:  use X image width        */
                                      /*   2:  use Y image width        */
             FINT   * vecdot,         /* = 1 (0):  dot-pt (cross-pt)    */
                                      /*   vector grid                  */
             FINT   * nxpix,          /* total-image dimensions         */
             FINT   * nypix,          /*  ""                            */
             FINT   * nhues,          /* number of colors in palette    */
             FINT     hues[][3],      /* tile/smooth palette RGB-values */
             FINT   * nhuev,          /* number of colors in v-palette  */
             FINT     huev[][3],      /* vector-palette RGB-values      */
             FINT   * nhuez,          /* number of colors in c-palette  */
             FINT     huez[][3],      /* contour-palette RGB-values     */
             FINT     miss[3],        /* "missing" RGB                  */
             FINT   * ncols,          /* grid dimension                 */
             FINT   * nrows,          /*  ""                            */
             FINT   * ncol0,          /* window extent in cols          */
             FINT   * ncol1,          /*  ""                            */
             FINT   * nrow0,          /* window extent in rows          */
             FINT   * nrow1,          /*  ""                            */
             FINT   * nproj,          /*  map projection type           */
             double * palp,           /*  Map projection parameter      */
             double * pbet,           /*   ""                           */
             double * pgam,           /*   ""                           */
             double * xcnt,           /*  Cartesian origin lon          */
             double * ycnt,           /*  ""               lat          */
             double * xorg,           /*  grid origin                   */
             double * yorg,           /*  ""                            */
             double * xcel,           /*  grid cell size                */
             double * ycel,            /*  ""  */
             FINT   * istat
             )
    {
    int     i, k, ic, ir, ig, ib, idot ;
    float   xx, yy, dx, dy, ddx, ddy, ratlon ;
    FREAL   a, b, c, x, y ;
    FREAL   xmin, ymin, xmax, ymax, t1, t2 ;
    char    mesg[ 256 ] ;

    /*------------------------------------------------------------------*/
    /*  Set up state variables                                          */
    /*------------------------------------------------------------------*/

    pltonly = *plotonly ;
    idot    = *vecdot ;
    nx      = *nxpix ;
    ny      = *nypix ;
    ncolors = *nhues ;
    nvcolors= *nhuev ;
    nzcolors= *nhuev ;
    nc      = *ncols ;
    nr      = *nrows ;
    nc0     = *ncol0 - 1 ;
    nc1     = *ncol1 - 1 ;
    nr0     = *nrow0 - 1 ;
    nr1     = *nrow1 - 1 ;
    mproj   = *nproj ;
    p_alp   = *palp ;
    p_bet   = *pbet ;
    p_gam   = *pgam ;
    xcent   = *xcnt ;
    ycent   = *ycnt ;
    xorig   = *xorg ;
    yorig   = *yorg ;
    xcell   = *xcel ;
    ycell   = *ycel ;
    mzone   = ( mproj == UTMGRD3 ? (int)( p_alp+0.5 ) : -1 ) ;
    wmohack = ( mproj == LATGRD3 ) && ( ( xorig >= 0.0 ) || ( xorig < -180.0 ) ) ;
    
    INITXY( nproj, palp, pbet, pgam, xcnt, ycnt ) ;

    /*--------------------------------------------------------------*/
    /*  Set up image and plot geometry:                             */
    /*  pltonly = 0:  legend present; plot smaller than image.      */
    /*            1:  plot only; X width determines image size      */
    /*            2:  plot only; Y width determines image size      */
    /*  IF LEGEND PRESENT:  scale the plot so that                  */
    /*  Minimum Left:  140 pixels                                   */
    /*                 margin  8 pixels                             */
    /*                 legend bar:  16 pixels                       */
    /*                 legend scale: 40 pixels                      */
    /*  Minimum Right:  60 pixels                                   */
    /*  Minimum Top:  100 pixels                                    */
    /*                title1:  40 pixels                            */
    /*                title2:  30 pixels                            */
    /*                title3:  30 pixels                            */
    /*  Minimum Bottom:  120 pixels                                 */
    /*                   date&time 50 pixels                        */
    /*                   min/max 30 pixels                          */
    /*--------------------------------------------------------------*/

    ic = nc1 - nc0 + 1 ;        /*  grid-window dimensions  */
    ir = nr1 - nr0 + 1 ;

    if ( mproj == LATGRD3 )
        {
        ratlon = (float) cos( M_PI * (double)( (nr0+nr1)/2 ) / 180.0 ) ;
        }
    else{
        ratlon = 1.0 ;
        } 
    xx = ratlon * xcell / ycell ;        /*  x-to-y cellsize ratios  */
    yy = 1.0 / xx ;
    if ( xx < 0.0 )
        {
        xx = -xx ;
        yy = -yy ;
        }

    if ( pltonly == 1 )                 /*  X direction controls width, height  */
        {
        nxg = nx - 2 ;
        dx  = (float) nxg / (float) ic ;      /* max X-direction max cell-size, pixels */
        dy  = yy * dx ;
        nyg = (int)( dy * (float) ir ) ;
        ny  = nyg + 1 ;
        ny0 = 1 ;                       /*  top    edge of tiled grid */
        ny1 = nyg + 2 ;                 /*  bottom edge of tiled grid */
        nx1 = nx - 1 ;                  /*  right  edge of tiled grid */
        nx0 = 1 ;                       /*  left   edge of tiled grid */
        fillx = 0 ;
        filly = 0 ;
        }
    else if ( pltonly == 2 )            /*  y direction controls the width, height  */
        {
        nyg = ny - 2 ;
        dy  = (float) nyg / (float) ir ;      /* max Y-direction max cell-size, pixels */
        dx  = xx * dy ;
        nxg = (int)( dx * (float) ic ) ;
        nx  = nxg + 1 ;
        ny0 = 1 ;                       /*  top    edge of tiled grid */
        ny1 = ny  - 1 ;                 /*  bottom edge of tiled grid */
        nx1 = nxg + 2 ;                 /*  right  edge of tiled grid */
        nx0 = 1 ;                       /*  left   edge of tiled grid */
        fillx = 0 ;
        filly = 0 ;
        }
    else{

        mx0 = ( nx + ny ) / 15 ;
        mx1 = ( nx + ny ) / 50 ;
        my0 = ( nx + ny ) / 40 ;
        mx0 = ( mx0 < 110 ? 110 : mx0 ) ;
        mx1 = ( mx1 <  80 ?  80 : mx1 ) ;
        my0 = ( my0 < 120 ? 120 : my0 ) ;
        my1 = my0 ;
        
        ft1 = (float)( nx + ny ) / 125.0 ;
        ft1 = ( ft1 < 20.0 ? 20.0 : ft1 ) ;
        ft2 = 0.8 * ft1 ;
        ft3 = 0.7 * ft1 ;

        nxg = nx - mx0 - mx1 ;
        nyg = ny - my0 - my1 ;
        dx  = (float) nxg / (float) ic ;      /* X-direction max cell-size, pixels */
        dy  = (float) nyg / (float) ir ;      /* Y-direction max cell-size, pixels */

        if ( dx <= xx * dy )            /* re-scale Y in terms of X  */
            {
            dy  = yy * dx ;
            nyg = (int)( dy * (float) ir ) ;
            }
        else{                           /* re-scale X in terms of X  */
            dx  = xx * dy;
            nxg = (int)( dx * (float) ic ) ;
            } ;

        fillx = nx - nxg - mx0 - mx1 ;
        filly = ny - nyg - my0 - my1 ;
        ny0   = my0 + filly / 5  ;          /*  top    edge of tiled grid */
        ny1   = ny0 + nyg ;                 /*  bottom edge of tiled grid */
        nx1   = nx  - mx1 ;                 /*  right  edge of tiled grid */
        nx0   = nx1 - nxg ;                 /*  left   edge of tiled grid */
        } ;

    ddx = 1.0 / dx ;
    ddy = 1.0 / dy ;

    xcellpix = ( (float) nx1 - nx0 ) / ( (float)( nc1 - nc0 + 1 ) ) ;
    ycellpix = ( (float) ny0 - ny1 ) / ( (float)( nr1 - nr0 + 1 ) ) ;
    if ( xcell < 0.0 ) xcellpix = -xcellpix ;
    if ( ycell < 0.0 ) ycellpix = -ycellpix ;

    /*--------------------------------------------------------------------------*/
    /*  Tile-plot, Smooth-plot Conversion:                                      */
    /*  image-tile pixels <ic2x[:],ic2y[:]> to grid-window <col,row> indices    */
    /*  interpolation poxels <kc2x[:],kc2y[:]> and coeffs <xtrp[:],ytrp[:]>     */
    /*  Handle all grid orientations:  xcell,ycell > 0 , < 0                    */
    /*--------------------------------------------------------------------------*/

    if ( ! ( ic2x = (int *) malloc( 2 * (size_t) ( nxg + nyg ) * sizeof( int ) ) ) )
        {
        m3mesgc( "GDPLOT/INITPLOT(): image-to-grid index malloc() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        } ;
    ir2y = ic2x + (size_t)nxg ;
    kc2x = ir2y + (size_t)nyg ;
    kr2y = kc2x + (size_t)nxg ;

    if ( ! ( xtrp = (float *) malloc( (size_t) ( nxg + nyg ) * sizeof( float ) ) ) )
        {
        m3mesgc( "GDPLOT/INITPLOT(): image-to-grid-coeff  malloc() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        } ;
    ytrp = xtrp + (size_t)nxg ;

    if ( xcell > 0.0 )
        {
        for ( i = 0 ; i < nxg ; i++ )
            {
            xx = ddx * (float) i ;
            ic2x[ i ] = nc0 + (int) xx ;
            xx = xx - 0.5 ;
            k  = nc0 + (int) xx ;
            if ( k < nc0 )
                {
                kc2x[ i ] = nc0 ;
                xtrp[ i ] = 1.0 ;
                }
            else if ( k > nc1 - 2 )
                {
                kc2x[ i ] = nc1 - 2 ;
                xtrp[ i ] = 0.0 ;
                }
            else{
                kc2x[ i ] = k ;
                xtrp[ i ] = 1.0 - xx + (float)(int)xx ;
                } ;
            }
        }                       /*  end for(i;;)  */

    else{                       /*  xcell negative  */

        for ( i = 0 ; i < nxg ; i++ )
            {
            xx = ddx * (float) i ;
            ic2x[ i ] = nc1 - (int) xx ;
            xx = xx - 0.5 ;
            k  = nc1 - (int) xx ;
            if ( k < nc0 )
                {
                kc2x[ i ] = nc0 ;
                xtrp[ i ] = 1.0 ;
                }
            else if ( k > nc1 - 2 )
                {
                kc2x[ i ] = nc1 - 2 ;
                xtrp[ i ] = 0.0 ;
                }
            else{
                kc2x[ i ] = k ;
                xtrp[ i ] = (float)(int)xx - xx ;
                } ;
            }
        } ;             /*  if xcell positive, or else negative  */

    if ( ycell > 0.0 )  /*  NOTE:  image scan line order is downward from top  */
        {
        for ( i = 0 ; i < nyg ; i++ )
            {
            yy = ddy * (float) i ;
            ir2y[ i ] = nr1 - ( (int) yy ) ;
            yy = yy + 0.5 ;
            k  = nr1 - (int) yy ;
            if ( k < nr0 )
                {
                kr2y[ i ] = nr0 ;
                ytrp[ i ] = 1.0 ;
                }
            else if ( k >= nr1 - 2 )
                {
                kr2y[ i ] = nr1 - 2 ;
                ytrp[ i ] = 0.0 ;
                }
            else{
                kr2y[ i ] = k ;
                ytrp[ i ] = yy - (float)(int) yy ;
                } ;
            }                   /*  end for(i;;)  */
        }

    else{                       /*  ycell negative:  */

        for ( i = 0 ; i < nyg ; i++ )
            {
            yy = ddy * (float) i ;
            ir2y[ i ] = nr0 + ( (int) yy ) ;
            yy = yy + 0.5 ;
            k  = nr0 + (int) yy ;
            if ( k < nr0 )
                {
                kr2y[ i ] = nr0 ;
                ytrp[ i ] = 1.0 ;
                }
            else if ( k >= nr1 - 2 )
                {
                kr2y[ i ] = nr1 - 2 ;
                ytrp[ i ] = 0.0 ;
                }
            else{
                kr2y[ i ] = k ;
                ytrp[ i ] = 1.0 - yy + (float)(int) yy ;
                } ;
            }           /*  end for(i;;)  */
        } ;             /*  if ycell positive, or else negative  */


    /*--------------------------------------------------------------------------*/
    /*  Vector-plot Conversion:                                                 */
    /*  vector-data-grid cell-centers to grid-window <col,row> indices          */
    /*--------------------------------------------------------------------------*/

    if ( idot==1 )
        {
        vc0 = nc0 ;
        vr0 = nr0 ;
        vc1 = nc1 + 1 ;
        vr1 = nr1 + 1 ;
        dx  = 0.0 ;
        dy  = 0.0 ;
        }
    else if ( idot==0 )
        {
        vc0 = nc0 ;
        vr0 = nr0 ;
        vc1 = nc1  ;
        vr1 = nr1  ;
        dx  = 0.5 * xcellpix ;
        dy  = 0.5 * ycellpix ;
        }
    else{
        m3mesgc( "GDPLOT/INITPLOT(): invalid flag VECDOT" ) ;
        *istat = -1;
        return ;
        } ;

    if ( ! ( vc2x = (int *) malloc( 2 * (size_t) ( nc + nr + 2 ) * sizeof( int ) ) ) )
        {
        m3mesgc( "GDPLOT/INITPLOT(): vector-to-grid index malloc() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        } ;
    vr2y = vc2x + (size_t)( nc+1 ) ;

    if ( xcell > 0.0 )
        {
        for ( i = vc0 ; i < vc1+1 ; i++ )
            {
            vc2x[ i ] = nx0 + (int)( dx + xcellpix * (float)( i-vc0 ) ) ;
            }
        }
    else
        {
        for ( i = vc0 ; i < vc1+1 ; i++ )
            {
            vc2x[ i ] = nx1 + (int)( dx + xcellpix * (float)( i-vc0 ) ) ;
            }
        }

    if ( xcell > 0.0 )
        {
        for ( i = vr0 ; i < vr1+1 ; i++ )
            {
            vr2y[ i ] = ny1 + (int)( dy + ycellpix * (float)( i-vr0 ) ) ;
            }
        }
    else
        {
        for ( i = vr0 ; i < vr1+1 ; i++ )
            {
            vr2y[ i ] = ny0 + (int)( dy + ycellpix * (float)( i-vr0 ) ) ;
            }
        }


    /*-----------------------------------------------------------------*/
    /*  Set up image and color map; initialize image to WHITE          */
    /*-----------------------------------------------------------------*/

    if ( ! ( img = gdImageCreate( nx, ny ) ) )  { *istat = -1; return; } ;

    if ( ( black = gdImageColorAllocate( img,       0,       0,       0 ) ) < 0 )  { *istat = -1; return; } ;
    if ( ( white = gdImageColorAllocate( img,     255,     255,     255 ) ) < 0 )  { *istat = -1; return; } ;
    if ( ( cmiss = gdImageColorAllocate( img, miss[0], miss[1], miss[1] ) ) < 0 )  { *istat = -1; return; } ;

    for ( i = 0 ; i < ncolors ; i++ )
        {
        ir = hues[i][0] ;
        ig = hues[i][1] ;
        ib = hues[i][2] ;
        ic =  gdImageColorAllocate( img, ir, ig, ib ) ;
        if ( ic < 0 )  { *istat = -1; return; } ;
        colors[i] = ic ;
        }

    for ( i = 0 ; i < nvcolors ; i++ )
        {
        ir = huev[i][0] ;
        ig = huev[i][1] ;
        ib = huev[i][2] ;
        ic =  gdImageColorAllocate( img, ir, ig, ib ) ;
        if ( ic < 0 )  { *istat = -1; return; } ;
        vcolors[i] = ic ;
        }

    for ( i = 0 ; i < nzcolors ; i++ )
        {
        ir = huez[i][0] ;
        ig = huez[i][1] ;
        ib = huez[i][2] ;
        ic =  gdImageColorAllocate( img, ir, ig, ib ) ;
        if ( ic < 0 )  { *istat = -1; return; } ;
        zcolors[i] = ic ;
        }

    /*-----------------------------------------------------------------*/
    /*  Set up map projection:  allow all grid orientations dx,dy vs 0 */
    /*-----------------------------------------------------------------*/

    t1   = xorig + xcell * (float) (nc0 ) ;
    t2   = xorig + xcell * (float) (nc1+1 ) ;
    xmin = ( t1 < t2 ? t1 : t2 ) ;
    xmax = ( t1 < t2 ? t2 : t2 ) ;

    t1   = yorig + ycell * (float) (nr0 ) ;
    t2   = yorig + ycell * (float) (nr1+1 ) ;
    ymin = ( t1 < t2 ? t1 : t2 ) ;
    ymax = ( t1 < t2 ? t2 : t2 ) ;

    a = (float) p_alp ;
    b = (float) p_bet ;
    c = (float) p_gam ;
    x = (float) xcent ;
    y = (float) ycent ;
    
    LLBOX( &xmin,   &ymin,   &xmax,   &ymax,
           &lonmin, &latmin, &lonmax, &latmax ) ;

    *istat = 0 ;                /*  success  */
    return ;

    }         /*  end INITPLOT()  */



/**********************************************************************/
/*  Initialize map from MCIDAS map file.                              */
/*  File structure:                                                   */
/*       int:  number of segments n                                   */
/*       array[ n ] of segment descriptors                            */
/*            int: scaled latmin                                      */
/*            int: scaled latmax                                      */
/*            int: scaled lonmax (west is positive)                   */
/*            int: scaled lonmin (")                                  */
/*            int: segment offset                                     */
/*            int: number of x,y values in segment                    */
/*       sequence of segment records:                                 */
/*            int:  scaled lat                                        */
/*            int:  scaled lon (west is positive)                     */
/*                                                                    */
/*  Scale factor is 1.0e4                                             */
/**********************************************************************/


void  INITMAP(
              FINT       * mapnum ,      /* 0 or 1 for map number         */
              const char * mapfile,
              FINT         rgb[3] ,
              FINT       * width  ,
              FINT       * istat  ,
              FSTR_L       mapfilelen
              )
    {
    int       ibuf[6] ;
    int       ii, jj, k, m, n ;
    int       mapno, color, stat ;
    char      fname[ 512 ] ;
    char      mesg[ 256 ] ;
    int       ilon1, ilon2, ilat1, ilat2 ;
    float     xlon1, xlon2, ylat1, ylat2 ;
    double    t1, t2, x1, x2, y1, y2, dx, dy ;
    float     uu, vv, ww, xx, yy ;
    int       idex, icnt, irec, npoly, nuse, npts ;
    int     * mapcnt ;
    int     * mapdex ;
    int     * mapoff ;
    long      noff ;
    FILE    * f ;
    gdPoint * mappnt ;
    llPoint * maplls ;          /*  Treat as maplls[npts][2]  */
    size_t    nread, ncnt ;

    if ( ! mapok )
        {
        *istat = 0 ;
        return ;
        } ;

    mapno = *mapnum ;
    if ( ( mapno < 1 ) || mapno > 3 )
        {
        m3mesgc( "GDPLOT/INITMAP():  Unsupported map number:  not 1-3" ) ;
        *istat = -1;
        return ;
        } ;

    color = gdImageColorAllocate( img, (int)rgb[0], (int)rgb[1], (int)rgb[2] ) ;
    if ( color < 0 ) color = black ;

    name2cstr( mapfile, fname, mapfilelen, (FSTR_L)512 ) ;
    if ( ! ( f = fopen( fname, "r" ) ) )
        {
        m3mesgc( "GDPLOT/INITMAP():  fopen() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        }

    ncnt  = (size_t)1 ;
    nread = fread( ibuf, sizeof(int), ncnt, f ) ;
    if ( nread != ncnt )              /* read number of map polygons  */
        {
        m3mesgc( "GDPLOT/INITMAP():  header fread() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        }

    npoly = bswap_32( ibuf[0] ) ;

    n  =  ( wmohack ? 3 * npoly : npoly ) ;                            /*  if wmohack, must wrap map twice  */

    if ( ! ( mapoff = (int *)malloc( n * sizeof(int) ) ) )
        {
        m3mesgc( "GDPLOT/INITMAP(): mapoff malloc() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        } ;
    if ( ! ( mapcnt = (int *)malloc( n * sizeof(int) ) ) )         /*  arrays subscripted by polygons  */
        {
        m3mesgc( "GDPLOT/INITMAP(): mapcnt malloc() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        } ;
    if ( ! ( mapdex = (int *)malloc( n * sizeof(int) ) ) )
        {
        m3mesgc( "GDPLOT/INITMAP(): mapdex malloc() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        } ;


    nuse = 0 ;
    npts = 0 ;
    for ( n = 0 ; n < npoly ; n++ )                      /* read polygon descriptors  */
        {

        ncnt  = (size_t)6 ;
        nread = fread( ibuf, sizeof(int), ncnt, f ) ;
        if ( nread != ncnt )
            {
            m3mesgc( "GDPLOT/INITMAP():  seg-header fread() failure" ) ;
            perror( (char *) 0 ) ;
            *istat = -1 ;
            return ;
            }

        ilat1 = bswap_32( ibuf[0] ) ;
        ilat2 = bswap_32( ibuf[1] ) ;
        ilon1 = bswap_32( ibuf[3] ) ;
        ilon2 = bswap_32( ibuf[2] ) ;

        ylat1 =  0.0001 * (float) ilat1 ;
        ylat2 =  0.0001 * (float) ilat2 ;
        xlon1 = -0.0001 * (float) ilon1 ;       /* "-0.0001 * ..." swaps min and max  */
        xlon2 = -0.0001 * (float) ilon2 ;
        
        if ( wmohack )                          /*  need 0-to-0360 "wrap around" */
            {
            xx    = xlon1 + 360.0 ;
            xlon1 = ( xx > 360.0 ? xlon1 : xx ) ;
            xx    = xlon2 + 360.0 ;
            xlon2 = ( xx > 360.0 ? xlon2 : xx ) ;
            }

        if ( ylat1 < latmin && ylat2 < latmin )  continue ;
        if ( ylat1 > latmax && ylat2 > latmax )  continue ;
        if ( xlon1 < lonmin && xlon2 < lonmin )  continue ;     /*  really, needs correction for "lon wrap around"  */
        if ( xlon1 > lonmax && xlon2 > lonmax )  continue ;

        irec  =  bswap_32( ibuf[4] ) ;
        icnt  =  bswap_32( ibuf[5] ) / 2 ;

        mapcnt[ nuse ] = icnt ;
        mapdex[ nuse ] = npts ;
        mapoff[ nuse ] = irec * sizeof( int ) ;

        nuse = nuse + 1 ;
        npts = npts + icnt ;

        }               /*  end loop on map-segments n  */

    if ( ! ( maplls  = (llPoint *)malloc( npts * sizeof(llPoint) ) ) )
        {
        m3mesgc( "GDPLOT/INITMAP():  maplls malloc() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        } ;

    n  =  ( wmohack ? 3 * npts : npts ) ;                                   /*  if wmohack, must wrap map twice  */
    if ( ! ( mappnt  = (gdPoint *)malloc( n * sizeof(gdPoint) ) ) )         /*  arrays subscripted by points   */
        {
        m3mesgc( "GDPLOT/INITMAP():  mappnt malloc() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1;
        return ;
        } ;


    for ( n = 0 ; n < nuse ; n++ )                             /*  read McIDAS map points   */
        {
        noff = (long) mapoff[n] ;
        stat = fseek( f, noff, SEEK_SET ) ;
        if ( stat != 0 )
            {
            sprintf( mesg, "GDPLOT/INITMAP():  fseek() failure for polygon n=%d", n ) ;
            m3mesgc( mesg ) ;
            perror( (char *) 0 ) ;
            *istat = -1;
            return ;
            }
        ncnt  = (size_t)mapcnt[n] ;
        nread = fread( & maplls[ mapdex[n] ], sizeof(llPoint), ncnt, f ) ;
        if ( nread != ncnt )
            {
            sprintf( mesg,
                     "GDPLOT/INITMAP():  fread() failure for polygon n=%d:  ncnt=%d and nread=%d",
                     n, (int)ncnt, (int)nread ) ;
            m3mesgc( mesg ) ;
            perror( (char *) 0 ) ;
            mapcnt[n] = nread ;
            *istat = -1;
            return ;
            }
        }

    fclose( f ) ;


    t1 = (double) nc0 - 0.5;
    t2 = (double) nc1 + 0.5;
    t1 = xorig + xcell * t1 ;                   /*  Fixup for xcell,ycell positive or negative  */
    t2 = xorig + xcell * t2 ;
    if ( t1 < t2 )
        {
        x1 = t1 ;
        x2 = t2 ;
        }
    else{
        x1 = t2 ;
        x2 = t1 ;
        }
    dx = (double) nxg / ( x2 - x1 )  ;

    t1 = (double) nr0 - 0.5 ;
    t2 = (double) nr1 + 0.5 ;
    t1 = yorig + ycell * t1 ;                   /*  Fixup for xcell,ycell positive or negative  */
    t2 = yorig + ycell * t2 ;
    if ( t1 < t2 )
        {
        y1 = t1 ;
        y2 = t2 ;
        }
    else{
        y1 = t2 ;
        y2 = t1 ;
        }
    dy =(double) nyg /  ( y1 - y2 ) ;

    if ( mproj == LATGRD3 )                                     /*  process/transform map points: LL case  */
        {
        for ( n = 0 ; n < npts ; n++ )
            {
            ii = bswap_32( maplls[n].x ) ;
            jj = bswap_32( maplls[n].y ) ;
            xx = -0.0001 * (double)ii - x1 ;
            yy =  0.0001 * (double)jj - y2 ;
            mappnt[n].x = nx0 + (int)( dx * xx ) ;
            mappnt[n].y = ny0 + (int)( dy * yy ) ;
            }
        if ( wmohack )                      /*  draw map twice, with longitude-wrap */
            {
            for ( k = 0 ; k < npts ; k++, n++ )
                {
                ii = bswap_32( maplls[k].x ) ;
                jj = bswap_32( maplls[k].y ) ;
                xx = -0.0001 * (double)ii - x1  + 360.0 ;
                yy =  0.0001 * (double)jj - y2 ;
                mappnt[n].x = nx0 + (int)( dx * xx ) ;
                mappnt[n].y = ny0 + (int)( dy * yy ) ;
                }
            for ( k = 0 ; k < npts ; k++, n++ )
                {
                ii = bswap_32( maplls[k].x ) ;
                jj = bswap_32( maplls[k].y ) ;
                xx = -0.0001 * (double)ii - x1  - 360.0 ;
                yy =  0.0001 * (double)jj - y2 ;
                mappnt[n].x = nx0 + (int)( dx * xx ) ;
                mappnt[n].y = ny0 + (int)( dy * yy ) ;
                }
            for ( k = 0 , n = npoly ; k < npoly ; k++, n++ ) 
                {
                mapdex[n] = mapdex[k] + npts ;
                mapcnt[n] = mapcnt[k] ;
                }
            for ( k = 0 , n = 2*npoly ; k < npoly ; k++, n++ ) 
                {
                mapdex[n] = mapdex[k] + 2*npts ;
                mapcnt[n] = mapcnt[k] ;
                }
            npts  = 3 * npts ;
            npoly = 3 * npoly ;
            }
        }
    else{                                                       /*  process/transform map points: non-LL/UTM case  */
        for ( n = 0 ; n < npts ; n++ )
            {
            ii = bswap_32( maplls[n].x ) ;
            jj = bswap_32( maplls[n].y ) ;
            uu = -0.0001 * (double)ii ;
            vv =  0.0001 * (double)jj ;
            LLXY( &uu, &vv, &xx, &yy ) ;
            xx = xx - x1 ;
            yy = yy - y2 ;
            mappnt[n].x = nx0 + (int)( dx * xx ) ;
            mappnt[n].y = ny0 + (int)( dy * yy ) ;
            }
        }


    gdImageSetClip( img, nx0, ny0, nx1, ny1 ) ;
    for ( n = 0 ; n < npoly ; n++ )
        {
        idex = mapdex[n] ;
        icnt = mapcnt[n] ;
        gdImageOpenPolygon( img, &mappnt[ idex ], icnt, color ) ;
        } ;
    gdImageSetClip( img, 0, 0, nx-1, ny-1 ) ;

    if ( mapno == 1 )                                         /*  save pointers in state variables  */
        {
        wmap1   = *width ;
        cmap1   = color ;
        npoly1  = npoly ;
        npoint1 = npts  ;
        mapcnt1 = mapcnt ;
        mapdex1 = mapdex ;
        mappnt1 = mappnt ;
        }
    else if ( mapno == 2 )
        {
        wmap2   = *width ;
        cmap2   = color ;
        npoly2  = npoly ;
        npoint2 = npts  ;
        mapcnt2 = mapcnt ;
        mapdex2 = mapdex ;
        mappnt2 = mappnt ;
        }
    else if ( mapno == 3 )
        {
        wmap3   = *width ;
        cmap3   = color ;
        npoly3  = npoly ;
        npoint3 = npts  ;
        mapcnt3 = mapcnt ;
        mapdex3 = mapdex ;
        mappnt3 = mappnt ;
        }

    free( mapoff ) ;
    free( maplls ) ;

    *istat = 0 ;
    return ;        /*  success  */

    }         /*  end INITMAP()  */



/*---------------------------------------------------------------------*/
/*  Clear image to all-white background.                               */
/*                                                                     */
/*  Call before LEGEND(), IMGTILE(), IMGCONT(), etc.                   */
/*  Call INITPLOT() before using.                                      */
/*---------------------------------------------------------------------*/

void  IMGCLR()
    {
    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before IMGCONT()" ) ;
        return ;
        }

    gdImageFilledRectangle( img, 0, 0, nx, ny, white ) ;
    return ;
    }         /*  end IMGCLR()  */


/*---------------------------------------------------------------------*/
/*  Construct legend for a tiled image:                                */
/*  titles at top, legend-bar at left, text below.                     */
/*                                                                     */
/*  Call before each IMAGEWRITE().                                     */
/*  Call INITPLOT() once, before using.                                */
/*---------------------------------------------------------------------*/

void   LEGEND(
             FINT       * nvgrd,      /* number of labels for legend  */
             FREAL      * vgrd,       /* labels for legend            */
             FREAL      * gmax,       /* tile-max value               */
             FINT       * cmax,       /* grid-col for max             */
             FINT       * rmax,       /* grid-row for max             */
             FREAL      * gmin,       /* min value                    */
             FINT       * cmin,       /* grid-col for min             */
             FINT       * rmin,       /* grid-row for min             */
             FREAL      * gbar,       /* mean value                   */
             FREAL      * smax,       /* max speed                    */
             FINT       * csmax,      /* grid-col for max             */
             FINT       * rsmax,      /* grid-row for max             */
             FREAL      * smin,       /* min speed                    */
             FINT       * csmin,      /* grid-col for min             */
             FINT       * rsmin,      /* grid-row for min             */
             FREAL      * sbar,       /* RMS  value                   */
             FREAL      * vecscl,     /* Max for vector scale         */
             FREAL      * zmax,       /* max contour-vble value       */
             FINT       * czmax,      /* grid-col for max             */
             FINT       * rzmax,      /* grid-row for max             */
             FREAL      * zmin,       /* min contour-vble value       */
             FINT       * czmin,      /* grid-col for min             */
             FINT       * rzmin,      /* grid-row for min             */
             FREAL      * zbar,       /* mean contour-vble value      */
             FREAL      * zscl0,      /* Min for contour scale        */
             FREAL      * zscl1,      /* Max for contour scale        */
             const char * vname,      /* name  for  tile   variable   */
             const char * units,      /* units for  tile   variable   */
             const char * zname,      /* name  for contour variable   */
             const char * zunit,      /* units for contour variable   */
             const char * dtstr,      /* date&time string             */
             const char * title1,     /* Level-1 title                */
             const char * title2,     /* Level-2 title                */
             const char * title3,     /* Level-3 title                */
             const char * binfmt,     /* Format for legend bin labels */
             FSTR_L       vnamelen,
             FSTR_L       unitslen,
             FSTR_L       znamelen,
             FSTR_L       zunitlen,
             FSTR_L       dtstrlen,
             FSTR_L       title1len,
             FSTR_L       title2len,
             FSTR_L       title3len,
             FSTR_L       binfmtlen
             )
    {
    int       i, k, l, m, mm, n, nn, i0, i1, ix, iy, ix0, iy0, ix1, iy1, ix2, ix3 ;
    int       nlabel ;
    int       brect[  8 ] ;
    char      frmt[  16 ] ;
    char      cbuf[ 256 ] ;
    char      mesg[ 256 ] ;
    char    * err ;
    float     dy ;
    float     fontpix ;
    float     ggmax, ggmin, ggbar, ssmax, ssmin, ssbar, zzmax, zzmin, zzbar ;
    int       icmax, icmin, icsmax, icsmin, iczmax, iczmin ;
    int       irmax, irmin, irsmax, irsmin, irzmax, irzmin ;
    gdPoint points[5];

    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before LEGEND()" ) ;
        return ;
        }

    /*-----------------------------------------------------------------*/
    /*  Draw legend only if pltonly==0                                   */
    /*-----------------------------------------------------------------*/

    if ( pltonly ) return ;
    ggmax = *gmax ;
    ssmax = *smax ;
    zzmax = *zmax ;
    ggmin = *gmin ;
    ssmin = *smin ;
    zzmin = *zmin ;
    ggbar = *gbar ;
    ssbar = *sbar ;
    zzbar = *zbar ;
    
    icmax  = *cmax ;
    icmin  = *cmin ;
    irmax  = *rmax ;
    irmin  = *rmin ;
    icsmax = *csmax ;
    icsmin = *csmin ;
    irsmax = *rsmax ;
    irsmin = *rsmin ;
    iczmax = *czmax ;
    iczmin = *czmin ;
    irzmax = *rzmax ;
    irzmin = *rzmin ;

    /*-----------------------------------------------------------------*/
    /*  Titles above tiled image                                       */
    /*-----------------------------------------------------------------*/

    gdFTUseFontConfig(1);

    if ( title1len > 0 )
        {
        fontpix = ( title1len < 48 ? 24.0 : 18.0 ) ;
        iy0     = 35 + filly / 10 ;
        fstr2cstr( title1, cbuf, title1len, (FSTR_L)256 ) ;
        err = gdImageStringFT( NULL, brect, 0, "lucida", fontpix, 0.0, 0, 0, cbuf );
        if ( err ) m3mesgc( err ) ;
        else{
            ix = ( nx + brect[0] - brect[2] ) / 2 ;
            err = gdImageStringFT( img, brect, black, "lucida", fontpix, 0.0, ix, iy0, cbuf );
            if ( err ) m3mesgc( err ) ;
            }
        }

    if ( title2len > 0 )
        {
        fontpix = ( title2len < 96 ? 18.0 : 12.0 ) ;
        iy0     = 65 + ( 4 * filly ) / 30 ;
        fstr2cstr( title2, cbuf, title2len, (FSTR_L)256 ) ;
        err = gdImageStringFT( NULL, brect, 0, "lucida", fontpix, 0.0, 0, 0, cbuf );
        if ( err ) m3mesgc( err ) ;
        else{
            ix = ( nx + brect[0] - brect[2] ) / 2 ;
            err = gdImageStringFT( img, brect, black, "lucida", fontpix, 0.0, ix, iy0, cbuf );
            if ( err ) m3mesgc( err ) ;
            }
        }

    if ( title3len > 0 )
        {
        fontpix = ( title3len < 96 ? 18.0 : 12.0 ) ;
        iy0     = 90 + ( 5 * filly ) / 30 ;
        fstr2cstr( title3, cbuf, title3len, (FSTR_L)256 ) ;
        err = gdImageStringFT( NULL, brect, 0, "lucida", fontpix, 0.0, 0, 0, cbuf );
        if ( err ) m3mesgc( err ) ;
        else{
            ix = ( nx + brect[0] - brect[2] ) / 2 ;
            err = gdImageStringFT( img, brect, black, "lucida", fontpix, 0.0, ix, iy0, cbuf );
            if ( err ) m3mesgc( err ) ;
            }
        }

    /*-----------------------------------------------------------------*/
    /*  Legend below tiled image                                       */
    /*-----------------------------------------------------------------*/

    ix0 = nx0 ;
    m   = 10 + filly / 10 + 35 ;
    if ( *smax < AMISS3 ) m = m - 25 ;
    if ( *zmax < AMISS3 ) m = m - 25 ;
    if ( m < 35 )  m = 35 ;
    iy0 = ny1 + m ;
    if ( dtstrlen > 0 )
        {
        fstr2cstr( dtstr, cbuf, dtstrlen, (FSTR_L)256 ) ;
        err = gdImageStringFT( img, brect, black, "lucida", 14, 0.0, ix0, iy0, cbuf );
        if ( err ) m3mesgc( err ) ;
        iy0 = iy0 + 25 ;
        }

    if ( ggmax > AMISS3 )  
        {
        fstr2cstr( vname, cbuf, vnamelen, (FSTR_L)256 ) ;
        sprintf( cbuf, "%s   Min=%g at (c=%d,r=%d)   Max=%g at (c=%d,r=%d)   Mean=%g",
                      cbuf,
                      ggmin, icmin, irmin,
                      ggmax, icmax, irmax,
                      ggbar ) ;
        err = gdImageStringFT( img, brect, black, "lucida", 14, 0.0, ix0, iy0, cbuf );
        if ( err ) m3mesgc( err ) ;
        iy0 = iy0 + 25 ;
        }

    if ( ssmax > AMISS3 ) 
        {
        sprintf( cbuf, "|VECTOR|   Min=%g at (c=%d,r=%d)   Max=%g at (c=%d,r=%d)   Mean=%g",
                      ssmin, icsmin, irsmin,
                      ssmax, icsmax, irsmax,
                      ssbar ) ;
        err = gdImageStringFT( img, brect, black, "lucida", 14, 0.0, ix0, iy0, cbuf );
        if ( err ) m3mesgc( err ) ;
        } ;

    if ( zzmax > AMISS3 )  
        {
        fstr2cstr( zname, cbuf, vnamelen, (FSTR_L)256 ) ;
        sprintf( cbuf, "%s   Min=%g at (c=%d,r=%d)   Max=%g at (c=%d,r=%d)   Mean=%g",
                      cbuf,
                      zzmin, iczmin, irzmin,
                      zzmax, iczmax, irzmax,
                      zzbar ) ;
        err = gdImageStringFT( img, brect, black, "lucida", 14, 0.0, ix0, iy0, cbuf );
        if ( err ) m3mesgc( err ) ;
        iy0 = iy0 + 25 ;
        }

    iy0 = ny - 12 ;
    err = gdImageStringFT( img, brect, black, "lucida", 8.0, 0.0, ix0, iy0, 
                           "GDPLOT programs (c) copyright 2010-2012 BAMS, (c) Carlie J. Coats,Jr.,Ph.D 2013-2025" );
    if ( err ) m3mesgc( err ) ;

    /*-----------------------------------------------------------------*/
    /*  window extent labels for tiled image:  bottom and left         */
    /*-----------------------------------------------------------------*/

    ix0 = nx0 ;
    iy0 = ny1 + 20 ;
    sprintf( cbuf,"%d", nc0 ) ;
    err = gdImageStringFT( img, brect, black, "lucida", 12.0, 0.0, ix0, iy0, cbuf );
    if ( err ) m3mesgc( err ) ;

    sprintf( cbuf,"%d", nc1 ) ;
    err = gdImageStringFT( NULL, brect, 0,     "lucida", 12.0, 0.0,   0,   0, cbuf );
    if ( err ) m3mesgc( err ) ;
    ix0 = nx1 - brect[2] ;
    iy0 = ny1 + 20 ;
    err = gdImageStringFT( img,  brect, black, "lucida", 12.0, 0.0, ix0, iy0, cbuf );
    if ( err ) m3mesgc( err ) ;

    sprintf( cbuf,"%d", nr1 ) ;
    err = gdImageStringFT( NULL, brect, 0,     "lucida", 12.0, 0.0,   0,   0, cbuf );
    if ( err ) m3mesgc( err ) ;
    ix0 = nx1 + 4 ;
    iy0 = ny0 + ( brect[1] - brect[7] ) / 2 ;
    err = gdImageStringFT( img,  brect, black, "lucida", 12.0, 0.0, ix0, iy0, cbuf );
    if ( err ) m3mesgc( err ) ;

    sprintf( cbuf,"%d", nr0 ) ;
    err = gdImageStringFT( NULL, brect, 0,     "lucida", 12.0, 0.0,   0,   0, cbuf );
    if ( err ) m3mesgc( err ) ;
    ix0 = nx1 + 4 ;
    iy0 = ny1 + ( brect[1] - brect[7] ) / 2  ;
    err = gdImageStringFT( img,  brect, black, "lucida", 12.0, 0.0, ix0, iy0, cbuf );
    if ( err ) m3mesgc( err ) ;

    /*-----------------------------------------------------------------*/
    /*  GRID Scale bar and its legend                                  */
    /*  variable-name and units below legend-bar                       */
    /*  legend-bin labels to right of legend-bar                       */
    /*-----------------------------------------------------------------*/

    if ( ggmax > AMISS3 )  
        {
        fstr2cstr( binfmt, frmt, binfmtlen, (FSTR_L)16 ) ;

        nlabel = *nvgrd ;
        for ( l = 0 , n = 0 , i = 0 ; i < nlabel ; i++ )
            {
            sprintf( cbuf, frmt, vgrd[i] ) ;
            err = gdImageStringFT( NULL, brect, 0, "lucida", 12.0, 0.0,  0, 0, cbuf );
            k   = brect[1] - brect[7] ;     /*  height in Y of legend  */
            m   = brect[2] - brect[0] ;     /*  length in X of legend  */
            if ( k > l ) l = k ;
            if ( m > n ) n = m ;
            }

        ix0 = 8 ;                    /*  left  edge of legend-bar */
        ix1 = nx0 - n - 48 ;         /*  right edge of legend-bar */
        ix2 = nx0 - n - 36 ;         /*  legend-bar text */

        if ( vnamelen > 0 )
            {
            fstr2cstr( vname, cbuf, vnamelen, (FSTR_L)256 ) ;
            err = gdImageStringFT( NULL, brect, 0,     "lucida", 14.0, 0.0,  0,   0, cbuf );
            ix  = ( ix0 + ix1 + brect[0] - brect[2]  ) / 2 ;
            err = gdImageStringFT(  img, brect, black, "lucida", 14.0, 0.0, ix, ny1+25, cbuf );
            if ( err ) m3mesgc( err ) ;
            }

        if ( unitslen > 0 )
            {
            fstr2cstr( units, cbuf, unitslen, (FSTR_L)256 ) ;
            err = gdImageStringFT( NULL, brect, 0,     "lucida", 14.0, 0.0,  0,   0, cbuf );
            ix  = (ix0 + ix1 + brect[0] - brect[2] ) / 2 ;
            iy1 = ny1 + 20 + brect[1] - brect[7] ;
            err = gdImageStringFT(  img, brect, black, "lucida", 14.0, 0.0, ix, ny1+50, cbuf );
            if ( err ) m3mesgc( err ) ;
            }

        dy = (float) nyg / (float)( nlabel-1 ) ;
        for ( i = 0 ; i < nlabel ; i++ )
            {
            sprintf( cbuf, frmt, vgrd[i] ) ;
            iy0 =  ny1 - (int)( dy * (float)i ) + l/2 ;
            err = gdImageStringFT( img, brect, black, "lucida", 12.0, 0.0, ix2, iy0, cbuf );
            if ( err ) m3mesgc( err ) ;
            } ;

        dy = (float)( ny1-ny0 ) / (float) ncolors ;
        for ( i = 0 ; i < ncolors ; i++ )
            {
            iy0 =  ny1 - (int)( dy * (float)(i+1 ) ) ;
            iy1 =  ny1 - (int)( dy * (float) i     ) ;
            m   = colors[i] ;
            gdImageFilledRectangle( img, ix0, iy0, ix1, iy1, m ) ;
            } ;

        } ;             /*  if *gmax > AMISS3


    /*-----------------------------------------------------------------*/
    /*  CONTOUR Scale bar                                              */
    /*-----------------------------------------------------------------*/

    gdImageSetThickness( img, 1 + ( ny1 - ny0 ) / 300 ) ;

    if ( zzmax > AMISS3 )  
        {

        ix0 = nx - 70 ;
        ix1 = nx - 10 ;

        m  =  ny1-ny0-140 ;
        m  = ( m < 200 ? ny1-ny0-30 : m ) ;
        dy = (float)( m ) / (float) ( nvcolors + nzcolors - 1 ) ;
        dy = ( dy > 25.0 ? 25.0 : dy ) ;
        nn = (int)( dy * (float) nzcolors ) ;

        iy = ny1 - 10 ;
        if ( zunitlen > 0 )
            {
            fstr2cstr( zunit, cbuf, zunitlen, (FSTR_L)256 ) ;
            err = gdImageStringFT( NULL, brect, 0,     "lucida", 14.0, 0.0,  0,   0, cbuf );
            ix  = nx - 40 + (brect[0] - brect[2] ) / 2 ;
            err = gdImageStringFT(  img, brect, black, "lucida", 14.0, 0.0, ix, iy, cbuf );
            if ( err ) m3mesgc( err ) ;
            iy = iy - 20 ;
            }

        if ( znamelen > 0 )
            {
            fstr2cstr( zname, cbuf, znamelen, (FSTR_L)256 ) ;
            err = gdImageStringFT( NULL, brect, 0,     "lucida", 14.0, 0.0,  0,   0, cbuf );
            ix  = nx - 40 + (brect[0] - brect[2] ) / 2 ;
            err = gdImageStringFT(  img, brect, black, "lucida", 14.0, 0.0, ix, iy, cbuf );
            if ( err ) m3mesgc( err ) ;
            iy = iy - 20 ;
            }

        fstr2cstr( binfmt, frmt, binfmtlen, (FSTR_L)16 ) ;
        sprintf( cbuf, frmt, ( *zscl0 ) ) ;
        err = gdImageStringFT( img, brect, black, "lucida", 12.0, 0.0, nx-60, iy, cbuf );
        if ( err ) m3mesgc( err ) ;
        iy0 = iy - 20 ;

        for ( i = 0 ; i < nzcolors ; i++ )
            {
            iy = iy0 - (int)( dy * (float) i ) ;
            gdImageLine( img, ix0, iy, ix1, iy, zcolors[i] ) ;
            } ;

        iy = iy - 20 ;
        sprintf( cbuf, frmt, ( *zscl1 ) ) ;
        err = gdImageStringFT( img, brect, black, "lucida", 12.0, 0.0, nx-60, iy, cbuf );
        if ( err ) m3mesgc( err ) ;

        } ;             /*  if *zmax > AMISS3


    /*-----------------------------------------------------------------*/
    /*  VECTOR Scale bar                                               */
    /*-----------------------------------------------------------------*/

    if ( ssmax > AMISS3 )  
        {

        points[0].x = nx - 70 ;
        points[1].x = nx - 10 ;
        points[2].x = nx - 25 ;
        points[3].x = nx - 10 ;
        points[4].x = nx - 25 ;

        m  =  ny1-ny0-140 ;
        m  = ( m < 200 ? ny1-ny0-30 : m ) ;
        dy = (float)( m ) / (float) ( nvcolors + nzcolors - 1 ) ;
        dy = ( dy > 25.0 ? 25.0 : dy ) ;
        nn = (int)( dy * (float) nvcolors ) ;
        mm = m - (int)( dy * (float) ( nvcolors + nzcolors - 1 ) ) ;

        i0 = ny0 + 45 + mm/2 ;

        iy = i0 - 15 ;
        fstr2cstr( binfmt, frmt, binfmtlen, (FSTR_L)16 ) ;
        sprintf( cbuf, frmt, (*vecscl ) ) ;
        err = gdImageStringFT( img, brect, black, "lucida", 12.0, 0.0, nx-50, iy, cbuf );
        if ( err ) m3mesgc( err ) ;
        for ( i = 0; i < nvcolors ; i++ )
            {
            iy = i0 + (int)( dy * (float) (nvcolors-i-1 ) ) ;
            points[0].y = iy     ;
            points[1].y = iy     ;
            points[2].y = iy - 5 ;
            points[3].y = iy     ;
            points[4].y = iy + 5 ;
            gdImageOpenPolygon( img, points, 5, vcolors[i] ) ;          /*  arrow */
            } ;

        iy = i0 + (int)( dy * (float) ( nvcolors-1 ) ) + 20 ;
        err = gdImageStringFT( img, brect, black, "lucida", 12.0, 0.0, nx-50, iy, "0.0" );
        if ( err ) m3mesgc( err ) ;

        } ;             /*  if *smax > AMISS3

    gdImageSetThickness( img, 1 ) ;


    /*-----------------------------------------------------------------*/
    /*  Image-outline:                                                 */
    /*-----------------------------------------------------------------*/

    gdImageRectangle( img, nx0-1, ny0-1, nx1+1, ny1+1, black ) ;

    }         /*  end LEGEND()  */



/*---------------------------------------------------------------------*/
/*  Construct tiled image.                                             */
/*                                                                     */
/*  Call before IMGMAP() and IMAGEWRITE().                             */
/*  Call INITPLOT() before using.                                      */
/*---------------------------------------------------------------------*/

void  IMGTILE(
             FREAL     * vgrd,        /* grid of values to scale       */
             FREAL     * vbase   ,      /* base value for plot          */
             FREAL     * vscale         /* scale factor for plot        */
             )
    {
    int   i, j, k, x, y, color ;
    float v, vfac, v0, vmax ;
    char  mesg[256] ;

    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before IMGTILE()" ) ;
        return ;
        }

    v0   = * vbase  ;
    vfac = * vscale ;
    vmax = (float)( ncolors-1 ) ;

    for ( j = 0 ; j < nyg ; j++ )

        {
        y = ir2y[ j ] ;
        for ( i = 0 ; i < nxg ; i++ )
             {
             x = ic2x[ i ] ;
             v = VAL( vgrd, x, y ) ;
             if ( v < AMISS3 )
                 {
                 color = cmiss ;
                 }
             else{
                 v = vfac * ( v - v0 ) ;
                 if ( v < 0.0 )
                     {
                     color = colors[ 0 ] ;
                     }
                 else{
                     k = ( v < vmax ? (int)v : ncolors-1 )  ;
                     color = colors[ k ] ;
                     }
                 } ;

             gdImageSetPixel( img, i+nx0, j+ny0, color ) ;

             }
        }

    }         /*  end IMGTILE()  */



/*----------------------------------------------------------------------*/
/*  Construct smoothed colored image using bilinear interpolation       */
/*  from grid, with precomputed indices kc2x, kr2y, and coeffs          */
/*  xtrp, ytrp                                                          */
/*                                                                      */
/*  Call before IMGMAP() and IMAGEWRITE().                              */
/*  Call INITPLOT() before using.                                       */
/*----------------------------------------------------------------------*/

void  IMGSMTH(
             FREAL     * vgrd  ,        /* grid of values to scale      */
             FREAL     * vbase ,        /* base value for plot          */
             FREAL     * vscale         /* scale factor for plot        */
             )
    {
    int   i, j, k, x, y, color ;
    float v, vfac, v0, vmax ;
    float pp, px, qx, py, qy, psum, vsum ;
    char  mesg[256] ;

    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before IMGSMTH()" ) ;
        return ;
        }

    v0   = * vbase  ;
    vfac = * vscale ;
    vmax = (float)( ncolors-1 ) ;

    for ( j = 0 ; j < nyg ; j++ )

        {
        y  = kr2y[ j ] ;
        py = ytrp[ j ] ;
        qy = 1.0 - py ;
        for ( i = 0 ; i < nxg ; i++ )
             {
             x    = kc2x[ i ] ;
             px   = xtrp[ i ] ;
             qx   = 1.0 - px ;
             vsum = 0.0 ;
             psum = 0.0 ;

             v = VAL( vgrd, x, y ) ;
             if ( v > AMISS3 )
                 {
                 pp   = px * py ;
                 psum = psum + pp ;
                 vsum = vsum + pp * v ;
                 }

             v = VAL( vgrd, x+1, y ) ;
             if ( v > AMISS3 )
                 {
                 pp   = qx * py ;
                 psum = psum + pp ;
                 vsum = vsum + pp * v ;
                 }

             v = VAL( vgrd, x+1, y+1 ) ;
             if ( v > AMISS3 )
                 {
                 pp   = qx * qy ;
                 psum = psum + pp ;
                 vsum = vsum + pp * v ;
                 }

             v = VAL( vgrd, x, y+1 ) ;
             if ( v > AMISS3 )
                 {
                 pp   = px * qy ;
                 psum = psum + pp ;
                 vsum = vsum + pp * v ;
                 }

             if ( psum < 0.5 )  /* psum ~ 1.0 if all 4 points valid  */
                 {
                 color = cmiss ;
                 }
             else{
                 v = vfac * ( vsum / psum - v0 ) ;
                 if ( v < 0.0 )
                     {
                     color = colors[ 0 ] ;
                     }
                 else{
                     k = ( v < vmax ? (int)v : ncolors-1 )  ;
                     color = colors[ k ] ;
                     }
                 } ;

             gdImageSetPixel( img, i+nx0, j+ny0, color ) ;

             }
        }

    }         /*  end IMGSMTH()  */



/*---------------------------------------------------------------------*/
/*  Construct vector-arrow image overlay.                              */
/*                                                                     */
/*  Call before IMGMAP() and IMAGEWRITE().                             */
/*  Call INITPLOT() before using.                                      */
/*---------------------------------------------------------------------*/

void  IMGVECT(
             FREAL     * ugrd,          /* grid of X components         */
             FREAL     * vgrd,          /* grid of Y components         */
             FREAL     * spdmax,        /* max value for plot           */
             FINT      * vintvl,        /* sampling-interval for plot   */
             FINT      * vthick         /* line-thickness    for plot   */
             )
    {
    int     i, j, k, kk, x, y, iu, iv ;
    float   u, v, s, du, dv, sfac, smax ;
    gdPoint points[5];

    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before IMGVECT()" ) ;
        return ;
        }

    smax = ( *spdmax > 0.01 ? *spdmax : 0.01 ) ;
    sfac = (float)( nvcolors-1 ) / smax ;
    kk   = *vintvl ;
    du   = xcellpix * (float) kk ;
    dv   = ycellpix * (float) kk ;

    gdImageSetThickness( img, (*vthick) ) ;

    for ( j = vr0 + kk/2 ; j < vr1 ; j+=kk )

        {
        y = vr2y[ j ] ;

        for ( i = vc0 + kk/2 ; i < vc1 ; i+=kk )
             {
             x = vc2x[ i ] ;
             u = VAL( ugrd, i, j ) ;
             v = VAL( vgrd, i, j ) ;
             s = sqrtf( u*u + v*v ) ;

             if ( s > 0.01 * smax )
                 {
                 iu = (int)( u * du / s ) ;
                 iv = (int)( v * dv / s ) ;
                 k  = ( s < smax ? (int)( s * sfac ) : ncolors-1 )  ;
                 points[0].x = x - iu/2 ;
                 points[0].y = y - iv/2 ;
                 points[1].x = x + iu/2 ;
                 points[1].y = y + iv/2 ;
                 points[2].x = x + iv/5 ;
                 points[2].y = y - iu/5 ;
                 points[3].x = x + iu/2 ;
                 points[3].y = y + iv/2 ;
                 points[4].x = x - iv/5 ;
                 points[4].y = y + iu/5 ;
                 gdImageOpenPolygon( img, points, 5, vcolors[ k ] ) ;          /*  arrow head */
                 }
             else{
                 gdImageSetPixel( img, x, y, vcolors[ 0 ] ) ;
                 }

             }          /*  end for-loop on  */

        }               /*  end for-loop on j */

    gdImageSetThickness( img, 1 ) ;

    }                   /*  end IMGVECT()  */



/*---------------------------------------------------------------------*/
/*  Construct contour-line image overlay.                              */
/*                                                                     */
/*  Call before IMGMAP() and IMAGEWRITE().                             */
/*  Call INITPLOT() before using.                                      */
/*---------------------------------------------------------------------*/

void  IMGCONT(
             FREAL     * zgrd,          /* grid of Z values             */
             FREAL     * cbase ,        /* min value for cntours        */
             FREAL     * cintvl,        /* contour interval for plot    */
             FINT      * cthick         /* line-thickness   for plot    */
             )
    {
    int     i, j, k, m, m0, m1, n ;
    float   c0, ddc, zll, zlr, zul, zur, z0, z1, zz, s, t, x0, y0, xx, yy ;
    gdPoint points[4];

    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before IMGCONT()" ) ;
        return ;
        }

    gdImageSetThickness( img, (*cthick) ) ;

    c0   = * cbase ;
    ddc  = 1.0 / ( * cintvl ) ;
    
    if ( xcellpix > 0.0 )
        {
        x0 = (float) nx0 + 0.5 * xcellpix + 1 ;
        }
    else{
        x0 = (float) nx1 + 0.5 * xcellpix - 1 ;
        } ;
    
    if ( ycellpix > 0.0 )
        {
        y0 = (float) ny0 + 0.5 * ycellpix + 1 ;
        }
    else{
        y0 = (float) ny1 + 0.5 * ycellpix - 1 ;
        } ;

    for ( j = nr0 ; j < nr1-1 ; j++ )
        {
        yy = y0 + ycellpix * (float)( j - nr0 ) ;
        for ( i = nc0 ; i < nc1-1 ; i++ )
            {
            xx = x0 + xcellpix * (float)( i - nc0 ) ;
            
            zll = ddc * ( VAL( zgrd, i  , j   ) - c0 ) ;
            zlr = ddc * ( VAL( zgrd, i+1, j   ) - c0 ) ;
            zul = ddc * ( VAL( zgrd, i  , j+1 ) - c0 ) ;
            zur = ddc * ( VAL( zgrd, i+1, j+1 ) - c0 ) ;

            z0 = z1 = zll ;
            if ( zlr < z0 ) z0 = zlr ;
            if ( zur < z0 ) z0 = zur ;
            if ( zul < z0 ) z0 = zul ;
            if ( zlr > z1 ) z1 = zlr ;
            if ( zur > z1 ) z1 = zur ;
            if ( zul > z1 ) z1 = zul ;

            m0 = (int) z0 ;
            m1 = (int) z1 + 1 ;

            for( m = m0 ; m < m1 ; m++ )
                {
                s = (float)m ;
                k = 0 ;

                if ( ( zll <= s && zlr > s ) || ( zll > s && zlr <= s ) )       /*  bottom edge  */
                    {
                    t = ( zlr - s ) / ( zlr - zll ) ;
                    points[ k ].x = (int)( xx + t * xcellpix ) ;
                    points[ k ].y = (int)( yy ) ;
                    k++ ;
                    }
                if ( ( zlr <= s && zur > s ) || ( zlr > s && zur <= s ) )       /*  right edge  */
                    {
                    t = ( zur - s ) / ( zur - zlr ) ;
                    points[ k ].x = (int)( xx +     xcellpix ) ;
                    points[ k ].y = (int)( yy + t * ycellpix ) ;
                    k++ ;
                    }
                if ( ( zul <= s && zur > s ) || ( zul > s && zur <= s ) )       /*  top edge  */
                    {
                    t = ( zur - s ) / ( zur - zul ) ;
                    points[ k ].x = (int)( xx + t * xcellpix ) ;
                    points[ k ].y = (int)( yy +     ycellpix ) ;
                    k++ ;
                    }
                if ( ( zll <= s && zul > s ) || ( zll > s && zul <= s ) )       /*  left edge  */
                    {
                    t = ( zul - s ) / ( zul - zll ) ;
                    points[ k ].x = (int)( xx  ) ;
                    points[ k ].y = (int)( yy + t * ycellpix ) ;
                    k++ ;
                    }

                if ( k == 2 )
                    {
                    n = zcolors[ ( m < 0 ? 0 : ( m >= nzcolors-1 ? nzcolors-1 : m ) ) ] ;
                    gdImageLine( img, points[ 0 ].x, points[ 0 ].y, points[ 1 ].x, points[ 1 ].y, n ) ;
                    }
                else if ( k > 2 )
                    {
                    n = zcolors[ ( m < 0 ? 0 : ( m >= nzcolors-1 ? nzcolors-1 : m ) ) ] ;
                    gdImagePolygon( img, points, k, n ) ;
                    }

                }

            }          /*  end for-loop on  i */

        }               /*  end for-loop on j */

    gdImageSetThickness( img, 1 ) ;

    }                   /*  end IMGCONT()  */



/*---------------------------------------------------------------------*/
/*  Construct scalar obs-overlay for image.                            */
/*  Uses same color-palette as IMGTILE(), IMGSMTH()                    */
/*                                                                     */
/*  Call before IMGMAP() and IMAGEWRITE().                             */
/*  Call INITPLOT() before using.                                      */
/*---------------------------------------------------------------------*/

void  IMGOBS(
             FINT      * nobs,          /* Number of obs to plot        */
             FINT      * size,          /* Size of obs-markers          */
             FREAL     * lons,          /* obs lon coordinates          */
             FREAL     * lats,          /* obs lat coordinates          */
             FREAL     * vals,          /* obs values                   */
             FREAL     * vbase,         /* base value for scaling plot  */
             FREAL     * vscale         /* scale factor for plot        */
             )
    {
    int     i, k, n, x, y, xp0, yp0, dx ;
    float   v, v0, vfac, vmax, xx, yy, xx0, yy0, ddx, ddy ;
    char    mesg[256] ;
    gdPoint points[4];

    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before IMGOBS()" ) ;
        return ;
        }

    xx0 = (float)( xorig + xcell * (double)nc0 ) ;
    yy0 = (float)( yorig + ycell * (double)nr0 ) ;
    ddx = xcellpix * ( xcell > 0.0 ? 1.0 / xcell : -1.0 / xcell ) ;
    ddy = ycellpix * ( ycell > 0.0 ? 1.0 / ycell : -1.0 / ycell ) ;
    xp0 = ( xcell > 0.0 ? nx0 : nx1 ) ;
    yp0 = ( ycell > 0.0 ? ny1 : ny0 ) ;

    n    = *nobs ;
    dx   = *size / 2;
    v0   = * vbase  ;
    vfac = * vscale ;
    vmax = (float)( ncolors-1 ) ;

    for ( i = 0 ; i < n ; i++ )
        {
        if ( vals[i]  < AMISS3 ) continue ;

        LLXY( &lons[i], &lats[i], &xx, &yy ) ;

        x = xp0 + (int)( ( xx - xx0 ) * ddx ) ;
        if ( x < nx0 ) continue ;
        if ( x > nx1 ) continue ;

        y = yp0 + (int)( ( yy - yy0 ) * ddy ) ;
        if ( y < ny0 ) continue ;
        if ( y > ny1 ) continue ;

        points[0].x = x + dx ;         /*  diamond with diameter *size, drawn CCW */
        points[0].y = y      ;
        points[1].x = x      ;
        points[1].y = y + dx ;
        points[2].x = x - dx ;
        points[2].y = y      ;
        points[3].x = x      ;
        points[3].y = y - dx ;

        v = vfac * ( vals[i] - v0 ) ;
        k = ( v < 0.0 ? 0 : ( v < vmax ? (int)v : ncolors-1 ) ) ;

        gdImageFilledPolygon( img, points, 4, colors[ k ] ) ;
        gdImagePolygon      ( img, points, 4, black ) ;

        } ;

    }         /*  end IMGOBS()  */




/*---------------------------------------------------------------------*/
/*  Construct windvector-obs overlay for image.                        */
/*  Uses same color-palette as IMGVECT()                               */
/*                                                                     */
/*  Call before IMGMAP() and IMAGEWRITE().                             */
/*  Call INITPLOT() before using.                                      */
/*---------------------------------------------------------------------*/

void  IMGOVEC(
              FINT      * nobs,          /* Number of obs to plot        */
              FINT      * size,          /* Length of obs-vectors        */
              FREAL     * lons,          /* obs lon coordinates          */
              FREAL     * lats,          /* obs lat coordinates          */
              FREAL     * spds,          /* obs windspeed values         */
              FREAL     * angs,          /* obs wind-bearing values      */
              FREAL     * cosa,          /* gridded cos( x-axis : east ) */
              FREAL     * sina,          /* gridded sin( x-axis : east ) */
              FREAL     * vscale,        /* scale-max      for plot      */
              FINT      * vthick         /* line-thickness for plot      */
              )
    {
    int     i, k, n, x, y, xp0, yp0, iu, iv, cc, rr ;
    float   u, v, du, dv, ds, a, s, sfac, smax, xx, yy, xx0, yy0, ddx, ddy ;
    float   p, q, ca, sa, uu, vv ;
    char    mesg[256] ;
    gdPoint points[5];

    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before IMGOVEC()" ) ;
        return ;
        }

    xx0 = (float)( xorig + xcell * (double)nc0 ) ;
    yy0 = (float)( yorig + ycell * (double)nr0 ) ;
    ddx = xcellpix * ( xcell > 0.0 ? 1.0 / xcell : -1.0 / xcell ) ;
    ddy = ycellpix * ( ycell > 0.0 ? 1.0 / ycell : -1.0 / ycell ) ;
    xp0 = ( xcell > 0.0 ? nx0 : nx1 ) ;
    yp0 = ( ycell > 0.0 ? ny1 : ny0 ) ;

    n    = *nobs ;
    smax = *vscale ;
    sfac = (float)( nvcolors-1 ) / smax ;

    ds   = (float) (*size) ;
    du   = xcellpix * ds ;
    dv   = ycellpix * ds ;

    gdImageSetThickness( img, (*vthick) ) ;

    for ( i = 0 ; i < n ; i++ )
        {

        LLXY( &lons[i], &lats[i], &xx, &yy ) ;

        x = xp0 + (int)( ( xx - xx0 ) * ddx ) ;
        if ( x < nx0 ) continue ;
        if ( x > nx1 ) continue ;

        y = yp0 + (int)( ( yy - yy0 ) * ddy ) ;
        if ( y < ny0 ) continue ;
        if ( y > ny1 ) continue ;

        s = spds[ i ] ;
        a = angs[ i ] ;
        if ( s  < AMISS3 ) continue ;
        if ( a  < AMISS3 ) continue ;
        
        u = -s * cosf( PI180 * a ) ;    /*  Easting, Northing components   */
        v = -s * sinf( PI180 * a ) ;

        if ( s > 0.01 * smax )
             {
             xx = ( xx - xorig ) / xcell ;      /* interpolate cosa(:,:), sina(:,:) to point <xx,yy>  */
             yy = ( yy - yorig ) / ycell ;
             cc = (int) xx ;
             rr = (int) yy ;
             p  = (float)( cc+1 ) - xx ;
             q  = (float)( rr+1 ) - yy ;
             p  = ( p > 1.0 ? 1.0 : ( p < 0.0 ? 0.0 : p ) ) ;
             q  = ( q > 1.0 ? 1.0 : ( q < 0.0 ? 0.0 : q ) ) ;
             cc = ( cc < 0 ? 0 : ( cc > nc-2 ? nc-2 : cc ) ) ;
             rr = ( rr < 0 ? 0 : ( rr > nr-2 ? nr-2 : rr ) ) ;

             ca =         p   *         q   * VAL( cosa, cc  , rr   ) +
                  ( 1.0 - p ) *         q   * VAL( cosa, cc+1, rr   ) +
                          p   * ( 1.0 - q ) * VAL( cosa, cc  , rr+1 ) +
                  ( 1.0 - p ) * ( 1.0 - q ) * VAL( cosa, cc+1, rr+1 ) ;
             sa =         p   *         q   * VAL( sina, cc  , rr   ) +
                  ( 1.0 - p ) *         q   * VAL( sina, cc+1, rr   ) +
                          p   * ( 1.0 - q ) * VAL( sina, cc  , rr+1 ) +
                  ( 1.0 - p ) * ( 1.0 - q ) * VAL( sina, cc+1, rr+1 ) ;

             uu = ca * u - sa * v ;     /* rotate Easting,Northing components to X,Y components  */
             vv = sa * u + ca * v ;
             iu = (int)( uu * du / s ) ;
             iv = (int)( vv * dv / s ) ;

             points[0].x = x - iu/2 ;
             points[0].y = y - iv/2 ;
             points[1].x = x + iu/2 ;
             points[1].y = y + iv/2 ;
             points[2].x = x + iv/5 ;
             points[2].y = y - iu/5 ;
             points[3].x = x + iu/2 ;
             points[3].y = y + iv/2 ;
             points[4].x = x - iv/5 ;
             points[4].y = y + iu/5 ;

             k  = ( s < smax ? (int)( s * sfac ) : ncolors-1 )  ;
             gdImageOpenPolygon( img, points, 5, vcolors[ k ] ) ;          /*  arrow head */
             }
         else{
             gdImageSetPixel( img, x, y, vcolors[ 0 ] ) ;
             }

        } ;

    gdImageSetThickness( img, 1 ) ;

    }         /*  end IMGOVEC()  */




/**********************************************************************/
/*  Draw inicated map from the indicated MCIDAS map file.             */
/*  PRECONDITIONS:                                                    */
/*      call INITPLOT(), and INITMAP() for the indicated map first.   */
/*      call after IMGTILE() and before IMGWRITE().                   */
/**********************************************************************/

void  IMGMAP( FINT * mapnum )        /* 0 or 1 for map number         */
    {
    int       mapno ;
    int       color ;
    int       npoly, ncnt, ndex, npnts ;
    int     * mapcnt ;
    int     * mapdex ;
    gdPoint * mappnt ;
    int       n, j, k ;
    char      mesg[ 256 ] ;

    if ( ! mapok ) return ;

    gdImageSetClip( img, nx0, ny0, nx1, ny1 ) ;

    mapno = *mapnum ;
    if ( 1 == mapno )
        {
        gdImageSetThickness( img, wmap1 ) ;
        color  = cmap1 ;
        npoly  = npoly1 ;
        npnts  = npoint1 ;
        mapcnt = mapcnt1 ;
        mapdex = mapdex1 ;
        mappnt = mappnt1 ;
        }
    else if ( 2 == mapno )
        {
        gdImageSetThickness( img, wmap2 ) ;
        color  = cmap2 ;
        npoly  = npoly2 ;
        npnts  = npoint2 ;
        mapcnt = mapcnt2 ;
        mapdex = mapdex2 ;
        mappnt = mappnt2 ;
        }
    else if ( 3 == mapno )
        {
        gdImageSetThickness( img, wmap3 ) ;
        color  = cmap3 ;
        npoly  = npoly3 ;
        npnts  = npoint3 ;
        mapcnt = mapcnt3 ;
        mapdex = mapdex3 ;
        mappnt = mappnt3 ;
        }
    else{
        sprintf( mesg, "Unsupported map number %d (maps are 1-3 only)", mapno ) ;
        m3mesgc( mesg ) ;
        return ;
        } ;

    for ( n = 0 ; n < npoly ; n++ )
        {
        ndex = mapdex[n] ;
        ncnt = mapcnt[n] ;
        gdImageOpenPolygon( img, &mappnt[ ndex ], ncnt, color ) ;
        } ;

    gdImageSetThickness( img, 1 ) ;

    gdImageSetClip( img, 0, 0, nx-1, ny-1 ) ;

    }         /*  end IMGMAP()  */



/**********************************************************************/
/*  Write the completed image to a file of the indicated type.        */
/*  PRECONDITIONS:                                                    */
/*      construct image with IMGTILE(), LEGEND(), and IMGMAP() first. */
/**********************************************************************/

void  IMGWRITE(
               FINT       * imgtype,      /* 1:  GIF, 2:  JPEG, 3:  PNG   */
               const char * imgfile,      /* output image file path name  */
               FINT       * istat ,
               FSTR_L       imgfilelen
               )
    {
    char      mesg[ 256 ] ;
    FSTR_L    flen ;
    char      fname[ 512 ] ;
    FILE    * f ;

    if ( ! img )
        {
        m3mesgc( "Call INITPLOT() before IMGWRITE()" ) ;
        * istat = -1 ;
        return ;
        }

    name2cstr( imgfile, fname, imgfilelen, (FSTR_L)512 ) ;
    if ( ! ( f = fopen( fname, "w" ) ) )
        {
        m3mesgc( "GDPLOT/IMGWRITE():  fopen() failure" ) ;
        perror( (char *) 0 ) ;
        *istat = -1 ;
        return ;
        }

    if       ( *imgtype == GIF )
        {
        gdImageGif( img, f ) ;
        }
    else if  ( *imgtype == JPG )
        {
        gdImageJpeg( img, f, -1 ) ;
        }
    else if  ( *imgtype == PNG )
        {
        gdImagePng( img, f ) ;
        }

    fclose( f ) ;

    *istat = 0 ;
    return ;

    }          /*  end IMGWRITE()  */
