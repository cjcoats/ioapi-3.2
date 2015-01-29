
        LOGICAL FUNCTION CRDICT3( FID )

C***********************************************************************
C Version "$Id: crdict3.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  66
C
C  FUNCTION:  Perform "Models-3 variables" part of netCDF file creation
C             for CREATE3 for the dictionary file with index FID.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:  Should only be called by CREATE3, after it has
C             performed the general attribute initializations appropriate
C             for all file types.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  INDEX1
C
C  REVISION  HISTORY:
C       prototype 3/92 by CJC
C
C       Modified 03/20010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID		!  file index within the STATE3 commons


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IERR            !  netCDF error status return
        INTEGER         FNUM            !  local for CDFID( FID )
        INTEGER         TDIM            !  timestep  dim #
        INTEGER         VDIM            !  file-variable-name dim #
        INTEGER         ZDIM            !  vertical-layer dim #
        INTEGER         NDIM            !  name-char-position dim #
        INTEGER         DDIM            !  var-desc-char-position dim #
        INTEGER         LDIM            !  file-desc-line-# dim #
        INTEGER         DIMS( 5 )       !  array of dims for NCVDEF()
        INTEGER         VAR             !  loop counter
        CHARACTER*16    VUNIT( MXVARS3 )! variable-unit table
        CHARACTER*16    VDESC( MXVARS3 )! variable-description table


C***********************************************************************
C   begin body of function  CRDICT3

C.......   Local copy of netCDF file ID:

        FNUM = CDFID3( FID )

C...........   Dimensions TDIM for dictionary index number,
C...........   VDIM for variable number

        TDIM = NCDDEF( FNUM, 'FINDX3', NCUNLIM, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF file dimension FINDX3.',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCDDEF() failed

        VDIM = NCDDEF( FNUM, 'VINDX', MXVARS3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF dimension VINDX',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCDDEF() failed

        ZDIM = NCDDEF( FNUM, 'ZINDX', MXLAYS3 + 1, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF dimension ZINDX',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCDDEF() failed

C...........   Dimensions NDIM for character-position in name variables,
C...........   DDIM for character-position in description variables,
C...........   LDIM for line-number in file-description variable:

        NDIM = NCDDEF( FNUM, 'NAME-POS', NAMLEN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF dimension NAME-POS.',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCDDEF() failed

        DDIM = NCDDEF( FNUM, 'DESC-POS', MXDLEN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF dimension DESC-POS.',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCDDEF() failed

        LDIM = NCDDEF( FNUM, 'DLINE-NUM', MXDESC3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF dimension DLINE-NUM.',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCDDEF() failed


C...........   File type:

        DIMS( 1 ) = TDIM	!  file indexes:  unlimited file index dimension

        TINDX3( FID ) = NCVDEF( FNUM, 'FLAG', NCLONG, 1, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable FLAG',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed

        CALL NCAPTC( FNUM, TINDX3( FID ), 'units', NCCHAR,
     &               NAMLEN3, '5461 or not', IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating FLAG attribute UNITS',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCAPTC() failed

        CALL NCAPTC( FNUM, TINDX3( FID ), 'var_desc', NCCHAR, MXDLEN3,
     &               'Record-valid flags:  OKFLAG3=5461, or not', IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating FLAG attribute VAR_DESC',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCAPTC() failed


C...............   Variable list:

        VLIST3(  1,FID ) = 'FNAME'	!  type:  char*16
        VUNIT (  1 )     = 'name text'
        VDESC (  1 )     = 'File-schema name variable'

        VLIST3(  2,FID ) = 'FTYPE'	!  type:  integer
        VUNIT (  2 )     = '-1...5'
        VDESC (  2 )     = 'file type variable'

        VLIST3(  3,FID ) = 'TSTEP'	!  type:  integer
        VUNIT (  3 )     = 'hhmmss'
        VDESC (  3 )     = 'time step variable'

        VLIST3(  4,FID ) = 'NVARS'	!  type:  integer
        VUNIT (  4 )     = 'count'
        VDESC (  4 )     = 'variables-count variable'

        VLIST3(  5,FID ) = 'NLAYS'	!  type:  integer
        VUNIT (  5 )     = 'count'
        VDESC (  5 )     = 'layers-count variable'

        VLIST3(  6,FID ) = 'NROWS'	!  type:  integer
        VUNIT (  6 )     = 'count'
        VDESC (  6 )     = 'rows-count variable'

        VLIST3(  7,FID ) = 'NCOLS'	!  type:  integer
        VUNIT (  7 )     = 'count'
        VDESC (  7 )     = 'columns-count variable'

        VLIST3(  8,FID ) = 'NTHIK'	!  type:  integer
        VUNIT (  8 )     = 'count'
        VDESC (  8 )     = 'bdy perimeter thickness variable'

        VLIST3(  9,FID ) = 'GDTYP'	!  type:  integer
        VUNIT (  9 )     = 'token'
        VDESC (  9 )     = 'horizontal grid type (see FDESC3.EXT)'

        VLIST3( 10,FID ) = 'VGTYP'	!  type:  integer
        VUNIT ( 10 )     = 'token'
        VDESC ( 10 )     = 'vertical grid type (see FDESC3.EXT)'

        VLIST3( 11,FID ) = 'VGLVS'	!  type:  float
        VUNIT ( 11 )     = 'vert.proj'
        VDESC ( 11 )     = 'level surfaces'

        VLIST3( 12,FID ) = 'P_ALP'	!  type:  double
        VUNIT ( 12 )     = 'deg'
        VDESC ( 12 )     = 'First map projection parameter angle'

        VLIST3( 13,FID ) = 'P_BET'	!  type:  double
        VUNIT ( 13 )     = 'deg'
        VDESC ( 13 )     = 'Second map projection parameter angle'

        VLIST3( 14,FID ) = 'P_GAM'	!  type:  double
        VUNIT ( 14 )     = 'deg'
        VDESC ( 14 )     = 'Third map projection parameter angle'

        VLIST3( 15,FID ) = 'XCENT'	!  type:  double
        VUNIT ( 15 )     = 'deg.lon'
        VDESC ( 15 )     = 'X-coordinate map coord origin variable'

        VLIST3( 16,FID ) = 'YCENT'	!  type:  double
        VUNIT ( 16 )     = 'deg.lat'
        VDESC ( 16 )     = 'Y-coordinate map coord origin variable'

        VLIST3( 17,FID ) = 'XORIG'	!  type:  double
        VUNIT ( 17 )     = 'position'
        VDESC ( 17 )     = 'X-coordinate grid origin variable'

        VLIST3( 18,FID ) = 'YORIG'	!  type:  double
        VUNIT ( 18 )     = 'position'
        VDESC ( 18 )     = 'Y-coordinate grid origin variable'

        VLIST3( 19,FID ) = 'XCELL'	!  type:  double
        VUNIT ( 19 )     = 'size'
        VDESC ( 19 )     = 'X-coordinate cell size variable'

        VLIST3( 20,FID ) = 'YCELL'	!  type:  double
        VUNIT ( 20 )     = 'size'
        VDESC ( 20 )     = 'Y-coordinate cell size  variable'

        VLIST3( 21,FID ) = 'GDNAM'	!  type:  char*16
        VUNIT ( 21 )     = 'name text'
        VDESC ( 21 )     = 'grid domain name variable'

        VLIST3( 22,FID ) = 'FDESC'	!  type:  char*80( MXDESC3=4 )
        VUNIT ( 22 )     = 'desc text'
        VDESC ( 22 )     = 'file description variable'

        VLIST3( 23,FID ) = 'VNAME'	!  type:  char*16( NVARS )
        VUNIT ( 23 )     = 'name text'
        VDESC ( 23 )     = 'variable-names-list variable'

        VLIST3( 24,FID ) = 'VUNIT'	!  type:  char*16( NVARS )
        VUNIT ( 24 )     = 'name text'
        VDESC ( 24 )     = 'variable-units-list  variable'

        VLIST3( 25,FID ) = 'VDESC'	!  type:  char*80( NVARS )
        VUNIT ( 25 )     = 'desc text'
        VDESC ( 25 )     = 'variable-descriptions-list  variable'

        VLIST3( 26,FID ) = 'VTYPE'	!  type:  integer
        VUNIT ( 26 )     = 'basic datatype'
        VDESC ( 26 )     = 'M3INT, M3REAL, or M3DBLE for '//
     &          'INTEGER, REAL, or DOUBLE PRECISION types'


C.......   Put header attributes:  number of variables and
C.......   variables-list for the file:

        NVARS3( FID ) = 26
        CALL NCAPT( FNUM, NCGLOBAL, 'NVARS', NCLONG,
     &              1, NVARS3( FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF attribute NVARS',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF          !  ierr nonzero:  NCAPT() failed

        CALL NCAPTC( FNUM, NCGLOBAL, 'VAR-LIST', NCCHAR,
     &               NAMLEN3 * NVARS3( FID ), VLIST3( 1,FID ), IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating attribute VAR-LIST ',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCAPTC() failed


C.......   Make netCDF definitions for all these variables:

C...........   Schema name:

        DIMS( 1 )  = NDIM	!  namlen3 (=16) characters per name
        DIMS( 2 )  = TDIM	!  unlimited file index dimension

        VINDX3( 1,FID ) = NCVDEF( FNUM, VLIST3( 1,FID ), NCCHAR, 2,
     &                            DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable FNAME' ,
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed

        DIMS( 1 ) = TDIM	!  unlimited file index dimension

        DO  111  VAR = 2 , 10	!  define the integer variables

            VINDX3( VAR,FID ) = NCVDEF( FNUM, VLIST3( VAR,FID ),
     &                                  NCLONG, 1, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'Error creating netCDF variable '
     &              // VLIST3( VAR,FID ),
     &              'File name ' // FLIST3( FID ) ,
     &              'netCDF error number', IERR
                CRDICT3 = .FALSE.
                CALL NCABOR( FNUM, IERR )
                RETURN
            END IF              !  ierr nonzero:  NCVDEF() failed

111     CONTINUE	!  end loop on integer variables

        DIMS( 1 )  = ZDIM	!  number of layer surfaces
        DIMS( 2 )  = TDIM	!  unlimited file index dimension

        VINDX3( 11,FID ) = NCVDEF( FNUM, VLIST3( 11,FID ),
     &                              NCDOUBLE, 1, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable '
     &          // VLIST3( 11,FID ),
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed

        DIMS( 1 ) = TDIM	!  unlimited file index dimension

        DO  122  VAR = 12 , 20	!  define the double variables

            VINDX3( VAR,FID ) = NCVDEF( FNUM, VLIST3( VAR,FID ),
     &                                  NCDOUBLE, 1, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'Error creating netCDF variable '
     &              // VLIST3( VAR,FID ),
     &              'File name ' // FLIST3( FID ) ,
     &              'netCDF error number', IERR
                CRDICT3 = .FALSE.
                CALL NCABOR( FNUM, IERR )
                RETURN
            END IF              !  ierr nonzero:  NCVDEF() failed

122     CONTINUE	!  end loop on double variables


C...........   Grid name:

        DIMS( 1 )  = NDIM	!  namlen3 (=16) characters per name
        DIMS( 2 )  = TDIM	!  unlimited file index dimension

        VINDX3( 21,FID ) = NCVDEF( FNUM, VLIST3( 21,FID ), NCCHAR, 2,
     &                             DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable GDNAM' ,
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed


C...........   File description:  MXDESC3 lines of MXDLEN3 characters per file

        DIMS( 1 )  = DDIM	!  mxdlen3 (=80) characters per line
        DIMS( 2 )  = LDIM	!  mxdesc3 (= 4) lines
        DIMS( 3 )  = TDIM       !  unlimited file index dimension

        VINDX3( 22,FID ) = NCVDEF( FNUM, VLIST3( 22,FID ), NCCHAR, 3,
     &                             DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable ' // VLIST3( 22,FID ),
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed


C...........   Variable names, units, and descriptions:

        DIMS( 1 ) = NDIM	!  namlen3 (=16) columns
        DIMS( 2 ) = VDIM	!  mxvars3 (=60) variables
        DIMS( 3 ) = TDIM	!  unlimited file index dimension

        VINDX3( 23,FID ) = NCVDEF( FNUM, VLIST3( 23,FID ), NCCHAR, 3,
     &                             DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable ' // VLIST3( 23,FID ),
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed

        VINDX3( 24,FID ) = NCVDEF( FNUM, VLIST3( 24,FID ), NCCHAR, 3,
     &                             DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable ' // VLIST3( 24,FID ),
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed

        DIMS( 1 ) = DDIM	!  mxdlen3 (=80) chars per description
        DIMS( 2 ) = VDIM	!  mxvars3 (=60) variables per file
        DIMS( 3 ) = TDIM	!  unlimited file index dimension

        VINDX3( 25,FID ) = NCVDEF( FNUM, VLIST3( 25,FID ), NCCHAR, 3,
     &                             DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable ' // VLIST3( 25,FID ),
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed

        DIMS( 1 ) = VDIM	!  mxvars3 (=60) variables per file
        DIMS( 2 ) = TDIM	!  unlimited file index dimension

        VINDX3( 26,FID ) = NCVDEF( FNUM, VLIST3( 26,FID ), NCLONG, 2,
     &                             DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error creating netCDF variable ' // VLIST3( 26,FID ),
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CRDICT3 = .FALSE.
            CALL NCABOR( FNUM, IERR )
            RETURN
        END IF              !  ierr nonzero:  NCVDEF() failed


        DO  133  VAR = 1 , NVARS3( FID )	!  put units and descriptions:

            CALL NCAPTC( FNUM, VINDX3( VAR,FID ), 'units', NCCHAR,
     &                   NAMLEN3, VUNIT( VAR ), IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'Error creating ' // VLIST3( VAR,FID ) //
     &                  ' attribute UNITS',
     &              'File name ' // FLIST3( FID ) ,
     &              'netCDF error number', IERR
                CRDICT3 = .FALSE.
                CALL NCABOR( FNUM, IERR )
                RETURN
            END IF              !  ierr nonzero:  NCAPTC() failed

            CALL NCAPTC( FNUM, VINDX3( VAR,FID ), 'var_desc', NCCHAR,
     &                   MXDLEN3, VDESC( VAR ), IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'Error creating ' // VLIST3( VAR,FID )
     &                  // ' attribute VAR_DESC',
     &              'File name:  ' // FLIST3( FID ),
     &              'netCDF error number', IERR
                CRDICT3 = .FALSE.
                CALL NCABOR( FNUM, IERR )
                RETURN
            END IF              !  ierr nonzero:  NCAPTC() failed

133     CONTINUE	!  end loop putting units and descriptions.


C.......   Put FNUM back into data mode:

        CALL NCENDF( FNUM, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error returning netCDF file into data mode.',
     &          'File name ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            CALL NCABOR( FNUM, IERR )
            CRDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCENDF() failed

        COUNT3  =  COUNT3  +  1
        CRDICT3 = .TRUE.
        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CRDICT3 <<<',
     &            3 ( /5X , A , : ) , I5, // )


        END FUNCTION CRDICT3

