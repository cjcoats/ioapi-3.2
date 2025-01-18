
        MODULE MATXATTS

        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !! Version "$Id: matxatts.f 100 2015-01-16 16:52:16Z coats $"
        !! Copyright (c) 2005-2013 Baron Advanced Meteorological Systems.
        !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
        !! See file "LGPL.txt" for conditions of use.
        !!...................................................................
        !!  DESCRIPTION:
        !!      Use the M3IO "extra-attributes" interface to matrix files.
        !!
        !!      GETMTXATT:  get input and output grid description
        !!      attributes for matrix-files and return as arguments
        !!
        !!      SETMTXATT:  set or check input and output grid description
        !!      attributes from arguments for matrix-files:  if file
        !!      is NEW, set the attributes; else check them.
        !!
        !!      CHKMTXATT:  check input and output grid description
        !!      attributes for matrix-files against attributes from
        !!      the argument list
        !!
        !!  PRECONDITIONS:
        !!      FNAME already opened by OPEN3()
        !!      IMODE  is one of { INGRD3, OUTGRD3 }
        !!      
        !!      Set attributes before writing any data.
        !!
        !!  REVISION  HISTORY:
        !!      Prototype 12/2004 by Carlie J. Coats, Jr., BAMS
        !!      Version   04/2011 by CJC:  FORTRAN-90'isms for I/O API v3.1
        !!      Version   10/2013 by CJC:  bug-fixes for "rdatt3c" calls
        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            USE M3UTILIO

            IMPLICIT NONE


            !!........  PARAMETERs:

            INTEGER, PARAMETER ::  INGRD3  = 1
            INTEGER, PARAMETER ::  OUTGRD3 = 2

            CHARACTER(LEN=5), PRIVATE, PARAMETER :: GDNAMSTR = 'GDNAM'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: GDTYPSTR = 'GDTYP'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: P_ALPSTR = 'P_ALP'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: P_BETSTR = 'P_BET'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: P_GAMSTR = 'P_GAM'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: XCENTSTR = 'XCENT'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: YCENTSTR = 'YCENT'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: XORIGSTR = 'XORIG'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: YORIGSTR = 'YORIG'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: XCELLSTR = 'XCELL'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: YCELLSTR = 'YCELL'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: NCOLSSTR = 'NCOLS'
            CHARACTER(LEN=5), PRIVATE, PARAMETER :: NROWSSTR = 'NROWS'

            CHARACTER(LEN=4), PRIVATE, PARAMETER :: MODENAME( 2 ) =  
     &          (/ '_IN ' , '_OUT' /)


            !!........  PUBLIC Routines:


            PUBLIC      GETMTXATT, SETMTXATT, CHKMTXATT


        CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        LOGICAL FUNCTION  GETMTXATT( FNAME, IMODE, GDNAM,
     &                  GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                  XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS )


            !!........  Arguments:

            CHARACTER( LEN=* ), INTENT(  IN ):: FNAME
            INTEGER           , INTENT(  IN ):: IMODE
            CHARACTER( LEN=* ), INTENT( OUT ):: GDNAM
            INTEGER           , INTENT( OUT ):: GDTYP
            REAL*8            , INTENT( OUT ):: P_ALP
            REAL*8            , INTENT( OUT ):: P_BET
            REAL*8            , INTENT( OUT ):: P_GAM
            REAL*8            , INTENT( OUT ):: XCENT
            REAL*8            , INTENT( OUT ):: YCENT
            REAL*8            , INTENT( OUT ):: XORIG
            REAL*8            , INTENT( OUT ):: YORIG
            REAL*8            , INTENT( OUT ):: XCELL
            REAL*8            , INTENT( OUT ):: YCELL
            INTEGER           , INTENT( OUT ):: NCOLS
            INTEGER           , INTENT( OUT ):: NROWS
 

            !!........  Parameter:

            CHARACTER*24, PARAMETER :: PNAME = 'MATXATTS/GETMTXATT'


            !!........  Local Variables:

            INTEGER             N
            LOGICAL             EFLAG
            CHARACTER*16        ANAME, GNAME
            CHARACTER*256       MESG


            !!........  body:

            EFLAG = .FALSE.

            IF ( IMODE .NE. INGRD3 .AND. IMODE .NE. OUTGRD3 ) THEN
                WRITE( MESG, '(A, I10)' ) 'Unrecognized IMODE =', IMODE
                CALL M3WARN( PNAME, 0, 0, MESG )
                GETMTXATT = .FALSE.
                RETURN
            END IF

            ANAME = GDNAMSTR // MODENAME( IMODE )
            IF ( .NOT.RDATTC( FNAME, ALLVAR3, ANAME, GNAME ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            ELSE
                GDNAM = GNAME
            END IF

            ANAME = GDTYPSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3INT, 1,
     &                        N, GDTYP ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = P_ALPSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, P_ALP ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = P_BETSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, P_BET ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = P_GAMSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, P_GAM ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = XCENTSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, XCENT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = YCENTSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, YCENT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = XORIGSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, XORIG ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = YORIGSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, YORIG ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = XCELLSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, XCELL ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = YCELLSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1,
     &                        N, YCELL ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = NCOLSSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3INT, 1,
     &                        N, NCOLS ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = NROWSSTR // MODENAME( IMODE )
            IF ( .NOT.RDATT3( FNAME, ALLVAR3, ANAME, M3INT, 1,
     &                        N, NROWS ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' //TRIM( ANAME )//
     &                  '" from ' // FNAME 
                CALL M3MESG( MESG )
            END IF


            GETMTXATT = ( .NOT.EFLAG )
            RETURN


        END FUNCTION  GETMTXATT


        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        LOGICAL FUNCTION  SETMTXATT( FNAME, IMODE, GDNAM,
     &                  GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                  XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS )


            !!........  Include file:

            INCLUDE 'NETCDF.EXT'      ! netCDF  declarations
            INCLUDE 'STATE3.EXT'      ! I/O API internal state


            !!........  Arguments:

            CHARACTER( LEN=* ), INTENT( IN ):: FNAME
            INTEGER           , INTENT( IN ):: IMODE
            CHARACTER( LEN=* ), INTENT( IN ):: GDNAM
            INTEGER           , INTENT( IN ):: GDTYP
            REAL*8            , INTENT( IN ):: P_ALP
            REAL*8            , INTENT( IN ):: P_BET
            REAL*8            , INTENT( IN ):: P_GAM
            REAL*8            , INTENT( IN ):: XCENT
            REAL*8            , INTENT( IN ):: YCENT
            REAL*8            , INTENT( IN ):: XORIG
            REAL*8            , INTENT( IN ):: YORIG
            REAL*8            , INTENT( IN ):: XCELL
            REAL*8            , INTENT( IN ):: YCELL
            INTEGER           , INTENT( IN ):: NCOLS
            INTEGER           , INTENT( IN ):: NROWS
 

            !!........  Parameter:

            CHARACTER*24, PARAMETER :: PNAME = 'MATXATTS/SETMTXATT'


            !!........  External function:  logical name to I/O API file ID

            INTEGER     NAME2FID
            EXTERNAL    NAME2FID


            !!........  Local Variables:

            INTEGER             FID
            LOGICAL             EFLAG
            CHARACTER*16        ANAME, GNAME
            CHARACTER*256       MESG


            !!........  body:

            EFLAG = .FALSE.

            IF ( IMODE .NE. INGRD3 .AND. IMODE .NE. OUTGRD3 ) THEN
                WRITE( MESG, '(A, I10)' ) 'Unrecognized IMODE =', IMODE
                CALL M3WARN( PNAME, 0, 0, MESG )
                SETMTXATT = .FALSE.
                RETURN
            END IF

            !!  Get I/O API file id.
            !!  If we can't put file into netCDF define mode, must be an
            !!  existing already-written file:  return CHKMTXATT():

            FID = NAME2FID( FNAME )

            IF ( FID .LE. 0 ) THEN

                MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
                CALL M3WARN( PNAME, 0, 0, MESG )
                SETMTXATT = .FALSE.
                RETURN

            ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN

                MESG = 'File "' // TRIM( FNAME ) //'" not netCDF'
                CALL M3WARN( PNAME, 0, 0, MESG )
                SETMTXATT = .FALSE.
                RETURN

            END IF

            ANAME = GDNAMSTR // MODENAME( IMODE )
            GNAME = GDNAM
            IF ( .NOT.WRATTC( FNAME, ALLVAR3, ANAME, GNAME ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = GDTYPSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3INT,
     &                        1, GDTYP ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
                CALL M3WARN( PNAME, 0, 0, MESG )
            END IF

            ANAME = P_ALPSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, P_ALP ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = P_BETSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, P_BET ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = P_GAMSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, P_GAM ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = XCENTSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, XCENT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = YCENTSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, YCENT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = XORIGSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, XORIG ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = YORIGSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, YORIG ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = XCELLSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, XCELL ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = YCELLSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE,
     &                        1, YCELL ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = NCOLSSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3INT,
     &                        1, NCOLS ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF

            ANAME = NROWSSTR // MODENAME( IMODE )
            IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3INT,
     &                        1, NROWS ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not write attribute "' //TRIM( ANAME )//
     &                  '" to ' // FNAME 
                CALL M3MESG( MESG )
            END IF


            SETMTXATT = ( .NOT.EFLAG )
            RETURN


        END FUNCTION  SETMTXATT


        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        LOGICAL FUNCTION  CHKMTXATT( FNAME, IMODE, GDNAM,
     &                  GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                  XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS )


            !!........  Arguments:

            CHARACTER( LEN=* ), INTENT( IN ):: FNAME
            INTEGER           , INTENT( IN ):: IMODE
            CHARACTER( LEN=* ), INTENT( IN ):: GDNAM
            INTEGER           , INTENT( IN ):: GDTYP
            REAL*8            , INTENT( IN ):: P_ALP
            REAL*8            , INTENT( IN ):: P_BET
            REAL*8            , INTENT( IN ):: P_GAM
            REAL*8            , INTENT( IN ):: XCENT
            REAL*8            , INTENT( IN ):: YCENT
            REAL*8            , INTENT( IN ):: XORIG
            REAL*8            , INTENT( IN ):: YORIG
            REAL*8            , INTENT( IN ):: XCELL
            REAL*8            , INTENT( IN ):: YCELL
            INTEGER           , INTENT( IN ):: NCOLS
            INTEGER           , INTENT( IN ):: NROWS
 

            !!........  Parameter:

            CHARACTER*24, PARAMETER :: PNAME = 'MATXATTS/CHKMTXATT'


            !!........  Local Variables:

            CHARACTER( LEN=NAMLEN3 ):: GDNAM1
            INTEGER                 :: GDTYP1
            REAL*8                  :: P_ALP1
            REAL*8                  :: P_BET1
            REAL*8                  :: P_GAM1
            REAL*8                  :: XCENT1
            REAL*8                  :: YCENT1
            REAL*8                  :: XORIG1
            REAL*8                  :: YORIG1
            REAL*8                  :: XCELL1
            REAL*8                  :: YCELL1
            INTEGER                 :: NCOLS1
            INTEGER                 :: NROWS1
 
            LOGICAL                 :: EFLAG
            CHARACTER*256           :: MESG


            !!........  Statement Function:  REAL*8 "definitely unequal"

            LOGICAL         DBLERR
            REAL*8          P, Q

            DBLERR( P, Q ) =
     &      ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )


            !!........  body:

            EFLAG = .FALSE.

            IF ( IMODE .NE. INGRD3 .AND. IMODE .NE. OUTGRD3 ) THEN
                WRITE( MESG, '(A, I10)' ) 'Unrecognized IMODE =', IMODE
                CALL M3WARN( PNAME, 0, 0, MESG )
            END IF

            IF ( .NOT.GETMTXATT( FNAME, IMODE, GDNAM1,
     &                           GDTYP1, P_ALP1, P_BET1, P_GAM1,
     &                           XCENT1, YCENT1, XORIG1, YORIG1,
     &                           XCELL1, YCELL1, NCOLS1, NROWS1 ) ) THEN
                MESG = 'Could not get attributes for checking'
                CALL M3WARN( PNAME, 0, 0, MESG )
                CHKMTXATT = .FALSE.
                RETURN
            END IF

            IF ( GDNAM .NE. GDNAM1 ) THEN
                MESG = 'GDNAM mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
            END IF

            IF ( GDTYP .NE. GDTYP1 ) THEN
                MESG = 'GDTYP mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NCOLS .NE. NCOLS1 ) THEN
                MESG = 'NCOLS mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NROWS .NE. NROWS1 ) THEN
                MESG = 'NROWS mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_ALP, P_ALP1 ) ) THEN
                MESG = 'P_ALP mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_BET, P_BET1 ) ) THEN
                MESG = 'P_BET mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_GAM, P_GAM1 ) ) THEN
                MESG = 'P_GAM mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( XCENT, XCENT1 ) ) THEN
                MESG = 'XCENT mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YCENT, YCENT1 ) ) THEN
                MESG = 'YCENT mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( XORIG, XORIG1 ) ) THEN
                MESG = 'XORIG mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YORIG, YORIG1 ) ) THEN
                MESG = 'YORIG mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( XCELL, XCELL1 ) ) THEN
                MESG = 'XCELL mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YCELL, YCELL1 ) ) THEN
                MESG = 'YCELL mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF


            CHKMTXATT = ( .NOT.EFLAG )
            RETURN


        END FUNCTION  CHKMTXATT


        END MODULE MATXATTS

