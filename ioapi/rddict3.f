
        LOGICAL FUNCTION RDDICT3 ( FID, FNAME )

C***********************************************************************
C Version "$Id: rddict3.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  74
C
C  FUNCTION:  
C       read the file definition for specified description-name FMAME
C       from the dictionary file with logical name FLIST3( FID ) into
C       the commons in include file FDESC3.EXT
C
C  RETURN 
C       VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  
C       Called from READ3() on a file already open.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  netCDF, INDEX1
C
C  REVISION  HISTORY:  prototype 3/92 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'FDESC3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER      , INTENT(IN   ) :: FID   !  index for file
        CHARACTER*(*), INTENT(IN   ) :: FNAME !  name of requested file description


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         VID             !  index (record) number within FNAMES
        INTEGER         IDUM            !  Scratch variable
        INTEGER         FNUM            !  netCDF file ID from NCCRE()
        INTEGER         IERR            !  netCDF error status return
        INTEGER         FLAG            !  index-flag from dictionary
        INTEGER         DIMS ( 6 )      !  array of dims for NCVGT()
        INTEGER         DELTS( 6 )      !  array of edge deltas for NCVGT()
        
        
C.......   State variable:  names table, file ID for last call
        
        CHARACTER*16    FNAMES( MXVARS3 )
        INTEGER, SAVE :: LID = -1


C.............................................................................
C   begin body of subroutine  RDDICT3

C.......   Get dictionary ID

        FNUM = CDFID3( FID )

        IF ( FTYPE3( FID ) .NE. DCTNRY3 ) THEN
            RDDICT3 = .FALSE.
            RETURN
        END IF
        
C.......   Look up the "variable" requested from the FNAMES table:
        
        IF ( FID .NE. LID ) THEN
        
            IDUM = MAX( MXVARS3, MXREC3( FID ) )
            
            DIMS ( 1 ) = 1
            DELTS( 1 ) = NAMLEN3
            DIMS ( 2 ) = 1
            DELTS( 2 ) = IDUM
            CALL NCVGTC( FNUM, VINDX3( 1,FID ), DIMS, DELTS,
     &                   FNAMES, NAMLEN3 * IDUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'netCDF error number', IERR,
     &              'Dictionary file:  ' // FLIST3( FID ) //
     &              '; entry requested:', VINDX3( 1,FID ),
     &              'Error reading netCDF dictionary variable VDESC.'
                RDDICT3 = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCVGTC() failed
            
            LID = FID
        
        END IF          !  if new FID
        
        VID = INDEX1( FNAME, IDUM, FNAMES )

        CALL NCVGT1( FNUM, TINDX3( FID ), VID, FLAG, IERR )
        IF ( IERR .EQ. 8 ) THEN
            RDDICT3 = .FALSE.   !  FLAG not yet set.
            RETURN
        ELSE IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary entry requested:', VID,
     &          'Error reading netCDF file record flag in ' //
     &          'dictionary file ' // FLIST3( FID )
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        IF ( FLAG .NE. OKFLAG3 .AND. FLAG .NE. VID ) THEN
            WRITE( LOGDEV,91010 )
     &          'Dictionary entry requested:', FNAME,
     &          'Record not available in dictionary file ' 
     &          // FLIST3( FID )
            RDDICT3 = .FALSE.
            RETURN
        END IF


C.......   Read characteristics from this record:

        DIMS ( 1 ) = VID
        CALL NCVGT1( FNUM, VINDX3( 2,FID ), DIMS, FTYPE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable FTYPE.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 3,FID ), DIMS, TSTEP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable TSTEP.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 4,FID ), DIMS, NVARS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable NVARS.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 5,FID ), DIMS, NLAYS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable NLAYS.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 6,FID ), DIMS, NROWS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable NROWS.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 7,FID ), DIMS, NCOLS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable NCOLS.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 8,FID ), DIMS, NTHIK3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable NTHIK.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 9,FID ), DIMS, GDTYP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable GDTYP.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 10,FID ), DIMS, VGTYP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable VGTYP.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed
        
        DIMS ( 1 ) = 1
        DIMS ( 2 ) = VID
        DELTS( 1 ) = NLAYS3D + 1
        DELTS( 2 ) = 1      
        CALL NCVGT( FNUM, VINDX3( 11,FID ),
     &              DIMS, DELTS, VGLVS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &      'netCDF error number', IERR,
     &      'Error reading variable VGLVS from file ' // FLIST3( FID )
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGTC() failed, or succeeded
           
        
        DIMS ( 1 ) = VID

        CALL NCVGT1( FNUM, VINDX3( 12,FID ), DIMS, P_ALP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable P_ALP.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 13,FID ), DIMS, P_BET3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable P_BET.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 14,FID ), DIMS, P_GAM3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable P_GAM.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 15,FID ), DIMS, XCENT3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable XCENT.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 16,FID ), DIMS, YCENT3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable YCENT.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 17,FID ), DIMS, XORIG3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable XORIG.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 18,FID ), DIMS, YORIG3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable YORIG.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 19,FID ), DIMS, XCELL3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable XCELL.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        CALL NCVGT1( FNUM, VINDX3( 20,FID ), DIMS, YCELL3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable YCELL.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGT1() failed

        DIMS ( 1 ) = 1
        DIMS ( 2 ) = VID
        DELTS( 1 ) = NAMLEN3
        DELTS( 2 ) = 1
        CALL NCVGTC( FNUM, VINDX3( 21,FID ), DIMS, DELTS,
     &               GDNAM3D, NAMLEN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable GDNAM.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGTC() failed

        DIMS ( 1 ) = 1
        DIMS ( 2 ) = 1
        DIMS ( 3 ) = VID
        DELTS( 1 ) = MXDLEN3
        DELTS( 2 ) = MXDESC3
        DELTS( 3 ) = 1
        CALL NCVGTC( FNUM, VINDX3( 22,FID ), DIMS, DELTS,
     &               FDESC3D, MXDLEN3 * MXDESC3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable FILEDESC.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGTC() failed

        DIMS ( 1 ) = 1
        DIMS ( 2 ) = 1
        DIMS ( 3 ) = VID
        DELTS( 1 ) = NAMLEN3
        DELTS( 2 ) = NVARS3D
        DELTS( 3 ) = 1
        CALL NCVGTC( FNUM, VINDX3( 23,FID ), DIMS, DELTS,
     &               VNAME3D, NAMLEN3 * MXVARS3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable VNAME.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGTC() failed

        DIMS ( 1 ) = 1
        DIMS ( 2 ) = 1
        DIMS ( 3 ) = VID
        DELTS( 1 ) = NAMLEN3
        DELTS( 2 ) = NVARS3D
        DELTS( 3 ) = 1
        CALL NCVGTC( FNUM, VINDX3( 24,FID ), DIMS, DELTS,
     &               UNITS3D, NAMLEN3 * MXVARS3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable UNITS.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGTC() failed


        DIMS ( 1 ) = 1
        DIMS ( 2 ) = 1
        DIMS ( 3 ) = VID
        DELTS( 1 ) = MXDLEN3
        DELTS( 2 ) = NVARS3D
        DELTS( 3 ) = 1
        CALL NCVGTC( FNUM, VINDX3( 25,FID ), DIMS, DELTS,
     &               VDESC3D, MXDLEN3 * MXVARS3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable VDESC.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGTC() failed

        DIMS ( 1 ) = 1
        DIMS ( 2 ) = VID
        DELTS( 1 ) = NVARS3D
        DELTS( 2 ) = 1
        CALL NCVGT( FNUM, VINDX3( 26,FID ), DIMS, DELTS,
     &              VTYPE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'netCDF error number', IERR,
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:', VID,
     &          'Error reading netCDF dictionary variable VDESC.'
            RDDICT3 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  NCVGTC() failed


        RDDICT3 = .TRUE.
        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine RDDICT3 <<<',
     &            3 ( /5X , A , : , I5 ), // )


        END FUNCTION RDDICT3

