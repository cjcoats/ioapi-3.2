
        LOGICAL FUNCTION WRDICT3 ( FID, FNAME )

C***********************************************************************
C Version "$Id: wrdict3.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 88
C
C  FUNCTION:  write the file definition stored in the commons of the
C             include-file FDESC3.EXT to logical record indexed by 
C             name FNAME, of the dictionary file with logical name DNAME.  
C             Logs warning message iff record for FNAME already exists.
C
C  RETURN VALUE:  TRUE iff the operation succeeds.
C
C
C  PRECONDITIONS REQUIRED:  
C	FID is STATE3 subscript for a dictionary file already opened 
C	for write by OPEN3();  >= 1.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:  
C	prototype 3/1992 by CJC
C
C	Modified 11/1994 by CJC to use numeric FID argument for dictionary,
C	logical name argument FNAME for dictionary-entry identifier.
C
C       revised  6/1999 by CJC:  OpenMP thread-safety for log-file
C
C       revised  2/2002 by CJC:  OpenMP thread-safety for netCDF
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'FDESC3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER      , INTENT(IN   ) :: FID     !  output-file subscript for STATE3 arrays
        CHARACTER*(*), INTENT(IN   ) :: FNAME   !  name of the data schema to be written


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
        INTEGER, EXTERNAL :: INIT3      !  initialize I/O system files.
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         VID             !  record number to be written.
        INTEGER         IDUM            !  holds return value for INIT3()
        INTEGER         FLAG            !
        INTEGER         FNUM            !  netCDF file ID from NCCRE()
        INTEGER         IERR            !  netCDF error status return
        INTEGER         DIMS( 5 )       !  array of dims for NCVPT()
        INTEGER         DELS( 5 )       !  array of edge deltas for NCVPT()
        LOGICAL         EFLAG
        CHARACTER*16    FIL16

C...........   STATE VARIABLE:  names table, file ID for last call
        
        CHARACTER*16, SAVE :: FNAMES( MXVARS3 )
        INTEGER,      SAVE :: LID = -1

C.............................................................................
C   begin body of subroutine  WRDICT3

C.......   Check whether file description FNAME already exists
C.......   (if so, write warning message)
C.......   Look up the "variable" requested from the FNAMES table:

        EFLAG = .FALSE.
        FIL16 = FNAME

!$OMP CRITICAL( S_LOGOUT )
!$OMP CRITICAL( S_NC )
        IDUM = MIN( MXVARS3, MXREC3( FID ) )
        FNUM = CDFID3( FID )
            
        IF( FID .NE. LID ) THEN
        
            DIMS( 1 ) = 1
            DELS( 1 ) = NAMLEN3
            DIMS( 2 ) = 1
            DELS( 2 ) = IDUM
            CALL NCVGTC( FNUM, VINDX3( 1,FID ), DIMS, DELS,
     &                   FNAMES, NAMLEN3 * IDUM, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'netCDF error number', IERR,
     &              'Dictionary file:  ' // FLIST3( FID ) //
     &              '; entry requested:', FID,
     &              'Error reading netCDF dictionary variable VDESC.'
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  NCVGTC() failed
        
            LID = FID
        
        END IF          !  fid, lid differ
        
        VID = INDEX1( FIL16, IDUM, FNAMES )
        
        IF ( VID .GT. 0 ) THEN
            CALL M3WARN( ' ', 0, 0, 
     &                  'File description ' // FIL16 
     &                  // ' being overwritten' )
        ELSE
            VID = IDUM + 1
            IF ( VID .GT. MXVARS3 ) THEN
                WRITE( LOGDEV,91010 )
     &          'Dictionary file:  ' // FLIST3( FID ) //
     &          '; entry requested:' // FIL16,
     &          'Maximum record number exceeded'
                EFLAG = .TRUE.
                GO TO 999
            END IF
            FNAMES( VID ) = FLIST3( FID )
        END IF
        
C.......   Write characteristics to this record:

        DIMS( 1 ) = 1
        DIMS( 2 ) = VID
        DELS( 1 ) = NAMLEN3
        DELS( 2 ) = 1
        CALL NCVPTC( FNUM, VINDX3( 1,FID ), DIMS, DELS, FLIST3( FID ),
     &               NAMLEN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable FNAME ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPTC() failed

                
        DIMS( 1 ) = VID
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = 1
        
        CALL NCVPT1( FNUM, VINDX3( 2,FID ), DIMS, FTYPE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable FTYPE ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 3,FID ), DIMS, TSTEP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable TSTEP ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 4,FID ), DIMS, NVARS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable NVARS ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 5,FID ), DIMS, NLAYS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable NLAYS ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 6,FID ), DIMS, NROWS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable NROWS ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 7,FID ), DIMS, NCOLS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable NCOLS ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 8,FID ), DIMS, NTHIK3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable NTHIK ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 9,FID ), DIMS, GDTYP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable GDTYP ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 10,FID ), DIMS, VGTYP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable VGTYP ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        DIMS( 1 ) = 1
        DIMS( 2 ) = VID
        DELS( 1 ) = NCOLS3D + 1
        DELS( 2 ) = 1
        
        CALL NCVPT( FNUM, VINDX3( 11,FID ), DIMS, DELS, VGLVS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable VGLVS ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed
        
        DIMS( 1 ) = VID
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = 1
        
        CALL NCVPT1( FNUM, VINDX3( 12,FID ), DIMS, P_ALP3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable P_ALP ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 13,FID ), DIMS, P_BET3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable P_BET ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 14,FID ), DIMS, P_GAM3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable P_GAM ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 15,FID ), DIMS, XCENT3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable XCENT ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 16,FID ), DIMS, YCENT3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable YCENT ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 17,FID ), DIMS, XORIG3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable XORIG ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 18,FID ), DIMS, YORIG3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable YORIG ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 19,FID ), DIMS, XCELL3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable XCELL ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        CALL NCVPT1( FNUM, VINDX3( 20,FID ), DIMS, YCELL3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable YCELL ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

        DIMS( 1 ) = 1
        DIMS( 2 ) = VID
        DELS( 1 ) = NAMLEN3
        DELS( 2 ) = 1
        
        CALL NCVPTC( FNUM, VINDX3( 21,FID ), DIMS, DELS, GDNAM3D,
     &               NAMLEN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable GDNAM ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPTC() failed

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = VID
        DELS( 1 ) = MXDLEN3
        DELS( 2 ) = MXDESC3
        DELS( 3 ) = 1
        CALL NCVPTC( FNUM, VINDX3( 22,FID ), DIMS, DELS, FDESC3D,
     &               MXDLEN3 * MXDESC3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable FILEDESC ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPTC() failed

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = VID
        DELS( 1 ) = NAMLEN3
        DELS( 2 ) = NVARS3D
        DELS( 3 ) = 1
        CALL NCVPTC( FNUM, VINDX3( 23,FID ), DIMS, DELS, VNAME3D,
     &               NAMLEN3 * NVARS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable VNAME ' //
     &          'in file ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPTC() failed

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = VID
        DELS( 1 ) = NAMLEN3
        DELS( 2 ) = NVARS3D
        DELS( 3 ) = 1
        CALL NCVPTC( FNUM, VINDX3( 24,FID ), DIMS, DELS, UNITS3D,
     &               NAMLEN3 * NVARS3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable UNITS ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPTC() failed


        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = VID
        DELS( 1 ) = MXDLEN3
        DELS( 2 ) = NVARS3D
        DELS( 3 ) = 1
        CALL NCVPTC( FNUM, VINDX3( 25,FID ), DIMS, DELS, VDESC3D,
     &               MXDLEN3 * NVARS3D , IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable VDESC '//
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPTC() failed

        DIMS( 1 ) = 1
        DIMS( 2 ) = VID
        DELS( 1 ) = NVARS3D
        DELS( 2 ) = 1
        CALL NCVPT( FNUM, VINDX3( 26,FID ), 
     &              DIMS, DELS, VTYPE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF dictionary variable VTYPE '//
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPTC() failed


C.......   Write record-flag value:

        FLAG = VID
        CALL NCVPT1( FNUM, TINDX3( FID ), VID, FLAG, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )
     &          'Error writing netCDF file record flag ' //
     &          'in file:  ' // FLIST3( FID ) ,
     &          'Dictionary entry ' // FIL16,
     &          'netCDF error number', IERR
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCVPT1() failed

999     CONTINUE

!$OMP END CRITICAL( S_NC )
!$OMP END CRITICAL( S_LOGOUT )

        IF  ( EFLAG ) THEN
            WRDICT3 = .FALSE.
            RETURN
	END IF

        MXREC3( FID ) = MAX( MXREC3( FID ), VID )
        
        WRDICT3 = .TRUE.
        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91001   FORMAT ( //5X , '>>> WARNING in subroutine WRDICT3 <<<',
     &            /5X , 3A,
     &            /5X , A , I5, // )

91002   FORMAT ( //5X , '>>> WARNING in subroutine WRDICT3 <<<',
     &            /5X , 5A,
     &            /5X , A , I5, // )

91010   FORMAT ( //5X , '>>> WARNING in subroutine WRDICT3 <<<',
     &            3 ( /5X , A ) , I5, // )

91030   FORMAT ( //5X , '>>> WARNING in subroutine WRDICT3 <<<',
     &            2 ( /5X, A ) , I5 ,// )

        END FUNCTION WRDICT3

