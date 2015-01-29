
        LOGICAL FUNCTION  XTRACT3 ( FNAME, VNAME, LAY0, LAY1,
     &                              ROW0, ROW1, COL0, COL1,
     &                              JDATE, JTIME, BUFFER )

C***********************************************************************
C Version "$Id: xtract3.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  123
C
C  FUNCTION:  Read into the array BUFFER(*) all the data from the
C             Models-3 data file with logical name FNAME for variable
C             with name VNAME, for timestep JDATE:JTIME (formatted
C             YYYYDDD,HHMMSS), that fits in the data window :
C
C                  LAY0 <= LAY <= LAY1
C                  ROW0 <= ROW <= ROW1
C                  COL0 <= COL <= COL1
C
C
C  RETURN VALUE:  TRUE iff the data is available and the operation succeeds.
C
C  PRECONDITIONS REQUIRED:  FNAME is the logical name of a Models-3 data
C              file already opened by OPEN3() or CREATE3().
C
C  SUBROUTINES AND FUNCTIONS CALLED:  INDEX1, INIT3, JSTEP3, RDVARS
C
C  REVISION  HISTORY:
C       prototype 3/92 by CJC
C
C       modified  7/94 by CJC to handle restart files (tstep < 0 )
C
C       modified  8/94 by CJC to handle BUFFERED "files"
C
C       modified 10/94 by CJC to handle WRITE3() granularity at
C       the level of individual variables; char*(*) name arguments.
C
C       Modified  5/98, 1/00  by CJC for OpenMP thread-safety
C
C       Modified 5/2003 by CJC to use RDTFLAG(); bugfix:  xtbuf() is
C       LOGICAL, not INTEGER (correction by David Wong, US EPA)
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C
C       Modified 11/2004 by CJC:  new "verbose-flag" argument to RDTFLAG
C
C       Modified 10/30/2005 by CJC:  removed extraneous "VNAME=VNAME"
C       following suggestion by Dr. Michael Bane that this causes
C       segfault on some systems.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 05/2011 by CJC:  better error-messages
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER      , INTENT(IN   ) :: LAY0            !  lower layer bound
        INTEGER      , INTENT(IN   ) :: LAY1            !  upper layer bound
        INTEGER      , INTENT(IN   ) :: ROW0            !  lower row bound
        INTEGER      , INTENT(IN   ) :: ROW1            !  upper row bound
        INTEGER      , INTENT(IN   ) :: COL0            !  lower col bound
        INTEGER      , INTENT(IN   ) :: COL1            !  upper col bound
        INTEGER      , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER      , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        REAL         , INTENT(  OUT) :: BUFFER(*)       !  input buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
        INTEGER, EXTERNAL :: INIT3      !  initialize I/O system files.
        INTEGER, EXTERNAL :: JSTEP3     !  compute time step record numbers
        INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup
        LOGICAL, EXTERNAL :: RDTFLAG    !  check time step record availability
        LOGICAL, EXTERNAL :: RDVARS     !  read variables in data window
        LOGICAL, EXTERNAL :: XTBUF3     !  read data window from BUFFERED "files"
        INTEGER, EXTERNAL :: XTRBIN3    !  read data window from BINFIL3 files
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       FID             !  file-subscript for STATE3 arrays
        INTEGER       VID             !  vble-subscript for STATE3 arrays
        INTEGER       VAR             !  loop counter (subscript)
        INTEGER       FLEN, VLEN      !  name lengths for file, vble
        INTEGER       STEP            !  time step record number
        INTEGER       IERR            !  netCDF error status return
        INTEGER       DELTA           !  volume of a NCVGT call
        INTEGER       DCOLS           !  column-size of read-window
        INTEGER       DROWS           !  row-size of read-window
        INTEGER       DLAYS           !  layer-size of read-window
        INTEGER       DIMS( 5 )       !  corner arg array for NCVGT()
        INTEGER       DELS( 5 )       !  corner arg array for NCVGT()
        CHARACTER*256 MESG            !  message/warning buffer
        LOGICAL       EFLAG


C***********************************************************************
C   begin body of function  XTRACT3

C.......   Check that Models-3 I/O API has been initialized:

        FLEN  = LEN_TRIM( FNAME )
        VLEN  = LEN_TRIM( VNAME )
        FID   = NAME2FID( FNAME )

        EFLAG = ( FID .LE. 0 )

        IF ( EFLAG ) THEN
            MESG = 'Invalid file name "' // FNAME // '"'
            CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
	    XTRACT3 = .FALSE.
            RETURN
        END IF          !  if len( fname ) > 16, or if len( vname ) > 1

        IF ( VLEN .GT. NAMLEN3 ) THEN
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', VLEN
            CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
	    XTRACT3 = .FALSE.
            RETURN
        END IF          !  if len( vname ) > 16
        
        IF ( FTYPE3( FID ) .NE. GRDDED3 ) THEN
            WRITE( MESG, '( 4A, I3)' )
     &          'File type error in XTRACT3:  ',
     &          FNAME( 1:FLEN ), ' must be GRDDED3 = 1 ',
     &          'Actual file type' , FTYPE3( FID )
            CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
	    XTRACT3 = .FALSE.
            RETURN
        END IF          !  if file type nongridded, or file volatile


C.......   Perform range checks on variable, layer, row, col:

        IF ( VNAME .EQ. ALLVAR3 ) THEN

            VID = ALLAYS3

        ELSE 
            
            VID = INDEX1 ( VNAME, NVARS3( FID ), VLIST3( 1,FID ) )
            
            IF ( VID .EQ. 0 ) THEN
                MESG = 'Requested variable '   // VNAME( 1:VLEN ) //
     &                 'not available in file '// FNAME( 1:FLEN ) //
     &                 'which contains variables:'
                CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
                DO  VAR = 1, NVARS3( FID )
                     CALL M3MSG2( VLIST3( VAR,FID ) )
                END DO
                XTRACT3 = .FALSE.
                RETURN
            END IF
 
        END IF          !  end check on variable
 
        IF ( LAY0 .LT. 1     .OR.  
     &       LAY1 .LT. LAY0  .OR. 
     &       LAY1 .GT. NLAYS3( FID ) ) THEN
        
            WRITE( MESG, '( A, I9 )' ) 
     &          'Lower bound:            ', LAY0
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I9 )' ) 
     &          'Upper bound:            ', LAY1
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I9 )' ) 
     &          'Actual number of layers:', NLAYS3( FID )
            CALL M3MSG2( MESG )
            MESG = 'Error in layer-bounds specification for file ' //
     &          FNAME // ' variable ' // VNAME
            CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
            XTRACT3 = .FALSE.
            RETURN
     
        ELSE IF ( ROW0 .LT. 1     .OR.  
     &            ROW1 .LT. ROW0  .OR. 
     &            ROW1 .GT. NROWS3( FID ) ) THEN
        
            WRITE( MESG, '( A, I9 )' ) 
     &          'Lower bound:            ', ROW0
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I9 )' ) 
     &          'Upper bound:            ', ROW1
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I9 )' ) 
     &          'Actual number of layers:', NROWS3( FID )
            CALL M3MSG2( MESG )
            MESG = 'Error in row-bounds VNAME for file ' //
     &          FNAME // ' variable ' // VNAME
            CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
            XTRACT3 = .FALSE.
            RETURN
     
        ELSE IF ( COL0 .LT. 1     .OR.  
     &            COL1 .LT. COL0  .OR. 
     &            COL1 .GT. NCOLS3( FID ) ) THEN
        
            WRITE( MESG, '( A, I9 )' ) 
     &          'Lower bound:            ', COL0
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I9 )' ) 
     &          'Upper bound:            ', COL1
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I9 )' ) 
     &          'Actual number of layers:', NCOLS3( FID )
            CALL M3MSG2( MESG )
            MESG = 'Error in col-bounds specification for file ' //
     &          FNAME // ' variable ' // VNAME
            CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
            XTRACT3 = .FALSE.
            RETURN

        END IF          !  end range checks on col, row, layer
 
     
        IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN     !  BUFFERED "file"

            XTRACT3 = XTBUF3( FID,  VID,  LAY0, LAY1, 
     &                        ROW0, ROW1, COL0, COL1,
     &                        JDATE, JTIME, BUFFER )
            RETURN
        
        ELSE IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN     !  virtual "file"
           
            MESG = 'Virtual file XTRACT3 not supported for "' //
     &             FNAME // '" variable "' // VNAME // '"'
            CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
            XTRACT3 = .FALSE.
            RETURN

        END IF !  if file buffered


        !!   Note:  rdtflag() calls NCSNC()

        IF ( .NOT. RDTFLAG( FID,VID, JDATE,JTIME, STEP, .TRUE. ) ) THEN

            MESG = 'Time step not available for file:  ' // FNAME
            CALL M3WARN( 'XTRACT3', JDATE, JTIME, MESG )
            XTRACT3 = .FALSE.
            RETURN

        END IF

        IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN     !  native-binary file

            IERR = XTRBIN3( FID,  VID,  LAY0, LAY1, 
     &                      ROW0, ROW1, COL0, COL1,
     &                      STEP, BUFFER )
            XTRACT3 = ( IERR .NE. 0 )
            RETURN
        
        END IF



C...........   Read data from netCDF file into BUFFER()

        DCOLS = COL1  -  COL0 + 1
        DROWS = ROW1  -  ROW0 + 1
        DLAYS = LAY1  -  LAY0 + 1

        DIMS( 1 ) = COL0
        DELS( 1 ) = DCOLS

        DIMS( 2 ) = ROW0
        DELS( 2 ) = DROWS

        DIMS( 3 ) = LAY0
        DELS( 3 ) = DLAYS

        DIMS( 4 ) = STEP
        DELS( 4 ) = 1

        DELTA = DCOLS * DROWS * DLAYS

        XTRACT3 = RDVARS( FID, VID, DIMS, DELS, DELTA, BUFFER )

        RETURN

        END FUNCTION  XTRACT3

