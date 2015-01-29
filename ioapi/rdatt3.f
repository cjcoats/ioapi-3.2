
        LOGICAL FUNCTION   RDATT3( FNAME, VNAME, ANAME, ATYPE, AMAX,
     &                             ASIZE, AVAL )
        IMPLICIT NONE
        LOGICAL            RDATTC

C***********************************************************************
C Version "$Id: rdatt3.f 100 2015-01-16 16:52:16Z coats $"
C BAMS/MCNC/EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems, and 
C (C) 2014 UNC Institute for the Environment
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line   90
C   Entry  RDATTC  starts at line  109
C
C  FUNCTION:
C       Reads the attribute named ANAME for the variable VNAME in the
C       file FNAME into AVAL( AMAX ).  If VNAME == ALLVAR3, reads
C       global attribute ANAME.
C       AVAL must have type ATYPE, which should be one of M3REAL, M3INT, or
C       M3DBLE.
C
C       CHARACTER-string attributes use
C               ENTRY   RDATTC( FNAME, VNAME, ANAME, CVAL )
C
C  PRECONDITIONS REQUIRED:
C       File must have been previously opened.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       netCDF
C
C  REVISION  HISTORY:
C       prototype 1/2002 by Carlie J. Coats, Jr., MCNC-EMC
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 9/2013 by CJC:  Fortran-90 stuff; use NAME2FID()
C
C       Modified 12/2014 by CJC: logic cleanup
C***********************************************************************

C...........   INCLUDES:

      INCLUDE 'NETCDF.EXT'      ! netCDF  constants
      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'STATE3.EXT'      ! I/O API internal state


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME         !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME         !  variable name, or ALLVARS3
        CHARACTER*(*), INTENT(IN   ) :: ANAME         !  attribute name
        INTEGER      , INTENT(IN   ) :: ATYPE         !  attribute type (M3REAL, M3INT, M3DBLE)
        INTEGER      , INTENT(IN   ) :: AMAX          !  attribute dimensionality
        INTEGER      , INTENT(  OUT) :: ASIZE         !  attribute actual size
        REAL         , INTENT(  OUT) :: AVAL( AMAX )  !  attribute value (numeric)
        CHARACTER*(*), INTENT(  OUT) :: CVAL          !  attribute value (character-string)


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INIT3      !  Initialize I/O API
        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
        INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         F, V            !  subscripts for STATE3 arrays
        INTEGER         FID, VID        !  netCDF ID's
        INTEGER         VLEN            !  name length for vble
        INTEGER         IERR            !  netCDF error status return
        INTEGER         ATYP, ITYP, ALEN
        LOGICAL         EFLAG
        CHARACTER*16    PNAME
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*16    VAR16           !  scratch vble-name buffer
        CHARACTER*16    ATT16           !  scratch  att-name buffer
        CHARACTER*256   MESG            !  message-buffer


C***********************************************************************
C   begin body of subroutine  RDATT3

C...........   Check attribute type

        IF ( ( ATYPE .NE. NF_INT   ) .AND.
     &       ( ATYPE .NE. NF_FLOAT ) .AND.
     &       ( ATYPE .NE. NF_DOUBLE  ) ) THEN

            WRITE( MESG , '( 3 A, I10 )' )
     &           'RDATT3:  Attribute "'  , ANAME,
     &           '" has unsupported type', ATYPE
            CALL M3WARN( 'RDATT3', 0, 0, MESG )
            RDATT3 = .FALSE.
            RETURN

        END IF

        ITYP  = ATYPE
        PNAME = 'RDATT3'
        GO TO 111

        ENTRY RDATTC( FNAME, VNAME, ANAME, CVAL )
        ITYP   = NF_CHAR
        PNAME = 'RDATT3C'

        !! fall through to  111

111     CONTINUE

C.......   Preliminary checks:

        EFLAG = .FALSE.
        F     = NAME2FID( FNAME )


        VLEN  = LEN_TRIM( VNAME )
        F     = NAME2FID( FNAME )

        IF ( F .LE. 0 ) THEN
            EFLAG = .TRUE.
        ELSE IF ( CDFID3( F ) .LT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File:  "' // FIL16 // '" is NOT A NetCDF file.'
            CALL M3MSG2( MESG )
        ELSE IF ( VLEN .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', VLEN
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16

        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( PNAME, 0, 0, MESG )
            RDATT3 = .FALSE.
            RETURN
        END IF

        VAR16 = VNAME   !  fixed-length-16 scratch copy of name
        FIL16 = FNAME   !  fixed-length-16 scratch copy of name
        ATT16 = ANAME   !  fixed-length-16 scratch copy of name
        FID   = CDFID3( F )

C...........   Get ID for variable(s) to be read.

        IF ( VAR16 .EQ. ALLVAR3 ) THEN

            VID = NCGLOBAL

        ELSE

            V = INDEX1( VAR16, NVARS3( F ) , VLIST3( 1,F ) )
            IF ( V .EQ. 0 ) THEN
                EFLAG = .TRUE.
                MESG = 'Variable "'      // VAR16 //
     &                 '" not in file "' // FIL16 // '"'
                CALL M3MSG2( MESG )
            ELSE
                VID = VINDX3( V, F )
            END IF

        END IF          !  if VAR16 is 'ALL', or not.
        
        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3MESG( MESG )
            GO TO 999
        END IF


C...........   Check attribute:  supported type; actual type and size;
C...........   value.
C...........   Somewhat tortured logic-structure due to the fact that
C...........   one can't execute a RETURN within a critical section.

!$OMP   CRITICAL( S_NC )

        IERR = NF_INQ_ATT( FID, VID, ANAME, ATYP, ALEN )

        IF ( IERR .NE. NF_NOERR ) THEN

            MESG = 'Error inquiring type&size for attribute "' //
     &             ANAME // '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE IF ( ITYP .NE. ATYP ) THEN

            MESG = 'Bad type for attribute "' //
     &             ANAME // '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE IF ( ITYP .EQ. NF_CHAR ) THEN

            IF ( ALEN .GT. LEN( CVAL ) ) THEN

                MESG = 'Bad size for CHAR attribute "' // ANAME //
     &             '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
                CALL M3WARN( PNAME, 0, 0, MESG )
                EFLAG = .TRUE.

            ELSE

                IERR = NF_GET_ATT_TEXT( FID, VID, ANAME, CVAL )

            END IF

        ELSE    !  if ( ityp  .ne. nf_char ) then...

            IF ( ALEN .GT. AMAX ) THEN

                MESG = 'Bad size for nonCHAR attribute "' // ANAME //
     &             '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
                CALL M3WARN( PNAME, 0, 0, MESG )
                EFLAG = .TRUE.

            ELSE IF ( ITYP .EQ. NF_INT ) THEN

                IERR = NF_GET_ATT_INT( FID, VID, ANAME, AVAL )

            ELSE IF ( ITYP .EQ. NF_FLOAT ) THEN

                IERR = NF_GET_ATT_REAL( FID, VID, ANAME, AVAL )

            ELSE IF ( ITYP .EQ. NF_DOUBLE  ) THEN

                IERR = NF_GET_ATT_DOUBLE( FID, VID, ANAME, AVAL )

            END IF
            ASIZE = ALEN

        END IF  !  if ierr; else if type-mismatch; else char; else...

!$OMP   END CRITICAL( S_NC )

        IF ( IERR .NE. NF_NOERR ) THEN
            EFLAG = .TRUE.
        END IF
        
999     CONTINUE

        IF ( EFLAG ) THEN
            MESG = 'Error reading attribute "' // ANAME //
     &             '" for file "' // FNAME //
     &             '" and vble "' // VNAME // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
        END IF          !  ierr nonzero:  NCAPTC) failed
        IF ( ITYP .EQ. NF_CHAR ) THEN
            RDATTC = ( .NOT. EFLAG )
        ELSE
            RDATT3 = ( .NOT. EFLAG )
        END IF

        RETURN

        END FUNCTION RDATT3

