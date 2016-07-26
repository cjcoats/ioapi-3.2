
PROGRAM M3MASK

    !!***********************************************************************
    !!  Program body starts at line   104
    !!
    !!  DESCRIPTION:
    !!      Read and grid ASCII "mask" file, that contains <col,row>
    !!      locations of mask cells.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       see splash screen
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  7/2016 by Carlie J. Coats, Jr., UNC IE
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::   PNAME  = 'M3MASK'   
    CHARACTER*16, PARAMETER ::   BLANK  = ' '
    CHARACTER*64, PARAMETER ::   BAR    = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT
    INTEGER         COL, ROW, L
    LOGICAL         EFLAG
    CHARACTER*256   MESG
    
    
    !!  Structures for mask data:

    INTEGER, ALLOCATABLE :: MASK( :,: )

    !!***********************************************************************
    !!   begin body of program M3MASK

    LDEV = INIT3()
    EFLAG  = .FALSE.         !!  no errors found yet
    WRITE( *, '( 5X, A )' )  BLANK, BAR,                                        &
'Program "M3MASK" to read ASCII mask data:  list-formatted lines containing',   &
'in-mask locations:',                                                           &
'',                                                                             &
'   <col> <row>',                                                               &
'',                                                                             &
'and create a "MASK" output file on the same grid as the input LLFILE.',        &
'',                                                                             &
'',                                                                             &
'PRECONDITIONS REQUIRED: ',                                                     &
'      setenv MASKDATA    <path-name for ASCII mask file',                      &
'      setenv LLFILE      <path-name for M3IO gridded file (e.g., GRDCRO2D)',   &
'      setenv MASKFILE    <path-name for M3IO output file',                     &
'',                                                                             &
'Program copyright (C) 2016 UNC Institute for the Environment.',                &
'All rights reserved.',                                                         &
BAR, ''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program terminated at user request', 2 )
    END IF


    !!...........   Open and describe input files:

    LDEV = GETEFILE( 'MASKDATA', .TRUE., .TRUE., PNAME )
    IF ( LDEV .LT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Could not open "MASKDATA"' )
    END IF

    IF ( .NOT. OPEN3( 'LLFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        CALL M3WARN( PNAME, 0, 0, 'Failure opening "LLFILE"' )
    ELSE IF ( .NOT. DESC3( 'LLFILE' ) ) THEN
        EFLAG = .TRUE.
        CALL M3WARN( PNAME, 0, 0, 'Failure getting file description for "LLFILE"' )
    END IF


    IF ( EFLAG ) THEN
        MESG = 'Fatal input file error(s)'
        CALL M3EXIT( PNAME, 0, 0,MESG, 2 )
    END IF


    !!...........   Allocate working arrays:  dims from DESC3() call

    ALLOCATE( MASK( NCOLS3D, NROWS3D ), STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10 )' ) 'Allocation failure:  STATUS=', ISTAT
        EFLAG = .TRUE.
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    VNAME3D( 1 ) = 'MASK'
    VTYPE3D( 1 ) = M3INT
    UNITS3D( 1 ) = 'none'
    VDESC3D( 1 ) = '1==mask region, 0==outside-mask region'
    NVARS3D = 1
    SDATE3D = 0
    STIME3D = 0
    TSTEP3D = 0
    NLAYS3D = 1

    IF ( .NOT. OPEN3( 'MASKFILE', FSUNKN3, PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Failure opening "MASKFILE"', 2 )
    END IF


    !!...........   Create mask:  read and process input file

    MASK = 0        !!  default, array assignment
    
    DO L = 1, 999999999

        READ( LDEV, *, END=99, IOSTAT=ISTAT )  COL, ROW
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I9, A, I10 )' ) 'Error reading "MASKDATA" at line', L, ': STATUS=', ISTAT
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE IF ( COL .LT. 1 .OR. COL .GT. NCOLS3D .OR.     &
                  ROW .LT. 1 .OR. ROW .GT. NROWS3D  ) THEN
            WRITE( MESG, '( A, I5, 2X, A, I5 )' ) '<COL=', COL, 'ROW=', ROW, '> out of grid bounds'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MASK( COL,ROW ) = 1
        END IF

    END DO
    
99  CONTINUE


    IF ( EFLAG ) THEN
        MESG = 'Fatal input file error(s)'
        CALL M3EXIT( PNAME, 0, 0,MESG, 2 )
    END IF
    
    IF ( .NOT. WRITE3( 'MASKFILE', 'MASK', 0, 0, MASK )  ) THEN
        MESG = 'Fatal output error(s)'
        CALL M3EXIT( PNAME, 0, 0,MESG, 2 )
    END IF


    CALL M3EXIT( PNAME, 0, 0, 'Success in program', 0 )


END PROGRAM M3MASK


    
