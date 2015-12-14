MODULE MODNCFIO

    !!.........................................................................
    !!  Version "$Id: modncfio.F90 266 2015-11-20 16:59:47Z coats $"
    !!  Copyright (c) 2015 UNC Institute for the Environment.
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!  See file "LGPL.txt" for conditions of use.
    !!..............................................................
    !!
    !!      This is a merge of netCDF version 3.x/4.x (version 3.6.2) "fortran/netcdf.inc"
    !!      with PnetCDF (version 1.6.1), but under another INCLUDE-file naming convention.
    !!      NetCDF copyright 1990-2008 University Corporation for Atmospheric Research;
    !!      see URL  http://www.unidata.ucar.edu/packages/netcdf/index.html
    !!
    !!  Compile with preprocessor definition "-DIOAPI_NCF4=1" for netCDF-4 INTEGER*8 support
    !!
    !!  Compile with preprocessor definition "-DIOAPI_PNCF=1" for pNetCDF parallel-I/O support
    !!
    !!  DO NOT EDIT !!!!
    !!
    !!        The EDSS/Models-3 I/O API depends in an essential manner
    !!        upon the contents of this MODULE file.  ANY CHANGES are
    !!        likely to result in very obscure, difficult-to-diagnose
    !!        bugs caused by an inconsistency between standard "libioapi.a"
    !!        object-libraries and whatever code is compiled with the
    !!        resulting modified MODULE-file.
    !!
    !!        By making any changes to this MODULE file, the user
    !!        explicitly agrees that in the case any assistance is
    !!        required of MCNC or of the I/O API author, Carlie J. Coats, Jr.
    !!        THE USER AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE
    !!        UNC AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
    !!        RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
    !!        REQUIRED.
    !!   
    !!.........................................................................

    IMPLICIT NONE

    !! netCDF version 3 fortran interface:
    !! external netcdf data types:

    INTEGER, PARAMETER :: nf_byte     =  1
    INTEGER, PARAMETER :: nf_int1     =  nf_byte
    INTEGER, PARAMETER :: nf_char     =  2
    INTEGER, PARAMETER :: nf_short    =  3
    INTEGER, PARAMETER :: nf_int2     =  nf_short
    INTEGER, PARAMETER :: nf_int      =  4
    INTEGER, PARAMETER :: nf_float    =  5
    INTEGER, PARAMETER :: nf_real     =  nf_float
    INTEGER, PARAMETER :: nf_double   =  6
    INTEGER, PARAMETER :: nf_ubyte    =  7
    INTEGER, PARAMETER :: nf_ushort   =  8
    INTEGER, PARAMETER :: nf_uint     =  9
    INTEGER, PARAMETER :: nf_int64    = 10
    INTEGER, PARAMETER :: nf_uint64   = 11
    INTEGER, PARAMETER :: nf_string   = 12
    INTEGER, PARAMETER :: nf_vlen     = 13
    INTEGER, PARAMETER :: nf_opaque   = 14
    INTEGER, PARAMETER :: nf_enum     = 15
    INTEGER, PARAMETER :: nf_compound = 16


    !!........ default fill values:

    INTEGER, PARAMETER :: nf_fill_byte   = -127
    INTEGER, PARAMETER :: nf_fill_int1   = nf_fill_byte
    INTEGER, PARAMETER :: nf_fill_char   = 0
    INTEGER, PARAMETER :: nf_fill_short  = -32767
    INTEGER, PARAMETER :: nf_fill_int2   =  nf_fill_short
    INTEGER, PARAMETER :: nf_fill_int    = -2147483647
    REAL   , PARAMETER :: nf_fill_float  = 9.9692099683868690e+36
    REAL   , PARAMETER :: nf_fill_real   = nf_fill_float
    REAL*8 , PARAMETER :: nf_fill_double = 9.9692099683868690e+36
    INTEGER, PARAMETER :: nf_fill_ubyte  = 255
    INTEGER, PARAMETER :: nf_fill_ushort = 65535


    !!........ mode flags for opening and creating a netcdf dataset:

    INTEGER, PARAMETER :: nf_nowrite          =    0
    INTEGER, PARAMETER :: nf_write            =    1
    INTEGER, PARAMETER :: nf_clobber          =    0
    INTEGER, PARAMETER :: nf_noclobber        =    4
    INTEGER, PARAMETER :: nf_fill             =    0
    INTEGER, PARAMETER :: nf_nofill           =  256
    INTEGER, PARAMETER :: nf_lock             = 1024
    INTEGER, PARAMETER :: nf_share            = 2048
    INTEGER, PARAMETER :: nf_64bit_offset     =  512
    INTEGER, PARAMETER :: nf_sizehint_default =    0
    INTEGER, PARAMETER :: nf_align_chunk      =   -1
    INTEGER, PARAMETER :: nf_format_classic   =    1
    INTEGER, PARAMETER :: nf_format_64bit     =    2
    INTEGER, PARAMETER :: nf_format_netcdf4   =    3
    INTEGER, PARAMETER :: nf_format_netcdf4_classic = 4


    !!........ size argument for defining an unlimited dimension:

    INTEGER, PARAMETER :: nf_unlimited = 0


    !!........ global attribute id:

    INTEGER, PARAMETER :: nf_global = 0


    !!........ implementation limits:

    INTEGER, PARAMETER :: nf_max_dims  = 1024
    INTEGER, PARAMETER :: nf_max_attrs = 8192
    INTEGER, PARAMETER :: nf_max_vars  = 8192
    INTEGER, PARAMETER :: nf_max_name  =  256
    INTEGER, PARAMETER :: nf_max_var_dims = nf_max_dims

    !!........ error codes:

    INTEGER, PARAMETER :: nf_noerr        =   0
    INTEGER, PARAMETER :: nf_ebadid       = -33
    INTEGER, PARAMETER :: nf_eexist       = -35
    INTEGER, PARAMETER :: nf_einval       = -36
    INTEGER, PARAMETER :: nf_eperm        = -37
    INTEGER, PARAMETER :: nf_enotindefine = -38
    INTEGER, PARAMETER :: nf_eindefine    = -39
    INTEGER, PARAMETER :: nf_einvalcoords = -40
    INTEGER, PARAMETER :: nf_emaxdims     = -41
    INTEGER, PARAMETER :: nf_enameinuse   = -42
    INTEGER, PARAMETER :: nf_enotatt      = -43
    INTEGER, PARAMETER :: nf_emaxatts     = -44
    INTEGER, PARAMETER :: nf_ebadtype     = -45
    INTEGER, PARAMETER :: nf_ebaddim      = -46
    INTEGER, PARAMETER :: nf_eunlimpos    = -47
    INTEGER, PARAMETER :: nf_emaxvars     = -48
    INTEGER, PARAMETER :: nf_enotvar      = -49
    INTEGER, PARAMETER :: nf_eglobal      = -50
    INTEGER, PARAMETER :: nf_enotnc       = -51
    INTEGER, PARAMETER :: nf_ests         = -52
    INTEGER, PARAMETER :: nf_emaxname     = -53
    INTEGER, PARAMETER :: nf_eunlimit     = -54
    INTEGER, PARAMETER :: nf_enorecvars   = -55
    INTEGER, PARAMETER :: nf_echar        = -56
    INTEGER, PARAMETER :: nf_eedge        = -57
    INTEGER, PARAMETER :: nf_estride      = -58
    INTEGER, PARAMETER :: nf_ebadname     = -59
    INTEGER, PARAMETER :: nf_erange       = -60
    INTEGER, PARAMETER :: nf_enomem       = -61
    INTEGER, PARAMETER :: nf_evarsize     = -62
    INTEGER, PARAMETER :: nf_edimsize     = -63
    INTEGER, PARAMETER :: nf_etrunc       = -64

    !!........ error handling modes:

    INTEGER, PARAMETER :: nf_fatal   = 1
    INTEGER, PARAMETER :: nf_verbose = 2


    !!........ miscellaneous routines:

    CHARACTER*80, EXTERNAL :: nf_inq_libvers
    CHARACTER*80, EXTERNAL :: nf_strerror
    LOGICAL,      EXTERNAL :: nf_issyserr


    !!........ control routines:

    INTEGER, EXTERNAL :: nf_inq_base_pe
    !!........                         (integer             ncid,
    !!........                          integer             pe)

    INTEGER, EXTERNAL :: nf_set_base_pe
    !!........                         (integer             ncid,
    !!........                          integer             pe)

    INTEGER, EXTERNAL :: nf_create
    !!........                         (character*(*)       path,
    !!........                          integer             cmode,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf__create
    !!........                         (character*(*)       path,
    !!........                          integer             cmode,
    !!........                          integer             initialsz,
    !!........                          integer             chunksizehint,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf__create_mp
    !!........                         (character*(*)       path,
    !!........                          integer             cmode,
    !!........                          integer             initialsz,
    !!........                          integer             basepe,
    !!........                          integer             chunksizehint,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf_open
    !!........                         (character*(*)       path,
    !!........                          integer             mode,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf__open
    !!........                         (character*(*)       path,
    !!........                          integer             mode,
    !!........                          integer             chunksizehint,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf__open_mp
    !!........                         (character*(*)       path,
    !!........                          integer             mode,
    !!........                          integer             basepe,
    !!........                          integer             chunksizehint,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf_set_fill
    !!........                         (integer             ncid,
    !!........                          integer             fillmode,
    !!........                          integer             old_mode)

    INTEGER, EXTERNAL :: nf_set_default_format
    !!........                          (integer             format,
    !!........                          integer             old_format)

    INTEGER, EXTERNAL :: nf_redef
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf_enddef
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf__enddef
    !!........                         (integer             ncid,
    !!........                          integer             h_minfree,
    !!........                          integer             v_align,
    !!........                          integer             v_minfree,
    !!........                          integer             r_align)

    INTEGER, EXTERNAL :: nf_sync
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf_abort
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf_close
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf_delete
    !!........                         (character*(*)       ncid)


    !!........ general inquiry routines:


    INTEGER, EXTERNAL :: nf_inq
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     ndims,
    !!........                          INTEGER ::     nvars,
    !!........                          INTEGER ::     ngatts,
    !!........                          INTEGER ::     unlimdimid)

    INTEGER, EXTERNAL :: nf_inq_ndims
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     ndims)

    INTEGER, EXTERNAL :: nf_inq_nvars
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     nvars)

    INTEGER, EXTERNAL :: nf_inq_natts
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     ngatts)

    INTEGER, EXTERNAL :: nf_inq_unlimdim
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     unlimdimid)

    INTEGER, EXTERNAL :: nf_inq_format
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     format)


    !!........ dimension routines:


    INTEGER, EXTERNAL :: nf_def_dim
    !!........                         (INTEGER ::     ncid,
    !!........                          character(*):: name,
    !!........                          INTEGER ::     len,
    !!........                          INTEGER ::     dimid)

    INTEGER, EXTERNAL :: nf_inq_dimid
    !!........                         (INTEGER ::     ncid,
    !!........                          character(*):: name,
    !!........                          INTEGER ::     dimid)

    INTEGER, EXTERNAL :: nf_inq_dim
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     dimid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     len)

    INTEGER, EXTERNAL :: nf_inq_dimname
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     dimid,
    !!........                          character(*)        name)

    INTEGER, EXTERNAL :: nf_inq_dimlen
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     dimid,
    !!........                         (INTEGER ::     len)

    INTEGER, EXTERNAL :: nf_rename_dim
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     dimid,
    !!........                          character(*)        name)


    !!........ general attribute routines:


    INTEGER, EXTERNAL :: nf_inq_att
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len)

    INTEGER, EXTERNAL :: nf_inq_attid
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     attnum)

    INTEGER, EXTERNAL :: nf_inq_atttype
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     xtype)

    INTEGER, EXTERNAL :: nf_inq_attlen
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     len)

    INTEGER, EXTERNAL :: nf_inq_attname
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     attnum,
    !!........                          character(*)        name)

    INTEGER, EXTERNAL :: nf_copy_att
    !!........                         (INTEGER ::     ncid_in,
    !!........                         (INTEGER ::     varid_in,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     ncid_out,
    !!........                         (INTEGER ::     varid_out)

    INTEGER, EXTERNAL :: nf_rename_att
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        curname,
    !!........                          character(*)        newname)

    INTEGER, EXTERNAL :: nf_del_att
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name)


    !!........ attribute put/get routines:


    INTEGER, EXTERNAL :: nf_put_att_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)             name,
    !!........                         (INTEGER ::     len,
    !!........                          character(*)             text)

    INTEGER, EXTERNAL :: nf_get_att_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)             name,
    !!........                          character(*)             text)

    INTEGER, EXTERNAL :: nf_put_att_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)             name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                          nf_int1_t                i1vals(*))

    INTEGER, EXTERNAL :: nf_get_att_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_put_att_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_get_att_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_put_att_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_att_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_att_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_att_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,

    INTEGER, EXTERNAL :: nf_put_att_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                          double              dvals(*))

    INTEGER, EXTERNAL :: nf_get_att_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                          double              dvals(*))


    !!........ general variable routines:


    INTEGER, EXTERNAL :: nf_def_var
    !!........                         (INTEGER ::     ncid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     datatype,
    !!........                         (INTEGER ::     ndims,
    !!........                         (INTEGER ::     dimids(*),
    !!........                         (INTEGER ::     varid)

    INTEGER, EXTERNAL :: nf_inq_var
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     datatype,
    !!........                         (INTEGER ::     ndims,
    !!........                         (INTEGER ::     dimids(*),
    !!........                         (INTEGER ::     natts)

    INTEGER, EXTERNAL :: nf_inq_varid
    !!........                         (INTEGER ::     ncid,
    !!........                          character(*)        name,
    !!........                         (INTEGER ::     varid)

    INTEGER, EXTERNAL :: nf_inq_varname
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name)

    INTEGER, EXTERNAL :: nf_inq_vartype
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     xtype)

    INTEGER, EXTERNAL :: nf_inq_varndims
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     ndims)

    INTEGER, EXTERNAL :: nf_inq_vardimid
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     dimids(*))

    INTEGER, EXTERNAL :: nf_inq_varnatts
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     natts)

    INTEGER, EXTERNAL :: nf_rename_var
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        name)

    INTEGER, EXTERNAL :: nf_copy_var
    !!........                         (INTEGER ::     ncid_in,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     ncid_out)


    !!........ entire variable put/get routines:


    INTEGER, EXTERNAL :: nf_put_var_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        text)

    INTEGER, EXTERNAL :: nf_get_var_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*)        text)

    INTEGER, EXTERNAL :: nf_put_var_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_get_var_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_put_var_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_get_var_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_put_var_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_var_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_var_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_var_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_put_var_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_get_var_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_put_var_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          INTEGER*8 ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_var_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          INTEGER*8 ::     ivals(*))


    !!........ single variable put/get routines:


    INTEGER, EXTERNAL :: nf_put_var1_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          character*1 :: text)

    INTEGER, EXTERNAL :: nf_get_var1_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          character*1 :: text)

    INTEGER, EXTERNAL :: nf_put_var1_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          nf_int1_t           i1val)

    INTEGER, EXTERNAL :: nf_get_var1_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          nf_int1_t ::   i1val)

    INTEGER, EXTERNAL :: nf_put_var1_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          nf_int2_t           i2val)

    INTEGER, EXTERNAL :: nf_get_var1_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          nf_int2_t           i2val)

    INTEGER, EXTERNAL :: nf_put_var1_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                         (INTEGER ::     ival)

    INTEGER, EXTERNAL :: nf_get_var1_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                         (INTEGER ::     ival)

    INTEGER, EXTERNAL :: nf_put_var1_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          real                rval)

    INTEGER, EXTERNAL :: nf_get_var1_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          real                rval)

    INTEGER, EXTERNAL :: nf_put_var1_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          doubleprecision     dval)

    INTEGER, EXTERNAL :: nf_get_var1_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          doubleprecision     dval)

    INTEGER, EXTERNAL :: nf_put_var1_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          INTEGER*8, EXTERNAL ::     ival)

    INTEGER, EXTERNAL :: nf_get_var1_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          INTEGER*8, EXTERNAL ::     ival)

    !!........ variable array put/get routines:


    INTEGER, EXTERNAL :: nf_put_vara_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          character(*)        text)

    INTEGER, EXTERNAL :: nf_get_vara_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          character(*)        text)

    INTEGER, EXTERNAL :: nf_put_vara_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_get_vara_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_put_vara_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_get_vara_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_put_vara_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_vara_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_vara_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_vara_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_put_vara_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_get_vara_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          doubleprecision :: dvals(*))

    INTEGER, EXTERNAL :: nf_put_vara_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          INTEGER*8 ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_vara_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          INTEGER*8 ::     ivals(*))


    !!........ strided variable put/get routines:


    INTEGER, EXTERNAL :: nf_put_vars_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          character(*) :: text)

    INTEGER, EXTERNAL :: nf_get_vars_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          character(*) :: text)

    INTEGER, EXTERNAL :: nf_put_vars_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_get_vars_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_put_vars_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_get_vars_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_put_vars_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_vars_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_vars_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_vars_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_put_vars_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_get_vars_double
    !!........                         (INTEGER ::     ncid,
    !!........                          integer             varid,
    !!........                          integer             start(*),
    !!........                          integer             count(*),
    !!........                          integer             stride(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_put_vars_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          INTEGER*8::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_vars_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          INTEGER*8::     ivals(*))


    !!........ mapped variable put/get routines:


    INTEGER, EXTERNAL :: nf_put_varm_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          character(*)        text)

    INTEGER, EXTERNAL :: nf_get_varm_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          character(*)        text)

    INTEGER, EXTERNAL :: nf_put_varm_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_get_varm_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          nf_int1_t           i1vals(*))

    INTEGER, EXTERNAL :: nf_put_varm_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_get_varm_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          nf_int2_t           i2vals(*))

    INTEGER, EXTERNAL :: nf_put_varm_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_varm_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_varm_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_varm_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_put_varm_double
    !!........                         (integer             ncid,
    !!........                          integer             varid,
    !!........                          integer             start(*),
    !!........                          integer             count(*),
    !!........                          integer             stride(*),
    !!........                          integer             imap(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_get_varm_double
    !!........                         (INTEGER ::     ncid,
    !!........                          integer             varid,
    !!........                          integer             start(*),
    !!........                          integer             count(*),
    !!........                          integer             stride(*),
    !!........                          integer             imap(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_put_varm_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          INTEGER*8, EXTERNAL ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_varm_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          INTEGER*8, EXTERNAL ::     ivals(*))

    !!........ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !!........ begin netcdf 2.4 backward compatibility:

    !!........ functions in the fortran interface

    INTEGER, EXTERNAL :: nccre
    INTEGER, EXTERNAL :: ncopn
    INTEGER, EXTERNAL :: ncddef
    INTEGER, EXTERNAL :: ncdid
    INTEGER, EXTERNAL :: ncvdef
    INTEGER, EXTERNAL :: ncvid
    INTEGER, EXTERNAL :: nctlen
    INTEGER, EXTERNAL :: ncsfil

    !!........ netcdf data types:

    INTEGER, PARAMETER :: ncbyte   = nf_byte      !! = 1
    INTEGER, PARAMETER :: ncchar   = nf_char      !! = 2
    INTEGER, PARAMETER :: ncshort  = nf_short     !! = 3
    INTEGER, PARAMETER :: nclong   = nf_int       !! =  4
    INTEGER, PARAMETER :: ncfloat  = nf_float     !! =  5
    INTEGER, PARAMETER :: ncdouble = nf_double    !! =  6


    !!........     masks for the struct nc flag field; passed in as 'mode' arg to
    !!........     nccreate and ncopen.
    !!........     read/write, 0 => readonly
    !!........     in create phase, cleared by ncendef
    !!........     on create destroy existing file
    !!........     in define mode, cleared by ncendef
    !!........     synchronise numrecs on change (x'10')
    !!........     synchronise whole header on change (x'20')
    !!........     numrecs has changed (x'40')
    !!........     header info has changed (x'80')
    !!........     prefill vars on endef and increase of record, the default behavior
    !!........     do not fill vars on endef and increase of record (x'100')
    !!........     isa link (x'8000')

    INTEGER, PARAMETER :: ncrdwr   = nf_write       !! = 1
    INTEGER, PARAMETER :: nccreat  =     2
    INTEGER, PARAMETER :: ncexcl   = nf_noclobber   !! = 4
    INTEGER, PARAMETER :: ncindef  =     8
    INTEGER, PARAMETER :: ncnsync  =    16
    INTEGER, PARAMETER :: nchsync  =    32
    INTEGER, PARAMETER :: ncndirty =    64
    INTEGER, PARAMETER :: nchdirty =   128
    INTEGER, PARAMETER :: ncfill   = nf_fill        !! =    0
    INTEGER, PARAMETER :: ncnofill = nf_nofill      !! =  256
    INTEGER, PARAMETER :: nclink   = 32768


    !!........     'mode' arguments for nccreate and ncopen

    INTEGER, PARAMETER :: ncnowrit = nf_nowrite     !! = 0
    INTEGER, PARAMETER :: ncwrite  = ncrdwr
    INTEGER, PARAMETER :: ncclob   = nf_clobber
    INTEGER, PARAMETER :: ncnoclob = nf_noclobber


    !!........     'size' argument to ncdimdef for an unlimited dimension

    INTEGER, PARAMETER :: ncunlim = nf_unlimited    !! = 0


    !!........     attribute id to put/get a global attribute

    INTEGER, PARAMETER :: ncglobal  = 0


    !!........     advisory maximums:

    INTEGER, PARAMETER :: maxncop  = 64
    INTEGER, PARAMETER :: maxncdim = nf_max_dims      !! = 1024
    INTEGER, PARAMETER :: maxncatt = nf_max_attrs     !! = 8192
    INTEGER, PARAMETER :: maxncvar = nf_max_vars      !! = 8192
    !!........     not enforced
    INTEGER, PARAMETER :: maxncnam = nf_max_name      !! =  256
    INTEGER, PARAMETER :: maxvdims = maxncdim


    !!........     global netcdf error status variable
    !!........     initialized in error.c

    INTEGER, PARAMETER :: ncnoerr  = nf_noerr
    INTEGER, PARAMETER :: ncebadid = nf_ebadid
    INTEGER, PARAMETER :: ncenfile = -31   ! nc_syserr
    INTEGER, PARAMETER :: nceexist = nf_eexist
    INTEGER, PARAMETER :: nceinval = nf_einval
    INTEGER, PARAMETER :: nceperm  = nf_eperm
    INTEGER, PARAMETER :: ncenotin = nf_enotindefine
    INTEGER, PARAMETER :: nceindef = nf_eindefine
    INTEGER, PARAMETER :: ncecoord = nf_einvalcoords
    INTEGER, PARAMETER :: ncemaxds = nf_emaxdims
    INTEGER, PARAMETER :: ncename  = nf_enameinuse
    INTEGER, PARAMETER :: ncenoatt = nf_enotatt
    INTEGER, PARAMETER :: ncemaxat = nf_emaxatts
    INTEGER, PARAMETER :: ncebadty = nf_ebadtype
    INTEGER, PARAMETER :: ncebadd  = nf_ebaddim
    INTEGER, PARAMETER :: nceunlim = nf_eunlimpos
    INTEGER, PARAMETER :: ncemaxvs = nf_emaxvars
    INTEGER, PARAMETER :: ncenotvr = nf_enotvar
    INTEGER, PARAMETER :: nceglob  = nf_eglobal
    INTEGER, PARAMETER :: ncenotnc = nf_enotnc
    INTEGER, PARAMETER :: ncests   = nf_ests
    INTEGER, PARAMETER :: ncentool = nf_emaxname
    INTEGER, PARAMETER :: ncfoobar =  32
    INTEGER, PARAMETER :: ncsyserr = -31

    !!........     global options variable. used to determine behavior of error handler.
    !!........     initialized in lerror.c

    INTEGER, PARAMETER :: ncfatal  = nf_fatal   !! = 1
    INTEGER, PARAMETER :: ncverbos = nf_verbose !! = 2

    !!........     default fill values.  these must be the same as in the c interface.

    INTEGER, PARAMETER :: filbyte  = nf_fill_byte       !! = -127
    INTEGER, PARAMETER :: filchar  = nf_fill_char       !! = 0
    INTEGER, PARAMETER :: filshort = nf_fill_short      !! = -32767
    INTEGER, PARAMETER :: fillong  = nf_fill_int        !! = -2147483647
    REAL   , PARAMETER :: filfloat = nf_fill_float      !! = 9.9692099683868690e+36
    REAL*8 , PARAMETER :: fildoub  = nf_fill_double     !! = 9.9692099683868690e+36


    !!........!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#ifdef IOAPI_PNCF

    !!......   pnetcdf fortran defines
    !!......   PnetCDF library version numbers

    integer, PARAMETER :: PNETCDF_VERSION_MAJOR = 1
    integer, PARAMETER :: PNETCDF_VERSION_MINOR = 6
    integer, PARAMETER :: PNETCDF_VERSION_SUB   = 1

    !!......   size argument for defining an unlimited dimension:

    integer*8, parameter :: nfmpi_unlimited = 0

    !!......   PnetCDF APIs
    !!......   miscellaneous routines:

    CHARACTER*80, EXTERNAL :: NFMPI_INQ_LIBVERS
    CHARACTER*80, EXTERNAL :: NFMPI_STRERROR
    LOGICAL     , EXTERNAL :: NFMPI_ISSYSERR

    !!......   control routines:

    INTEGER, EXTERNAL :: nfmpi_create
    INTEGER, EXTERNAL :: nfmpi_open
    INTEGER, EXTERNAL :: nfmpi_inq_format
    INTEGER, EXTERNAL :: nfmpi_inq_file_format
    INTEGER, EXTERNAL :: nfmpi_inq_file_info
    INTEGER, EXTERNAL :: nfmpi_get_file_info
    INTEGER, EXTERNAL :: nfmpi_delete
    INTEGER, EXTERNAL :: nfmpi_enddef
    INTEGER, EXTERNAL :: nfmpi__enddef
    INTEGER, EXTERNAL :: nfmpi_redef
    INTEGER, EXTERNAL :: nfmpi_set_default_format
    INTEGER, EXTERNAL :: nfmpi_inq_default_format
    INTEGER, EXTERNAL :: nfmpi_sync
    INTEGER, EXTERNAL :: nfmpi_abort
    INTEGER, EXTERNAL :: nfmpi_close
    INTEGER, EXTERNAL :: nfmpi_set_fill
    INTEGER, EXTERNAL :: nfmpi_def_var_fill
    INTEGER, EXTERNAL :: nfmpi_inq_var_fill
    INTEGER, EXTERNAL :: nfmpi_fill_var_rec

    !!......   general inquiry routines:

    INTEGER, EXTERNAL :: nfmpi_inq
    INTEGER, EXTERNAL :: nfmpi_inq_ndims
    INTEGER, EXTERNAL :: nfmpi_inq_nvars
    INTEGER, EXTERNAL :: nfmpi_inq_num_rec_vars
    INTEGER, EXTERNAL :: nfmpi_inq_num_fix_vars
    INTEGER, EXTERNAL :: nfmpi_inq_natts
    INTEGER, EXTERNAL :: nfmpi_inq_unlimdim
    INTEGER, EXTERNAL :: nfmpi_inq_striping
    INTEGER, EXTERNAL :: nfmpi_inq_malloc_size
    INTEGER, EXTERNAL :: nfmpi_inq_malloc_max_size
    INTEGER, EXTERNAL :: nfmpi_inq_malloc_list
    INTEGER, EXTERNAL :: nfmpi_inq_files_opened
    INTEGER, EXTERNAL :: nfmpi_inq_recsize

    !!......   dimension routines:EXTERNAL :: nfmpi_inq_recsize

    INTEGER, EXTERNAL :: nfmpi_def_dim
    INTEGER, EXTERNAL :: nfmpi_inq_dimid
    INTEGER, EXTERNAL :: nfmpi_inq_dim
    INTEGER, EXTERNAL :: nfmpi_inq_dimname
    INTEGER, EXTERNAL :: nfmpi_inq_dimlen
    INTEGER, EXTERNAL :: nfmpi_rename_dim

    !!......   general attribute routines:

    INTEGER, EXTERNAL :: nfmpi_inq_att
    INTEGER, EXTERNAL :: nfmpi_inq_attid
    INTEGER, EXTERNAL :: nfmpi_inq_atttype
    INTEGER, EXTERNAL :: nfmpi_inq_attlen
    INTEGER, EXTERNAL :: nfmpi_inq_attname
    INTEGER, EXTERNAL :: nfmpi_copy_att
    INTEGER, EXTERNAL :: nfmpi_rename_att
    INTEGER, EXTERNAL :: nfmpi_del_att

    !!......   attribute put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_att,        nfmpi_get_att
    INTEGER, EXTERNAL :: nfmpi_put_att_text,   nfmpi_get_att_text
    INTEGER, EXTERNAL :: nfmpi_put_att_int1,   nfmpi_get_att_int1
    INTEGER, EXTERNAL :: nfmpi_put_att_int2,   nfmpi_get_att_int2
    INTEGER, EXTERNAL :: nfmpi_put_att_int,    nfmpi_get_att_int
    INTEGER, EXTERNAL :: nfmpi_put_att_real,   nfmpi_get_att_real
    INTEGER, EXTERNAL :: nfmpi_put_att_double, nfmpi_get_att_double
    INTEGER, EXTERNAL :: nfmpi_put_att_int8,   nfmpi_get_att_int8

    !!......   independent data mode routines:

    INTEGER, EXTERNAL :: nfmpi_begin_indep_data
    INTEGER, EXTERNAL :: nfmpi_end_indep_data

    !!......   general variable routines:

    INTEGER, EXTERNAL :: nfmpi_def_var
    INTEGER, EXTERNAL :: nfmpi_inq_var
    INTEGER, EXTERNAL :: nfmpi_inq_varid
    INTEGER, EXTERNAL :: nfmpi_inq_varname
    INTEGER, EXTERNAL :: nfmpi_inq_vartype
    INTEGER, EXTERNAL :: nfmpi_inq_varndims
    INTEGER, EXTERNAL :: nfmpi_inq_vardimid
    INTEGER, EXTERNAL :: nfmpi_inq_varnatts
    INTEGER, EXTERNAL :: nfmpi_rename_var

    !!......   entire variable put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_var
    INTEGER, EXTERNAL :: nfmpi_put_var_text
    INTEGER, EXTERNAL :: nfmpi_put_var_int1
    INTEGER, EXTERNAL :: nfmpi_put_var_int2
    INTEGER, EXTERNAL :: nfmpi_put_var_int
    INTEGER, EXTERNAL :: nfmpi_put_var_real
    INTEGER, EXTERNAL :: nfmpi_put_var_double
    INTEGER, EXTERNAL :: nfmpi_put_var_int8

    INTEGER, EXTERNAL :: nfmpi_get_var
    INTEGER, EXTERNAL :: nfmpi_get_var_text
    INTEGER, EXTERNAL :: nfmpi_get_var_int1
    INTEGER, EXTERNAL :: nfmpi_get_var_int2
    INTEGER, EXTERNAL :: nfmpi_get_var_int
    INTEGER, EXTERNAL :: nfmpi_get_var_real
    INTEGER, EXTERNAL :: nfmpi_get_var_double
    INTEGER, EXTERNAL :: nfmpi_get_var_int8

    INTEGER, EXTERNAL :: nfmpi_get_var_all
    INTEGER, EXTERNAL :: nfmpi_get_var_text_all
    INTEGER, EXTERNAL :: nfmpi_get_var_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_var_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_var_int_all
    INTEGER, EXTERNAL :: nfmpi_get_var_real_all
    INTEGER, EXTERNAL :: nfmpi_get_var_double_all
    INTEGER, EXTERNAL :: nfmpi_get_var_int8_all

    !!......   single element variable put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_var1
    INTEGER, EXTERNAL :: nfmpi_put_var1_text
    INTEGER, EXTERNAL :: nfmpi_put_var1_int1
    INTEGER, EXTERNAL :: nfmpi_put_var1_int2
    INTEGER, EXTERNAL :: nfmpi_put_var1_int
    INTEGER, EXTERNAL :: nfmpi_put_var1_real
    INTEGER, EXTERNAL :: nfmpi_put_var1_double
    INTEGER, EXTERNAL :: nfmpi_put_var1_int8

    INTEGER, EXTERNAL :: nfmpi_put_var1_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_text_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_int_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_real_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_double_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_int8_all

    INTEGER, EXTERNAL :: nfmpi_get_var1
    INTEGER, EXTERNAL :: nfmpi_get_var1_text
    INTEGER, EXTERNAL :: nfmpi_get_var1_int1
    INTEGER, EXTERNAL :: nfmpi_get_var1_int2
    INTEGER, EXTERNAL :: nfmpi_get_var1_int
    INTEGER, EXTERNAL :: nfmpi_get_var1_real
    INTEGER, EXTERNAL :: nfmpi_get_var1_double
    INTEGER, EXTERNAL :: nfmpi_get_var1_int8

    INTEGER, EXTERNAL :: nfmpi_get_var1_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_text_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_int_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_real_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_double_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_int8_all

    !!......   variable sub-array put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_vara
    INTEGER, EXTERNAL :: nfmpi_put_vara_text
    INTEGER, EXTERNAL :: nfmpi_put_vara_int1
    INTEGER, EXTERNAL :: nfmpi_put_vara_int2
    INTEGER, EXTERNAL :: nfmpi_put_vara_int
    INTEGER, EXTERNAL :: nfmpi_put_vara_real
    INTEGER, EXTERNAL :: nfmpi_put_vara_double
    INTEGER, EXTERNAL :: nfmpi_put_vara_int8

    INTEGER, EXTERNAL :: nfmpi_put_vara_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_text_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_int_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_real_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_double_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_int8_all

    INTEGER, EXTERNAL :: nfmpi_get_vara
    INTEGER, EXTERNAL :: nfmpi_get_vara_text
    INTEGER, EXTERNAL :: nfmpi_get_vara_int1
    INTEGER, EXTERNAL :: nfmpi_get_vara_int2
    INTEGER, EXTERNAL :: nfmpi_get_vara_int
    INTEGER, EXTERNAL :: nfmpi_get_vara_real
    INTEGER, EXTERNAL :: nfmpi_get_vara_double
    INTEGER, EXTERNAL :: nfmpi_get_vara_int8

    INTEGER, EXTERNAL :: nfmpi_get_vara_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_text_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_int_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_real_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_double_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_int8_all

    !!......   strided variable put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_vars
    INTEGER, EXTERNAL :: nfmpi_put_vars_text
    INTEGER, EXTERNAL :: nfmpi_put_vars_int1
    INTEGER, EXTERNAL :: nfmpi_put_vars_int2
    INTEGER, EXTERNAL :: nfmpi_put_vars_int
    INTEGER, EXTERNAL :: nfmpi_put_vars_real
    INTEGER, EXTERNAL :: nfmpi_put_vars_double
    INTEGER, EXTERNAL :: nfmpi_put_vars_int8

    INTEGER, EXTERNAL :: nfmpi_put_vars_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_text_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_int_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_real_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_double_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_int8_all

    INTEGER, EXTERNAL :: nfmpi_get_vars
    INTEGER, EXTERNAL :: nfmpi_get_vars_text
    INTEGER, EXTERNAL :: nfmpi_get_vars_int1
    INTEGER, EXTERNAL :: nfmpi_get_vars_int2
    INTEGER, EXTERNAL :: nfmpi_get_vars_int
    INTEGER, EXTERNAL :: nfmpi_get_vars_real
    INTEGER, EXTERNAL :: nfmpi_get_vars_double
    INTEGER, EXTERNAL :: nfmpi_get_vars_int8

    INTEGER, EXTERNAL :: nfmpi_get_vars_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_text_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_int_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_real_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_double_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_int8_all

    !!......   mapped variable put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_varm
    INTEGER, EXTERNAL :: nfmpi_put_varm_text
    INTEGER, EXTERNAL :: nfmpi_put_varm_int1
    INTEGER, EXTERNAL :: nfmpi_put_varm_int2
    INTEGER, EXTERNAL :: nfmpi_put_varm_int
    INTEGER, EXTERNAL :: nfmpi_put_varm_real
    INTEGER, EXTERNAL :: nfmpi_put_varm_double
    INTEGER, EXTERNAL :: nfmpi_put_varm_int8

    INTEGER, EXTERNAL :: nfmpi_put_varm_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_text_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_int_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_real_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_double_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_int8_all

    INTEGER, EXTERNAL :: nfmpi_get_varm
    INTEGER, EXTERNAL :: nfmpi_get_varm_text
    INTEGER, EXTERNAL :: nfmpi_get_varm_int1
    INTEGER, EXTERNAL :: nfmpi_get_varm_int2
    INTEGER, EXTERNAL :: nfmpi_get_varm_int
    INTEGER, EXTERNAL :: nfmpi_get_varm_real
    INTEGER, EXTERNAL :: nfmpi_get_varm_double
    INTEGER, EXTERNAL :: nfmpi_get_varm_int8

    INTEGER, EXTERNAL :: nfmpi_get_varm_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_text_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_int_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_real_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_double_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_int8_all

    !!......   Non-blocking APIs
    !!......
    !!......   entire variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_var
    INTEGER, EXTERNAL :: nfmpi_iput_var_text
    INTEGER, EXTERNAL :: nfmpi_iput_var_int1
    INTEGER, EXTERNAL :: nfmpi_iput_var_int2
    INTEGER, EXTERNAL :: nfmpi_iput_var_int
    INTEGER, EXTERNAL :: nfmpi_iput_var_real
    INTEGER, EXTERNAL :: nfmpi_iput_var_double
    INTEGER, EXTERNAL :: nfmpi_iput_var_int8

    INTEGER, EXTERNAL :: nfmpi_iget_var
    INTEGER, EXTERNAL :: nfmpi_iget_var_text
    INTEGER, EXTERNAL :: nfmpi_iget_var_int1
    INTEGER, EXTERNAL :: nfmpi_iget_var_int2
    INTEGER, EXTERNAL :: nfmpi_iget_var_int
    INTEGER, EXTERNAL :: nfmpi_iget_var_real
    INTEGER, EXTERNAL :: nfmpi_iget_var_double
    INTEGER, EXTERNAL :: nfmpi_iget_var_int8

    !!......   Nonblocking single-element variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_var1
    INTEGER, EXTERNAL :: nfmpi_iput_var1_text
    INTEGER, EXTERNAL :: nfmpi_iput_var1_int1
    INTEGER, EXTERNAL :: nfmpi_iput_var1_int2
    INTEGER, EXTERNAL :: nfmpi_iput_var1_int
    INTEGER, EXTERNAL :: nfmpi_iput_var1_real
    INTEGER, EXTERNAL :: nfmpi_iput_var1_double
    INTEGER, EXTERNAL :: nfmpi_iput_var1_int8

    INTEGER, EXTERNAL :: nfmpi_iget_var1
    INTEGER, EXTERNAL :: nfmpi_iget_var1_text
    INTEGER, EXTERNAL :: nfmpi_iget_var1_int1
    INTEGER, EXTERNAL :: nfmpi_iget_var1_int2
    INTEGER, EXTERNAL :: nfmpi_iget_var1_int
    INTEGER, EXTERNAL :: nfmpi_iget_var1_real
    INTEGER, EXTERNAL :: nfmpi_iget_var1_double
    INTEGER, EXTERNAL :: nfmpi_iget_var1_int8

    !!......   Nonblocking subarray variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_vara
    INTEGER, EXTERNAL :: nfmpi_iput_vara_text
    INTEGER, EXTERNAL :: nfmpi_iput_vara_int1
    INTEGER, EXTERNAL :: nfmpi_iput_vara_int2
    INTEGER, EXTERNAL :: nfmpi_iput_vara_int
    INTEGER, EXTERNAL :: nfmpi_iput_vara_real
    INTEGER, EXTERNAL :: nfmpi_iput_vara_double
    INTEGER, EXTERNAL :: nfmpi_iput_vara_int8

    INTEGER, EXTERNAL :: nfmpi_iget_vara
    INTEGER, EXTERNAL :: nfmpi_iget_vara_text
    INTEGER, EXTERNAL :: nfmpi_iget_vara_int1
    INTEGER, EXTERNAL :: nfmpi_iget_vara_int2
    INTEGER, EXTERNAL :: nfmpi_iget_vara_int
    INTEGER, EXTERNAL :: nfmpi_iget_vara_real
    INTEGER, EXTERNAL :: nfmpi_iget_vara_double
    INTEGER, EXTERNAL :: nfmpi_iget_vara_int8

    !!......   Nonblocking strided variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_vars
    INTEGER, EXTERNAL :: nfmpi_iput_vars_text
    INTEGER, EXTERNAL :: nfmpi_iput_vars_int1
    INTEGER, EXTERNAL :: nfmpi_iput_vars_int2
    INTEGER, EXTERNAL :: nfmpi_iput_vars_int
    INTEGER, EXTERNAL :: nfmpi_iput_vars_real
    INTEGER, EXTERNAL :: nfmpi_iput_vars_double
    INTEGER, EXTERNAL :: nfmpi_iput_vars_int8

    INTEGER, EXTERNAL :: nfmpi_iget_vars
    INTEGER, EXTERNAL :: nfmpi_iget_vars_text
    INTEGER, EXTERNAL :: nfmpi_iget_vars_int1
    INTEGER, EXTERNAL :: nfmpi_iget_vars_int2
    INTEGER, EXTERNAL :: nfmpi_iget_vars_int
    INTEGER, EXTERNAL :: nfmpi_iget_vars_real
    INTEGER, EXTERNAL :: nfmpi_iget_vars_double
    INTEGER, EXTERNAL :: nfmpi_iget_vars_int8

    !!......   Nonblocking mapped variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_varm
    INTEGER, EXTERNAL :: nfmpi_iput_varm_text
    INTEGER, EXTERNAL :: nfmpi_iput_varm_int1
    INTEGER, EXTERNAL :: nfmpi_iput_varm_int2
    INTEGER, EXTERNAL :: nfmpi_iput_varm_int
    INTEGER, EXTERNAL :: nfmpi_iput_varm_real
    INTEGER, EXTERNAL :: nfmpi_iput_varm_double
    INTEGER, EXTERNAL :: nfmpi_iput_varm_int8

    INTEGER, EXTERNAL :: nfmpi_iget_varm
    INTEGER, EXTERNAL :: nfmpi_iget_varm_text
    INTEGER, EXTERNAL :: nfmpi_iget_varm_int1
    INTEGER, EXTERNAL :: nfmpi_iget_varm_int2
    INTEGER, EXTERNAL :: nfmpi_iget_varm_int
    INTEGER, EXTERNAL :: nfmpi_iget_varm_real
    INTEGER, EXTERNAL :: nfmpi_iget_varm_double
    INTEGER, EXTERNAL :: nfmpi_iget_varm_int8

    !!......   Nonblocking entire variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_var
    INTEGER, EXTERNAL :: nfmpi_bput_var_text
    INTEGER, EXTERNAL :: nfmpi_bput_var_int1
    INTEGER, EXTERNAL :: nfmpi_bput_var_int2
    INTEGER, EXTERNAL :: nfmpi_bput_var_int
    INTEGER, EXTERNAL :: nfmpi_bput_var_real
    INTEGER, EXTERNAL :: nfmpi_bput_var_double
    INTEGER, EXTERNAL :: nfmpi_bput_var_int8

    !!......   Nonblocking single element variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_var1
    INTEGER, EXTERNAL :: nfmpi_bput_var1_text
    INTEGER, EXTERNAL :: nfmpi_bput_var1_int1
    INTEGER, EXTERNAL :: nfmpi_bput_var1_int2
    INTEGER, EXTERNAL :: nfmpi_bput_var1_int
    INTEGER, EXTERNAL :: nfmpi_bput_var1_real
    INTEGER, EXTERNAL :: nfmpi_bput_var1_double
    INTEGER, EXTERNAL :: nfmpi_bput_var1_int8

    !!......   Nonblocking subarray variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_vara
    INTEGER, EXTERNAL :: nfmpi_bput_vara_text
    INTEGER, EXTERNAL :: nfmpi_bput_vara_int1
    INTEGER, EXTERNAL :: nfmpi_bput_vara_int2
    INTEGER, EXTERNAL :: nfmpi_bput_vara_int
    INTEGER, EXTERNAL :: nfmpi_bput_vara_real
    INTEGER, EXTERNAL :: nfmpi_bput_vara_double
    INTEGER, EXTERNAL :: nfmpi_bput_vara_int8

    !!......   Nonblocking strided variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_vars
    INTEGER, EXTERNAL :: nfmpi_bput_vars_text
    INTEGER, EXTERNAL :: nfmpi_bput_vars_int1
    INTEGER, EXTERNAL :: nfmpi_bput_vars_int2
    INTEGER, EXTERNAL :: nfmpi_bput_vars_int
    INTEGER, EXTERNAL :: nfmpi_bput_vars_real
    INTEGER, EXTERNAL :: nfmpi_bput_vars_double
    INTEGER, EXTERNAL :: nfmpi_bput_vars_int8

    !!......   Nonblocking mapped variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_varm
    INTEGER, EXTERNAL :: nfmpi_bput_varm_text
    INTEGER, EXTERNAL :: nfmpi_bput_varm_int1
    INTEGER, EXTERNAL :: nfmpi_bput_varm_int2
    INTEGER, EXTERNAL :: nfmpi_bput_varm_int
    INTEGER, EXTERNAL :: nfmpi_bput_varm_real
    INTEGER, EXTERNAL :: nfmpi_bput_varm_double
    INTEGER, EXTERNAL :: nfmpi_bput_varm_int8

    !!......   Nonblocking control APIs

    INTEGER, EXTERNAL :: nfmpi_wait
    INTEGER, EXTERNAL :: nfmpi_wait_all
    INTEGER, EXTERNAL :: nfmpi_cancel

    INTEGER, EXTERNAL :: nfmpi_buffer_attach
    INTEGER, EXTERNAL :: nfmpi_buffer_detach
    INTEGER, EXTERNAL :: nfmpi_inq_buffer_usage
    INTEGER, EXTERNAL :: nfmpi_inq_buffer_size
    INTEGER, EXTERNAL :: nfmpi_inq_put_size
    INTEGER, EXTERNAL :: nfmpi_inq_get_size
    INTEGER, EXTERNAL :: nfmpi_inq_header_size
    INTEGER, EXTERNAL :: nfmpi_inq_header_extent
    INTEGER, EXTERNAL :: nfmpi_inq_varoffset
    INTEGER, EXTERNAL :: nfmpi_inq_nreqs

    !!......   varn routines:

    INTEGER, EXTERNAL :: nfmpi_put_varn
    INTEGER, EXTERNAL :: nfmpi_put_varn_text
    INTEGER, EXTERNAL :: nfmpi_put_varn_int1
    INTEGER, EXTERNAL :: nfmpi_put_varn_int2
    INTEGER, EXTERNAL :: nfmpi_put_varn_int
    INTEGER, EXTERNAL :: nfmpi_put_varn_real
    INTEGER, EXTERNAL :: nfmpi_put_varn_double
    INTEGER, EXTERNAL :: nfmpi_put_varn_int8

    INTEGER, EXTERNAL :: nfmpi_put_varn_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_text_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_int_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_real_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_double_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_int8_all

    !!......   Nonblocking varn routines

    INTEGER, EXTERNAL :: nfmpi_iput_varn
    INTEGER, EXTERNAL :: nfmpi_iput_varn_text
    INTEGER, EXTERNAL :: nfmpi_iput_varn_int1
    INTEGER, EXTERNAL :: nfmpi_iput_varn_int2
    INTEGER, EXTERNAL :: nfmpi_iput_varn_int
    INTEGER, EXTERNAL :: nfmpi_iput_varn_real
    INTEGER, EXTERNAL :: nfmpi_iput_varn_double
    INTEGER, EXTERNAL :: nfmpi_iput_varn_int8

    INTEGER, EXTERNAL :: nfmpi_iget_varn
    INTEGER, EXTERNAL :: nfmpi_iget_varn_text
    INTEGER, EXTERNAL :: nfmpi_iget_varn_int1
    INTEGER, EXTERNAL :: nfmpi_iget_varn_int2
    INTEGER, EXTERNAL :: nfmpi_iget_varn_int
    INTEGER, EXTERNAL :: nfmpi_iget_varn_real
    INTEGER, EXTERNAL :: nfmpi_iget_varn_double
    INTEGER, EXTERNAL :: nfmpi_iget_varn_int8

    INTEGER, EXTERNAL :: nfmpi_bput_varn
    INTEGER, EXTERNAL :: nfmpi_bput_varn_text
    INTEGER, EXTERNAL :: nfmpi_bput_varn_int1
    INTEGER, EXTERNAL :: nfmpi_bput_varn_int2
    INTEGER, EXTERNAL :: nfmpi_bput_varn_int
    INTEGER, EXTERNAL :: nfmpi_bput_varn_real
    INTEGER, EXTERNAL :: nfmpi_bput_varn_double
    INTEGER, EXTERNAL :: nfmpi_bput_varn_int8

    !!......   vard routines:

    INTEGER, EXTERNAL :: nfmpi_put_vard
    INTEGER, EXTERNAL :: nfmpi_get_vard

    INTEGER, EXTERNAL :: nfmpi_put_vard_all
    INTEGER, EXTERNAL :: nfmpi_get_vard_all


#endif


END MODULE MODNCFIO



!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!!      Support-functions needed for linking in non-netCDF4 mode:  return FALSE
!!      since netCDF-Fortran-4 not available
!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#ifndef IOAPI_NCF4

#define NF_FATAL (1)

    INTEGER FUNCTION nf_put_var_int64( NCID, VARID, ivals )
        INTEGER    ::     ncid, varid
        INTEGER(8) ::     ivals(*)
        nf_put_var_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VAR_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_var_int64

    INTEGER FUNCTION nf_get_var_int64( NCID, VARID, ivals )
        INTEGER    ::     ncid, varid
        INTEGER(8) ::     ivals(*)
        nf_get_var_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VAR_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_var_int64

    INTEGER FUNCTION nf_put_var1_int64( NCID, VARID, INDEX, ivals )
        INTEGER    ::     ncid, varid, index(*)
        INTEGER(8) ::     ivals(*)
        nf_put_var1_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VAR1_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_var1_int64

    INTEGER FUNCTION nf_get_var1_int64( NCID, VARID, INDEX, ivals )
        INTEGER    ::     ncid, varid, index(*)
        INTEGER(8) ::     ivals(*)
        nf_get_var1_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VAR1_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_var1_int64

    INTEGER FUNCTION nf_put_vara_int64( NCID, VARID, START, COUNT, ivals )
        INTEGER    ::     ncid, varid, start(*), count(*)
        INTEGER(8) ::     ivals(*)
        nf_put_vara_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VARA_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_vara_int64

    INTEGER FUNCTION nf_get_vara_int64( NCID, VARID, START, COUNT, ivals )
        INTEGER    ::     ncid, varid, start(*), count(*)
        INTEGER(8) ::     ivals(*)
        nf_get_vara_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VARA_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_vara_int64

    INTEGER FUNCTION nf_put_vars_int64( NCID, VARID, START, COUNT, STRIDE, ivals )
        INTEGER    ::     ncid, varid, start(*), count(*), stride(*)
        INTEGER(8) ::     ivals(*)
        nf_put_vars_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VARS_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_vars_int64

    INTEGER FUNCTION nf_get_vars_int64( NCID, VARID, START, COUNT, STRIDE, ivals )
        INTEGER    ::     ncid, varid, start(*), count(*), stride(*)
        INTEGER(8) ::     ivals(*)
        nf_get_vars_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VARS_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_vars_int64

    INTEGER FUNCTION nf_put_varm_int64( NCID, VARID, START, COUNT, STRIDE, IMAP, ivals )
        INTEGER    ::     ncid, varid, start(*), count(*), stride(*), imap(*)
        INTEGER(8) ::     ivals(*)
        nf_put_varm_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VARM_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_varm_int64

    INTEGER FUNCTION nf_get_varm_int64( NCID, VARID, START, COUNT, STRIDE, IMAP, ivals )
        INTEGER    ::     ncid, varid, start(*), count(*), stride(*), imap(*)
        INTEGER(8) ::     ivals(*)
        nf_get_varm_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VARM_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_varm_int64


#endif
