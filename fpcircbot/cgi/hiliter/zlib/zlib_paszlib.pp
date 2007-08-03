unit zlib_paszlib;

interface

uses
  zlib_zbase;

const
  ZLIB_VERSION = '1.1.2';

type
  { Compatibility types }
  PByte   = ^Byte;
  Uint    = Cardinal;
  Ulong   = Cardinal;
  Ulongf  = ULong;
  Pulongf = ^Ulongf;
  z_off_t = longint;

  TAllocfunc = function (opaque:pointer; items:uInt; size:uInt):pointer;
  TFreeFunc = procedure (opaque:pointer; address:pointer);

  TInternalState = record
    end;
  PInternalState = ^TInternalstate;

  TZStream = z_stream;
  PZstream = ^TZStream;

  gzFile = pointer;


const
  Z_NO_FLUSH = 0;

  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = -(1);
  Z_STREAM_ERROR = -(2);
  Z_DATA_ERROR = -(3);
  Z_MEM_ERROR = -(4);
  Z_BUF_ERROR = -(5);
  Z_VERSION_ERROR = -(6);

  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = -(1);

  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY = 0;
  Z_ASCII = 1;
  Z_UNKNOWN = 2;

  Z_DEFLATED = 8;

  Z_NULL = 0;

function zlibVersion:string;
function deflate(var strm:TZstream; flush:longint):longint;
function deflateEnd(var strm:TZstream):longint;
function inflate(var strm:TZstream; flush:longint):longint;
function inflateEnd(var strm:TZstream):longint;
function deflateSetDictionary(var strm:TZstream;dictionary : pchar; dictLength:uInt):longint;
function deflateCopy(var dest,source:TZstream):longint;
function deflateReset(var strm:TZstream):longint;
function deflateParams(var strm:TZstream; level:longint; strategy:longint):longint;
function inflateSetDictionary(var strm:TZStream;dictionary : pchar; dictLength:uInt):longint;
function inflateSync(var strm:TZStream):longint;
function inflateReset(var strm:TZStream):longint;
function compress(dest:pchar;var destLen:uLongf; source : pchar; sourceLen:uLong):longint;
function compress2(dest:pchar;var destLen:uLongf; source : pchar; sourceLen:uLong; level:longint):longint;
function uncompress(dest:pchar;var destLen:uLongf; source : pchar; sourceLen:uLong):longint;
function gzopen(path:pchar; mode:pchar):gzFile;
function gzsetparams(Thefile:gzFile; level:longint; strategy:longint):longint;
function gzread(thefile:gzFile; buf : pointer; len:cardinal):longint;
function gzwrite(thefile:gzFile; buf: pointer; len:cardinal):longint;
function gzputs(thefile:gzFile; s:pchar):longint;
function gzgets(thefile:gzFile; buf:pchar; len:longint):pchar;
function gzputc(thefile:gzFile; c:char):longint;
function gzgetc(thefile:gzFile):char;
function gzflush(thefile:gzFile; flush:longint):longint;
function gzseek(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;
function gzrewind(thefile:gzFile):longint;
function gztell(thefile:gzFile):z_off_t;
function gzeof(thefile:gzFile):longbool;
function gzclose(thefile:gzFile):longint;
function gzerror(thefile:gzFile; var errnum:longint):string;
function adler32(theadler:uLong;buf : pchar; len:uInt):uLong;
function crc32(thecrc:uLong;buf : pchar; len:uInt):uLong;
function deflateInit_(var strm:TZStream; level:longint; version:pchar; stream_size:longint):longint;
function inflateInit_(var strm:TZStream; version:pchar; stream_size:longint):longint;
function deflateInit2_(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:pchar; stream_size:longint):longint;
function inflateInit2_(var strm:TZStream; windowBits:longint; version:pchar; stream_size:longint):longint;
function deflateInit(var strm:TZStream;level : longint) : longint;
function inflateInit(var strm:TZStream) : longint;
function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;
function inflateInit2(var strm:TZStream; windowBits : longint) : longint;
function zError(err:longint):string;
function inflateSyncPoint(z:PZstream):longint;
function get_crc_table:pointer;

implementation

uses
  zlib_zutil,zlib_zdeflate,zlib_zinflate,zlib_zcompres,zlib_zuncompr,zlib_gzio,zlib_adler,zlib_gzcrc;

function zlibVersion:string;
begin
  zlibversion:=zlib_zbase.zlibversion;
end;

function deflate(var strm:TZstream; flush:longint):longint;
begin
  deflate:=zlib_zdeflate.deflate(strm,flush);
end;

function deflateEnd(var strm:TZstream):longint;
begin
  deflateEnd:=zlib_zdeflate.deflateEnd(strm);
end;

function inflate(var strm:TZstream; flush:longint):longint;
begin
  inflate:=zlib_zinflate.inflate(strm,flush);
end;

function inflateEnd(var strm:TZstream):longint;
begin
  inflateEnd:=zlib_zinflate.inflateEnd(strm);
end;

function deflateSetDictionary(var strm:TZstream;dictionary : pchar; dictLength:uInt):longint;
begin
  deflateSetDictionary:=zlib_zdeflate.deflateSetDictionary(strm,pbytef(dictionary),dictlength);
end;

function deflateCopy(var dest,source:TZstream):longint;
begin
  deflateCopy:=zlib_zdeflate.deflateCopy(@dest,@source);
end;

function deflateReset(var strm:TZstream):longint;
begin
  deflateReset:=zlib_zdeflate.deflateReset(strm);
end;

function deflateParams(var strm:TZstream; level:longint; strategy:longint):longint;
begin
  deflateParams:=zlib_zdeflate.deflateParams(strm,level,strategy);
end;

function inflateSetDictionary(var strm:TZStream;dictionary : pchar; dictLength:uInt):longint;
begin
  inflateSetDictionary:=zlib_zinflate.inflateSetDictionary(strm,pbytef(dictionary),dictlength);
end;

function inflateSync(var strm:TZStream):longint;
begin
  inflateSync:=zlib_zinflate.inflateSync(strm);
end;

function inflateReset(var strm:TZStream):longint;
begin
  inflateReset:=zlib_zinflate.inflateReset(strm);
end;

function compress(dest:pchar;var destLen:uLongf; source : pchar; sourceLen:uLong):longint;
begin
  compress:=zlib_zcompres.compress(pbytef(dest),destlen,pbytef(source),sourcelen);
end;

function compress2(dest:pchar;var destLen:uLongf; source : pchar; sourceLen:uLong; level:longint):longint;
begin
  compress2:=zlib_zcompres.compress2(pbytef(dest),destlen,pbytef(source),sourcelen,level);
end;

function uncompress(dest:pchar;var destLen:uLongf; source : pchar; sourceLen:uLong):longint;
begin
  uncompress:=zlib_zuncompr.uncompress(pbytef(dest),destlen,pbytef(source),sourcelen);
end;

function gzopen(path:pchar; mode:pchar):gzFile;
begin
  gzopen:=zlib_gzio.gzopen(path,mode);
end;

function gzsetparams(Thefile:gzFile; level:longint; strategy:longint):longint;
begin
  gzsetparams:=zlib_gzio.gzsetparams(thefile,level,strategy);
end;

function gzread(thefile:gzFile; buf : pointer; len:cardinal):longint;
begin
  gzread:=zlib_gzio.gzread(thefile,buf,len);
end;

function gzwrite(thefile:gzFile; buf: pointer; len:cardinal):longint;
begin
  gzwrite:=zlib_gzio.gzwrite(thefile,buf,len);
end;

function gzputs(thefile:gzFile; s:pchar):longint;
begin
  gzputs:=zlib_gzio.gzputs(thefile,s);
end;

function gzgets(thefile:gzFile; buf:pchar; len:longint):pchar;
begin
  gzgets:=zlib_gzio.gzgets(thefile,buf,len);
end;

function gzputc(thefile:gzFile; c:char):longint;
begin
  gzputc:=zlib_gzio.gzputc(thefile,c);
end;

function gzgetc(thefile:gzFile):char;
begin
  gzgetc:=chr(zlib_gzio.gzgetc(thefile));
end;

function gzflush(thefile:gzFile; flush:longint):longint;
begin
  gzflush:=zlib_gzio.gzflush(thefile,flush);
end;

function gzseek(thefile:gzFile; offset:z_off_t; whence:longint):z_off_t;
begin
  gzseek:=zlib_gzio.gzseek(thefile,offset,whence);
end;

function gzrewind(thefile:gzFile):longint;
begin
  gzrewind:=zlib_gzio.gzrewind(thefile);
end;

function gztell(thefile:gzFile):z_off_t;
begin
  gztell:=zlib_gzio.gztell(thefile);
end;

function gzeof(thefile:gzFile):longbool;
begin
  gzeof:=zlib_gzio.gzeof(thefile);
end;

function gzclose(thefile:gzFile):longint;
begin
  gzclose:=zlib_gzio.gzclose(thefile);
end;

function gzerror(thefile:gzFile; var errnum:longint):string;
begin
  gzerror:=zlib_gzio.gzerror(thefile,errnum);
end;

function adler32(theadler:uLong;buf : pchar; len:uInt):uLong;
begin
  adler32:=zlib_adler.adler32(theadler,pbytef(buf),len);
end;

function crc32(thecrc:uLong;buf : pchar; len:uInt):uLong;
begin
  crc32:=zlib_gzcrc.crc32(thecrc,pbytef(buf),len);
end;

function deflateInit_(var strm:TZStream; level:longint; version:pchar; stream_size:longint):longint;
begin
  deflateInit_:=zlib_zdeflate.deflateInit_(@strm,level,version,stream_size);
end;

function inflateInit_(var strm:TZStream; version:pchar; stream_size:longint):longint;
begin
  inflateInit_:=zlib_zinflate.inflateInit_(@strm,version,stream_size);
end;

function deflateInit2_(var strm:TZStream; level:longint; method:longint; windowBits:longint; memLevel:longint;strategy:longint; version:pchar; stream_size:longint):longint;
begin
  deflateInit2_:=zlib_zdeflate.deflateInit2_(strm,level,method,windowBits,memlevel,strategy,version,stream_size);
end;

function inflateInit2_(var strm:TZStream; windowBits:longint; version:pchar; stream_size:longint):longint;
begin
  inflateInit2_:=zlib_zinflate.inflateInit2_(strm,windowBits,version,stream_size);
end;

function deflateInit(var strm:TZStream;level : longint) : longint;
begin
  deflateInit:=zlib_zdeflate.deflateInit(strm,level);
end;

function inflateInit(var strm:TZStream) : longint;
begin
  inflateInit:=zlib_zinflate.inflateInit(strm);
end;

function deflateInit2(var strm:TZStream;level,method,windowBits,memLevel,strategy : longint) : longint;
begin
  deflateInit2:=zlib_zdeflate.deflateInit2(strm,level,method,windowbits,memlevel,strategy);
end;

function inflateInit2(var strm:TZStream; windowBits : longint) : longint;
begin
  inflateInit2:=zlib_zinflate.inflateInit2_(strm,windowBits,ZLIB_VERSION,sizeof(TZStream));
end;

function zError(err:longint):string;
begin
  zerror:=zlib_zbase.zerror(err);
end;

function inflateSyncPoint(z:PZstream):longint;
begin
  inflateSyncPoint:=zlib_zinflate.inflateSyncPoint(z^);
end;

function get_crc_table:pointer;
begin
  get_crc_table:=zlib_gzcrc.get_crc_table;
end;

end.
