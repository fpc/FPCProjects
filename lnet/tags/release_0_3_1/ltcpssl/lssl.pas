{ 
  ssl.pas

  Created by Paul Davidson on 2005/08/31.
  Copyright (c) 2005 Corax Networks.Inc.. All rights reserved.
  Used with permission. This file may be used under the modified LGPL license.
}

unit lSSL;

{$MACRO ON}
{$IFDEF DARWIN}
    {$LINKLIB libcrypto.dylib}
    {$LINKLIB libssl.dylib}
    {$DEFINE extdecl := cdecl}
{$ELSE}
    {$IFNDEF MSWINDOWS}
        {$LINKLIB libcrypto.so}
        {$LINKLIB libssl.so}
        {$LINKLIB c}
        {$DEFINE extdecl := cdecl}
    {$ELSE}
        {$DEFINE extdecl := stdcall}
    {$ENDIF}
{$ENDIF}

interface

uses
  SysUtils;


const
{$IFNDEF MSWINDOWS}
  libCrypto = 'crypto';
  libSSL    = 'ssl';
{$ELSE}
  // TODO: find out how it works in windblows
  libCrypto = '';
  libSSL    = '';
{$ENDIF}

  EVP_MAX_MD_SIZE = 16 + 20;

  OPENSSL_DES_DECRYPT = 0;
  OPENSSL_DES_ENCRYPT = 1;

  SSL_ERROR_NONE              = 0;
  SSL_ERROR_SSL               = 1;
  SSL_ERROR_WANT_READ         = 2;
  SSL_ERROR_WANT_WRITE        = 3;
  SSL_ERROR_WANT_X509_LOOKUP  = 4;
  SSL_ERROR_SYSCALL           = 5;
  SSL_ERROR_ZERO_RETURN       = 6;
  SSL_ERROR_WANT_CONNECT      = 7;
  SSL_ERROR_WANT_ACCEPT       = 8;

  SSL_OP_NO_SSLv2 = $01000000;
  SSL_OP_NO_SSLv3 = $02000000;
  SSL_OP_NO_TLSv1 = $04000000;
  SSL_OP_ALL      = $000FFFFF;
  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;

  X509_V_OK                                       = 0;
  X509_V_ILLEGAL                                  = 1;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT            = 2;
  X509_V_ERR_UNABLE_TO_GET_CRL                    = 3;
  X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE     = 4;
  X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE      = 5;
  X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY   = 6;
  X509_V_ERR_CERT_SIGNATURE_FAILURE               = 7;
  X509_V_ERR_CRL_SIGNATURE_FAILURE                = 8;
  X509_V_ERR_CERT_NOT_YET_VALID                   = 9;
  X509_V_ERR_CERT_HAS_EXPIRED                     = 10;
  X509_V_ERR_CRL_NOT_YET_VALID                    = 11;
  X509_V_ERR_CRL_HAS_EXPIRED                      = 12;
  X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD       = 13;
  X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD        = 14;
  X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD       = 15;
  X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD       = 16;
  X509_V_ERR_OUT_OF_MEM                           = 17;
  X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT          = 18;
  X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN            = 19;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY    = 20;
  X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE      = 21;
  X509_V_ERR_CERT_CHAIN_TOO_LONG                  = 22;
  X509_V_ERR_CERT_REVOKED                         = 23;
  X509_V_ERR_INVALID_CA                           = 24;
  X509_V_ERR_PATH_LENGTH_EXCEEDED                 = 25;
  X509_V_ERR_INVALID_PURPOSE                      = 26;
  X509_V_ERR_CERT_UNTRUSTED                       = 27;
  X509_V_ERR_CERT_REJECTED                        = 28;
  X509_V_ERR_SUBJECT_ISSUER_MISMATCH              = 29;
  X509_V_ERR_AKID_SKID_MISMATCH                   = 30;
  X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH          = 31;
  X509_V_ERR_KEYUSAGE_NO_CERTSIGN                 = 32;
  X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER             = 33;
  X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION         = 34;
  X509_V_ERR_APPLICATION_VERIFICATION             = 50;


type
  DESCBlock   = array[ 0..7 ] of byte;
  DESKSStruct = packed record
      ks : DESCBlock;
      weak_key : integer;
  end;  // DES_KS_Struct

  DESKeySchedule = array[ 1..16 ] of DESKSStruct;
  pFunction      = procedure;
  pInteger       = ^integer;


  tccSSLVersion = (   SSLVer_First,
                      SSLV2,
                      SSLV3,
                      TLSV1,
                      SSLALL,
                      SSLVer_Last );


  function  BIOCtrlPending( p : pointer ) : integer; extdecl; external libCrypto name 'BIO_ctrl_pending';
  procedure BIOFreeAll( p : pointer ); extdecl; external libCrypto name 'BIO_free_all';
  function  BIOSMem : pointer; extdecl; external libCrypto name 'BIO_s_mem';
  function  BIONew( p : pointer ) : pointer; extdecl; external libCrypto name 'BIO_new';
  function  BIORead( p : pointer; var buf: string; l : integer ) : integer; extdecl; external libCrypto name 'BIO_read';
  function  BIOWrite( p : pointer; buf : string; l : integer ) : integer; extdecl; external libCrypto name 'BIO_write';

  procedure CryptoCleanupAllExData; extdecl; external libCrypto name 'CRYPTO_cleanup_all_ex_data';

  procedure DESECBEncrypt( i : DESCBlock; o : DESCBlock; ks : DESKeySchedule; enc : integer ); extdecl; external libCrypto name 'DES_ecb_encrypt';
  function  DESsetkeychecked( i : DESCBlock; schedule : DESKeySchedule ) : integer; extdecl; external libCrypto name 'DES_set_key_checked';
  procedure DESSetOddParity( i : DESCBlock ); extdecl; external libCrypto name 'DES_set_odd_parity';

  procedure ErrClearError; extdecl; external libCrypto name 'ERR_clear_error';
  function  ErrErrorString( i : integer; var buf: string ) : string; extdecl; external libCrypto name 'ERR_error_string';
  procedure ErrFreeStrings; extdecl; external libCrypto name 'ERR_free_strings';
  function  ErrGetError : integer; extdecl; external libCrypto name 'ERR_get_error';
  procedure ErrRemoveState( pid : integer ); extdecl; external libCrypto name 'ERR_remove_state';

  procedure EVPCleanup; extdecl; external libCrypto name 'EVP_cleanup';

  function  SSLAccept( p : pointer ) : integer; extdecl; external libSSL name 'SSL_accept';
  function  SSLCipherGetBits( p : pointer; var bits : integer ) : integer; extdecl; external libSSL name 'SSL_CIPHER_get_bits';
  function  SSLCipherGetName( p : pointer ) : string; extdecl; external libSSL name 'SSL_CIPHER_get_name';
  function  SSLConnect( p : pointer ) : integer; extdecl; external libSSL name 'SSL_connect';
  function  SSLCtxCheckPrivateKeyFile( p : pointer ) : integer; extdecl; external libSSL name 'SSL_CTX_check_private_key';
  procedure SSLCtxFree( p : pointer ); extdecl; external libSSL name 'SSL_CTX_free';
  function  SSLCtxLoadVerifyLocations( p : pointer; const fil : string; const path : string) : integer; extdecl; external libSSL name 'SSL_CTX_load_verify_locations';
  function  SSLCtxNew( p : pointer ) : pointer; extdecl; external libSSL name 'SSL_CTX_new';
  function  SSLCtxSetCipherList( p : pointer; var s : string ) : integer; extdecl; external libSSL name 'SSL_CTX_set_cipher_list';
  procedure SSLCtxSetDefaultPasswdCb( p : pointer; cb : pointer ); extdecl; external libSSL name 'SSL_CTX_set_default_passwd_cb';
  procedure SSLCtxSetDefaultPasswdCbUserdata( p : pointer; u : pointer ); extdecl; external libSSL name 'SSL_CTX_set_default_passwd_cb_userdata';
  procedure SSLCtxSetVerify( p : pointer; mode : integer; arg2 : pFunction ); extdecl; external libSSL name 'SSL_CTX_set_verify';
  function  SSLCtxUseCertificateChainFile( p : pointer ; const fil : string ) : integer; extdecl; external libSSL name 'SSL_CTX_use_certificate_chain_file';
  function  SSLCtxUsePrivateKeyFile( p : pointer; const fil : string; typ : integer ) : integer; extdecl; external libSSL name 'SSL_CTX_use_PrivateKey_file';
  function  SSLEVPMD5 : pointer; extdecl; external libSSL name 'EVP_md5';
  procedure SSLFree( p : pointer ); extdecl; external libSSL name 'SSL_free';
  function  SSLGetCurrentCipher( p : pointer ) : pointer; extdecl; external libSSL name 'SSL_get_current_cipher';
  function  SSLGetError( p : pointer ; ret : integer) : integer; extdecl; external libSSL name 'SSL_get_error';
  function  SSLGetPeerCertificate( p : pointer ) : pointer; extdecl; external libSSL name 'SSL_get_peer_certificate';
  function  SSLGetVerifyResult( p : pointer ) : integer; extdecl; external libSSL name 'SSL_get_verify_result';
  function  SSLGetVersion( p : pointer ) : string; extdecl; external libSSL name 'SSL_get_version';
  function  SSLLibraryInit : integer; extdecl; external libSSL name 'SSL_library_init';
  procedure SSLLoadErrorStrings; extdecl; external libSSL name 'SSL_load_error_strings';
  function  SSLNew( p : pointer ) : pointer; extdecl; external libSSL name 'SSL_new';
  function  SSLPeek( p : pointer; buf: pointer; n : integer) : integer; extdecl; external libSSL name 'SSL_peek';
  function  SSLPending( p : pointer ) : integer; extdecl; external libSSL name 'SSL_pending';
  function  SSLRead( p : pointer; buf: pointer; n : integer) : integer; extdecl; external libSSL name 'SSL_read';
  function  SSLSetFd( p : pointer; fd : integer) : integer; extdecl; external libSSL name 'SSL_set_fd';
  function  SSLShutdown( p : pointer ) : integer; extdecl; external libSSL name 'SSL_shutdown';
  function  SSLV1MethodTLS : pointer; extdecl; external libSSL name 'TLSv1_method';
  function  SSLV2Method : pointer; extdecl; external libSSL name 'SSLv2_method';
  function  SSLV23Method : pointer; extdecl; external libSSL name 'SSLv23_method';
  function  SSLV3Method : pointer; extdecl; external libSSL name 'SSLv3_method';
  function  SSLWrite( p : pointer; buf: pointer; n : integer) : integer; extdecl; external libSSL name 'SSL_write';
  function  SSLX509Digest( p : pointer; typ : pointer; md : string; var l : integer ) : integer; extdecl; external libSSL name 'X509_digest';
  procedure SSLX509Free( p : pointer ); extdecl; external libSSL name 'X509_free';
  function  SSLX509GetIssuerName( p : pointer ) : pointer; extdecl; external libSSL name 'X509_get_issuer_name';
  function  SSLX509GetSubjectName( p : pointer ) : pointer; extdecl; external libSSL name 'X509_get_subject_name';
  function  SSLX509NameHash( p : pointer ) : cardinal; extdecl; external libSSL name 'X509_NAME_hash';
  function  SSLX509NameOneline( p : pointer; var buf : string; size: integer ) : string; extdecl; external libSSL name 'X509_NAME_oneline';

  function X509Print( b : pointer; a : pointer ) : integer; extdecl; external libCrypto name 'X509_print';


implementation

end.
