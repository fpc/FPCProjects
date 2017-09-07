RSA Signature library
========
Version 1.0, Joost van der Sluis (CNOC) -- 4 Sep 2017

Introduction
------------

This library makes it possible to handle RSA (public) keys in the PEM and DER/
BER format. And it can be used to verify or sign content with a RSA-sha256
signature. It uses the OpenSSL library for the RSA-encryption.

How to use?
------------

The usage is pretty straight-forward. The TcnocRSAPublicKey class can be used
to construct a RSA-key. Based onto a number and exponent. It can be streamed
to a BER of PEM structure. And it can verify the signature of some content.

Example
------------

This packages comes with an example, called rsaexample. This executable can
create a PEM or BER key, based onto a number and exponent or verify an
signature.

How to get started with RSA
------------

Here are a few steps to get an idea how to work with RSA-ssh256 signatures.

You can create a new private RSA-key using openssl: (in reality, use 2048 bits
at least)

    openssl genrsa -out private.pem 1024

The generated key is a base64-encoded, DER-encoded (ASN.1) RSA private key in
the file private.pem. This may look like this:

    -----BEGIN RSA PRIVATE KEY-----
    MIICXQIBAAKBgQDEd4uG/wUpH6fOTjXKdifxCPpBhNQ1yqJ8kgt+3gUKM7iEvwgX
    c8/iwdpqhr6mVvlsIhkdw17FtxqKfTa86mjPyfIq0s8DYw1+r+VPcFMSh6QHqVuI
    mJ8e+yk4eu+OmfmdjVXnB4geUSyS+XZ4GbBn9i6/jvlUwsqTFSgf9UXpGQIDAQAB
    AoGAFUsbnTsAlS9wIYUKgXIC7DXb4cQCt/3JVJUDl/F6PFvUsNZ/iwMrUdlO0eLe
    wOqGR/j+HeKx393zi3T8vMbvKv0ssp+oP+8ryJOCMAHUaUiDeSq8/WCGUn6gXO4s
    AqWyuQjid6xpx8fMeK9z41WTdh2axOE5FY0apcnCfzD5J/ECQQD90fsD5hz6T7XF
    mzjSN+SbOMsWif8UkW4LbhB/JRY7mc66aGZ8U8nXLQNfuRVWkcyJc9E4d74B1/Yr
    dq3l4LWFAkEAxid5bWxSFbZ6Q1HIVe/RX2YYINnSjXiWoNebzOdPQZmgoiVhu6CX
    KvN4P6cDDmqzdjaSEq2abFDmfn8iSMKfhQJBAMM7BRFtqTJbp1wUVbNUbJF8MLHo
    ePxwFC3UffneI+i6GInHDbVpmWHZ7LNvIFuTD7lWDUKaz9qJK38o7P9ZRSUCQEEW
    gE1QElpteRWZzHtl7dkvOOe5hO7RWHD2wYd5x0/d/aSlMMgnxN/n32zRGDDpHY/L
    9wlJmvrPTBbq/cD2pN0CQQCJFjP1jS4NO4XJE6WiuxlU45pEYSMWR6vXIHVme/dO
    AJrfxVP+/87VUwxBUvxXNqBZZDJ/GAL+pTCxAhPG8Asb
    -----END RSA PRIVATE KEY-----

To see the real contents of the key use:

    openssl asn1parse -in private.pem

For the key given above this looks like this:

      0:d=0  hl=4 l= 605 cons: SEQUENCE
      4:d=1  hl=2 l=   1 prim: INTEGER   :00
      7:d=1  hl=3 l= 129 prim: INTEGER   :C4778B86FF05291FA7CE4E35CA7627F108FA4184D435CAA27C920B7EDE050A33B884BF081773CFE2C1DA6A86BEA656F96C22191DC35EC5B71A8A7D36BCEA68CFC9F22AD2CF03630D7EAFE54F70531287A407A95B88989F1EFB29387AEF8E99F99D8D55E707881E512C92F9767819B067F62EBF8EF954C2CA9315281FF545E919
    139:d=1  hl=2 l=   3 prim: INTEGER   :010001
    144:d=1  hl=3 l= 128 prim: INTEGER   :154B1B9D3B00952F7021850A817202EC35DBE1C402B7FDC954950397F17A3C5BD4B0D67F8B032B51D94ED1E2DEC0EA8647F8FE1DE2B1DFDDF38B74FCBCC6EF2AFD2CB29FA83FEF2BC893823001D4694883792ABCFD6086527EA05CEE2C02A5B2B908E277AC69C7C7CC78AF73E35593761D9AC4E139158D1AA5C9C27F30F927F1
    275:d=1  hl=2 l=  65 prim: INTEGER   :FDD1FB03E61CFA4FB5C59B38D237E49B38CB1689FF14916E0B6E107F25163B99CEBA68667C53C9D72D035FB9155691CC8973D13877BE01D7F62B76ADE5E0B585
    342:d=1  hl=2 l=  65 prim: INTEGER   :C627796D6C5215B67A4351C855EFD15F661820D9D28D7896A0D79BCCE74F4199A0A22561BBA0972AF3783FA7030E6AB376369212AD9A6C50E67E7F2248C29F85
    409:d=1  hl=2 l=  65 prim: INTEGER   :C33B05116DA9325BA75C1455B3546C917C30B1E878FC70142DD47DF9DE23E8BA1889C70DB5699961D9ECB36F205B930FB9560D429ACFDA892B7F28ECFF594525
    476:d=1  hl=2 l=  64 prim: INTEGER   :4116804D50125A6D791599CC7B65EDD92F38E7B984EED15870F6C18779C74FDDFDA4A530C827C4DFE7DF6CD11830E91D8FCBF709499AFACF4C16EAFDC0F6A4DD
    542:d=1  hl=2 l=  65 prim: INTEGER   :891633F58D2E0D3B85C913A5A2BB1954E39A4461231647ABD72075667BF74E009ADFC553FEFFCED5530C4152FC5736A05964327F1802FEA530B10213C6F00B1B

These are the numbers that make the RSA-key. The first number (00) is the
version-number and is not really relevant. The other numbers are (in order) the
modulus (n), public exponent (e), private exponent (d), prime-1 (p), prime-2
(q), exponent-1 (d mod (p-1)), exponent-2 (d mod (q-1)) and the coefficient
((inverse of q) mod p).

These numbers can be used as parameters for the rsaexample. The modulus and
public exponent without the other numbers make the public key. To create the
public RSA-key for the given private key do the following:

    rsaexample composepublicpemkey -o public.pem -e 0x010001 -n 0xC4778B86FF05291FA7CE4E35CA7627F108FA4184D435CAA27C920B7EDE050A33B884BF081773CFE2C1DA6A86BEA656F96C22191DC35EC5B71A8A7D36BCEA68CFC9F22AD2CF03630D7EAFE54F70531287A407A95B88989F1EFB29387AEF8E99F99D8D55E707881E512C92F9767819B067F62EBF8EF954C2CA9315281FF545E919

This will write the public key to the file public.pem. It is also possible to
generate this file using openssl (compare the results to be sure):

    openssl rsa -in private.pem -outform PEM -pubout -out public.pem

Again, it is possible to dump the real contents of the key using openssl, but
the asn1parse command is not able to parse (dump) the complete public key. (The
BIT STRING part will be missing)

    openssl asn1parse -in public.pem

The result looks like:

     0:d=0  hl=3 l= 159 cons: SEQUENCE
     3:d=1  hl=2 l=  13 cons: SEQUENCE
     5:d=2  hl=2 l=   9 prim: OBJECT            :rsaEncryption
    16:d=2  hl=2 l=   0 prim: NULL
    18:d=1  hl=3 l= 141 prim: BIT STRING

It is also possible to dump the contents using the asn1parse example from the
cnocASN1-package. To do this, the public key has to be exported in the DER-
format. (Note that the PEM-format in reality is the DER-format, only base64
encoded, and with an header and footer) This can be done both by openssl and
rsaexample:

    openssl rsa -in private.pem -outform DER -pubout -out public.der
    rsaexample composepublicderkey -o public.der -e 0x010001 -n 0xC4778B86FF05291FA7CE4E35CA7627F108FA4184D435CAA27C920B7EDE050A33B884BF081773CFE2C1DA6A86BEA656F96C22191DC35EC5B71A8A7D36BCEA68CFC9F22AD2CF03630D7EAFE54F70531287A407A95B88989F1EFB29387AEF8E99F99D8D55E707881E512C92F9767819B067F62EBF8EF954C2CA9315281FF545E919

To show the contents of these public keys in DER-format use:

    asn1parser -u -i public.der

The result will look like this:

    SEQUENCE (Universal, 16). Length: 159 Constructed.
      SEQUENCE (Universal, 16). Length: 13 Constructed.
        OBJECT IDENTIFIER (Universal, 6). Length: 9 Primitive. 1.2.840.113549.1.1.1
        NULL (Universal, 5). Length: 0 Primitive.
      BIT STRING (Universal, 3). Length: 141 Primitive. 0011000010000001100010010000001010000001100000010000000011000100011101111000101110000110111111110000010100101001000111111010011111001110010011100011010111001010011101100010011111110001000010001111101001000001100001001101010000110101110010101010001001111100100100100000101101111110110111100000010100001010001100111011100010000100101111110000100000010111011100111100111111100010110000011101101001101010100001101011111010100110010101101111100101101100001000100001100100011101110000110101111011000101101101110001101010001010011111010011011010111100111010100110100011001111110010011111001000101010110100101100111100000011011000110000110101111110101011111110010101001111011100000101001100010010100001111010010000000111101010010101101110001000100110001001111100011110111110110010100100111000011110101110111110001110100110011111100110011101100011010101010111100111000001111000100000011110010100010010110010010010111110010111011001111000000110011011000001100111111101100010111010111111100011101111100101010100110000101100101010010011000101010010100000011111111101010100010111101001000110010000001000000011000000010000000000000001

Note that asn1parser does not dump the contents of the BIT STRING also in the
format of the exponent and modulus of the public key, but gives a binary
representation of the content.

Now, assume you want to sign the contents of the file named content.txt. Sign
the content with the private key as follows:

    openssl dgst -sign private.pem -out content.signature < content.txt

To verify the signature use one of the following commands and the public key:

    openssl dgst -verify public.pem -signature content.signature < content.txt
    rsaexample verify -s content.signature -c content.txt -e 0x010001 -n 0xC4778B86FF05291FA7CE4E35CA7627F108FA4184D435CAA27C920B7EDE050A33B884BF081773CFE2C1DA6A86BEA656F96C22191DC35EC5B71A8A7D36BCEA68CFC9F22AD2CF03630D7EAFE54F70531287A407A95B88989F1EFB29387AEF8E99F99D8D55E707881E512C92F9767819B067F62EBF8EF954C2CA9315281FF545E919

It is also possible to sign the content using rsaexample, for this all parts of
the private keys have to be provided:

    ./rsaexample sign -c content.txt -o content.signature -n 0xC4778B86FF05291FA7CE4E35CA7627F108FA4184D435CAA27C920B7EDE050A33B884BF081773CFE2C1DA6A86BEA656F96C22191DC35EC5B71A8A7D36BCEA68CFC9F22AD2CF03630D7EAFE54F70531287A407A95B88989F1EFB29387AEF8E99F99D8D55E707881E512C92F9767819B067F62EBF8EF954C2CA9315281FF545E919 -e 0x010001 -d 0x154B1B9D3B00952F7021850A817202EC35DBE1C402B7FDC954950397F17A3C5BD4B0D67F8B032B51D94ED1E2DEC0EA8647F8FE1DE2B1DFDDF38B74FCBCC6EF2AFD2CB29FA83FEF2BC893823001D4694883792ABCFD6086527EA05CEE2C02A5B2B908E277AC69C7C7CC78AF73E35593761D9AC4E139158D1AA5C9C27F30F927F1 -p 0xFDD1FB03E61CFA4FB5C59B38D237E49B38CB1689FF14916E0B6E107F25163B99CEBA68667C53C9D72D035FB9155691CC8973D13877BE01D7F62B76ADE5E0B585 -q 0xC627796D6C5215B67A4351C855EFD15F661820D9D28D7896A0D79BCCE74F4199A0A22561BBA0972AF3783FA7030E6AB376369212AD9A6C50E67E7F2248C29F85 -1 0xC33B05116DA9325BA75C1455B3546C917C30B1E878FC70142DD47DF9DE23E8BA1889C70DB5699961D9ECB36F205B930FB9560D429ACFDA892B7F28ECFF594525 -2 0x4116804D50125A6D791599CC7B65EDD92F38E7B984EED15870F6C18779C74FDDFDA4A530C827C4DFE7DF6CD11830E91D8FCBF709499AFACF4C16EAFDC0F6A4DD -f 0x891633F58D2E0D3B85C913A5A2BB1954E39A4461231647ABD72075667BF74E009ADFC553FEFFCED5530C4152FC5736A05964327F1802FEA530B10213C6F00B1B

I hope that reading this gave a rough understanding on RSA keys and how to sign
content with them.

Wanna help?
------------

Do you have any questions, bugs or a patch? Please contact me at
<joost@cnoc.nl>. Also if you want to extend this library, please tell me.
Maybe we can work together.

License
------------

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This is the same license as from the Free Pascal RTL and FCL, and the Lazarus
LCL.
