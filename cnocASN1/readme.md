ASN.1 BER/DER-parser
========
Version 1.0, Joost van der Sluis (CNOC) -- 18 Aug 2017

Introduction
------------

Although this packages is named cnocASN1, it is not really an ASN.1 library. It
is a BER/DER encoding and decoding library, written to be able to work with RSA
encryption keys. It is not a full-blown ASN.1 library.

In the basics, ASN.1 is a language to be able to define data-structures. Ber is
a format to encode a structure, as described by an ASN.1 document.

This package is not able to parse or work with an ASN.1 schema. But it is
possible to create Pascal-classes which represent an ASN.1 schema, and work with
those.

How to use?
------------

When a class implements the IcnocASN1EncodableClass interface, instances of the
class can be streamed into the DER format. To load an instance of a class from
a stream in the BER-format, the class has to implement the
IcnocASN1DecodableClass interface.
The TcnocASN1BERDecoder derrives the right class from a TcnocASN1TagFactory.

With a TcnocASN1TagFactory with only the TcnocASN1GenericElement registered, it
is possible to de- and en-code all BER-streams. The TcnocASN1GenericElement is
an opaque type with no information about the underlying data structure
whatsoever. This method is similar to using BER-streams without any ASN.1
schema.

In the cnocASN1Element unit there are some more classes that represent some of
the general (called 'universal' in ASN.1) types. These can be added to a
factory, so that more information about a BER-stream can be retrieved. Note that
it is not possible to extract the real/full data with only these types
registered. To be able to parse the full content of a BER-encoded stream, the
factory needs a collection of all the structures of a specific ASN.1-scheme.

A simple example of a construction with a Pascal-class that represents an ASN.1-
structure can be found in the cnocRSA package, which has a representation of
the RSAPublicKey ASN.1-structure.

Example
------------

This packages comes with an example, called asn1parser. This executable can
parse a BER-encoded stream, and show it's contents. It is also able to
encode-the stream again and output it to file. (Useless, but.. hey... it's an
example)
It is possible to run the example with and without the universal classes.

More information about ASN.1, BER and DER
------------

People who tend to use ASN.1 like to write long texts with a lot of nonsense
to describe the ASN.1, BER and DER systems. This makes it hard to read and
understand. And this is, but this is clearly my own opinion, one of the reasons
that the format is not being used that much any more.

The [Layman's Guide to a Subset of ASN.1, BER, and DER] [guide] by Burton S.
Kaliski Jr. helped me a lot, though.

[guide]: http://luca.ntop.org/Teaching/Appunti/asn1.html

For more documentation and tools see the [OSS site]. And the [extensive book
about ASN.1] [book] from Olivier Dubuisson.

[OSS site]: http://www.oss.com/
[book]: http://www.oss.com/asn1/resources/books-whitepapers-pubs/dubuisson-asn1-book.PDF

There is also an [online tool] which parses BER-encoded data and compares the
decoded and original data.

[online tool]: http://lapo.it/asn1js/

BER versus DER
------------

One note about the difference between the BER and DER formats. Using the BER
format, there are multiple ways to encode the same structure. This means that
two different BER-streams can represent the same data. The DER-format is just
an addition to the BER-encoding rules, to specify which format to use in those
cases. This way the DER-encoded representation of a structure will always be
the same. Note that this means that any DER-encoded stream also is BER-encoded,
but not the other way around.

This packages handles the difference as follows: while decoding a stream, the
BER-decoding rules are followed. This way both BER- and DER-encoded streams can
be read. When a structure is encoded, the DER-format is used.

Wanna help?
------------
Do you have any questions, bugs or a patch? Please contact me at
<joost@cnoc.nl>. Also if you want to extend this library or add a real ASN.1
parser, please tell me. Maybe we can work together.

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
