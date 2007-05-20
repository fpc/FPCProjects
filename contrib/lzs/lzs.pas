Unit lzs;
{$mode objfpc}

interface 

Type
      TLZBase = class
                  Buffer : pbyte ;
                  Node   : plongint;
                  pos    : longint;
                  private 
                    function dad(addme:longint):plongint; {$ifdef doinline} inline; {$endif}
                    function root(addme:longint):plongint;{$ifdef doinline} inline; {$endif}
                    function lson(addme:longint):plongint;{$ifdef doinline} inline; {$endif}
                    function rson(addme:longint):plongint;{$ifdef doinline} inline; {$endif}
                    function lzInsert(i:longint;run:longint):longint;
                    procedure lzdelete(z:longint);
                  public
                    procedure Compress(bytestocompress:LONGINT);
                    Function  Expand:Boolean;
                    function parentread(var Data;maxBytes:Longint):Longint; virtual; abstract;
                    function parentwrite(const Data;bytes:Longint):Boolean;   virtual; abstract;
                  end;
                  

implementation

{$DEFINE MSEXPAND}
{$DEFINE DOINLINE}

CONST N         = 4096; // Must be power of two.
      F         =   16;
      THRESHOLD =    3;
      CNUL      =   -1;

Const
	headermagic1=$44445A53; // SZDD
	headermagic2=$3327F088;
	headermagic3=$0041;

// translate macro's with inline procedures

function min(k,l:longint):longint;{$ifdef doinline} inline; {$endif}
Begin
 if K>l Then
  Min:=l
 else
  Min:=k;
end;

// dad (node+1)
// lson (node+1+N)
// rson (node+1+N+N)
// root (node+1+N+N+N)
// CNUL -1

function TLZBase.dad(addme:longint):plongint; {$ifdef doinline} inline; {$endif}

begin
 dad:=@node[addme+1];
end;

function TLZBase.root(addme:longint):plongint;{$ifdef doinline} inline; {$endif}

begin
 root:=@node[N+N+N+1+addme];
end;

function TLZBase.lson(addme:longint):plongint;{$ifdef doinline} inline; {$endif}

begin
 lson:=@node[N+1+addme];
end;

function TLZBase.rson(addme:longint):plongint;{$ifdef doinline} inline; {$endif}

begin
 rson:=@node[N+N+1+addme];
end;

function TLZBase.lzInsert(i:longint;run:longint):longint;

Var
    c,j,k,l,n,match   : longint;
    p                 : plongint;

Begin
    k:=1; l:=1;
    match:=THRESHOLD-1;
    p:=root(longint(buffer[i]) and 255);
    lson(i)^:=CNUL;
    Rson(i)^:=CNUL;
    j:=p^;
    while (p^<>CNUL) DO
      Begin
        n:=min(k,l);
	while (n < run) do
   	  begin
     	    c := ord(buffer[j+n]) - ord(buffer[i+n]);
     	    if (c <> 0) then
              break;
            inc(n);
          end;
        if (n>match) then
	  begin
	    match:=n;
	    pos:=j;
	  end;
  	if (c<0) then
	  begin
	      p:=lson(j);
	      k:=n;
          end
        else
          if(c>0) Then
	    begin
	      p:=rson(j);
	      l:=n;
	    end
        else
	    begin
              dad(j)^:=CNUL;
	      dad(lson(j)^)^:=lson(i)-node;		{lson(0^+i-node}
	      dad(rson(j)^)^:=rson(i)-node;
	      lson(i)^:=lson(j)^;
	      rson(i)^:=rson(j)^;
              break;
            end;
        j:=p^;
      end;
    dad(i)^:=p-node;
    p^:=i;
    lzInsert:=match;
end;

procedure TLZBase.lzdelete(z:longint);

var j:longint;

begin
    if(dad(z)^<>CNUL) Then
      Begin
        if(rson(z)^=CNUL) Then
          j:=lson(z)^
        else
          if(lson(z)^=CNUL) Then
  	    begin
              j:=rson(z)^;
            end
          else
            begin
              j:=lson(z)^;
              if (rson(j)^)<>CNUL THEN
                begin
                  repeat
             	    j:=rson(j)^;
                  until (rson(j)^=CNUL);
                  node[dad(j)^]:=lson(j)^;
           	  dad(lson(j)^)^:=dad(j)^;
           	  lson(j)^:=lson(z)^;
           	  dad(lson(z)^)^:=lson(j)-node;
                end;
              rson(j)^:=rson(z)^;
              dad(rson(z)^)^:=rson(j)-node;
            end;
      dad(j)^:=dad(z)^;
      node[dad(z)^]:=j;
      dad(z)^:=CNUL;
    end;
end;

TYPE MSHeader = packed Record
		 Magic,
		 Magic2    : Cardinal;
		 Magic3	   : Integer;
		 xfilesize : Cardinal;
	        end;

procedure TLZBase.Compress(bytestocompress:LONGINT);

const bufsize=512;

var Ch,I,Run,Len,Match,xSize,Mask   : Longint;
    Buf           : Array[0..16] OF byte;
    ReadBuf,
    WriteBuf 			    : array[0..bufsize-1] OF BYTE;
    Readptr,
    WritePtr,
    InBuf	                    : Longint;
    Header		            : MSHEADER;		

procedure WriteBytes(nbytes:cardinal);// inline;

Var WI : Longint;

begin
  WI:=BufSize-WritePtr;
  If NBytes>WI Then
    Begin
      Move (buf,WriteBuf[WritePtr],WI);
      ParentWrite(WriteBuf,BufSize);
      WritePtr:=Nbytes-WI;
      Move (buf[WI],writebuf,WritePtr);
    End
  Else
    begin
      Move (buf,WriteBuf[writeptr],nbytes);
      Inc(WritePtr,nbytes); 
    End;
end;

function loadstuff:longint; {$ifdef doinline} {inline;} {$endif} // don't inline

Begin
  If Inbuf<>BufSize Then  // END OF FILE
    Exit(-1);
  Inbuf:=ParentRead(ReadBuf,Bufsize);
  LoadStuff:=ORD(ReadBuf[0]);
  ReadPtr:=1;
end;

function Readbyte:longint; {$ifdef doinline} inline; {$endif}

begin
  If ReadPtr<InBuf Then
    Begin
      ReadByte:=ORD(ReadBuf[readptr]);
      Inc(readPtr);
    End
  Else 
    ReadByte:=LoadStuff;
end;

Begin
  ReadPtr:=0;
  WritePtr:=0;
  Inbuf:=ParentRead(ReadBuf,BufSize);
  If Inbuf=0 Then
    Exit;
  Getmem(Buffer,N+F+(N+1+N+N+256)*sizeof(longint)); {28.5kb or so, on 32-bit}
  IF Assigned(Buffer) Then
    Begin
{$ifdef MSEXPAND}
      header.magic :=headermagic1; // SZDD
      header.magic2:=headermagic2;
      header.magic3:=headermagic3;
      header.xfilesize:=BytesToCompress;
      ParentWrite(header,sizeof(header));
{$endif}
      node:=plongint(buffer+N+F);
      filldword(Root(0)^,256,cnul);
      filldword(dad(cnul)^,N-CNUL,cnul);
      xsize:=1;
      Mask:=1;
      buf[0]:=0;
      i:=N-F-F;
      Len:=0;
      ch:=ReadByte;
      While (Len<F) AND (ch<>-1) DO
        Begin
          Buffer[I+F]:=Ch;
	  I:=(I+1) AND (N-1);
	  Inc(Len); 
 	  IF len<F then
           ch:=ReadByte;
        End;
      run:=len;
      Repeat
        ch:=ReadByte;
	If I>=(N-F) Then
          Begin
            LzDelete(I+F-N);
	    Buffer[I+F]:=ch;
	    Buffer[I+F-N]:=ch;
	  End
        Else	
          Begin
	    LzDelete(I+F);
	    Buffer[I+F]:=ch;
          End;
	Match:=LzInsert(I,Run);
	If Ch=-1 Then
	  Begin
	    Dec(Run);
	    Dec(Len);
	  End;
        If Len>=Run Then
          Begin
            inc(len);
            If Match>= THRESHOLD THen
              Begin
{$ifdef MSEXPAND}
	        Buf[xsize]:=Pos;
	        Inc(xsize);
	        Buf[xsize]:=((Pos shr 4) and 240)+(match-3);
                Inc(xsize);
                Dec(len,Match);	      
{$ELSE}	
	        Buf[0]:=Buf[0] OR Mask;
	        plongint(Buf+xsize)^:=((match-3) shl 13) or ((i-pos-1) and (N-1));		
 	        inc(xsize,2);
                Dec(len,Match);
{$endif}
              End
            Else
              Begin
{$ifdef MSEXPAND}
	        Buf[0]:=Buf[0] OR Mask;
{$endif}
	        buf[xsize]:=buffer[i];
                inc(xsize);      	
	        dec(len);
              End;
	    mask:=mask+mask;
            If (Mask and 255)=0 Then
              Begin
                WriteBytes(xsize);
         	xSize:=1;
	 	Mask:=1;
		buf[0]:=0;
              End
             
          End
          else
 	    inc(len);
        I:=(I+1) and (N-1);
      Until Len=0;
      If xSize>1 Then
        WriteBytes(xSize);
      If WritePtr>0 Then
       ParentWrite(WriteBuf,WritePtr);
      Freemem(Buffer);
   End;
End;

Function TLzBase.Expand:Boolean;

const bufsize=512;

Var bits,ch,i,j,len,mask      : Longint;
{$IFDEF MSEXPAND}
    Header		            : MSHEADER;
{$ENDIF}
    ReadBuf,
    WriteBuf 			    : array[0..bufsize-1] OF BYTE;
    Readptr,
    WritePtr,
    InBuf	                    : Longint;

procedure putb(b:byte);	{$ifdef doinline} inline; {$endif}

begin
  WriteBuf[WritePtr]:=b;
  INC(WritePtr);
  IF WritePTR=BufSize Then
   BEGIN
    ParentWrite(WriteBuf,BufSize);
    WritePtr:=0;
   End;
end;

function loadstuff:longint; {$ifdef doinline} inline; {$endif}

Begin
  If Inbuf<>BufSize Then  // END OF FILE
    Exit(-1);
  Inbuf:=ParentRead(ReadBuf,Bufsize);
  if inbuf=0 then exit(-1);
  LoadStuff:=ORD(ReadBuf[0]);
  ReadPtr:=1;
end;

function getb:longint; {$ifdef doinline} inline; {$endif}

begin
  If ReadPtr<InBuf Then
    Begin
      getb:=ORD(ReadBuf[readptr]);
      Inc(readPtr);
    End
  Else 
    getb:=LoadStuff;
end;

function getw:longint; {$ifdef doinline} inline; {$endif}

var c,c2 : Longint;

begin
  If ReadPtr<(InBuf-1) Then
    Begin
      getw:=ORD(pword(@ReadBuf[readptr])^);
      Inc(readPtr,2);
    End
  Else 
   Begin
     IF ReadPtr=(Inbuf-1) Then
      Begin
       c:=ORD(ReadBuf[ReadPtr]);
       c2:=loadstuff;
       if c2=-1 then
         exit(-1);	
       getw:=C+ (c2 SHL 8);
      End
     Else
      Begin
       if LoadStuff=-1 then
	exit(-1);
       getw:=Pword(@readbuf[0])^;
       ReadPtr:=2;
      End;
   End;
end;

Begin
{$IFDEF MSEXPAND}
  I:=ParentRead(Header,SIZEOF(HEADER));
  if (i<>sizeof(header)) OR (header.magic<>headermagic1) OR

       (header.magic2<>headermagic2) OR (header.magic3<>headermagic3) Then
    
          Exit(FALSE);  // C code copies till -1 here. I prefer to exit with error
{$endif}
   GetMem(buffer,N);
   IF Assigned (Buffer) Then
    Begin
      ReadPtr:=0;
      WritePtr:=0;
      Inbuf:=ParentRead(ReadBuf,BufSize);
      I:=N-F;

      fillchar(buffer[0],N,' ');
      While true Do
        Begin
          Bits:=Getb;
          if bits=-1 then break;
          Mask:=1;
          While (Mask AND 255)<>0 DO
            Begin
{$ifdef MSEXPAND}
              IF (Bits AND Mask) = 0 Then
		Begin
        	  j:=getb;
		  If J=-1 Then 
		   Break;
      		  Len:=Getb;
	          Inc(J, (len AND 240) shl 4);
		  len:=(len and 15)+3;
{$ELSE}
		if (bits and mask)=0 Then
		  Begin
		    J:=getw;
	  	    Len:=((J shr 12) and 15)+3;		
		    j:=(i-j-1) and (N-1);
{$ENDIF}
		    while (len>0) DO
		     Begin
		       buffer[i]:=Buffer[j];
                       putb(buffer[j]);
		       J:=(J+1) AND (N-1);
		       I:=(I+1) AND (N-1);
		       DEC(LEN);
		     END;
                     dec(len);
                  END
		ELSE
               begin
                 ch:=getb;
    	         IF CH=-1 Then
	  	   Break;
	         buffer[i]:=ch;
                 putb(ch);
	         I:=(I+1) AND (N-1);  	
	       end;
               Mask:=Mask shl 1;
	     end;
	end;
    bits:=getb;
  end;
  Expand:=True;
      If WritePtr>0 Then
       ParentWrite(WriteBuf,WritePtr);

  Freemem(buffer);
end;	

end.
