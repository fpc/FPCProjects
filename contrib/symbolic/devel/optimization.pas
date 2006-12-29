
implementation

procedure OptimizeCalcNode(p:Pnode;simlevel:longint);

VAR Nodes       : array[0..3] of pnode;
    TreeType
    NodePrio    : array[0..3] of longint;
    TreeType,
    TotalNode,
    CiNodes,
    CrNodes,
    I,J,K       : longint;
    First       : pnode;

begin
 if simlevel<>0 then {Simlevel=0 -> No rearranging}
 with p^
 IF (nodetype=calcnode) and (Op in [Mulo,Addo]) then
   begin
    IF (left^.nodetype=calcnode) and (left^.Op=op) then
     begin
      Node[0]:=left^.left;  {B}
      node[1]:=Left^.Right;  {C}
      TotalNode:=2;
      TreeType:=1;
     end
    else
      begin
       Node[0]:=Left;       {A}
       TotalNode:=1;
       TreeType:=0;
      end;
    IF (right^.nodetype=calcnode) and (right^.Op=op) then
     begin
      Node[TotalNode]:=right^.left;  {E}
      node[Totalnode+1]:=right^.Right; {F}
      Inc(totalnode,2);
      Inc(TreeType,2);
     end
    else
      begin
       if totalnode=1 then  {Already covered in standard simlevel case}
        exit;
       Node[Totalnode]:=right;
       Inc(totalnode);
      end;
   CiNodes:=0;
   CrNodes:=0;
   FOR I:=0 to TotalNode-1 DO
    begin
     if Node[I]^.nodetype=iconstnode then
      INC(CiNodes);
     if Node[I]^.nodetype=constnode then
      INC(CrNodes);
    END;

   IF (SimLevel=3) OR ((SimLevel=2) and (CrNodes>0)) then {Convert Ci to Cr}
    FOR I:=0 to TotalNode-1 DO
     if (Node[I]^.Nodetype=iconstnode) then
      begin
       j:=Node[I]^.Ivalue;
       Node[I]^.Value:=J;
       Node[I]^.nodetype:=ConstNode;
       Dec(CiNodes);
       Inc(CrNodes);
      end;
   IF CiNodes>0 Then
    Begin
     First:=NIL;
     I:=0
     repeat
      if (Node[I]^.Nodetype=iconstnode) then
       begin
        IF First=NIL THEN
         begin
          First:=Node[I]
          J:=First^.ivalue;
         end
        else
         begin
          if op=addo then
           J:=J+Node[i]^.ivalue
          else
           j:=j*node[i]^.ivalue;
          Dec(totalnode)
          If I<>TotalNode then
           node[i]:=Node[TotalNode];
         end;
      end;
      inc(i);
     until I>=TotalNode;
     First^.Ivalue:=j;
     end;
   IF CrNodes>1 Then
    Begin
     First:=NIL;                {First from Ci is still in node array}
     I:=0
     repeat
      if (Node[I]^.Nodetype=constnode) then
       begin
        IF First=NIL THEN
         begin
          First:=Node[I]
          J:=First^.value;
         end
        else
         begin
          if op=addo then
           J:=J+Node[i]^.value
          else
           j:=j*node[i]^.value;
          Dec(totalnode)            {We eliminated one node}
          Dispose(Node[I]);
          If I<>TotalNode then
           node[i]:=Node[TotalNode];    {Last to first}
         end;
      end;
     Inc(i);
    until I>=TotalNode;
     First^.value:=j;
     end;
   FOR I:=0 to TotalNode-1 DO  {Set up sorting values}
    begin
     If Node[I]^.nodetype=IconstNode then
      j:=0
     else
      If Node[I]^.nodetype=constNode then
       j:=1
      else
       if ExprIsConstant IN Node[I]^.flags then
        j:=2
       else
        j:=3;
     NodePrio[I]:=J;
    end;
   For I:=0 TO TotalNode-1 DO   {I know, bubble is bad, but it is 4*4 iterations}
    For J:=0 TO TotalNode-1 DO        {maximal}
     begin
      If NodePrio[I]< NodePrio[J] then
       begin
        First:=Node[I];
        Node[I]:=Node[J];
        Node[J]:=First;
        K:=NodePrio[I];
        NodePrio[I]:=NodePrio[J];
        NodePrio[J]:=K;
      end;
     end;
    end;
    {Enough preparation. Let's kick some tree  :-)}
    If (TreeType=2) and (TotalNode>2)  then       {Only right?}
     begin
      TreeType:=1;
      First:=Right;
      Right:=Left;
      Left:=Right;                      {Convert to only left}
     end;
    CASE TotalNode OF
     1 : begin
          First:=Left;
  end;

