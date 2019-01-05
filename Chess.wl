(* ::Package:: *)

(* :Title: Chess                            *)
(* :Author: Arne Eide                       *)
(* :Summary:
   
*)
(* :Context: Chess`     *)
(* :Package Version: 0.1                    *)
(* :Copyright: Arne Eide                    *)
(* :History:                                *)
(* :Keywords:                               *)
(* :Source:                                 *)
(* :Warning: None.                          *)
(* :Mathematica Version: 11.0               *)
(* :Limitation: None.                       *)
(* :Discussion:                             *)

BeginPackage["Chess`"]

(*
Function descriptions
*)

Chess::usage = "This package"

opts::usage = ""

Board::usage = ""
Chessboard::usage = ""
Coord::usage = ""

ShowBoard::usage = ""
PieceSize::usage = ""
BoardColour::usage = ""
ShowPieces::usage = ""
PawnConvert::usage = ""
ShowPGN::usage = ""
Interact::usage = ""

show::usage = ""
bsize::usage = ""
psize::usage = ""
col::usage = ""
incl::usage = ""
conv::usage = ""
pgn::usage = ""
inter::usage = ""

MoveFromPGN::usage = ""
PGNconvert::usage = ""
PGNlisting::usage = ""
ShortPGN::usage = ""

Static::usage = ""

answer::usage = ""

wKimage::usage = ""
wQimage::usage = ""
wRimage::usage = ""
wBimage::usage = ""
wNimage::usage = ""
wPimage::usage = ""
bKimage::usage = ""
bQimage::usage = ""
bRimage::usage = ""
bBimage::usage = ""
bNimage::usage = ""
bPimage::usage = ""

king::usage = ""
queen::usage = ""
rook::usage = ""
bishop::usage = ""
knight::usage = ""
pawn::usage = ""
empty::usage = ""
enpassant::usage = ""

Startposition::usage = ""

id::usage = ""
image::usage = ""
status::usage = ""
pos::usage = ""
MoveChoices::usage = ""

piece0::usage = ""
piece1::usage = ""
piece2::usage = ""
piece3::usage = ""
piece4::usage = ""
piece5::usage = ""
piece6::usage = ""
piece7::usage = ""
piece8::usage = ""
piece9::usage = ""
piece10::usage = ""
piece11::usage = ""
piece12::usage = ""
piece13::usage = ""
piece14::usage = ""
piece15::usage = ""
piece16::usage = ""
piece17::usage = ""
piece18::usage = ""
piece19::usage = ""
piece20::usage = ""
piece21::usage = ""
piece22::usage = ""
piece23::usage = ""
piece24::usage = ""
piece25::usage = ""
piece26::usage = ""
piece27::usage = ""
piece28::usage = ""
piece29::usage = ""
piece30::usage = ""
piece31::usage = ""
piece32::usage = ""
piece33::usage = ""

Pieces::usage = ""
wPieces::usage = ""
bPieces::usage = ""

Movelist::usage = ""
Capturelist::usage = ""

place::usage = ""

whitepos::usage = ""
blackpos::usage = ""
whitelongpos::usage = ""
blacklongpos::usage = ""

Choices::usage = ""
whiteChoices::usage = ""
blackChoices::usage = ""

whiteoptionlist::usage = ""
blackoptionlist::usage = ""

wPawnStrikes::usage = ""
bPawnStrikes::usage = ""

Inturn::usage = ""
Move::usage = ""
take::usage = ""

whiteoptionslist::usage = ""
blackoptionslist::usage = ""
optionslist::usage = ""
MakeQueen::usage = ""

attackonwhite::usage = ""
attackonblack::usage = ""

RandomMove::usage = ""

interactive::usage = ""

PGN::usage = ""


(*
Package content
*)

Begin["`Private`"]

(*
Read PGN (experimental)
*)

MoveFromPGN[move_]:=
Quiet@Block[{},

(*  Ordinary officer moves {B,3,3}, {K,1,2}, etc.  AND officers take pieces {B,x,3,3}, {K,x,1,2}, etc.*)

If[(Length[move]===3 && MemberQ[{king,queen,rook,bishop,knight},move[[1]]]) || (Length[move]===4 && MemberQ[{king,queen,rook,bishop,knight},move[[1]]] && move[[2]]===Global`x),
test={#[id],Take[move,-2]}&/@Select[
If[EvenQ[Length[Movelist]],Take[Pieces,{9,17}],Take[Pieces,{26,33}]],#[status]===First[move] &&MemberQ[#[MoveChoices],Take[move,-2]]&]
];

(*  Officers take pieces {B,x,3,3}, {K,x,1,2}, etc.  *)

If[Length[move]===4 && MemberQ[{king,queen,rook,bishop,knight},move[[1]]] && MemberQ[{Global`a,Global`b,Global`c,Global`d,Global`e,Global`f,Global`g,Global`h},move[[2]]],
test={#[id],Take[move,-2]}&/@Select[
If[EvenQ[Length[Movelist]],Take[Pieces,{9,17}],Take[Pieces,{26,33}]],#[status]===First[move] && First@Last[#[pos]]===Switch[move[[2]], Global`a, 1, Global`b, 2, Global`c, 3, Global`d, 4, Global`e, 5, Global`f, 6, Global`g, 7, Global`h, 8]&&MemberQ[#[MoveChoices],Take[move,-2]]&]
];

(*  Identified officer moves {B,5,3,3}, {R,2,1,2}, etc.  *)

If[Length[move]===4 && MemberQ[{king,queen,rook,bishop,knight},move[[1]]]&&MemberQ[Range[8],move[[2]]],
test={#[id],Take[move,-2]}&/@Select[
If[EvenQ[Length[Movelist]],Take[Pieces,{9,17}],Take[Pieces,{26,33}]],#[status]===First[move] && Last@Last[#[pos]]===move[[2]]&&MemberQ[#[MoveChoices],Take[move,-2]]&]
];


(*  Pawns moves take pieces {c,x,4,4}, etc.  *)

If[
Length[move] === 4 && MemberQ[{Global`a,Global`b,Global`c,Global`d,Global`e,Global`f,Global`g,Global`h},move[[1]]] && move[[2]] === Global`x,
  test={#[id], Take[move,-2]}& /@ Select[
    If[
      EvenQ[Length[Movelist]], 
      Take[Pieces,{2,9}],
      Take[Pieces,{18,26}]
    ],
    First@Last[#[pos]] === Switch[First[move], Global`a, 1, Global`b, 2, Global`c, 3, Global`d, 4, Global`e, 5, Global`f, 6, Global`g, 7, Global`h, 8] && MemberQ[#[MoveChoices],Take[move,-2]]&]
];

(*  ordinary pawn moves and castle  {2, 4}, {0, short}, etc. *)

If[Length[move]===2 &&move[[1]] !=0,

test={#[id],move}&/@Select[Pieces,If[EvenQ[Length[Movelist]],0,16]<#[id]<If[EvenQ[Length[Movelist]],9,25] &&MemberQ[#[MoveChoices],move]&],

If[EvenQ[Length[Movelist]]&&move==={0,Chess`Private`short},test={{16,{7,1}}}];
If[EvenQ[Length[Movelist]]&&move==={0,Chess`Private`long},test={{16,{3,1}}}];
If[OddQ[Length[Movelist]]&&move==={0,Chess`Private`short},test={{32,{7,8}}}];
If[OddQ[Length[Movelist]]&&move==={0,Chess`Private`long},test={{32,{3,8}}}];

];

test
]

PGNconvert[pgn_,no_]:=
  Block[{linegames = #+{1,-1}& /@ Partition[Flatten[Position[StringSplit[pgn,"\n"],""]],2],test},
    test = StringSplit[StringJoin[Riffle[Take[StringSplit[pgn,"\n"],linegames[[no]]]," "]],{". " , "  "," ","."}];
    If[IntegerQ[Length[test]/3],
      test = Partition[test,3],
      test = Partition[Append[test,""],3]
    ];
    test = If[Last[#]==="+",Most[#],#]& @ Characters[#]& /@ Flatten[Rest /@ test];
    test = Select[(Join[Take[#,Length[#]-2],Take[#,-2]/.{"a"->"1","b"->"2","c"->"3","d"->"4","e"->"5","f"->"6","g"->"7","h"->"8"}]&/@test),#!={}&];
    test = test /.{{"O","-","O"}->{0, short},{"O","-","O","-","O"}->{0, long},{"1","/","2","-","1","/","2"}->{},{"0","-","1"}->{},{"1","-","0"}->{},"K"->king,"Q"->queen,"R"->rook,"B"->bishop,"N"->knight};
    test = ToExpression /@ #& /@ test;
    Select[test, # != {}&]
  ]

PGNlisting[pgn_,no_]:=
  Block[{linegames=#+{1,-1}&/@Partition[Flatten[Position[StringSplit[pgn,"\n"],""]],2]},
    If[1 <= no <= Length[linegames],
      Grid[{{
        Multicolumn[StringSplit[StringJoin[Riffle[Take[StringSplit[pgn,"\n"],linegames[[no]]]," "]],{". " , "  "," ","."}],3,Appearance -> "Horizontal"],
        "\t",
        Take[StringSplit[pgn,"\n"],{If[no===1,1,linegames[[no-1,2]]+2],linegames[[no,1]]-2}]//TableForm
        }},
        Alignment->Top
      ]
    ]
  ]
  
ShortPGN[pgn_,no_]:=
  Block[{linegames=#+{1,-1}&/@Partition[Flatten[Position[StringSplit[pgn,"\n"],""]],2]},
    If[1 <= no <= Length[linegames],Take[StringSplit[pgn,"\n"],{If[no===1,1,linegames[[no-1,2]]+2],linegames[[no,1]]-2}]//TableForm
    ]
  ]  

(*
Importing piece images
*)

packageDir = DirectoryName[$InputFileName];

If[FileExistsQ[FileNameJoin[{packageDir, "pieceImages.mx"}]],
  Get[packageDir<>"pieceImages.mx"],
  
  wKimage=Import["https://marcelk.net/chess/pieces/merida/80/WhiteKing.png"];
  wQimage=Import["https://marcelk.net/chess/pieces/merida/80/WhiteQueen.png"];
  wRimage=Import["https://marcelk.net/chess/pieces/merida/80/WhiteRook.png"];
  wBimage=Import["https://marcelk.net/chess/pieces/merida/80/WhiteBishop.png"];
  wNimage=Import["https://marcelk.net/chess/pieces/merida/80/WhiteKnight.png"];
  wPimage=Import["https://marcelk.net/chess/pieces/merida/80/WhitePawn.png"];
  bKimage=Import["https://marcelk.net/chess/pieces/merida/80/BlackKing.png"];
  bQimage=Import["https://marcelk.net/chess/pieces/merida/80/BlackQueen.png"];
  bRimage=Import["https://marcelk.net/chess/pieces/merida/80/BlackRook.png"];
  bBimage=Import["https://marcelk.net/chess/pieces/merida/80/BlackBishop.png"];
  bNimage=Import["https://marcelk.net/chess/pieces/merida/80/BlackKnight.png"];
  bPimage=Import["https://marcelk.net/chess/pieces/merida/80/BlackPawn.png"];
  DumpSave[packageDir<>"pieceImages.mx",{wKimage,wQimage,wRimage,wBimage,wNimage,wPimage,bKimage,bQimage,bRimage,bBimage,bNimage,bPimage}];
  Print["Chess piece images were downloaded."]
  
]

(*
Options
*)

Options[Chess] =
  {
  ShowBoard   -> Static,
  ImageSize   -> 240,
  PieceSize   -> Automatic,
  BoardColour -> RGBColor[0.8196,0.5451,0.2784],
  ShowPieces  -> All,
  PawnConvert -> MakeQueen,
  ShowPGN     -> True,
  Interact    -> True
  };

optionslist = {ShowBoard, ImageSize, PieceSize, BoardColour, ShowPieces, PawnConvert, ShowPGN, Interact};

(*
Displaying chessboard
*)

Board[opts___] := 
Block[{show, bsize, psize, col, incl, conv, pgn, inter},
  {show, bsize, psize, col, incl, conv, pgn, inter} = optionslist /. {opts} /. Options[Chess];
  Flatten[{Reverse@#, #} & /@ Partition[Riffle[Table[col, {17}], Lighter@Lighter@col, 2], 8], 1]
]

place[no_,opts___] :=
Block[{show, bsize, psize, col, incl, conv, pgn, inter},
  {show, bsize, psize, col, incl, conv, pgn, inter} = optionslist /. {opts} /. Options[Chess];
  Graphics[
    If[Length[Last[#[pos]]]===2, 
      Text[Show[#[image], ImageSize -> If[psize === Automatic, bsize/10, psize]],Last[#[pos]]-{.5,.5}],
      {}
    ]& @ Symbol["piece"<>ToString[no]]
  ]
]

Chessboard[opts___]:=
Block[{show, bsize, psize, col, incl, conv, pgn, inter},
  {show, bsize, psize, col, incl, conv, pgn, inter} = optionslist /. {opts} /. Options[Chess];
  Show[{
    ArrayPlot[
      Board[BoardColour -> col],
      FrameTicks -> {
        {Transpose[{Range[8],Range[8,1,-1]}], Transpose[{Range[8],Range[8,1,-1]}]},
        {Transpose[{Range[8],ToString/@Take[Alphabet[],8]}], Transpose[{Range[8],ToString/@Take[Alphabet[],8]}]}
        }
    ],
    place[#, PieceSize -> If[psize === Automatic, bsize/10, psize]]&/@ Switch[incl, All,Range[32], White, Range[16], Black, Range[17,32], _, {}]},
    ImageSize->bsize
  ]
]

RandomMove:=
If[
  EvenQ[Length[Movelist]],
  {#,RandomChoice[whiteoptionlist[#]]}&@RandomChoice[Keys[whiteoptionlist]],
  {#,RandomChoice[blackoptionlist[#]]}&@RandomChoice[Keys[blackoptionlist]]
]

interactive[opts___]:=
  Block[{},
    {showp, bsizep, psizep, colp, inclp, convp, pgnp, interp} = optionslist /. {opts} /. Options[Chess];
    
    DynamicModule[{pt={4.9,1.9}},
      LocatorPane[
        Dynamic[pt],
          Grid[{{
            Dynamic[
              test = Select[Pieces,Last[#[pos]]===(Floor/@pt)+{1,1}&];
              
              If[Length[test] > 0 && Inturn[test[[1]][id]],
              
                pno =test[[1]][id];
                movopt =test[[1]][MoveChoices];
                test2 =test;
                pt1 =Last[test2[[1]][pos]],
                
                pt1 = {}
              ];
              
              If[MemberQ[movopt,(Floor/@pt)+{1,1}],
                pt2 = (Floor/@pt)+{1,1},
                pt2 = {}
              ];
              
              Show[{
                Chessboard[ShowPieces -> None, ImageSize -> bsizep, BoardColour -> colp],
                
                
                If[Length[pt2] > 0 && interp,
                  Move[pno, pt2, opts];
                  Graphics[{Opacity[.5], Lighter@Green, Rectangle[pt2-{1,1},pt2], Rectangle[pt3-{1,1}, pt3]}],
                  
                  If[Length[pt1]>0 && interp,
                    Graphics[{Opacity[.5], Lighter@Red, Rectangle[pt1-{1,1},pt1]}],
                    Graphics[{}]
                  ]
                
                ],
                
                place[#, PieceSize -> If[psizep === Automatic, bsizep/10, psizep]]& /@ Switch[
                  inclp, All,Range[32], White, Range[16], Black, Range[17, 32], _, {}
                ],
                
                If[Length[pt1] > 0 && interp,
                  pt3 = pt1;
                  Graphics[{Opacity[.8], Lighter@Green, PointSize[.05], Point[#-{.5,.5}]& /@ (#[MoveChoices]& @ test2[[1]])}],
                  Graphics[{}]
                ]
              }]
              
            ]}},
            Alignment -> Top
          ],
          Appearance -> None
        ]
      ]
    ]

Chess[opts___]:=
Block[{},
  {showc, bsizec, psizec, colc, inclc, convc, pgnc, interc} = optionslist /. {opts} /. Options[Chess];
    Switch[showc, 
      None,        Startposition, 
      Static,      Chessboard[ShowBoard -> showc, ImageSize -> bsizec, PieceSize -> psizec, BoardColour -> colc, ShowPieces -> inclc, PawnConvert -> convc, ShowPGN -> pgnc, Interact-> interc], 
      Dynamic,     Dynamic[Chessboard[ShowBoard -> showc, ImageSize -> bsizec, PieceSize -> psizec, BoardColour -> colc, ShowPieces -> inclc, PawnConvert -> convc, ShowPGN -> pgnc, Interact-> interc]],
      Interactive, interactive[ShowBoard -> showc, ImageSize -> bsizec, PieceSize -> psizec, BoardColour -> colc, ShowPieces -> inclc, PawnConvert -> convc, ShowPGN -> pgnc, Interact-> True],
      
      _,           Column[{Row[Flatten@{
                      Button["Restart",Startposition],
                      
                      Button["Back",
                        If[Length[Movelist]!=0,
                          Switch[Last[Movelist][[1]],
                            1, piece1[pos] = Most@piece1[pos], 2, piece2[pos] = Most@piece2[pos], 3, piece3[pos] = Most@piece3[pos], 4, piece4[pos] = Most@piece4[pos],
                            5, piece5[pos] = Most@piece5[pos], 6, piece6[pos] = Most@piece6[pos], 7, piece7[pos] = Most@piece7[pos], 8, piece8[pos] = Most@piece8[pos],
                            9, piece9[pos] = Most@piece9[pos], 10,piece10[pos] = Most@piece10[pos], 11,piece11[pos] = Most@piece11[pos], 12,piece12[pos] = Most@piece12[pos],
                            13,piece13[pos] = Most@piece13[pos], 14,piece14[pos] = Most@piece14[pos], 15,piece15[pos] = Most@piece15[pos], 16,piece16[pos] = Most@piece16[pos],
                            17,piece17[pos] = Most@piece17[pos], 18,piece18[pos] = Most@piece18[pos], 19,piece19[pos] = Most@piece19[pos], 20,piece20[pos] = Most@piece20[pos],
                            21,piece21[pos] = Most@piece21[pos], 22,piece22[pos] = Most@piece22[pos], 23,piece23[pos] = Most@piece23[pos], 24,piece24[pos] = Most@piece24[pos],
                            25,piece25[pos] = Most@piece25[pos], 26,piece26[pos] = Most@piece26[pos], 27,piece27[pos] = Most@piece27[pos], 28,piece28[pos] = Most@piece28[pos],
                            29,piece29[pos] = Most@piece29[pos], 30,piece30[pos] = Most@piece30[pos], 31,piece31[pos] = Most@piece31[pos],32,piece32[pos] = Most@piece32[pos]
                          ]; 
                          
                          If[NumberQ[Last[Movelist][[2,3]]],
                            Switch[Last[Movelist][[2,3]],
                              1,piece1[pos]=Most@piece1[pos], 2,piece2[pos]=Most@piece2[pos], 3,piece3[pos]=Most@piece3[pos], 4,piece4[pos]=Most@piece4[pos],
                              5,piece5[pos]=Most@piece5[pos], 6,piece6[pos]=Most@piece6[pos], 7,piece7[pos]=Most@piece7[pos], 8,piece8[pos]=Most@piece8[pos],
                              9,piece9[pos]=Most@piece9[pos], 10,piece10[pos]=Most@piece10[pos], 11,piece11[pos]=Most@piece11[pos], 12,piece12[pos]=Most@piece12[pos],
                              13,piece13[pos]=Most@piece13[pos], 14,piece14[pos]=Most@piece14[pos], 15,piece15[pos]=Most@piece15[pos], 16,piece16[pos]=Most@piece16[pos],
                              17,piece17[pos]=Most@piece17[pos], 18,piece18[pos]=Most@piece18[pos], 19,piece19[pos]=Most@piece19[pos], 20,piece20[pos]=Most@piece20[pos],
                              21,piece21[pos]=Most@piece21[pos], 22,piece22[pos]=Most@piece22[pos], 23,piece23[pos]=Most@piece23[pos], 24,piece24[pos]=Most@piece24[pos],
                              25,piece25[pos]=Most@piece25[pos], 26,piece26[pos]=Most@piece26[pos], 27,piece27[pos]=Most@piece27[pos], 28,piece28[pos]=Most@piece28[pos],
                              29,piece29[pos]=Most@piece29[pos], 30,piece30[pos]=Most@piece30[pos], 31,piece31[pos]=Most@piece31[pos], 32,piece32[pos]=Most@piece32[pos]
                            ];
                            
                            Capturelist = Most@Capturelist
                          ];
                          
                          If[Last[Movelist][[2,3]]==="0-0" && Last[Movelist][[1]]===16, piece14[pos]=Most@piece14[pos]];
                          If[Last[Movelist][[2,3]]==="0-0-0" && Last[Movelist][[1]]===16, piece13[pos]=Most@piece13[pos]];
                          If[Last[Movelist][[2,3]]==="0-0" && Last[Movelist][[1]]===32, piece30[pos]=Most@piece30[pos]];
                          If[Last[Movelist][[2,3]]==="0-0-0" && Last[Movelist][[1]]===32, piece29[pos]=Most@piece29[pos]];
                          
                          If[Movelist != {}, Movelist = Most@Movelist];
                          
                          whiteChoices;
                          blackChoices;
                          
                          piece16[MoveChoices]=
                            Complement[
                              piece16[MoveChoices], 
                              Union@Flatten[DeleteCases[blackoptionlist[#]&/@Select[#[id]&/@Select[Pieces,!#[status]===pawn&],#>16&],_Missing],1],
                              bPawnStrikes
                            ];
                            
                          piece32[MoveChoices]=
                            Complement[
                              piece32[MoveChoices],
                              Union@Flatten[DeleteCases[whiteoptionlist[#]&/@Select[#[id]&/@Select[Pieces,!#[status]===pawn&],#< 17&],_Missing],1],
                              wPawnStrikes
                            ];
                            
                          castle = {};
                          
                        ]   (* end of if-then *)
                        
                      ],
                      
                      
                      If[pgnc,
                        {Button["Next", If[Length[showc] > Length[Movelist], Move[MoveFromPGN[showc[[Length[Movelist]+1]]][[1]]]]],
                        Button["End", Move[MoveFromPGN[#][[1]]]&/@ Drop[showc, Length[Movelist]]]},
                        Button["Random Move", Move[RandomMove]]
                      ]
                      
                    }], 
                    
                    interactive[ShowBoard -> showc, ImageSize -> bsizec, PieceSize -> psizec, BoardColour -> colc, ShowPieces -> inclc, PawnConvert -> convc, ShowPGN -> pgnc, Interact-> interc]
    }]
      
  ]
]

(*
Listing moves
*)

castle = {};

xcor = Take[Alphabet[], 8];

Coord[pos_]:= xcor[[pos[[1]]]]<>ToString[pos[[2]]]

PGN :=
Module[{pgn},
  pgn=(
    Switch[#[[2,2]],
      king,  If[NumberQ[#[[2,3]]],"Kx", "K"],
      queen, If[NumberQ[#[[2,3]]],"Qx", "Q"],
      rook,  If[NumberQ[#[[2,3]]],"Rx", "R"],
      bishop,If[NumberQ[#[[2,3]]],"Bx", "B"],
      knight,If[NumberQ[#[[2,3]]],"Nx", "N"],
      pawn,  If[NumberQ[#[[2,3]]], Alphabet[][[Symbol["piece"<>ToString[#[[1]]]][pos][[Position[Symbol["piece"<>ToString[#[[1]]]][pos],#[[2,1]]][[1, 1]] - 1, 1]]]] <> "x", ""]
    ] <> Coord[#[[2,1]]]& /@ Movelist);
  pgn= ReplacePart[pgn,(#/.{List -> Rule})&/@Transpose[{First/@Position[Movelist,"0-0"] ,Table["0-0",Length[First /@ Position[Movelist,"0-0"]]]}]];
  pgn= ReplacePart[pgn,(#/.{List -> Rule})&/@Transpose[{First/@Position[Movelist,"0-0-0"] ,Table["0-0-0",Length[First /@ Position[Movelist,"0-0-0"]]]}]];
  pgn = StringJoin[StringRiffle[Flatten@Riffle[ToString[#]<>"."&/@Range[Ceiling[Length[pgn]/2]],Partition[pgn,2]], " "]]<>If[OddQ[Length[pgn]]," "<>Last[pgn],""]
  StringReplace[pgn, ". " -> "."]
]

(*
Piece properties
*)

(*********************************)
(*   Initial and current positions   *)
(*********************************)

Startposition:=
(
piece0=<|id->0,image->{},status->enpassant,pos->{{}}|>;
piece1= <|id-> 1,image->wPimage,status->pawn,pos->{{1,2}},MoveChoices->{}|>;
piece2= <|id-> 2,image->wPimage,status->pawn,pos->{{2,2}},MoveChoices->{}|>;
piece3= <|id-> 3,image->wPimage,status->pawn,pos->{{3,2}},MoveChoices->{}|>;
piece4= <|id-> 4,image->wPimage,status->pawn,pos->{{4,2}},MoveChoices->{}|>;
piece5= <|id-> 5,image->wPimage,status->pawn,pos->{{5,2}},MoveChoices->{}|>;
piece6= <|id-> 6,image->wPimage,status->pawn,pos->{{6,2}},MoveChoices->{}|>;
piece7= <|id-> 7,image->wPimage,status->pawn,pos->{{7,2}},MoveChoices->{}|>;
piece8= <|id-> 8,image->wPimage,status->pawn,pos->{{8,2}},MoveChoices->{}|>;
piece9= <|id-> 9,image->wNimage,status->knight,pos->{{2,1}},MoveChoices->{}|>;
piece10=<|id->10,image->wNimage,status->knight,pos->{{7,1}},MoveChoices->{}|>;
piece11=<|id->11,image->wBimage,status->bishop,pos->{{3,1}},MoveChoices->{}|>;
piece12=<|id->12,image->wBimage,status->bishop,pos->{{6,1}},MoveChoices->{}|>;
piece13=<|id->13,image->wRimage,status->rook,pos->{{1,1}},MoveChoices->{}|>;
piece14=<|id->14,image->wRimage,status->rook,pos->{{8,1}},MoveChoices->{}|>;
piece15=<|id->15,image->wQimage,status->queen,pos->{{4,1}},MoveChoices->{}|>;
piece16=<|id->16,image->wKimage,status->king,pos->{{5,1}},MoveChoices->{}|>;
piece17=<|id->17,image->bPimage,status->pawn,pos->{{1,7}},MoveChoices->{}|>;
piece18=<|id->18,image->bPimage,status->pawn,pos->{{2,7}},MoveChoices->{}|>;
piece19=<|id->19,image->bPimage,status->pawn,pos->{{3,7}},MoveChoices->{}|>;
piece20=<|id->20,image->bPimage,status->pawn,pos->{{4,7}},MoveChoices->{}|>;
piece21=<|id->21,image->bPimage,status->pawn,pos->{{5,7}},MoveChoices->{}|>;
piece22=<|id->22,image->bPimage,status->pawn,pos->{{6,7}},MoveChoices->{}|>;
piece23=<|id->23,image->bPimage,status->pawn,pos->{{7,7}},MoveChoices->{}|>;
piece24=<|id->24,image->bPimage,status->pawn,pos->{{8,7}},MoveChoices->{}|>;
piece25=<|id->25,image->bNimage,status->knight,pos->{{2,8}},MoveChoices->{}|>;
piece26=<|id->26,image->bNimage,status->knight,pos->{{7,8}},MoveChoices->{}|>;
piece27=<|id->27,image->bBimage,status->bishop,pos->{{3,8}},MoveChoices->{}|>;
piece28=<|id->28,image->bBimage,status->bishop,pos->{{6,8}},MoveChoices->{}|>;
piece29=<|id->29,image->bRimage,status->rook,pos->{{1,8}},MoveChoices->{}|>;
piece30=<|id->30,image->bRimage,status->rook,pos->{{8,8}},MoveChoices->{}|>;
piece31=<|id->31,image->bQimage,status->queen,pos->{{4,8}},MoveChoices->{}|>;
piece32=<|id->32,image->bKimage,status->king,pos->{{5,8}},MoveChoices->{}|>;
piece33=<|id->33,image->{},status->enpassant,pos->{{}}|>;

Movelist    = {};
Capturelist = {};

whiteChoices;
blackChoices;

)

Pieces := Symbol["piece"<>ToString[#]]&/@Range[0,33];

wPieces := Symbol["piece"<>ToString[#]]&/@Range[0,16];

bPieces := Symbol["piece"<>ToString[#]]&/@Range[17,33];

activePieces := Select[{#[id],Last[#[pos]]}&/@(Symbol["piece"<>ToString[#]]&/@Range[32]),Length[#[[2]]]===2&]

(* Checking if same position occurs for several Pieces *)

validposQ:=Length@activePieces===Length[Union[Last/@activePieces]]

whitepos:=Select[Last[#[pos]]&/@ Rest[wPieces],Length[#]===2&];

blackpos:=Select[Last[#[pos]]&/@ Most[bPieces],Length[#]===2&];

whitelongpos:=Select[{#[id],Last[#[pos]]}&/@wPieces,#[[2]]!={}&]

blacklongpos:=Select[{#[id],Last[#[pos]]}&/@bPieces,#[[2]]!={}&]

(* Unoccupied positions *)

empty:=Complement[Flatten[Table[{i,j},{i,8},{j,8}],1],Join[whitepos,blackpos]]

(* enpassant represents a virtual piece whenever the situation requires that *)

(*
Possible moves
*)

Choices[{no_,pawn}]:=
Block[{curpos,opt={}},
curpos=Last[Symbol["piece"<>ToString[no]][pos]];
If[no<9&&curpos!={},
If[MemberQ[empty,curpos+{0,1}],
AppendTo[opt,curpos+{0,1}];
If[curpos[[2]]===2&&MemberQ[empty,curpos+{0,2}],AppendTo[opt,curpos+{0,2}]]
] ;
opt=Join[opt,Intersection[Join[blackpos,{Last[piece33[pos]]}],{curpos+{1,1},curpos+{-1,1}}]]
];
If[16<no<25&&curpos!={},
If[MemberQ[empty,curpos-{0,1}],
AppendTo[opt,curpos-{0,1}];
If[curpos[[2]]===7&&MemberQ[empty,curpos-{0,2}],AppendTo[opt,curpos-{0,2}]]
];
opt=Join[opt,Intersection[Join[whitepos,{Last[piece0[pos]]}],{curpos-{1,1},curpos-{-1,1}}]]
 ];
Select[opt,#!={}&]
]

Choices[{no_,knight}]:=
If[Symbol["piece"<>ToString[no]][status]===knight&&Last[Symbol["piece"<>ToString[no]][pos]]!={},
   Intersection[
  If[no<17, Join[empty,blackpos], Join[empty,whitepos]], 
  (Last[Symbol["piece"<>ToString[no]][pos]]+#)&/@{{1,2},{1,-2},{2,1},{2,-1},{-1,2},{-1,-2},{-2,1},{-2,-1}}
],
{}
]

optionblock[no_,dir_]:= Block[
{interlist,list1},
interlist=Intersection[
Join[{Last[Symbol["piece"<>ToString[no]][pos]]},empty],
Last[Symbol["piece"<>ToString[no]][pos]]+#&/@(dir*#&/@Range[0,8])
];
list1=If[
First@First[#],
Complement[Take[interlist,Last@First[#]+1],{Last[Symbol["piece"<>ToString[no]][pos]]}],
{}
]&@(({First[#]===Abs[dir],Length[#]}&/@Split@Transpose[Abs/@Differences/@Transpose@interlist])/.{{}->{{False}}});
If[list1==={},
AppendTo[list1,If[MemberQ[If[no<17,blackpos,whitepos],Last[Symbol["piece"<>ToString[no]][pos]]+dir],Last[Symbol["piece"<>ToString[no]][pos]]+dir,{}]],
AppendTo[list1,If[MemberQ[If[no<17,blackpos,whitepos],Last[list1]+dir],Last[list1]+dir,{}]]
]
];

optionreverseblock[no_,dir_]:= Block[
{interlist,list1},
interlist=Intersection[
Join[{Last[Symbol["piece"<>ToString[no]][pos]]},empty],
Last[Symbol["piece"<>ToString[no]][pos]]+#&/@(dir*#&/@Range[0,8])
];
list1=If[
First@Last[#],
Complement[Take[Reverse@interlist,Last@Last[#]+1],{Last[Symbol["piece"<>ToString[no]][pos]]}],
{}
]&@(({First[#]===Abs[dir],Length[#]}&/@Split@Transpose[Abs/@Differences/@Transpose@interlist])/.{{}->{{False}}});
If[
list1==={},
AppendTo[list1,If[MemberQ[If[no<17,blackpos,whitepos],Last[Symbol["piece"<>ToString[no]][pos]]+dir],Last[Symbol["piece"<>ToString[no]][pos]]+dir,{}]],
AppendTo[list1,If[MemberQ[If[no<17,blackpos,whitepos],First[list1]+dir],First[list1]+dir,{}]]
]
];

Choices[{no_,bishop}]:=
If[
Symbol["piece"<>ToString[no]][status]===bishop&&Last[Symbol["piece"<>ToString[no]][pos]]!={},
Complement[Select[Join[
optionblock[no,{1,1}],
optionblock[no,{1,-1}],
optionreverseblock[no,{-1,1}],
optionreverseblock[no,{-1,-1}]
],#!={}&],{Last[Symbol["piece"<>ToString[no]][pos]]}],
{}
];

Choices[{no_,rook}]:=
If[
Symbol["piece"<>ToString[no]][status]===rook&&Last[Symbol["piece"<>ToString[no]][pos]]!={},
Complement[Select[Join[
optionblock[no,{1,0}],
optionblock[no,{0,1}],
optionreverseblock[no,{0,-1}],
optionreverseblock[no,{-1,0}]
],#!={}&],{Last[Symbol["piece"<>ToString[no]][pos]]}],
{}
];

Choices[{no_,queen}]:=
If[
Symbol["piece"<>ToString[no]][status]===queen&&Last[Symbol["piece"<>ToString[no]][pos]]!={},
Complement[Select[Join[
optionblock[no,{1,1}],
optionblock[no,{1,-1}],
optionreverseblock[no,{-1,1}],
optionreverseblock[no,{-1,-1}],
optionblock[no,{1,0}],
optionblock[no,{0,1}],
optionreverseblock[no,{0,-1}],
optionreverseblock[no,{-1,0}]
],#!={}&],{Last[Symbol["piece"<>ToString[no]][pos]]}],
{}
];

Choices[{no_,king}]:=
If[
Symbol["piece"<>ToString[no]][status]===king && Last[Symbol["piece"<>ToString[no]][pos]]!={},
Complement[Intersection[
If[no<17,Join[empty,blackpos],Join[empty,whitepos]],
(*If[no<17,Flatten[Values@blackoptionlist,1],Flatten[Values@whiteoptionlist,1]],*)
(Last[Symbol["piece"<>ToString[no]][pos]]+#)&/@{{1,1},{1,-1},{-1,1},{-1,-1},{1,0},{0,1},{-1,0},{0,-1}}]
],{}
];

whiteChoices:=(

piece1[MoveChoices]=Flatten[Choices[{1,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece2[MoveChoices]=Flatten[Choices[{2,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece3[MoveChoices]=Flatten[Choices[{3,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece4[MoveChoices]=Flatten[Choices[{4,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece5[MoveChoices]=Flatten[Choices[{5,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece6[MoveChoices]=Flatten[Choices[{6,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece7[MoveChoices]=Flatten[Choices[{7,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece8[MoveChoices]=Flatten[Choices[{8,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece9[MoveChoices]=Flatten[Choices[{9,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece10[MoveChoices]=Flatten[Choices[{10,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece11[MoveChoices]=Flatten[Choices[{11,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece12[MoveChoices]=Flatten[Choices[{12,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece13[MoveChoices]=Flatten[Choices[{13,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece14[MoveChoices]=Flatten[Choices[{14,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece15[MoveChoices]=Flatten[Choices[{15,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece16[MoveChoices]=Flatten[Choices[{16,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
(* castling *)
If[Length[piece16[pos]]===1&&Length[piece14[pos]]===1&&MemberQ[empty,{6,1}]&&MemberQ[empty,{7,1}]&&Length[attackonwhite]===0,AppendTo[piece16[MoveChoices],{7,1}]];
If[Length[piece16[pos]]===1&&Length[piece13[pos]]===1&&MemberQ[empty,{2,1}]&&MemberQ[empty,{3,1}]&&MemberQ[empty,{4,1}]&&Length[attackonwhite]===0,AppendTo[piece16[MoveChoices],{3,1}]];
);

blackChoices:=(
piece17[MoveChoices]=Flatten[Choices[{17,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece18[MoveChoices]=Flatten[Choices[{18,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece19[MoveChoices]=Flatten[Choices[{19,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece20[MoveChoices]=Flatten[Choices[{20,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece21[MoveChoices]=Flatten[Choices[{21,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece22[MoveChoices]=Flatten[Choices[{22,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece23[MoveChoices]=Flatten[Choices[{23,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece24[MoveChoices]=Flatten[Choices[{24,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece25[MoveChoices]=Flatten[Choices[{25,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece26[MoveChoices]=Flatten[Choices[{26,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece27[MoveChoices]=Flatten[Choices[{27,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece28[MoveChoices]=Flatten[Choices[{28,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece29[MoveChoices]=Flatten[Choices[{29,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece30[MoveChoices]=Flatten[Choices[{30,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece31[MoveChoices]=Flatten[Choices[{31,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
piece32[MoveChoices]=Flatten[Choices[{32,#}]&/@{pawn,knight,bishop,rook,queen,king},1];
(* castling *)
If[Length[piece32[pos]]===1 && Length[piece30[pos]]===1 && MemberQ[empty,{6,8}] && MemberQ[empty,{7,8}] && Length[attackonblack]===0, AppendTo[piece32[MoveChoices],{7,8}]];
If[Length[piece32[pos]]===1 && Length[piece29[pos]]===1 && MemberQ[empty,{2,8}] && MemberQ[empty,{3,8}] && MemberQ[empty,{4,8}] && Length[attackonblack]===0, AppendTo[piece32[MoveChoices],{3,8}]];
);

wPawnStrikes:=
Intersection[
Join[
If[piece1[status]===pawn,(Last@piece1[pos]/.{{}->{-1,-1}})+#&/@{{1,1},{-1,1}},{}],
If[piece2[status]===pawn,(Last@piece2[pos]/.{{}->{-1,-1}})+#&/@{{1,1},{-1,1}},{}],
If[piece3[status]===pawn,(Last@piece3[pos]/.{{}->{-1,-1}})+#&/@{{1,1},{-1,1}},{}],
If[piece4[status]===pawn,(Last@piece4[pos]/.{{}->{-1,-1}})+#&/@{{1,1},{-1,1}},{}],
If[piece5[status]===pawn,(Last@piece5[pos]/.{{}->{-1,-1}})+#&/@{{1,1},{-1,1}},{}],
If[piece6[status]===pawn,(Last@piece6[pos]/.{{}->{-1,-1}})+#&/@{{1,1},{-1,1}},{}],
If[piece7[status]===pawn,(Last@piece7[pos]/.{{}->{-1,-1}})+#&/@{{1,1},{-1,1}},{}],
If[piece8[status]===pawn,(Last@piece8[pos]/.{{}->{-1,-1}})+#&/@{{1,1},{-1,1}},{}]
],empty]

bPawnStrikes:=
Intersection[
Join[
If[piece17[status]===pawn,(Last@piece17[pos]/.{{}->{1,1}})-#&/@{{1,1},{-1,1}},{}],
If[piece18[status]===pawn,(Last@piece18[pos]/.{{}->{1,1}})-#&/@{{1,1},{-1,1}},{}],
If[piece19[status]===pawn,(Last@piece19[pos]/.{{}->{1,1}})-#&/@{{1,1},{-1,1}},{}],
If[piece20[status]===pawn,(Last@piece20[pos]/.{{}->{1,1}})-#&/@{{1,1},{-1,1}},{}],
If[piece21[status]===pawn,(Last@piece21[pos]/.{{}->{1,1}})-#&/@{{1,1},{-1,1}},{}],
If[piece22[status]===pawn,(Last@piece22[pos]/.{{}->{1,1}})-#&/@{{1,1},{-1,1}},{}],
If[piece23[status]===pawn,(Last@piece23[pos]/.{{}->{1,1}})-#&/@{{1,1},{-1,1}},{}],
If[piece24[status]===pawn,(Last@piece24[pos]/.{{}->{1,1}})-#&/@{{1,1},{-1,1}},{}]
],empty]

(*
Moving pieces
*)

Inturn[no_]:=If[no < 17 && EvenQ[Length[Movelist]] || no > 16 && OddQ[Length[Movelist]], True, False ]

Move[no_, to_, opts___]:=
Block[{show, bsize, psize, col, incl, conv, pgn, inter, int, capture},
  {show, bsize, psize, col, incl, conv, pgn, inter} = optionslist /. {opts} /. Options[Chess];
If[MemberQ[Symbol["piece"<>ToString[no]][MoveChoices],to]&&Inturn[no],


Switch[no,
(* white pawn Moves (Pieces 1-8) include the possibility of enpassant *)
1,AppendTo[piece1[pos],to];If[to==={1,4}&&Length[piece1[pos]]===2,AppendTo[piece0[pos],{1,3}];piece0[id]=1,piece0[pos]={{}}],
2,AppendTo[piece2[pos],to];If[to==={2,4}&&Length[piece2[pos]]===2,AppendTo[piece0[pos],{2,3}];piece0[id]=2,piece0[pos]={{}}],
3,AppendTo[piece3[pos],to];If[to==={3,4}&&Length[piece3[pos]]===2,AppendTo[piece0[pos],{3,3}];piece0[id]=3,piece0[pos]={{}}],
4,AppendTo[piece4[pos],to];If[to==={4,4}&&Length[piece4[pos]]===2,AppendTo[piece0[pos],{4,3}];piece0[id]=4,piece0[pos]={{}}],
5,AppendTo[piece5[pos],to];If[to==={5,4}&&Length[piece5[pos]]===2,AppendTo[piece0[pos],{5,3}];piece0[id]=5,piece0[pos]={{}}],
6,AppendTo[piece6[pos],to];If[to==={6,4}&&Length[piece6[pos]]===2,AppendTo[piece0[pos],{6,3}];piece0[id]=6,piece0[pos]={{}}],
7,AppendTo[piece7[pos],to];If[to==={7,4}&&Length[piece7[pos]]===2,AppendTo[piece0[pos],{7,3}];piece0[id]=7,piece0[pos]={{}}],
8,AppendTo[piece8[pos],to];If[to==={8,4}&&Length[piece8[pos]]===2,AppendTo[piece0[pos],{8,3}];piece0[id]=8,piece0[pos]={{}}],
9,AppendTo[piece9[pos],to];piece33[pos]={{}},
10,AppendTo[piece10[pos],to];piece33[pos]={{}},
11,AppendTo[piece11[pos],to];piece33[pos]={{}},
12,AppendTo[piece12[pos],to];piece33[pos]={{}},
13,AppendTo[piece13[pos],to];piece33[pos]={{}},
14,AppendTo[piece14[pos],to];piece33[pos]={{}},
15,AppendTo[piece15[pos],to];piece33[pos]={{}},
16,AppendTo[piece16[pos],to];piece33[pos]={{}};
If[to==={7,1} && Length[piece16[pos]]===2, AppendTo[piece14[pos],{6,1}]; castle = "0-0"];  (* white king short castling *)
If[to==={3,1}&&Length[piece16[pos]]===2,AppendTo[piece13[pos],{4,1}]; castle = "0-0-0"],   (* white king long castling *)
(* black pawn Moves (Pieces 17-24) include the possibility of enpassant *)
17,AppendTo[piece17[pos],to];If[to==={1,5}&&Length[piece17[pos]]===2,AppendTo[piece33[pos],{1,6}];piece33[id]=17,piece33[pos]={{}}],
18,AppendTo[piece18[pos],to];If[to==={2,5}&&Length[piece18[pos]]===2,AppendTo[piece33[pos],{2,6}];piece33[id]=18,piece33[pos]={{}}],
19,AppendTo[piece19[pos],to];If[to==={3,5}&&Length[piece19[pos]]===2,AppendTo[piece33[pos],{3,6}];piece33[id]=19,piece33[pos]={{}}],
20,AppendTo[piece20[pos],to];If[to==={4,5}&&Length[piece20[pos]]===2,AppendTo[piece33[pos],{4,6}];piece33[id]=20,piece33[pos]={{}}],
21,AppendTo[piece21[pos],to];If[to==={5,5}&&Length[piece21[pos]]===2,AppendTo[piece33[pos],{5,6}];piece33[id]=21,piece33[pos]={{}}],
22,AppendTo[piece22[pos],to];If[to==={6,5}&&Length[piece22[pos]]===2,AppendTo[piece33[pos],{6,6}];piece33[id]=22,piece33[pos]={{}}],
23,AppendTo[piece23[pos],to];If[to==={7,5}&&Length[piece23[pos]]===2,AppendTo[piece33[pos],{7,6}];piece33[id]=23,piece33[pos]={{}}],
24,AppendTo[piece24[pos],to];If[to==={8,5}&&Length[piece24[pos]]===2,AppendTo[piece33[pos],{8,6}];piece33[id]=24,piece33[pos]={{}}],
25,AppendTo[piece25[pos],to];piece0[pos]={{}},
26,AppendTo[piece26[pos],to];piece0[pos]={{}},
27,AppendTo[piece27[pos],to];piece0[pos]={{}},
28,AppendTo[piece28[pos],to];piece0[pos]={{}},
29,AppendTo[piece29[pos],to];piece0[pos]={{}},
30,AppendTo[piece30[pos],to];piece0[pos]={{}},
31,AppendTo[piece31[pos],to];piece0[pos]={{}},
32,AppendTo[piece32[pos],to];piece0[pos]={{}};
If[to==={7,8}&&Length[piece32[pos]]===2,AppendTo[piece30[pos],{6,8}]; castle = "0-0"];  (* black king short castling *)
If[to==={3,8}&&Length[piece32[pos]]===2,AppendTo[piece29[pos],{4,8}]; castle = "0-0-0"]      (* black king long castling *)
];

(* Converting pawn to queen *)

If[(no<9 && Last[to]===8 && Symbol["piece"<>ToString[no]][image]===wPimage) || (16<no<25 && Last[to]===1 && Symbol["piece"<>ToString[no]][image]===bPimage),

Switch[no,
1, If[
     conv===MakeQueen,
     piece1[image]=wQimage; piece1[status]=queen,
     answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece1[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
     piece1[image] = Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage]; piece1[status]=answer
   ],
2,If[conv===MakeQueen,piece2[image]= wQimage;piece2[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece2[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece2[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece2[status]=answer],
3,If[conv===MakeQueen,piece3[image]=wQimage;piece3[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece3[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece3[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece3[status]=answer],
4,If[conv===MakeQueen,piece4[image]=wQimage;piece4[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece4[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece4[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece4[status]=answer],
5,If[conv===MakeQueen,piece5[image]=wQimage;piece5[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece5[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece5[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece5[status]=answer],
6,If[conv===MakeQueen,piece6[image]=wQimage;piece6[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece6[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece6[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece6[status]=answer],
7,If[conv===MakeQueen,piece7[image]=wQimage;piece7[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece7[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece7[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece7[status]=answer],
8,If[conv===MakeQueen,piece8[image]=wQimage;piece8[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece8[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece8[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece8[status]=answer],
17,If[conv===MakeQueen,piece17[image]=bQimage;piece17[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece17[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece17[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece17[status]=answer],
18,If[conv===MakeQueen,piece18[image]=bQimage;piece18[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece18[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece18[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece18[status]=answer],
19,If[conv===MakeQueen,piece19[image]=bQimage;piece19[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece19[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece19[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece19[status]=answer],
20,If[conv===MakeQueen,piece20[image]=bQimage;piece20[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece20[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece20[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece20[status]=answer],
21,If[conv===MakeQueen,piece21[image]=bQimage;piece21[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece21[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece21[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece21[status]=answer],
22,If[conv===MakeQueen,piece22[image]=bQimage;piece22[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece22[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece22[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece22[status]=answer],
23,If[conv===MakeQueen,piece23[image]=bQimage;piece23[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece23[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece23[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece23[status]=answer],
24,If[conv===MakeQueen,piece24[image]=bQimage;piece24[status]=queen,answer=ChoiceDialog["Convert pawn on "<>ToString[Coord@Last[piece24[pos]]]<>" to",{Queen->queen,Rook->rook,Bishop->bishop,Knight->knight}];
piece24[image]=Switch[answer, queen, wQimage, rook, wRimage, bishop, wBimage, knight, wNimage];piece24[status]=answer]
]
];

(* Capture opposite colour piece  *)

int=Intersection[whitepos,blackpos];

If[Length[int] > 0,
  capture=#[id]&/@Select[If[no<17,bPieces,wPieces],Last[#[pos]]===First@Intersection[whitepos,blackpos]&];
  take[First@capture];
  AppendTo[Capturelist,First@capture];
  AppendTo[Movelist,no->{to, Symbol["piece"<>ToString[no]][status], First@capture}],
  AppendTo[Movelist,no-> {to, Symbol["piece"<>ToString[no]][status], castle}]
  (*AppendTo[Movelist,no\[Rule] Drop[{to,Symbol["piece"<>ToString[no]][status], castle},If[ListQ[castle],-1,0]]]*)
];

whiteChoices;
blackChoices;

(* Avoid to place the king in chess (not always!!!) *)
piece16[MoveChoices]=
Complement[
piece16[MoveChoices],
Union@Flatten[DeleteCases[blackoptionlist[#]&/@Select[#[id]&/@Select[Most@Rest@Pieces,!#[status]===pawn&],#>16&],_Missing],1],
bPawnStrikes
];

piece32[MoveChoices]=
Complement[
piece32[MoveChoices],
Union@Flatten[DeleteCases[whiteoptionlist[#]&/@Select[#[id]&/@Select[Most@Rest@Pieces,!#[status]===pawn&],#< 17&],_Missing],1],
wPawnStrikes
];

castle = {};

]

]

Move[{no_,to_}]:= Move[no,to]

take[no_,to_:{}]:=
( 
Switch[no,
1,AppendTo[piece1[pos],to],
2,AppendTo[piece2[pos],to],
3,AppendTo[piece3[pos],to],
4,AppendTo[piece4[pos],to],
5,AppendTo[piece5[pos],to],
6,AppendTo[piece6[pos],to],
7,AppendTo[piece7[pos],to],
8,AppendTo[piece8[pos],to],
9,AppendTo[piece9[pos],to],
10,AppendTo[piece10[pos],to],
11,AppendTo[piece11[pos],to],
12,AppendTo[piece12[pos],to],
13,AppendTo[piece13[pos],to],
14,AppendTo[piece14[pos],to],
15,AppendTo[piece15[pos],to],
16,AppendTo[piece16[pos],to],
17,AppendTo[piece17[pos],to],
18,AppendTo[piece18[pos],to],
19,AppendTo[piece19[pos],to],
20,AppendTo[piece20[pos],to],
21,AppendTo[piece21[pos],to],
22,AppendTo[piece22[pos],to],
23,AppendTo[piece23[pos],to],
24,AppendTo[piece24[pos],to],
25,AppendTo[piece25[pos],to],
26,AppendTo[piece26[pos],to],
27,AppendTo[piece27[pos],to],
28,AppendTo[piece28[pos],to],
29,AppendTo[piece29[pos],to],
30,AppendTo[piece30[pos],to],
31,AppendTo[piece31[pos],to],
32,AppendTo[piece32[pos],to]
];
piece0[pos]={{}};
piece33[pos]={{}}
)

whiteoptionlist:=Association[(#[[1]]->#[[2]])&/@Select[{#,Symbol["piece"<>ToString[#]][MoveChoices]}&/@Range[16],#[[2]]!={}&]];
blackoptionlist:=Association[(#[[1]]->#[[2]])&/@Select[{#,Symbol["piece"<>ToString[#]][MoveChoices]}&/@Range[17,32],#[[2]]!={}&]];

attackonwhite:=Last[Symbol["piece"<>ToString[#]][pos]]&/@(First[#[[1]]]&/@Position[blackoptionlist,Last@piece16[pos]]);
attackonblack:=Last[Symbol["piece"<>ToString[#]][pos]]&/@(First[#[[1]]]&/@Position[whiteoptionlist,Last@piece32[pos]]);

(*optionlist:=Join[whiteoptionlist,blackoptionlist]*)

(*evaluate:=100*{#[[2]]/Total[#],#[[1]]/Total[#]}&@Rest[First/@DominantColors[Show[{
Graphics[{Opacity[.2],Red,Rectangle[#,#+{1,1}]}]&/@DeleteMissing[Flatten[#[MoveChoices]&/@wPieces,1]],
Graphics[{Opacity[.2],Blue,Rectangle[#,#+{1,1}]}]&/@DeleteMissing[Flatten[#[MoveChoices]&/@bPieces,1]]
},Axes->False,PlotRange->{{1,9},{1,9}},AspectRatio->1,ImageSize->100],3,{"Coverage","Color"}]]*)

Startposition

End[]

EndPackage[]
