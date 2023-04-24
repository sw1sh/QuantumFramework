(* Mathematica Package *)

BeginPackage["Wolfram`IBMQLoad`"]
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"] (* Begin Private Context *)

If[! ListQ[System`$Services], Get["OAuth`"]]

Block[{dir = DirectoryName[System`Private`$InputFileName]},
	KeyClient`addKeyservice["IBMQ", dir]
]


End[] (* End Private Context *)
EndPackage[]

