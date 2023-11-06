BeginPackage["Cotengra`"];

GreedyPath
OptimalPath


Begin["`Private`"];

$libraryName = "libcotengra";

libraryFunctions = LibraryFunctionLoad[
	$libraryName,
	"load_wstp_functions",
	LinkObject,
	LinkObject
][$libraryName];


GreedyPath[
	input : {{___String}...},
	output : {___String},
	sizeDict : KeyValuePattern[_String -> _Integer],
	costMod : _ ? NumericQ | None : None,
	temperature : _ ? NumericQ | None : None,
	simplify : True | False | None : None,
	useSSA : True | False | None : None
] := Block[{ds = Developer`DataStore, path},
	Enclose[
		path = List @@ List @@@ Confirm @ libraryFunctions["optimize_greedy"][
			ds @@ ds @@@ input,
			ds @@ output,
			ds @@ ds @@@ Normal[N /@ sizeDict],
			ds @ Replace[N[costMod], None -> Sequence[]],
			ds @ Replace[N[temperature], None -> Sequence[]],
			ds @ Replace[simplify, None -> Sequence[]],
			ds @ Replace[useSSA, None -> Sequence[]]
		];
		path + 1
	]
]

OptimalPath[
	input : {{___String}...},
	output : {___String},
	sizeDict : KeyValuePattern[_String -> _Integer],
	minimize : _String | None : None,
	costCap : _ ? NumericQ | None : None,
	searchOuter : True | False | None : None,
	simplify : True | False | None : None,
	useSSA : True | False | None : None
] := Block[{ds = Developer`DataStore, path},
	Enclose[
		path = List @@ List @@@ Confirm @ libraryFunctions["optimize_optimal"][
			ds @@ ds @@@ input,
			ds @@ output,
			ds @@ ds @@@ Normal[N /@ sizeDict],
			ds @ Replace[minimize, None -> Sequence[]],
			ds @ Replace[N[costCap], None -> Sequence[]],
			ds @ Replace[searchOuter, None -> Sequence[]],
			ds @ Replace[simplify, None -> Sequence[]],
			ds @ Replace[useSSA, None -> Sequence[]]
		];
		path + 1
	]
]

End[]
EndPackage[]