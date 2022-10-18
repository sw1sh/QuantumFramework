Package["Wolfram`QuantumFramework`"]

PackageExport[QuEST]
PackageExport[FromQuESTLink]
PackageExport[ToQuESTLink]

PackageScope[QuESTCompile]
PackageScope[QuESTApply]



$QuESTPackageURL = "https://qtechtheory.org/questlink.m"

QuEST::pkg = "Can't import QuESTLink package from `1`. Please Import or Get it manually and try again."
QuEST::link = "Can't install QuEST library. Please download or compilie it manually following instructions on https://github.com/QuEST-Kit/QuEST and load the library with Install['quest_link']"


QuESTLinkQ[] := ! FailureQ[Quiet[Needs["QuEST`"]]]

ImportQuEST[] := If[! QuESTLinkQ[],
	Get[$QuESTPackageURL];
	If[! QuESTLinkQ[], Message[QuEST::pkg, $QuESTPackageURL]; $Failed]
]

InitializeQuEST[] := (
	ImportQuEST[];
	If[	Links["*quest_link*"] === {},
		CreateDownloadedQuESTEnv[];
		If[ Links["*quest_link*"] === {},
			Message[QuEST::link]; $Failed,
			True
		],
		True
	]
)


FromQuESTLink[name_Symbol] /; Context[name] == "QuEST`Gate`" := ToUpperCase @ SymbolName[name]

FromQuESTLink[Subscript[Depol, order__][args___]] := QuantumChannel[{"Depol", args}, {order} + 1]

FromQuESTLink[Subscript[C, order__Integer][op_]] := QuantumOperator[{"C", FromQuESTLink[op], {order} + 1}]

FromQuESTLink[QuEST`Gate`R[angle_, ops_]] := QuantumOperator[{"R", angle, Splice[FromQuESTLink /@ Flatten[{Replace[ops, t_Times :> List @@ t]}]]}]

FromQuESTLink[Subscript[M, order__Integer]] := QuantumMeasurementOperator[{order} + 1]

FromQuESTLink[Subscript[name_Symbol, order__Integer]] := QuantumOperator[FromQuESTLink[name], {order} + 1]
FromQuESTLink[Subscript[name_Symbol, order__Integer][args___]] := QuantumOperator[{FromQuESTLink[name], args}, {order} + 1]

FromQuESTLink[expr_] := Missing["NotImplemented", expr]

FromQuESTLink[ops_List] := Enclose @ With[{width = Max[Cases[ops, Subscript[_, order__Integer] :> {order}, All]] + 1}, QuantumCircuitOperator[Confirm @ FromQuESTLink[#] & /@ ops]]


ToQuESTLink[args___] /; ! QuESTLinkQ[] := Enclose[Confirm @ ImportQuEST[]; ToQuESTLink[args]]

ToQuESTLink[qo_QuantumOperator] /; MatchQ[qo["Dimensions"], {2 ..}] := With[{order = qo["TargetOrder"] - 1},
	Enclose[Confirm @ ToQuESTLink[qo["Label"], order], Subscript[QuEST`Gate`U, Sequence @@ order][qo["Reverse"]["MatrixRepresentation"]] &]
]
ToQuESTLink[qco_QuantumCircuitOperator] := Enclose @ Flatten[Confirm @* ToQuESTLink /@ qco["Operators"]]

ToQuESTLink[label_, order_List] := Replace[label, {
	name : "H" | "S" | "T" | "X" | "Y" | "Z" | "SWAP" :> Subscript[Symbol["QuEST`Gate`" <> name], Sequence @@ order],
	name : "RX" | "RY" | "RZ" :> Subscript[Symbol["QuEST`Gate`" <> Capitalize[ToLowerCase[name]]], Sequence @@ order],
	"I" :> Subscript[QuEST`Gate`Id, Sequence @@ order],
	"NOT" :> Subscript[QuEST`Gate`X, Sequence @@ order],
	Subscript["C", subLabel_][control1_, control0_] :> Join[{Subscript[C, Sequence @@ (control1 - 1)][ToQuESTLink[subLabel, order]]}, Subscript[X, # - 1] & /@ control0],
	Subscript["R", subLabel_CircleTimes][angle_] /; Length[subLabel] == Length[order] :> QuEST`Gate`R[angle, Times @@ MapThread[ToQuESTLink[#1, {#2}] &, {List @@ subLabel, order}]],
	Subscript["R", subLabel_][angle_] :> QuEST`Gate`R[angle, ToQuESTLink[subLabel, order]],
	"P"[phase_] :> Subscript[QuEST`Gate`Ph, Sequence @@ order][phase],
	"PhaseShift"[shift_] :> Subscript[QuEST`Gate`Ph, Sequence @@ order][2 Pi / 2 ^ shift],
	HoldForm[phase_] :> Subscript[QuEST`Gate`G, Sequence @@ order][phase],
	_ :> Missing["NotImplemented", label]
 }
]

ToQuESTLink[arg_] :=  Failure["Unknown", <|"MessageTemplate" -> "Unknown QuEST operator: ``", "MessageParameters" -> arg|>]


QuESTCompile[qco_QuantumCircuitOperator] /; InitializeQuEST[] :=
	Enclose @ QuantumOperator[QuEST`CalcCircuitMatrix[Confirm @ ToQuESTLink[qco]]]["Reverse"]

QuESTApply[qco_QuantumCircuitOperator, qs_QuantumState] /; InitializeQuEST[] := Enclose[
	Block[{s = QuEST`CreateQureg[qs["OutputQudits"]], c = Confirm @ ToQuESTLink[qco]},
		QuEST`InitStateFromAmps[s, Sequence @@ Transpose[ReIm @ qs["Reverse"]["StateVector"]]];
		QuEST`ApplyCircuit[s, c];
		QuantumState[Flatten[QuEST`GetQuregMatrix[s]]]["Reverse"]
	]
]

