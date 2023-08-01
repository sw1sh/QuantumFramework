Package["Wolfram`QuantumFramework`"]



$QuantumChannelProperties = {
    "QuantumOperator",
    "Operator",
    "TraceOrder", "TraceQudits",
    "Trace",
    "TracePreservingQ"
};


QuantumChannel["Properties"] := Union @ Join[
    $QuantumChannelProperties,
    $QuantumOperatorProperties
]

qc_QuantumChannel["ValidQ"] := QuantumChannelQ[qc]


QuantumChannel::undefprop = "QuantumChannel property `` is undefined for this channel";


(qc_QuantumChannel[prop_ ? propQ, args___]) /; QuantumChannelQ[qc] := With[{
    result = QuantumChannelProp[qc, prop, args]
},
    If[ TrueQ[$QuantumFrameworkPropCache] && QuantumChannelProp[qc, "Basis"]["ParameterArity"] == 0,
        QuantumChannelProp[qc, prop, args] = result,
        result
    ] /; !FailureQ[Unevaluated @ result] && (!MatchQ[result, _QuantumChannelProp] || Message[QuantumChannel::undefprop, prop])
]

QuantumChannelProp[qc_, "Properties"] :=
    Union @ Join[QuantumChannel["Properties"], qc["Operator"]["Properties"]]


(* getters *)

QuantumChannelProp[_[op_], "Operator" | "QuantumOperator"] := op


QuantumChannelProp[qc_, "TraceOrder"] := Select[qc["FullOutputOrder"], NonPositive]

QuantumChannelProp[qc_, "TraceQudits"] := Length @ qc["TraceOrder"]

QuantumChannelProp[qc_, "Trace"] := QuantumPartialTrace[qc["Operator"] @ qc["Operator"]["Dagger"], qc["TraceOrder"]]

QuantumChannelProp[qc_, "TracePreservingQ"] := With[{m = Chop @ qc["Trace"]["MatrixRepresentation"]},
    If[MatrixQ[m, NumericQ], m == IdentityMatrix[Length @ m], Undefined]
]

QuantumChannelProp[qc_, "EvolutionChannel"] := QuantumChannel[
    (I #)["EvolutionOperator"] & /@  qc["UnstackOutput", 1]
]

QuantumChannelProp[qc_, "NEvolutionChannel"] := QuantumChannel[
    (I #)["NEvolutionOperator"] & /@  qc["UnstackOutput", 1]
]

QuantumChannelProp[qc_, name : "Dagger" | "Conjugate" | "Dual", args___] := QuantumChannel[qc["Operator"][name, args]]

QuantumChannelProp[qc_, "Shift", n : _Integer ? NonNegative : 1] := QuantumChannel[qc["Operator"]["Reorder", qc["Order"] /. k_Integer /; k > 0 :> k + n]]

QuantumChannelProp[qc_, "Bend", autoShift : _Integer ? Positive : Automatic] := With[{shift = Replace[autoShift, Automatic :> Max[qc["Order"]]]},
    If[ qc["MatrixQ"],
        QuantumChannel @ QuantumOperator[
            qc["State"]["Bend"],
            {
                With[{neg = Select[qc["OutputOrder"], NonPositive], pos = Select[qc["OutputOrder"], Positive]},
                    Join[qc["OutputOrder"], neg - Count[qc["OutputOrder"], _ ? NonPositive], pos - Min[pos] + 1 + shift]
                ],
                Join[qc["InputOrder"], qc["InputOrder"] - Min[qc["InputOrder"]] + 1 + shift]
            }
        ],
        QuantumTensorProduct[qc, qc["Shift", shift]]
    ]
]


QuantumChannelProp[qc_, "CircuitDiagram", opts___] :=
    QuantumCircuitOperator[qc]["Diagram", opts]


(* operator properties *)

QuantumChannelProp[qc_, args : PatternSequence[prop_String, ___]] /;
    MemberQ[Intersection[qc["Operator"]["Properties"], qc["Properties"]], prop] := qc["Operator"][args]

