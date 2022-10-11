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
    If[TrueQ[$QuantumFrameworkPropCache], QuantumChannelProp[qc, prop, args] = result, result]
        /; !FailureQ[Unevaluated @ result] && (!MatchQ[result, _QuantumChannelProp] || Message[QuantumChannel::undefprop, prop])
]

QuantumChannelProp[qc_, "Properties"] :=
    Union @ Join[QuantumChannel["Properties"], qc["Operator"]["Properties"]]


(* getters *)

QuantumChannelProp[_[op_], "Operator" | "QuantumOperator"] := op


QuantumChannelProp[qc_, "TraceOrder"] := Take[qc["FullOutputOrder"], qc["OutputQudits"] - qc["InputQudits"]]

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

QuantumChannelProp[qc_, name : "Dagger"] := QuantumChannel[qc["Operator"][name]]


QuantumChannelProp[qc_, "CircuitDiagram", opts___] :=
    QuantumCircuitOperator[qc]["Diagram", opts]


(* operator properties *)

QuantumChannelProp[qc_, args : PatternSequence[prop_String, ___]] /;
    MemberQ[Intersection[qc["Operator"]["Properties"], qc["Properties"]], prop] := qc["Operator"][args]

