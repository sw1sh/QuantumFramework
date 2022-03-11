Package["Wolfram`QuantumFramework`"]



$QuantumChannelProperties = {
    "QuantumOperator",
    "Operator"
};


QuantumChannel["Properties"] := DeleteDuplicates @ Join[
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
    DeleteDuplicates @ Join[QuantumChannel["Properties"], qc["Operator"]["Properties"]]


(* getters *)

QuantumChannelProp[_[op_], "Operator" | "QuantumOperator"] := op


(* operator properties *)

QuantumChannelProp[qc_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qc["Operator"]["Properties"], qc["Properties"]], prop] := qc["Operator"][args]

