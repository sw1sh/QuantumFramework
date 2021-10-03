Package["QuantumFramework`"]

PackageScope["QuantumCircuitOperatorProp"]



$QuantumCircuitOperatorProperties = {
     "Operators", "Diagram", "Gates", "Orders", "CircuitOperator"
};

$QuantumCircuitOperatorProperties = DeleteDuplicates @ Join[$QuantumCircuitOperatorProperties, $QuantumMeasurementOperatorProperties];

QuantumCircuitOperator["Properties"] := $QuantumCircuitOperatorProperties

QuantumCircuitOperatorProp[qco_, "Properties"] := DeleteDuplicates @ Join[QuantumCircuitOperator["Properties"], Last[qco["Operators"]]["Properties"]]


qds_QuantumCircuitOperator["ValidQ"] := QuantumCircuitOperatorQ[qds]


QuantumCircuitOperator::undefprop = "property `` is undefined for this circuit";

(qds_QuantumCircuitOperator[prop_ ? propQ, args___]) /; QuantumCircuitOperatorQ[qds] := With[{
    result = Check[QuantumCircuitOperatorProp[qds, prop, args], $Failed]
},
    (QuantumCircuitOperatorProp[qds, prop, args] = result) /;
        !FailureQ[Unevaluated @ result] && (!MatchQ[Unevaluated @ result, _QuantumCircuitOperatorProp] || Message[QuantumCircuitOperator::undefprop, prop])
]

QuantumCircuitOperatorProp[QuantumCircuitOperator[operators_], "Operators"] := operators

QuantumCircuitOperatorProp[qco_, "Diagram"] := Show[drawGateGraphics[qco["Operators"]]]

QuantumCircuitOperatorProp[qco_, "CircuitOperator"] := Fold[ReverseApplied[Construct], qco["Operators"]]

QuantumCircuitOperatorProp[qco_, "Gates"] := Length @ qco["Operators"]

QuantumCircuitOperatorProp[qco_, "Orders" | "OperatorOrders"] := #["Order"] & /@ qco["Operators"]


(* operator properties *)

QuantumCircuitOperatorProp[qco_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qco["CircuitOperator"]["Properties"], qco["Properties"]], prop] := qco["CircuitOperator"][args]

