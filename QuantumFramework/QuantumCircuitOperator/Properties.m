Package["QuantumFramework`"]

PackageScope["QuantumCircuitOperatorProp"]



$QuantumCircuitOperatorProperties = {
     "Operators", "Diagram", "Gates", "CircuitOperator"
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

QuantumCircuitOperatorProp[qco_, "Orders"] := #["Order"] & /@ qco["Operators"]

QuantumCircuitOperatorProp[qco_, "Arity"] := Length[Union @@ qco["Orders"]]

QuantumCircuitOperatorProp[qco_, "InputDimension"] := Max[#["InputDimension"] & /@ qco["Operators"]]

QuantumCircuitOperatorProp[qco_, "Qudits"] := Max[Flatten[qco["Orders"]]]

QuantumCircuitOperatorProp[qco_, "OrderedMatrix" | "OrderedMatrixRepresentation"] := qco[{"OrderedMatrix", qco["Qudits"]}]

QuantumCircuitOperatorProp[qco_, {"OrderedMatrix" | "OrderedMatrixRepresentation", quditCount_}] := qco["CircuitOperator"][{"OrderedMatrix", quditCount}]

(*QuantumCircuitOperatorProp[qco_, {"OrderedMatrix" | "OrderedMatrixRepresentation", quditCount_}] := Module[{
    dimensionCount = qco["InputDimension"],
    orders = qco["Orders"],
    orderedMatrixRepresentations = #[{"OrderedMatrixRepresentation", quditCount}] & /@ qco["Operators"],
    minimumOrders, maximumOrders
},
    minimumOrders = Min /@ orders;
    maximumOrders = Max /@ orders;
    Do[
        If[ maximumOrders[[i]] < quditCount,
            orderedMatrixRepresentations[[i]] = KroneckerProduct[
                orderedMatrixRepresentations[[i]],
                IdentityMatrix[(quditCount - maximumOrders[[i]]) dimensionCount]
            ];
            If[ minimumOrders[[i]] > 1,
                orderedMatrixRepresentations[[i]] = KroneckerProduct[
                    IdentityMatrix[dimensionCount (minimumOrders[[i]] - 1)],
                    orderedMatrixRepresentations[[i]]
                ]
            ],
            If[ Length[orders[[i]]] == 1 && quditCount > 1,
                orderedMatrixRepresentations[[i]] = KroneckerProduct[
                    IdentityMatrix[dimensionCount (quditCount - 1)],
                    orderedMatrixRepresentations[[i]]
                ]
            ]
        ],
        {i, Length[orders]}
    ];
    Dot @@ orderedMatrixRepresentations
]*)

QuantumCircuitOperatorProp[qco_, "HermitianQ"] := HermitianMatrixQ[qco["OrderedMatrixRepresentation"]]

QuantumCircuitOperatorProp[qco_, "UnitaryQ"] := UnitaryMatrixQ[qco["OrderedMatrixRepresentation"]]


(* operator properties *)

QuantumCircuitOperatorProp[qco_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qco["CircuitOperator"]["Properties"], qco["Properties"]], prop] := qco["CircuitOperator"][args]

