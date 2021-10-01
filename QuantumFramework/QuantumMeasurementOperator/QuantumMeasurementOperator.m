Package["QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[op_]] := QuantumOperatorQ[op]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)


QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args]]


QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumOperatorQ], ___]] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumOperator[args], QuantumOperatorQ]]


(* mutation *)

QuantumMeasurementOperator[op_ ? QuantumMeasurementOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumOperator[op["QuantumOperator"], args]]

QuantumMeasurementOperator[op_ ? QuantumOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumOperator[op, args]]


(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] := Enclose @ Module[{ordered},
    ConfirmAssert[qs["OutputQudits"] >= qmo["Arity"], "Not enough output qudits"];

    ordered = qmo[{"Ordered", 1, qs["OutputQudits"]}];
    ConfirmAssert[ordered["InputDimensions"][[ordered["Order"]]] == qs["OutputDimensions"][[ordered["Order"]]],
        "Operator and state dimensions don't match"
    ];
    QuantumMeasurement @ ordered["SuperOperator"][{"Ordered", 1, qs["OutputQudits"]}][qs]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[op_ ? QuantumOperatorQ] := With[{
    newOp = qmo["SuperOperator"](*[{"Ordered", Min[op["InputOrder"], qmo["InputOrder"]], Max[op["InputOrder"], qmo["InputOrder"]]}]*)
},
    QuantumMeasurementOperator[newOp @ op, qmo["Order"]]
]

(*(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[op_ ? QuantumMeasurementOperatorQ] /; qmo["Type"] === op["Type"] === "Projection" :=
    QuantumTensorProduct[qmo, op]*)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[op_ ? QuantumMeasurementOperatorQ] := Module[{
    order, ordered1, ordered2
},
    (* ordering will insert identities to fit operator into specified range *)
    order = {"Ordered", Min[op["InputOrder"], qmo["InputOrder"]], Max[op["InputOrder"], qmo["InputOrder"]]};
    ordered1 = qmo["SuperOperator"][order];
    ordered2 = op["SuperOperator"][order];

    ordered1 = QuantumOperator[
        QuantumTensorProduct[
            (* put identities on the left of first operator for each unmatched output qudit of the second operator *)
            QuantumOperator[
                QuantumOperator[{"Identity", Take[ordered2["OutputDimensions"], Max[ordered2["OutputQudits"] - ordered1["InputQudits"], 0]]}],
                With[{qb = QuantumPartialTrace[
                        ordered2["Output"],
                        Complement[Range[ordered2["OutputQudits"]], Range[Max[ordered2["OutputQudits"] - ordered1["InputQudits"], 0]]
                    ]
                ]},
                    QuantumBasis[qb, qb]
                ]
            ],
            ordered1
        ],
        ordered2["OutputOrder"]
    ];

    QuantumMeasurementOperator[
        ordered1 @ ordered2 //
            (* permute two measured qudits based on given operator orders, left-most first *)
            (#[{"PermuteOutput", InversePermutation @ FindPermutation[Ordering @ {First[op["Order"]], First[qmo["Order"]]}]}] &) //
            (* uncurry two measured qudits into one *)
            (QuantumTensorProduct[
                QuantumOperator[
                    QuantumOperator[{"Uncurry", #["OutputDimensions"][[;; 2]]}, #["OutputOrder"][[;; 2]]],
                    With[{qb = QuantumPartialTrace[#["Output"], Drop[Range[#["OutputQudits"]], 2]]},
                        QuantumBasis[qb["Uncurry"], qb]
                    ]
                ],
                QuantumOperator[{"Identity", #["OutputDimensions"][[3 ;;]]}, #["OutputOrder"][[3 ;;]] ]
            ] @ # &),
        Union[qmo["Order"], op["Order"]]
    ]
]


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qm_QuantumMeasurement] := qmo[qm["State"]]


(* equality *)

QuantumMeasurementOperator /: (qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) ==
    (qmo2_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) := qmo1["MatrixRepresentation"] == qmo2["MatrixRepresentation"]

