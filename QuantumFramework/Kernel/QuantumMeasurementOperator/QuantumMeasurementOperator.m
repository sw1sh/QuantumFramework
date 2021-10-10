Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[qo_, _ ? orderQ]] := QuantumOperatorQ[qo]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ] := QuantumMeasurementOperator[qo, qo["Order"]]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args : PatternSequence[___, Except[_ ? orderQ]]] :=
    QuantumMeasurementOperator[QuantumOperator[qo, args, qo["FullOrder"]], qo["Order"]]

QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, args__, target_ ? orderQ] :=
    QuantumMeasurementOperator[QuantumOperator[qo, args], target]


(* mutation *)

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args : PatternSequence[___, Except[_ ? orderQ]]] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args], qmo["Target"]]

QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args___, target_ ? orderQ] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args], target]


QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumOperatorQ], ___]] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumOperator[args], QuantumOperatorQ]]


QuantumMeasurementOperator[qo_ ? QuantumOperatorQ, target_ ? orderQ] /; ! ContainsAll[qo["FullOrder"], target] :=
    QuantumMeasurementOperator[qo, qo["FullOrder"]]


(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] := Enclose @ With[{
    qudits = If[qmo["POVMQ"], qmo["Targets"], 1]
},
    ConfirmAssert[qs["OutputQudits"] >= qmo["Targets"], "Not enough output qudits"];

    QuantumMeasurement[
        QuantumState[
            ConfirmBy[qmo["SuperOperator"][{"Ordered", 1, qs["OutputQudits"], qs["Output"]}][qs], QuantumStateQ][
                {"Split", qudits}
            ],
            "Label" -> qmo["Label"][qs["Label"]]
        ],
        qmo["Target"]
    ]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qo_ ? QuantumOperatorQ] :=
    QuantumMeasurementOperator[qmo["SuperOperator"] @ qo, qmo["Target"]]


(qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qmo2_ ? QuantumMeasurementOperatorQ] := Enclose @ Module[{
    top, bottom, orderedTop, orderedBottom, targetOrder
},
    (*ConfirmAssert[! IntersectingQ[qmo1["Target"], qmo2["Target"]], "Measurements targets should not intersect"];*)

    top = qmo1["SuperOperator"];
    bottom = qmo2["SuperOperator"];

    orderedBottom = bottom[{"Ordered",
        DeleteDuplicates @ Join[bottom["InputOrder"], top["InputOrder"]],
        QuantumTensorProduct[
            bottom["Input"],
            top["Input"][{"Extract", DeleteCases[top["InputOrder"], Alternatives @@ bottom["InputOrder"]] /. top["OrderQuditMapping"]}]["Dual"]
        ]
    }];

    orderedTop = top[{"Ordered",
        DeleteDuplicates @ Join[top["InputOrder"], bottom["InputOrder"]],
        QuantumTensorProduct[
            top["Input"],
            bottom["Input"][{"Extract", DeleteCases[bottom["InputOrder"], Alternatives @@ top["InputOrder"]] /. bottom["OrderQuditMapping"]}]["Dual"]
        ]
    }];

    (* put identities on the left of first operator for each measurement qudit of the second operator *)
    targetOrder = orderedBottom["OutputOrder"][[ ;; qmo2["Targets"] ]];
    orderedTop = QuantumTensorProduct[
        QuantumOperator[{"Identity", bottom["Output"][{"Take", qmo2["Targets"]}]}, targetOrder],
        orderedTop
    ];

    QuantumMeasurementOperator[
        orderedTop @ orderedBottom //
            (* permute two sets of measured qudits based on given operator targets, left-most first *)
            (#[{"PermuteOutput", InversePermutation @ FindPermutation[DeleteDuplicates @ Join[qmo2["Target"], qmo1["Target"]]]}] &)
            ,
        "Label" -> qmo1["Label"] @* qmo2["Label"],
        Union[qmo1["Target"], qmo2["Target"]]
    ]
]


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qm_QuantumMeasurement] := Enclose @ With[{
    state =
        (* prepending identity to propogate measurement eigenvalues *)
        ConfirmBy[
            QuantumTensorProduct[
                QuantumOperator[{"Identity", First @ qm["Output"][{"Split", qm["Targets"]}]}, Range[qmo["FirstQudit"] - qm["Targets"], qmo["FirstQudit"] - 1]],
                qmo["SuperOperator"][{"Ordered", 1, qm["InputQudits"], qm["Input"]["Dual"]}]
            ],
            QuantumOperatorQ
        ][
            qm["State"][{"Split", qm["Qudits"]}]
        ],
    target = Union[qm["Target"], qmo["Target"]]
},
    QuantumMeasurement[
        QuantumState[
            state[{"PermuteOutput", InversePermutation @ FindPermutation @ Join[qm["Target"], qmo["Target"]]}][{"Split", Length @ target}],
            "Label" -> qmo["Label"] @ qm["Label"]
        ],
        target
    ]
]


(* equality *)

QuantumMeasurementOperator /: (qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) ==
    (qmo2_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) := qmo1["MatrixRepresentation"] == qmo2["MatrixRepresentation"]

