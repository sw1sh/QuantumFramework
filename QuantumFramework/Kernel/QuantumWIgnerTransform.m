Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumWignerTransform"]
PackageExport["QuantumWeylTransform"]

PackageScope[WignerBasis]



Options[WignerBasis] = {"Exact" -> True, "EvenDimensionMethod" -> 2, "Decompose" -> False}

WignerBasis[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := wignerBasis[qb, opts] = Block[{d, a, x, z},
    If[TrueQ[OptionValue["Decompose"]], Return[QuantumTensorProduct[WignerBasis[#, "Decompose" -> False, opts] & /@ qb["Decompose"]]]];
    d = qb["Dimension"];
    If[d == 1, Return[qb]];
    a = DiagonalMatrix[Exp[I 2 Pi Range[0, d - 1] / d]];
    z = qb["Matrix"] . a . PseudoInverse[qb["Matrix"]];
    If[ ! TrueQ[OptionValue["Exact"]], z = N[z]];
    x = FourierMatrix[d] . z . ConjugateTranspose[FourierMatrix[d]];
    QuditBasis @
        AssociationThread[
            Subscript["W", Row[#]] & /@ Tuples[Range[0, d - 1], 2],
            Chop @ FullSimplify @ Catenate @ If[ OddQ[d],
                normalizeMatrix /@ Table[fanoMatrix[d, q + 1, p + 1, x, z], {p, 0, 2 d - 1, 2}, {q, 0, 2 d - 1, 2}],
                Replace[OptionValue["EvenDimensionMethod"], {
                    1 :> Table[fanoMatrix[d, q, p, x, z], {p, 0, d - 1}, {q, 0, d - 1}],
					2 :> Table[fanoMatrix[d, q, p, x, z], {p, Join[Range[0, d - 2, 2], Range[1, d - 1, 2]]}, {q, Join[Range[0, d - 2, 2], Range[1, d - 1, 2]]}],
                    3 :> Table[fanoMatrix[d, q, p, x, z], {p, Join[Range[0, d - 2, 2], Range[1, d - 1, 2]]}, {q, 0, d - 1}],
                    _ :> Table[fanoMatrix[d, q + 1, p + 1, x, z], {p, 0, 2 d - 2, 2}, {q, 0, 2 d - 2, 2}]
                }]
            ]
        ]
]


Options[QuantumWignerTransform] = Options[WignerBasis]

QuantumWignerTransform[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := Chop @ QuditBasis[{"Wigner", qb, opts}]

QuantumWignerTransform[qb_ ? QuantumBasisQ, opts : OptionsPattern[]] /; qb["Picture"] =!= "PhaseSpace" :=
    Enclose @ QuantumBasis[
        ConfirmBy[QuantumWignerTransform[qb["Output"], opts], QuditBasisQ],
        ConfirmBy[QuantumWignerTransform[qb["Input"], opts], QuditBasisQ],
        "Picture" -> "PhaseSpace", qb["Meta"]
    ]


QuantumWignerTransform[qs_ ? QuantumStateQ, opts : OptionsPattern[]] /; qs["Picture"] =!= "PhaseSpace" :=
    Enclose @ Chop @ Simplify @ QuantumState[
        ConfirmBy[qs["Double"], QuantumStateQ],
        ConfirmBy[QuantumWignerTransform[qs["Basis"], opts, "Exact" -> ! qs["NumberQ"]], QuantumBasisQ]
    ]


QuantumWignerTransform[qo_ ? QuantumOperatorQ, opts : OptionsPattern[]] /; qo["Picture"] =!= "PhaseSpace" :=
	Enclose @ QuantumOperator[
		ConfirmBy[QuantumWignerTransform[qo["State"], opts], QuantumStateQ],
		qo["Order"]
	]

QuantumWignerTransform[qc_ ? QuantumChannelQ, opts : OptionsPattern[]] /; qc["Picture"] =!= "PhaseSpace" :=
    Enclose @ QuantumChannel[ConfirmBy[QuantumWignerTransform[qc["QuantumOperator"], opts], QuantumOperatorQ]]

QuantumWignerTransform[qmo_ ? QuantumMeasurementOperatorQ, opts : OptionsPattern[]] /; qmo["Picture"] =!= "PhaseSpace" :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumWignerTransform[qmo["Operator"], opts], QuantumOperatorQ], qmo["Target"]]

QuantumWignerTransform[qco_ ? QuantumCircuitOperatorQ, opts : OptionsPattern[]] :=
    Enclose @ QuantumCircuitOperator[If[BarrierQ[#], #, Confirm @ QuantumWignerTransform[#, opts, "Decompose" -> True]] & /@ qco["Elements"], qco["Label"]]



QuantumWeylTransform[qb_ ? QuditBasisQ] := Enclose @ QuditBasis[Catenate[{#, #} & /@ ConfirmBy[Sqrt[qb["Dimensions"]], AllTrue[IntegerQ]]]]

QuantumWeylTransform[qb_ ? QuantumBasisQ] /; qb["Picture"] === "PhaseSpace" :=
    Enclose @ QuantumBasis[
        ConfirmBy[QuantumWeylTransform[qb["Output"]], QuditBasisQ],
        ConfirmBy[QuantumWeylTransform[qb["Input"]], QuditBasisQ],
        "Picture" -> "Schrodinger", qb["Meta"]
    ]

QuantumWeylTransform[qs_ ? QuantumStateQ] /; qs["Picture"] === "PhaseSpace" :=
	Enclose @ QuantumState[qs, ConfirmBy[QuantumWeylTransform[qs["Basis"]], QuantumBasisQ]]["Unbend"]

QuantumWeylTransform[qo_ ? QuantumOperatorQ] /; qo["Picture"] === "PhaseSpace" :=
    Enclose @ QuantumOperator[ConfirmBy[QuantumWeylTransform[qo["State"]], QuantumStateQ]["VectorState"], qo["Order"]]

QuantumWeylTransform[qc_ ? QuantumChannelQ] /;
    qc["Picture"] === "PhaseSpace" := Enclose @ QuantumChannel[ConfirmBy[QuantumWeylTransform[qc["QuantumOperator"]], QuantumOperatorQ]]

QuantumWeylTransform[qmo_ ? QuantumMeasurementOperatorQ] /;
	qmo["Picture"] === "PhaseSpace" := Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumWeylTransform[qmo["QuantumOperator"]], QuantumOperatorQ], qmo["Target"]]

QuantumWeylTransform[qco_ ? QuantumCircuitOperatorQ] :=
    Enclose @ QuantumCircuitOperator[If[BarrierQ[#], #, Confirm @ QuantumWeylTransform[#]] & /@ qco["Elements"], qco["Label"]]

