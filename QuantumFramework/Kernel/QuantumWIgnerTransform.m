Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumWignerTransform"]
PackageExport["QuantumWeylTransform"]

PackageScope[WignerBasis]



Options[WignerBasis] = {"Exact" -> True, "Decompose" -> True}

WignerBasis[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := Block[{d, a, x, z},
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
            Chop @ FullSimplify @ Catenate[If[ OddQ[d],
                Table[fanoMatrix[d, q, - p, x, z], {p, 0, 2 d - 1, 2}, {q, 0, 2 d - 1, 2}],
                Table[2 fanoMatrix[d, p, q, x, z], {p, 0, d - 1}, {q, 0, - d + 1, - 1}]
            ]]
        ]
]


Options[QuantumWignerTransform] = Options[WignerBasis]

QuantumWignerTransform[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := Chop @ QuditBasis[{"Wigner", qb, opts}]

QuantumWignerTransform[qb_ ? QuantumBasisQ, opts : OptionsPattern[]] :=
    Enclose @ QuantumBasis[
        ConfirmBy[QuantumWignerTransform[qb["Output"], opts], QuditBasisQ],
        ConfirmBy[QuantumWignerTransform[qb["Input"], opts], QuditBasisQ],
        "Picture" -> "PhaseSpace", qb["Meta"]
    ]


QuantumWignerTransform[qs_ ? QuantumStateQ, opts : OptionsPattern[]] :=
    Enclose @ Chop @ Simplify @ QuantumState[
        ConfirmBy[qs["Double"], QuantumStateQ],
        ConfirmBy[QuantumWignerTransform[qs["Basis"], opts, "Exact" -> ! qs["NumberQ"]], QuantumBasisQ]
    ]


QuantumWignerTransform[qo_ ? QuantumOperatorQ, opts : OptionsPattern[]] :=
	Enclose @ QuantumOperator[
		ConfirmBy[QuantumWignerTransform[qo["State"], opts], QuantumStateQ],
		qo["Order"]
	]

QuantumWignerTransform[qc_ ? QuantumChannelQ, opts : OptionsPattern[]] :=
    Enclose @ QuantumChannel[ConfirmBy[QuantumWignerTransform[qc["QuantumOperator"], opts], QuantumOperatorQ]]

QuantumWignerTransform[qmo_ ? QuantumMeasurementOperatorQ, opts : OptionsPattern[]] /; qmo["Picture"] =!= "PhaseSpace" :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumWignerTransform[qmo["Operator"], opts], QuantumOperatorQ], qmo["Target"]]

QuantumWignerTransform[qco_ ? QuantumCircuitOperatorQ, opts : OptionsPattern[]] :=
    Enclose @ QuantumCircuitOperator[If[BarrierQ[#], #, Confirm @ QuantumWignerTransform[#, opts]] & /@ qco["Elements"], qco["Label"]]



QuantumWeylTransform[qb_ ? QuditBasisQ, _ : False] := Enclose @ QuditBasis[ConfirmBy[Sqrt[qb["Dimensions"]], AllTrue[IntegerQ]]]

QuantumWeylTransform[qb_ ? QuditBasisQ, True] := Enclose @ QuditBasis[Catenate[{#, #} & /@ ConfirmBy[Sqrt[qb["Dimensions"]], AllTrue[IntegerQ]]]]

QuantumWeylTransform[qb_ ? QuantumBasisQ, double_ : False] :=
    Enclose @ QuantumBasis[
        ConfirmBy[QuantumWeylTransform[qb["Output"], double], QuditBasisQ],
        ConfirmBy[QuantumWeylTransform[qb["Input"], double], QuditBasisQ],
        "Picture" -> "Schrodinger", qb["Meta"]
    ]

QuantumWeylTransform[qs_ ? QuantumStateQ] := QuantumState[qs, QuantumWeylTransform[qs["Basis"], True]]["Undouble"]

QuantumWeylTransform[qo_ ? QuantumOperatorQ] :=
    Enclose @ QuantumOperator[
        ConfirmBy[QuantumWeylTransform[qo["State"]], QuantumStateQ],
        qo["Order"]
    ]

QuantumWeylTransform[qc_ ? QuantumChannelQ] := Enclose @ QuantumChannel[ConfirmBy[QuantumWeylTransform[qc["QuantumOperator"]], QuantumOperatorQ]]

QuantumWeylTransform[qmo_ ? QuantumMeasurementOperatorQ] := Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumWeylTransform[qmo["QuantumOperator"]], QuantumOperatorQ], qmo["Target"]]

QuantumWeylTransform[qco_ ? QuantumCircuitOperatorQ] :=
    Enclose @ QuantumCircuitOperator[If[BarrierQ[#], #, Confirm @ QuantumWeylTransform[#]] & /@ qco["Elements"], qco["Label"]]

