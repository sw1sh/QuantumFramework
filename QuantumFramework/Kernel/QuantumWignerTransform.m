Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumWignerTransform"]
PackageExport["QuantumWeylTransform"]

PackageScope[WignerBasis]



Options[WignerBasis] = {"Exact" -> True, "Decompose" -> True, "Basis" -> "Pauli"}

WignerBasis[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := Block[{d, x, z},
    If[TrueQ[OptionValue["Decompose"]], Return[QuantumTensorProduct[WignerBasis[#, "Decompose" -> False, opts] & /@ qb["Decompose"]]]];
    d = qb["Dimension"];
    If[d == 1, Return[qb]];
    z = Switch[OptionValue["Basis"],
        "Pauli",
            ConjugateTranspose[pauliMatrix[3, d]],
        _,
            qb["Matrix"] . DiagonalMatrix[Exp[2 Pi I Range[0, d - 1] / d]] . PseudoInverse[qb["Matrix"]]
    ];
    If[ ! TrueQ[OptionValue["Exact"]], z = N[z]];
    x = FourierMatrix[d] . z . ConjugateTranspose[FourierMatrix[d]];
    QuditBasis @
        AssociationThread[
            If[ OddQ[d],
                Subscript["\[ScriptCapitalW]", Row[#]] & /@ Tuples[Range[0, d - 1], 2],
                Subsuperscript["\[ScriptCapitalW]", Row[{#1, #3}], Replace[{#2, #4}, {{0, 0} -> 1, {0, 1} -> 2, {1, 0} -> 4, {1, 1} -> 3}]] & @@@ Tuples[{Range[0, d/2 - 1], {0, 1}, Range[0, d/2 - 1], {0, 1}}]
            ],
            Chop @ FullSimplify @ Catenate[If[ OddQ[d],
                Table[fanoMatrix[d, 2 q, 2 p, x, z], {q, 0, d - 1}, {p, 0, d - 1}],
                Table[2 fanoMatrix[d, q, p, x, z], {q, 0, d - 1}, {p, 0, d - 1}]
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

QuantumWignerTransform[qmo_ ? QuantumMeasurementOperatorQ, opts: OptionsPattern[]] := QuantumMeasurementOperator[qmo["Double"], QuantumWignerTransform[qmo["Basis"], opts]]

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

