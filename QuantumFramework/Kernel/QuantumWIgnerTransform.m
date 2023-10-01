Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumWignerTransform"]
PackageExport["QuantumWeylTransform"]

PackageScope[WignerBasis]



Options[WignerBasis] = {"Exact" -> False, "EvenDimensionMethod" -> 2}

WignerBasis[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := wignerBasis[qb, opts] = Block[{d = qb["Dimension"], a, x, z},
    If[d == 1, Return[qb]];
    a = DiagonalMatrix[Exp[I 2 Pi Range[0, d - 1] / d]];
    z = qb["Matrix"] . a . PseudoInverse[qb["Matrix"]];
    If[ ! TrueQ[OptionValue["Exact"]], z = N[z]];
    x = FourierMatrix[d] . z . ConjugateTranspose[FourierMatrix[d]];
    QuditBasis @
        AssociationThread[
            Subscript["W", Row[#]] & /@ Tuples[Range[0, d - 1], 2],
            Chop @ FullSimplify @ Catenate @ If[ OddQ[d],
                Table[fanoMatrix[d, q + 1, p + 1, x, z], {p, 0, 2 d - 1, 2}, {q, 0, 2 d - 1, 2}],
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
    QuantumBasis[QuantumWignerTransform[qb["Output"], opts], QuantumWignerTransform[qb["Input"], opts], "PhaseSpace"]


QuantumWignerTransform[qs_ ? QuantumStateQ, opts : OptionsPattern[]] /; qs["Picture"] =!= "PhaseSpace" :=
    Chop @ QuantumState[qs["Double"], QuantumWignerTransform[qs["Basis"], opts, "Exact" -> ! qs["NumericQ"]]]


QuantumWignerTransform[qo_ ? QuantumOperatorQ, opts : OptionsPattern[]] /; qo["Picture"] =!= "PhaseSpace" :=
	QuantumOperator[
		QuantumWignerTransform[qo["State"], opts],
		qo["Order"]
	]


QuantumWignerTransform[qmo_ ? QuantumMeasurementOperatorQ, opts : OptionsPattern[]] /; qmo["Picture"] =!= "PhaseSpace" :=
    QuantumMeasurementOperator[QuantumWignerTransform[qmo["Operator"], opts], qmo["Target"]]


QuantumWeylTransform[qs_ ? QuantumStateQ] /;
	qs["Picture"] === "PhaseSpace" && IntegerQ[Sqrt[qs["OutputDimension"]]] && IntegerQ[Sqrt[qs["InputDimension"]]] :=
	QuantumState[qs, QuantumBasis[{1, 1} Sqrt[qs["OutputDimension"]], {1, 1} Sqrt[qs["InputDimension"]]]]["Unbend"]

