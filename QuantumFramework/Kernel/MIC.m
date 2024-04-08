Package["Wolfram`QuantumFramework`"]

PackageExport[QuantumWignerMICTransform]

PackageScope[QuantumWignerMICPOVM]
PackageScope[QuantumWignerMICBasis]
PackageScope[WoottersBasis]
PackageScope[GellMannMICPOVM]
PackageScope[RandomHaarPOVM]
PackageScope[RandomBlochMICPOVM]
PackageScope[QBismSICPOVM]
PackageScope[HesseSICPOVM]
PackageScope[HoggarSICPOVM]



a[d_] := If[OddQ[d / 2], 2, 4 (1 / 2 - 1 / d)]
b[d_] := If[EvenQ[d / 2], 2, 4 (1 / 2 - 1 / d)]
a2[d_] := If[OddQ[d / 2], 2, 2 - 1 / (d / 2) ^ 2]
b2[d_] := If[EvenQ[d / 2], 2, 2 - 1 / (d / 2) ^ 2]


Options[QuantumWignerMICPOVM] := Options[WignerBasis]


QuantumWignerMICPOVM[d : _Integer ? Positive : 2, opts : OptionsPattern[]] := Block[{
    W = QuditBasis["Wigner"[d, FilterRules[{opts}, Options[WignerBasis]], "Exact" -> True]]["Elements"],
    ev, pos, G, K
},
    ev = Eigenvalues /@ W;
    pos = Position[ev, ev_ /; OddQ[Count[ev, _ ? Negative]], {1}, Heads -> False];
    K = Simplify @ MapIndexed[
        If[ OddQ[d],
            1 / (d + 1) (#1 + IdentityMatrix[d]),
            1 / (2 d) (#1 + If[MemberQ[pos, #2], a[d], b[d]] IdentityMatrix[d])
        ] &,
        W
    ];

    If[ OddQ[d],
        1 / d K,

        (* dual *)
        K = Simplify @ GramDual[K];
        K = MapIndexed[1 / (2 d ^ 2) (#1 + If[MemberQ[pos, #2], a2[d], b2[d]] IdentityMatrix[d]) &, K],
        Permute[
            Permute[K, Catenate[d / 2 Range[0, 2 d - 1] + # & /@ Range[d / 2]]],
            FindPermutation @ Join[Catenate[d Range[0, d / 2 - 1] + # & /@ Range[d / 2]], Catenate[d Range[0, d / 2 - 1] + # + d / 2 & /@ Range[d / 2]]]
        ]
   ]
]


Options[QuantumWignerMICBasis] := Options[QuantumWignerMICPOVM]

QuantumWignerMICBasis[d : _Integer ? Positive : 2, opts : OptionsPattern[]] := Block[{povm = QuantumWignerMICPOVM[d, opts], dual},
    dual = GramDual[povm];
    Simplify @ QuditBasis @ AssociationThread[
        If[ OddQ[d],
            Table[Subscript["\[ScriptCapitalM]", i], {i, d ^ 2}],
            If[ d == 2,
                Table[Superscript["\[ScriptCapitalM]", i], {i, {"I", "X", "Z", "-Y"}}],
                Join[
                    Table[Subsuperscript["\[ScriptCapitalM]", i, 1], {i, d}],
                    Table[Subsuperscript["\[ScriptCapitalM]", i, 2], {i, d (d - 1)}]
                ]
            ]
        ],
        Simplify @ dual
    ]
]

QuantumWignerMICBasis[basisArgs_, opts : OptionsPattern[]] := QuantumTensorProduct[QuantumWignerMICBasis[#, opts] & /@ QuantumBasis[basisArgs]["Dimensions"]]


QuantumWignerMICTransform[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := Simplify @ QuantumTensorProduct[QuantumWignerMICBasis[#, opts] & /@ qb["Dimensions"]]

QuantumWignerMICTransform[qb_ ? QuantumBasisQ, opts : OptionsPattern[]] :=
    Enclose @ QuantumBasis[
        ConfirmBy[QuantumWignerMICTransform[qb["Output"], opts], QuditBasisQ],
        ConfirmBy[QuantumWignerMICTransform[qb["Input"], opts], QuditBasisQ],
        "Picture" -> "PhaseSpace", qb["Meta"]
    ]


QuantumWignerMICTransform[qs_ ? QuantumStateQ, opts : OptionsPattern[]] := Enclose @ Chop @ Simplify @ QuantumState[
    qs["Double"],
    ConfirmBy[QuantumWignerMICTransform[qs["Basis"], opts, "Exact" -> ! qs["NumberQ"]], QuantumBasisQ]
]

QuantumWignerMICTransform[qo_ ? QuantumOperatorQ, opts : OptionsPattern[]] := Enclose @ QuantumOperator[
    ConfirmBy[QuantumWignerMICTransform[qo["State"], opts], QuantumStateQ],
    qo["Order"]
]

QuantumWignerMICTransform[qc_ ? QuantumChannelQ, opts: OptionsPattern[]] := Enclose @ QuantumChannel[ConfirmBy[QuantumWignerMICTransform[qc["QuantumOperator"], opts], QuantumOperatorQ]]

QuantumWignerMICTransform[qmo_ ? QuantumMeasurementOperatorQ, opts: OptionsPattern[]] := QuantumMeasurementOperator[qmo["Double"], QuantumWignerMICTransform[qmo["POVM"]["Basis"], opts]]

QuantumWignerMICTransform[qco_ ? QuantumCircuitOperatorQ, opts: OptionsPattern[]] :=
    Enclose @ QuantumCircuitOperator[If[BarrierQ[#], #, Confirm @ QuantumWignerMICTransform[#, opts]] & /@ qco["Elements"], qco["Label"]]


(* POVMs *)

WoottersBasis[d_] := With[{w = Exp[2 Pi I / d]}, 
    Catenate @ Table[
        Sum[w ^ ((d - 1) k l / 2 + q  l - p  k) MatrixPower[pauliMatrix[1, d], k] . MatrixPower[pauliMatrix[3, d], l], {k, 0, d - 1}, {l, 0, d - 1}] / d,
        {p, 0, d - 1}, {q, 0, d - 1}
    ]
]

GellMannMICPOVM[d_, s_ : 0] := With[{
    sigma = GellMannMatrices[d],
    simplex = RegularSimplex[d ^ 2 - 1]
},
    identityMatrix[d] / d ^ 2 + 1 / d Sqrt[(d - 1) ^ (1 + s) / 2 / d] # . sigma & /@ simplex // Normal // Simplify
]

RandomGinibre[shape_List, realQ : True | False] := If[realQ, RandomReal[1 / Sqrt[2], shape], RandomComplex[{0, (1 + I) / Sqrt[2]}, shape]]

RandomHaarPOVM[d : _Integer ? Positive, k : _Integer ? Positive | Automatic : Automatic, n : _Integer ? Positive : 1, OptionsPattern[{"Real" -> False}]] := Block[{
    realQ = TrueQ[OptionValue["Real"]], povm, S
},
	povm = # . ConjugateTranspose[#] & /@ RandomGinibre[{Replace[k, Automatic :> If[realQ, d (d + 1) / 2, d ^ 2]], d, n}, realQ];
	S = MatrixPower[Total[povm], - 1 / 2];
	S . # . S & /@ povm
]

RandomBlochMICPOVM[d_] := With[{
    sigma = GellMannMatrices[d],
    simplex = AffineTransform[RandomReal[1, {d ^ 2 - 1, d ^ 2 - 1}]] @ RegularSimplex[d ^ 2 - 1]
},
    identityMatrix[d] / d ^ 2 + 1 / d Sqrt[(d - 1) / 2 / d] # . sigma & /@ simplex // Normal
]

DisplacementOperator[d_, a_, b_] := 
    (- Exp[I Pi/ d]) ^ (a b) *
        MatrixPower[pauliMatrix[1, d], b] . MatrixPower[pauliMatrix[3, d], a]



WeylHeisenbergGroup[fiducial_ ? VectorQ] := With[{d = Length[fiducial]},
    1 / d KroneckerProduct[#, Conjugate[#]] & /@ Catenate @ Table[
        DisplacementOperator[d, a, b] . fiducial,
        {b, 0, d - 1}, {a, 0, d - 1}
    ]
]

FactoredWeylHeisenbergGroup[fiducial_ ? VectorQ] := Block[{d = Length[fiducial], ds},
    ds = Catenate[Table @@@ FactorInteger[d]];
    1 / d KroneckerProduct[#, Conjugate[#]] & /@
        (KroneckerProduct[##] . fiducial & @@@ Tuples[Map[Catenate @ Table[DisplacementOperator[#, a, b], {b, 0, # - 1}, {a, 0, # - 1}] &, ds]])
]

QBismSICPOVM[d : _Integer : 2] := Enclose @ With[{
	fiducial = Normalize[ConfirmBy[
		DeleteCases[{}] @ Confirm @ Import[
			StringTemplate["https://raw.githubusercontent.com/heyredhat/qbism/master/qbism/sic_povms/d``.txt"][d],
			"Table"
		],
		MatrixQ[#, NumericQ] &
	] . {1, I}]
},
	WeylHeisenbergGroup[fiducial]
]

HesseSICPOVM[] := WeylHeisenbergGroup @ Normalize @ {0, 1, -1}

HoggarSICPOVM[] := FactoredWeylHeisenbergGroup @ Normalize @ {-1 + 2I, 1, 1, 1, 1, 1, 1, 1}

