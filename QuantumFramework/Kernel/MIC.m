Package["Wolfram`QuantumFramework`"]

PackageExport[WignerMICTransform]

PackageScope[WignerMICPOVM]
PackageScope[WignerMICBasis]



a[d_] := If[OddQ[d / 2], 2, 4 (1 / 2 - 1 / d)]
b[d_] := If[EvenQ[d / 2], 2, 4 (1 / 2 - 1 / d)]
a2[d_] := If[OddQ[d / 2], 2, 2 - 1 / (d / 2) ^ 2]
b2[d_] := If[EvenQ[d / 2], 2, 2 - 1 / (d / 2) ^ 2]


Options[WignerMICPOVM] := Options[WignerBasis]


WignerMICPOVM[d : _Integer ? Positive : 2, opts : OptionsPattern[]] := Block[{
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
        G = Simplify @ Outer[Tr @* Dot, K, K, 1];
        K = Inverse[G] . K;
        K = MapIndexed[1 / (2 d ^ 2) (#1 + If[MemberQ[pos, #2], a2[d], b2[d]] IdentityMatrix[d]) &, K],
        Permute[
            Permute[K, Catenate[d / 2 Range[0, 2 d - 1] + # & /@ Range[d / 2]]],
            FindPermutation @ Join[Catenate[d Range[0, d / 2 - 1] + # & /@ Range[d / 2]], Catenate[d Range[0, d / 2 - 1] + # + d / 2 & /@ Range[d / 2]]]
        ]
   ]
]


Options[WignerMICBasis] = Options[WignerMICPOVM]

WignerMICBasis[d : _Integer ? Positive : 2, opts : OptionsPattern[]] := Block[{povm = WignerMICPOVM[d, opts], G, dual},
    G = Simplify @ Outer[Tr @* Dot, povm, povm, 1];
    dual = Inverse[G] . povm;
    FullSimplify @ QuditBasis @ AssociationThread[
        If[ OddQ[d],
            Table[Subscript["\[ScriptCapitalM]", i], {i, d ^ 2}],
            Join[
                Table[Subsuperscript["\[ScriptCapitalM]", i, 1], {i, d}],
                Table[Subsuperscript["\[ScriptCapitalM]", i, 2], {i, d (d - 1)}]
            ]
        ],
        Simplify @ dual
    ]
]

WignerMICBasis[qb_QuantumBasis, opts : OptionsPattern[]] := FullSimplify @ QuantumBasis[
    QuantumTensorProduct[WignerMICBasis[#, opts] & /@ qb["OutputDimensions"]],
    QuantumTensorProduct[WignerMICBasis[#, opts] & /@ qb["InputDimensions"]]
]


WignerMICTransform[qb_ ? QuditBasisQ, opts : OptionsPattern[]] := FullSimplify @ QuantumTensorProduct[WignerMICBasis[#, opts] & /@ qb["Dimensions"]]

WignerMICTransform[qb_ ? QuantumBasisQ, opts : OptionsPattern[]] :=
    Enclose @ QuantumBasis[
        ConfirmBy[WignerMICTransform[qb["Output"], opts], QuditBasisQ],
        ConfirmBy[WignerMICTransform[qb["Input"], opts], QuditBasisQ],
        "Picture" -> "PhaseSpace", qb["Meta"]
    ]


WignerMICTransform[qs_ ? QuantumStateQ, opts : OptionsPattern[]] := Enclose @ Chop @ FullSimplify @ QuantumState[
    qs["Double"],
    ConfirmBy[WignerMICTransform[qs["Basis"], opts], QuantumBasisQ]
]

WignerMICTransform[qo_ ? QuantumOperatorQ, opts : OptionsPattern[]] := Enclose @ QuantumOperator[
    ConfirmBy[WignerMICTransform[qo["State"], opts], QuantumStateQ],
    qo["Order"]
]

WignerMICTransform[qc_ ? QuantumChannelQ, opts: OptionsPattern[]] := Enclose @ QuantumChannel[ConfirmBy[WignerMICTransform[qc["QuantumOperator"], opts], QuantumOperatorQ]]

WignerMICTransform[qmo_ ? QuantumMeasurementOperatorQ, opts: OptionsPattern[]] := Enclose @ QuantumOperator[
    QuantumTensorProduct[
        QuantumOperator[
            QuantumOperator["Spider"[QuantumBasis[{#}, {# ^ 2}]]],
            QuantumBasis[{#}, WignerMICBasis[#, opts], "Label" -> "M"]
        ] & /@ qmo["InputDimensions"]
    ],
    qmo["InputOrder"]
]

WignerMICTransform[qco_ ? QuantumCircuitOperatorQ, opts: OptionsPattern[]] :=
    Enclose @ QuantumCircuitOperator[If[BarrierQ[#], #, Confirm @ WignerMICTransform[#, opts]] & /@ qco["Elements"], qco["Label"]]

