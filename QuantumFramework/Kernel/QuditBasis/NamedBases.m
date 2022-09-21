Package["Wolfram`QuantumFramework`"]

PackageScope["$QuditBasisNames"]


$QuditBasisNames = {
    "Computational",
    "PauliX", "PauliY", "PauliZ",
    "X", "Y", "Z",
    "Bell",
    "Fourier",
    "Identity",
    "Schwinger", "Pauli", "Dirac", "Wigner"
}

QuditBasis[1, args___] := QuditBasis[args]

QuditBasis[dimension_Integer, args___] := QuditBasis[{"Computational", dimension}, args]

QuditBasis["Computational", args___] := QuditBasis[{"Computational", 2}, args]

QuditBasis[{"Computational", dimension_Integer}, args___] :=
    QuditBasis[QuditBasis[Range[dimension] - 1, identityMatrix[dimension] /. {{}} -> {}], args]


QuditBasis["Bell", args___] := QuditBasis[
    AssociationThread[{
            Superscript["\[CapitalPhi]", "+"],
            Superscript["\[CapitalPhi]", "-"],
            Superscript["\[CapitalPsi]", "+"],
            Superscript["\[CapitalPsi]", "-"]
        },
        (1 / Sqrt[2]) {{1, 0, 0, 1}, {1, 0, 0, -1}, {0, 1, 1, 0}, {0, 1, -1, 0}}
    ],
    args
]


QuditBasis[name : "X" | "Y" | "Z", args___] := QuditBasis["Pauli" <> name, args]

QuditBasis[{name : "X" | "Y" | "Z", dim_Integer : 2}, args___] := QuditBasis[{"Pauli" <> name, dim}, args]

QuditBasis[name : "PauliX" | "PauliY" | "PauliZ", args___] := QuditBasis[{name, 2}, args]

QuditBasis[{name : "PauliX" | "PauliY" | "PauliZ", dim_Integer : 2}, args___] := With[{
    es = eigensystem[pauliMatrix[name /. {"PauliX" -> 1, "PauliY" -> 2, "PauliZ" -> 3}, dim], "Normalize" -> True]
},
    QuditBasis[
        AssociationThread[
            Subscript["\[Psi]",
                Subscript[
                    ToLowerCase @ StringDelete[name, "Pauli"],
                    If[# > 0, "+" <> ToString[#], ToString[#]] /. {"+1" -> "+", "-1" -> "-"}
                ]
            ] & /@ First[es],
            Last[es]
        ],
        args
    ]
]


QuditBasis["Fourier"] := QuditBasis[{"Fourier", 2}]

QuditBasis[{"Fourier", dimension_Integer ? Positive}, args___] := QuditBasis[
    AssociationThread[
        Subscript["F", #] & /@ Range[dimension],
        ConjugateTranspose @ Partition[
            (1 / Sqrt[dimension]) (
                Exp[(2 Pi I (#[[2]] #[[1]])) / dimension] & /@
                Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]
            ),
            dimension
        ]
    ],
    args
]


QuditBasis["Identity"] := QuditBasis[{"Identity", 2}]

QuditBasis[{"Identity", dimension_Integer ? Positive}, args___] := QuditBasis[
    AssociationThread[
        Subscript["I", #] & /@ Range[dimension ^ 2],
        Partition[#, dimension] & /@ IdentityMatrix[dimension ^ 2]
    ],
    args
]


QuditBasis["Schwinger"] := QuditBasis[{"Schwinger", 2}]

QuditBasis[{"Schwinger", dimension_Integer ? Positive}, args___] := QuditBasis[
    AssociationThread[
        Subscript["S", Row[#]] & /@ Tuples[Range[0, dimension - 1], 2],
        Flatten /@ (
            Dot[
                MatrixPower[RotateLeft[IdentityMatrix[dimension]], #[[1]]],
                MatrixPower[((Exp[I 2 Pi / dimension]) ^ #) & /@ Range[0, dimension - 1] IdentityMatrix[dimension], #[[2]]]
            ]
        ) & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]],
    args
]

QuditBasis["Pauli", args___] := QuditBasis[
    AssociationThread[{Subscript["\[Sigma]", "0"],
        Subscript["\[Sigma]", "1"], Subscript["\[Sigma]", "2"],
        Subscript["\[Sigma]", "3"]},
        {{{1, 0}, {0, 1}}, {{0, 1}, {1, 0}}, {{0, -I}, {I, 0}}, {{1, 0}, {0, -1}}}
    ],
    args
]

QuditBasis["Dirac", args___] := Module[{
    pauliBasis1, pauliBasis2, pauliBasis3, pauliBasis4,
    gamma1, gamma2, gamma3, gamma4
},
    pauliBasis1 = {{1, 0}, {0, 1}};
    pauliBasis2 = {{0, 1}, {1, 0}};
    pauliBasis3 = {{0, -I}, {I, 0}};
    pauliBasis4 = {{1, 0}, {0, -1}};
    gamma1 = KroneckerProduct[pauliBasis4, pauliBasis1];
    gamma2 = KroneckerProduct[pauliBasis3, pauliBasis2];
    gamma3 = KroneckerProduct[pauliBasis3, pauliBasis3];
    gamma4 = KroneckerProduct[pauliBasis3, pauliBasis4];
    QuditBasis[AssociationThread[
        Subscript["\[Epsilon]", #] & /@ Range[0, 15],
        {
            IdentityMatrix[4],
            KroneckerProduct[pauliBasis4, pauliBasis1],
            KroneckerProduct[pauliBasis3, pauliBasis2],
            KroneckerProduct[pauliBasis3, pauliBasis3],
            KroneckerProduct[pauliBasis3, pauliBasis4],
            gamma1 . gamma2, gamma1 . gamma3, gamma1 . gamma4, gamma2 . gamma3, gamma2 . gamma4, gamma3 . gamma4,
            -I * (gamma2 . gamma3 . gamma4), I * (gamma1 . gamma3 . gamma4), -I * (gamma1 . gamma2 . gamma4), I * (gamma1 . gamma2 . gamma3),
            gamma1 . gamma2 . gamma3 . gamma4
       }],
       args
    ]
]


QuditBasis["Wigner", args___] := QuditBasis[{"Wigner", 2}, args]

QuditBasis[{"Wigner", dimension_Integer}, args___] := QuditBasis[{"Wigner", QuditBasis[dimension]}, args]

QuditBasis[{"Wigner", qb_QuditBasis /; QuditBasisQ[qb]}, args___] := Module[{
    dimension, positionBasis, momentumBasis, kernelElement
},
        dimension = First @ qb["Dimensions"];
        positionBasis = {#} & /@ Normal[qb["Association"]];
        momentumBasis = With[{dimension = #},
            {(dimension + 1) ->
                (1 / Sqrt[Length[positionBasis]]) *
                Total[(Exp[(I 2 Pi dimension #) / Length[positionBasis]] (Values @@ positionBasis[[# + 1]])) & /@
                    Range[0, Length[positionBasis] - 1]
                ]
            }
        ] & /@ Range[0, dimension - 1];
        kernelElement = ComplexExpand @ With[{eigensystem =
              Reverse @ Sort[
                  Apply[Rule, #] & /@ ComplexExpand @ Thread @ Eigensystem @ Partition[
                    Map[
                        With[{q1 = #[[1, 1]], p1 = #[[1, 2]], q2 = #[[2, 1]], p2 = #[[2, 2]]},
                            Exp[(2 Pi I (q1 - q2) (p1 - p2)) / dimension]
                        ] &,
                        Tuples[Tuples[Range[0, dimension - 1], 2], 2],
                        {1}
                    ],
                    dimension ^ 2
                ]
            ]
        },
            Dot[
                Transpose[Normalize[#] & /@ Values[eigensystem]],
                DiagonalMatrix[Sqrt[Keys[eigensystem]]],
                Inverse[Transpose[Normalize[#] & /@ Values[eigensystem]]]
            ]
        ];

        QuditBasis[AssociationThread[
            Subscript["W", Row[#]] & /@ Tuples[Range[0, dimension - 1], 2],

            Flatten[
                Table[
                    With[{labels = Thread[Tuples[Range[0, dimension - 1], 2] -> Range[dimension ^ 2]]},
                        Sqrt[dimension] * Total @ Map[
                            With[{q1 = #[[1]], p1 = #[[2]]},
                                Extract[kernelElement, ({{i, j}, #} /. labels)] *
                                    Flatten[Dot[Conjugate[Values[momentumBasis[[p1 + 1]]]],
                                       Transpose[Values[positionBasis[[q1 + 1]]]]]][[1]] (
                                    KroneckerProduct[
                                        Values[momentumBasis[[p1 + 1]]][[1]],
                                        Conjugate[Values[positionBasis[[q1 + 1]]][[1]]]
                                    ])
                            ] &,
                            Tuples[Range[0, dimension - 1], 2],
                            {1}
                        ]
                    ],
                    {i, 0, dimension - 1}, {j, 0, dimension - 1}
                ],
                {1, 2}
            ]
        ],
            args
        ]
]

QuditBasis[nameArg_ ? nameQ, args___] /; ! FreeQ[nameArg, _String ? (StringContainsQ["Basis"])] :=
    QuditBasis[nameArg /. name_String :> StringDelete[name, "Basis"], args]

