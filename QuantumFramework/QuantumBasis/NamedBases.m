Package["QuantumFramework`"]



QuantumBasis[args : (_String ? (MatchQ[Alternatives @@ $QuantumBasisPictures]) | _Rule) ...] := QuantumBasis["Computational", args]

QuantumBasis[dimension_Integer, args___] := QuantumBasis[{"Computational", dimension}, args]

QuantumBasis["Computational", args___] := QuantumBasis[{"Computational", 2}, args]

QuantumBasis[{"Computational", dimension_Integer}, args___] := QuantumBasis[identityMatrix[dimension] /. {{}} -> {}, args]


QuantumBasis["Bell", args___] := QuantumBasis[
    AssociationThread[{
            Ket[Superscript["\[CapitalPsi]", "+"]],
            Ket[Superscript["\[CapitalPhi]", "+"]],
            Ket[Superscript["\[CapitalPsi]", "-"]],
            Ket[Superscript["\[CapitalPhi]", "-"]]
        },
        (1 / Sqrt[2]) {{1, 0, 0, 1}, {0, 1, 1, 0}, {1, 0, 0, -1}, {0, 1, -1, 0}}
    ],
    args
]


QuantumBasis["PauliX", args___] := QuantumBasis[
    AssociationThread[{
            Ket[Subscript["\[Psi]", "x+"]],
            Ket[Subscript["\[Psi]", "x-"]]
        },
        (1 / Sqrt[2]) {{1, 1}, {1, -1}}
    ],
    args
]

QuantumBasis["PauliY", args___] := QuantumBasis[
    AssociationThread[{
            Ket[Subscript["\[Psi]", "y+"]],
            Ket[Subscript["\[Psi]", "y-"]]
        },
        (1 / Sqrt[2]) {{1, I}, {1, -I}}
    ],
    args
]

QuantumBasis["PauliZ", args___] := QuantumBasis[
    AssociationThread[{
            Ket[Subscript["\[Psi]", "z+"]],
            Ket[Subscript["\[Psi]", "z-"]]
        },
        IdentityMatrix[2]
    ],
    args
]


QuantumBasis["Fourier"] := QuantumBasis[{"Fourier", 2}]

QuantumBasis[{"Fourier", dimension_Integer ? Positive}, args___] := QuantumBasis[
    AssociationThread[
        Ket[Subscript["F", #]] & /@ Range[dimension],
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


QuantumBasis["Identity"] := QuantumBasis[{"Identity", 2}]

QuantumBasis[{"Identity", dimension_Integer ? Positive}, args___] := QuantumBasis[
    AssociationThread[
        Ket[Subscript["I", #]] & /@ Range[dimension ^ 2],
        Partition[#, dimension] & /@ IdentityMatrix[dimension ^ 2]
    ],
    args
]


QuantumBasis["Schwinger"] := QuantumBasis[{"Schwinger", 2}]

QuantumBasis[{"Schwinger", dimension_Integer ? Positive}, args___] := QuantumBasis[
    AssociationThread[
        Ket[Subscript["S", ToString[#]]] & /@ Tuples[Range[0, dimension - 1], 2],
        Flatten /@ (
            MatrixPower[RotateLeft[IdentityMatrix[dimension]], #[[1]]] .
            MatrixPower[((Exp[I 2 Pi / dimension]) ^ #) & /@ Range[0, dimension - 1] IdentityMatrix[dimension], #[[2]]]
        ) & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]],
    args
]

QuantumBasis["Pauli", args___] := QuantumBasis[
    AssociationThread[{Ket[Subscript["\[Sigma]", "0"]],
        Ket[Subscript["\[Sigma]", "1"]], Ket[Subscript["\[Sigma]", "2"]],
        Ket[Subscript["\[Sigma]", "3"]]},
        {{{1, 0}, {0, 1}}, {{0, 1}, {1, 0}}, {{0, -I}, {I, 0}}, {{1, 0}, {0, -1}}}
    ],
    args
]

QuantumBasis["Dirac", args___] := Module[{
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
    QuantumBasis[AssociationThread[
        Ket[Subscript["\[Epsilon]", ToString[#]]] & /@ Range[0, 15],
        {
            IdentityMatrix[4],
            KroneckerProduct[pauliBasis4, pauliBasis1],
            KroneckerProduct[pauliBasis3, pauliBasis2],
            KroneckerProduct[pauliBasis3, pauliBasis3],
            KroneckerProduct[pauliBasis3, pauliBasis4],
            gamma1 . gamma2, gamma1 . gamma3, gamma1 . gamma4, gamma2 . gamma3, gamma2 . gamma4, gamma3 . gamma4,
            -I (gamma2 . gamma3 . gamma4), I (gamma1 . gamma3 . gamma4), -I (gamma1 . gamma2 . gamma4), I (gamma1 . gamma2 . gamma3),
            gamma1 . gamma2 . gamma3 . gamma4
       }],
       args
    ]
]


QuantumBasis["Wigner", args___] := QuantumBasis[{"Wigner", 2}, args]

QuantumBasis[{"Wigner", dimension_Integer}, args___] := QuantumBasis[{"Wigner", QuantumBasis[dimension]}, args]

QuantumBasis[{"Wigner", qb_QuantumBasis}] := QuantumBasis[{"Wigner", qb}, "PhaseSpace"]

QuantumBasis[{"Wigner", qb_QuantumBasis /; QuantumBasisQ[qb]}, args___] := Module[{
    dimension, positionBasis, momentumBasis, kernelElement
},
        dimension = First @ qb["Dimensions"];
        positionBasis = {#} & /@ Normal[qb["BasisElementAssociation"]];
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
            Transpose[Normalize[#] & /@ Values[eigensystem]] .
                DiagonalMatrix[Sqrt[Keys[eigensystem]]] .
                Inverse[Transpose[Normalize[#] & /@ Values[eigensystem]]]
        ];

        QuantumBasis[AssociationThread[
            Ket[Subscript["W", ToString[#]]] & /@ Tuples[Range[0, dimension - 1], 2],

            Flatten[
                Table[
                    With[{labels = Thread[Tuples[Range[0, dimension - 1], 2] -> Range[dimension ^ 2]]},
                        Sqrt[dimension] * Total @ Map[
                            With[{q1 = #[[1]], p1 = #[[2]]},
                                Extract[kernelElement, ({{i, j}, #} /. labels)] *
                                    Flatten[(Conjugate[Values[momentumBasis[[p1 + 1]]]] .
                                       Transpose[Values[positionBasis[[q1 + 1]]]])][[1]] (
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

