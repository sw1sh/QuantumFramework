(* ::Package:: *)

Package["Wolfram`QuantumFramework`SecondQuantization`"]

PackageImport["Wolfram`QuantumFramework`"]

PackageExport["$FockSize"]

PackageExport["a"]

PackageExport["Commutator"]

PackageExport["OperatorVariance"]

PackageExport["CoherentState"]

PackageExport["FockState"]

PackageExport["AnnihilationOperator"]

PackageExport["DisplacementOperator"]

PackageExport["SqueezeOperator"]

PackageExport["ThermalState"]

PackageExport["CatState"]

PackageExport["QuadratureOperators"]

PackageExport["WignerFunction"]

PackageExport["HusimiFunction"]



(* ::Section:: *)
(*General Definitions*)


$FockSize = 15;


Commutator[a_QuantumOperator,b_QuantumOperator]:= a@b - b@a 


OperatorVariance[state_QuantumState, op_QuantumOperator]:= 
	(state["Dagger"]@ (op @ op)@ state)["Scalar"] - 
	(state["Dagger"]@ op @ state)["Scalar"]^2


FockState[n_,d_ :$FockSize]:= QuantumState[SparseArray[{n+1 -> 1}, d], d]


AnnihilationOperator[n_ :$FockSize]:= AnnihilationOperator[n] = 
	QuantumOperator[
	SparseArray[Band[{1,2}]->Sqrt[Range[n-1]],{n,n}],
	n]


a := AnnihilationOperator[]


CoherentState[d_:$FockSize] :=
    Block[{n=0},
       QuantumState[NestList[(n++; # \[FormalAlpha] / Sqrt[n])&, 1, d - 1], d, "Parameters" -> \[FormalAlpha]]
    ]


ThermalState[nbar_, size_:$FockSize] :=
    QuantumOperator[
        DiagonalMatrix[1 / (1 + nbar) 
            Table[(nbar / (1 + nbar)) ^ n, 
           {n, 0, size-1}
          ]
        ], 
    size]["MatrixQuantumState"]


DisplacementOperator[\[Alpha]_?NumberQ,dim_:$FockSize]:= 
										Exp[\[Alpha] AnnihilationOperator[dim]["Dagger"] -\[Alpha]\[Conjugate] AnnihilationOperator[dim]]


SqueezeOperator[xi_?NumericQ,dim_:$FockSize]:= Block[{a=AnnihilationOperator[dim]},
									Exp[0.5( Conjugate[xi](a @ a)-xi (a["Dagger"]@ a["Dagger"]))]
									]


QuadratureOperators[dim_:$FockSize]:= Block[{a=AnnihilationOperator[dim]},
							{1/2(a+a["Dagger"]),
							1/(2I)(a-a["Dagger"])};
						]


CatState[dim_:$FockSize] :=
    Block[{amplitudes},
        amplitudes =
            Transpose[
                Block[{n = 0},
                    NestList[
                        (
                            n++;
                            # {\[Alpha] / Sqrt[n], -\[Alpha] / Sqrt[n]}
                        )&
                        ,
                        {1, 1}
                        ,
                        dim - 1
                    ]
                ]
            ];
        QuantumState[amplitudes[[1]] + E ^ (I \[Phi]) amplitudes[[2]], dim,
             "Parameters" -> {\[Alpha], \[Phi]}]["Normalize"]
    ]


(* ::Section:: *)
(*Quasi-probability Distributions*)


(* ::Subsection:: *)
(*Wigner*)


(* ::Input::Initialization::Plain:: *)
Options[WignerFunction]={
"GParameter"->Sqrt[2]
};


(* ::Input::Initialization::Plain:: *)
WignerFunction[rho_, xvec_, yvec_, OptionsPattern[]] :=
    Module[{M, X, Y, A2, B, w0, L, diag, g},
        g = OptionValue["GParameter"];
        M = Length[rho];
        {X, Y} = Transpose[Outer[List, xvec, yvec], {3, 2, 1}];
        A2 = g * (X + I Y);
        B = Abs[A2] ^ 2;
        w0 = ConstantArray[2 rho[[1, -1]], {Length[xvec], Length[yvec
            ]}];
        L = M - 1;
        While[
            L > 0
            ,
            L--;
            diag = Diagonal[rho, L];
            w0 = WigLaguerreVal[L, B, diag] + w0 A2 * (L + 1) ^ -0.5;
                
        ];
        w0 = Re[w0] Exp[-B 0.5] (g^2 0.5 / \[Pi]);
        w0
    ]


(* ::Input::Initialization::Plain:: *)
WigLaguerreVal[L_, x_, c_] :=
    Module[{y0, y1, k, n},
        n = Length[c];
        Switch[n,
            1,
                y0 = c[[1]];
                y1 = 0.
            ,
            2,
                y0 = c[[1]];
                y1 = c[[2]]
            ,
            _,
                k = n;
                y0 = c[[-2]];
                y1 = c[[-1]];
                Do[
                    k--;
                    {y0, y1} = {c[[-i]] - y1 Sqrt[((k - 1.) (L + k - 
                        1.)) / ((L + k) k)], y0 - y1 (L + 2. k - 1 - x) Sqrt[1 / ((L + k) k)]
                        }
                    ,
                    {i, 3, n}
                ]
        ];
        y0 - y1 Sqrt[1 / (L + 1.)] (L + 1. - x)
    ]


(* ::Subsection:: *)
(*Husimi Q *)


(* ::Input::Initialization::Plain:: *)
HusimiFunction[\[Psi]_QuantumState, xvec_, yvec_, g_ : Sqrt[2]] :=
    Module[{X, Y, amat, qmat, d, v, qmatList, k, nonZeroEigenpairs},
        {X, Y} = Transpose[Outer[List, xvec, yvec], {3, 2, 1}];
        amat = 0.5 g (X + I Y);
        qmat = ConstantArray[0, Dimensions[amat]];
        If[\[Psi]["PureStateQ"],
            qmat = HusimiPure[\[Psi], amat]
            ,
            nonZeroEigenpairs = Select[Transpose[Eigensystem[\[Psi]["DensityMatrix"
                ]]], First[#] != 0&];
            qmatList = Map[(#[[1]] HusimiPure[QuantumState[#[[2]], Length[
                #[[2]]]], amat])&, nonZeroEigenpairs];
        ];
        qmat = 0.25 Total[Re /@ qmatList] g^2;
        qmat
    ];


HusimiPure[psi_QuantumState, alphaMat_] :=
    Module[{n, psiVec, qmat, coeffs},
        n = Times @@ psi["Dimensions"];
        psiVec = psi["Matrix"] // Flatten;
        qmat = Function[{q, x},
                    q . z ^ Range[Length[q] - 1, 0, -1] /. z -> x
                ][Reverse[psiVec / Sqrt[Factorial /@ Range[0, n - 1]]
                    ], Conjugate[alphaMat]] // Abs[#] ^ 2&;
        Re[qmat] Exp[-Abs[alphaMat] ^ 2] / Pi
    ]
