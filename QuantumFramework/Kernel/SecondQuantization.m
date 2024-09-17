(* ::Package:: *)

Package["Wolfram`QuantumFramework`SecondQuantization`"]

PackageImport["Wolfram`QuantumFramework`"]

PackageExport["$FockSize"]

PackageExport["SetFockSpaceSize"]

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

PackageExport["WignerRepresentation"]

PackageExport["HusimiQRepresentation"]



(* ::Section:: *)
(*General Definitions*)


SetFockSpaceSize[size:_Integer?Positive:16]:=$FockSize = size;


Commutator[a_QuantumOperator,b_QuantumOperator]:= a@b - b@a 


OperatorVariance[state_QuantumState, op_QuantumOperator]:= 
	(state["Dagger"]@ (op @ op)@ state)["Scalar"] - (state["Dagger"]@ op @ state)["Scalar"]^2


FockState[n_,size_ :$FockSize]:= 
If[n>size, Message[FockState::len,n,size],
QuantumState[SparseArray[{n+1 -> 1}, size], size]]
FockState::len="Argument `1` cannot be larger that the size of the space, `2`";


AnnihilationOperator[size_ :$FockSize]:= AnnihilationOperator[size] = 
	QuantumOperator[
	SparseArray[Band[{1,2}]->Sqrt[Range[size-1]],{size,size}],
	size]


CoherentState[size_:$FockSize] :=
    Block[{n=0},
       QuantumState[NestList[(n++; # \[FormalAlpha] / Sqrt[n])&, 1, size - 1], size, "Parameters" -> \[FormalAlpha]]
    ]


ThermalState[nbar_, size_:$FockSize] :=
    QuantumOperator[
        DiagonalMatrix[1 / (1 + nbar) 
            Table[(nbar / (1 + nbar)) ^ n, 
           {n, 0, size-1}
          ]
        ], 
    size]["MatrixQuantumState"]


Options[DisplacementOperator] = {"Ordering" -> "Normal"};
DisplacementOperator::invalidorder = "The value for the 'Ordering' option, `1`, is invalid. Choose from 'Normal', 'Weak', or 'Antinormal'.";

DisplacementOperator[\[Alpha]_, opts : OptionsPattern[]] := DisplacementOperator[\[Alpha], $FockSize, opts];

DisplacementOperator[\[Alpha]_, size_, OptionsPattern[]] :=
    Block[{a = AnnihilationOperator[size], ordering},
        
        ordering = OptionValue["Ordering"];
        
        Switch[ordering,
            "Normal",
                Exp[-\[Alpha] Conjugate[\[Alpha]]/2]  Exp[\[Alpha] (a["Dagger"])] @ Exp[-Conjugate[\[Alpha]] a],

            "Weak",
               Exp[\[Alpha] a["Dagger"]-Conjugate[\[Alpha]] a],

            "Antinormal",
                Exp[\[Alpha] Conjugate[\[Alpha]]/2]  Exp[-Conjugate[\[Alpha]] a] @ Exp[\[Alpha] (a["Dagger"])] ,

            _, 
                Message[DisplacementOperator::invalidorder, ordering];
                Abort[]
        ]
    ]


Options[SqueezeOperator] = {"Ordering" -> "Normal"};

SqueezeOperator::invalidorder = "The value for the 'Ordering' option, `1`, is invalid. Choose from 'Normal', 'Weak', or 'Antinormal'.";

SqueezeOperator[xi_, opts : OptionsPattern[]] := SqueezeOperator[xi, $FockSize, opts];

SqueezeOperator[xi_, size_, OptionsPattern[]] :=
    Module[{tau, nu, a = AnnihilationOperator[size], identityOp, ordering},
    
        ordering = OptionValue["Ordering"];
        
        tau = xi / Abs[xi] Tanh[Abs[xi]];
        
        nu = Log[Cosh[Abs[xi]]];
        
        identityOp = QuantumOperator["I"[size]];
        
        Switch[ordering,
            "Normal",
                Exp[-tau / 2 ((a["Dagger"]) @ (a["Dagger"]))] @ Exp[-
                    nu ((a["Dagger"]) @ a + 1/2 identityOp)] @ Exp[Conjugate[tau] / 2 (a 
                    @ a)]
            ,
            "Weak",
                Exp[1/2 (Conjugate[xi] (a @ a) - xi (a["Dagger"] @ a["Dagger"]))]
            ,
            "Antinormal",
                Exp[1/2 Conjugate[tau] (a @ a)] @ Exp[-nu ((a["Dagger"
                    ]) @ a + 1/2 identityOp)] @ Exp[-1/2 tau ((a["Dagger"]) @ (a["Dagger"
                    ]))]
            ,
            _,
                Message[SqueezeOperator::invalidorder, ordering];
                Abort[]
        ]
    ]


QuadratureOperators[size_:$FockSize]:= Block[{a=AnnihilationOperator[size]},
							{1/2(a+a["Dagger"]),
							1/(2I)(a-a["Dagger"])}
						]


CatState[size_:$FockSize] :=
    Block[{amplitudes,n=0},
        amplitudes =
            Transpose[
                    NestList[
                        (
                            n++;
                            # {\[FormalAlpha] / Sqrt[n], -\[FormalAlpha] / Sqrt[n]}
                        )&
                        ,
                        {1, 1}
                        ,
                        size - 1
                    ]
            ];
        QuantumState[amplitudes[[1]] + E ^ (I \[FormalPhi]) amplitudes[[2]], size,
             "Parameters" -> {\[FormalAlpha], \[FormalPhi]}]["Normalize"]
    ]


SetFockSpaceSize[];


(* ::Section:: *)
(*Quasi-probability Distributions*)


(* ::Subsection:: *)
(*Wigner*)


(* ::Input::Initialization::Plain:: *)
Options[WignerRepresentation]={
"GParameter"->Sqrt[2]
};


(* ::Input::Initialization::Plain:: *)
WignerRepresentation[psi_QuantumState, xvec_, yvec_, OptionsPattern[]] :=
    Module[{rho,M, X, Y, A2, B, w0, diag, g},
   rho = psi["DensityMatrix"];
        g = OptionValue["GParameter"];
        M = Length[rho];
        {X, Y} = Transpose[Outer[List, xvec, yvec], {3, 2, 1}];
        A2 = g * (X + I Y);
        B = Abs[A2] ^ 2;
        w0 = ConstantArray[2 rho[[1, -1]], {Length[xvec], Length[yvec
            ]}];
        While[
            M > 1,
            M--;
            diag = Diagonal[rho, M-1] If[M!=1,2,1];
            w0 = WigLaguerreVal[M-1, B, diag] + w0 A2 * M ^ -0.5;
        ];
         Re[w0] Exp[-B 0.5] (g^2 0.5 / \[Pi])
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
HusimiQRepresentation[\[Psi]_QuantumState, xvec_, yvec_, g_ : Sqrt[2]] :=
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
            qmat = 0.25 Total[Re /@ qmatList] g^2
        ];
        qmat
    ];


HusimiPure[psi_QuantumState, alphaMat_] :=
    Module[{n, psiVec, qmat},
        n = Times @@ psi["Dimensions"];
        psiVec = psi["Matrix"] // Flatten;
        qmat = Function[{q, x},
                    q . z ^ Range[Length[q] - 1, 0, -1] /. z -> x
                ][Reverse[psiVec / Sqrt[Factorial /@ Range[0, n - 1]]
                    ], Conjugate[alphaMat]] // Abs[#] ^ 2&;
        Re[qmat] Exp[-Abs[alphaMat] ^ 2] / Pi
    ]
