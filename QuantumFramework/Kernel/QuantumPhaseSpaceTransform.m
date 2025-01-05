Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumPhaseSpaceTransform"]
PackageExport["QuantumPositiveTransform"]



QuantumPhaseSpaceTransform[qb_ ? QuditBasisQ, phaseSpaceBasis_ ? QuditBasisQ] :=
    Enclose @ If[phaseSpaceBasis["Dimension"] == qb["Dimension"] ^ 2, phaseSpaceBasis, QuditBasis[qb["Dimension"] ^ 2]]

QuantumPhaseSpaceTransform[qb_ ? QuditBasisQ] :=
    QuantumPhaseSpaceTransform[qb, QuditBasis["Wigner"[qb["Dimensions"], "Exact" -> ! qb["NumberQ"]]]]

QuantumPhaseSpaceTransform[qb_ ? QuantumBasisQ] :=
    QuantumPhaseSpaceTransform[qb, QuantumBasis["Wigner"[qb["OutputDimensions"], "Exact" -> ! qb["NumberQ"]], "Wigner"[qb["InputDimensions"], "Exact" -> ! qb["NumberQ"]]]]

QuantumPhaseSpaceTransform[qb_ ? QuditBasisQ, args__] := QuantumPhaseSpaceTransform[qb, QuditBasis[args]]

QuantumPhaseSpaceTransform[qb_ ? QuantumBasisQ, args__] := Enclose @ Block[{
    basis = QuantumBasis[args], newBasis
},

    newBasis = If[
        basis["InputDimension"] == 1 && qb["InputDimension"] > 1,
        QuantumBasis[basis, "Input" -> basis["Output"]["Dual"]],
        basis
    ];

    newBasis = QuantumBasis[
        QuditBasis[newBasis["Output"], ConfirmBy[If[qb["OutputDimension"] == 1, 1, Log[newBasis["OutputDimension"], qb["OutputDimension"] ^ 2]], IntegerQ]],
        QuditBasis[newBasis["Input"], ConfirmBy[If[qb["InputDimension"] == 1, 1, Log[newBasis["InputDimension"], qb["InputDimension"] ^ 2]], IntegerQ]]
    ];

    Enclose @ QuantumBasis[
        QuantumPhaseSpaceTransform[qb["Output"], newBasis["Output"]],
        QuantumPhaseSpaceTransform[qb["Input"], newBasis["Input"]],
        "Picture" -> "PhaseSpace",
        qb["Options"]
    ]
]

QuantumPhaseSpaceTransform[qs_ ? QuantumStateQ, args___] := Enclose @ Chop @ Simplify @ QuantumState[
    ConfirmBy[qs["Double"], QuantumStateQ],
    ConfirmBy[QuantumPhaseSpaceTransform[If[qs["NumberQ"], N, Identity] @ qs["Basis"], args], QuantumBasisQ]
]

QuantumPhaseSpaceTransform[qo_ ? QuantumOperatorQ, args___] := Enclose @ QuantumOperator[
    ConfirmBy[QuantumPhaseSpaceTransform[qo["State"], args], QuantumStateQ],
    qo["Order"]
]

QuantumPhaseSpaceTransform[qc_ ? QuantumChannelQ, args___] :=
    Enclose @ QuantumChannel[ConfirmBy[QuantumPhaseSpaceTransform[qc["QuantumOperator"], args], QuantumOperatorQ]]["DiscardExtraQudits"]

QuantumPhaseSpaceTransform[qmo_ ? QuantumMeasurementOperatorQ, args___] :=
    Enclose @ QuantumMeasurementOperator[qmo["Double"], ConfirmBy[QuantumPhaseSpaceTransform[qmo["POVM"]["Basis"], args], QuantumBasisQ]]

QuantumPhaseSpaceTransform[qco_ ? QuantumCircuitOperatorQ, args___] :=
    Enclose @ QuantumCircuitOperator[If[BarrierQ[#], #, Confirm @ QuantumPhaseSpaceTransform[#, args]] & /@ qco["Elements"], qco["Label"]]



sectorBasis[args___] := With[{basis = QuditBasis[args]},
	QuantumTensorProduct[
		QuditBasis[
			SectorChart[
				Reverse @ Partition[{1, #} & /@ #, Length[#] / 2], ChartStyle -> {{ColorData[97][1], ColorData[97][2]}, None}, ImageSize -> 32
			] & /@ IdentityMatrix[#["Dimension"]],
			#["Elements"]
		] & /@ (basis + basis)["Decompose"]
	]
]

QuantumPositiveTransform[tensor_ ? TensorQ] := Enclose @ With[{dims = TensorDimensions[tensor], rank = TensorRank[tensor]},
    SparseArray @ Fold[
        If[#2 == 1, Join[Ramp[#1], Ramp[- #1], #2], Join[#1, Reverse[#1, #2 - 2], #2]] &,
        ArrayReshape[Chop @ tensor, Catenate[{1, #} & /@ dims]],
        2 Range[rank] - 1
    ]
]
QuantumPositiveTransform[qb_QuditBasis, ___] := sectorBasis[qb]
QuantumPositiveTransform[qb_QuantumBasis, ___] := QuantumBasis[QuantumPositiveTransform[qb["Output"]], QuantumPositiveTransform[qb["Input"]], qb["Options"]]
QuantumPositiveTransform[op_QuantumOperator, args___] := QuantumOperator[QuantumPositiveTransform[op["State"], args], op["Order"] /. {} -> {First[Flatten[op["Order"]]]}]
QuantumPositiveTransform[qs_QuantumState] /; qs["Picture"] == "PhaseSpace" := QuantumState[
	Flatten[QuantumPositiveTransform[qs["StateTensor"]]],
	QuantumBasis[Sequence @@ QuantumPositiveTransform[qs["QuditBasis"]["Canonical"]]["Split", qs["OutputQudits"]], "Label" -> qs["Label"]]
]
QuantumPositiveTransform[qs_QuantumState, args___] := QuantumPositiveTransform[QuantumPhaseSpaceTransform[qs, args]]
QuantumPositiveTransform[qc_QuantumChannel, args___] := QuantumChannel[QuantumPositiveTransform[qc["QuantumOperator"], args]]
QuantumPositiveTransform[qmo_QuantumMeasurementOperator, args___] := QuantumMeasurementOperator[QuantumPositiveTransform[qmo["SuperOperator"], args], qmo["Targets"]]
QuantumPositiveTransform[qc_QuantumCircuitOperator, args___] := Block[{ops = qc["NormalOperators"]},
	QuantumCircuitOperator[QuantumPositiveTransform[#, args] & /@ ops, "Label" -> qc["Label"]]
]

