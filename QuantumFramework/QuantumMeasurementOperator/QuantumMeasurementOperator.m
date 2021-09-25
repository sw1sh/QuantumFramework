Package["QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[op_]] := QuantumOperatorQ[op]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)


QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args]]


QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumOperatorQ], ___]] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumOperator[args], QuantumOperatorQ]]


(* mutation *)

QuantumMeasurementOperator[op_ ? QuantumFrameworkOperatorQ, order_ ? orderQ] := QuantumMeasurementOperator[Head[op][op, order]]

(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] := Enclose @ Module[{
    matrix, densityMatrix, partialTrace, values, projectors, eigenvalues, eigenvectors, probabilities, newStates, zeroProbabilities
},
    ConfirmAssert[qmo["Arity"] <= qs["OutputQudits"], "Operator's arity should be less or equal to number of state's output qudits"];
    ConfirmAssert[qs["OutputDimensions"][[ qmo["Order"] ]] == qmo["InputDimensions"][[ qmo["Order"] ]],
        "Operator's input dimensions should be equal state's output dimensions for a given order"];


    (*matrix = qmo["Matrix"];*)

    partialTrace = ResourceFunction["MatrixPartialTrace"][#, Except[qmo["Order"]], qs["OutputDimension"]] &;

    matrix = qmo[{"OrderedMatrixRepresentation", qs["OutputQudits"]}];

    eigenvalues = Eigenvalues[matrix];
    eigenvectors = Normalize /@ Eigenvectors[matrix];

    densityMatrix = partialTrace @ qs["Computational"]["DensityMatrix"];

    projectors = If[qmo["ProjectionQ"],
        (* projector[v_] := (*ConjugateTranspose[{v}] . {v}*) KroneckerProduct[v, Conjugate[v]] *)
        projector /@ eigenvectors,
        MatrixPower[#, 1 / 2] & /@ qmo["Tensor"]
    ];

    probabilities = Re @ Tr[densityMatrix . #] & /@ projectors;

    zeroProbabilities = Position[probabilities, 0];

    ConfirmAssert[Length[zeroProbabilities] < Length[probabilities], "All probabilities are zero"];

    projectors = Delete[projectors, zeroProbabilities];
    probabilities = Delete[probabilities, zeroProbabilities];
    newStates = MapThread[
        QuantumState[
            QuantumState[
                If[ qs["StateType"] === "Vector",
                    #1 . qs["StateVector"] / Sqrt[#2],
                    ConjugateTranspose[#1] . densityMatrix . #1 / #2
                ],
                QuantumBasis[
                    "Input" -> QuditBasis[qs["InputDimensions"]],
                    "Output" -> QuditBasis[qmo["OutputDimensions"][[qmo["Order"]]]]
                ]
            ](*,
            QuantumBasis[
                "Input" -> qs["Input"],
                "Output" -> QuditBasis[eigenvalues, eigenvectors]
            ]*)
        ] &,
        {projectors, probabilities}
    ];
    values = If[qmo["ProjectionQ"], QuditBasisName /@ eigenvalues, Range[First @ qmo["OutputDimensions"]]];
    values = Delete[values, zeroProbabilities];
    (*values = Catenate @ KeyValueMap[With[{v = #1, m = #2}, If[m > 1, Superscript[v, #] & /@ Range[m], {v}]] &, Counts[Delete[values, zeroProbabilities]]];*)
    QuantumMeasurement[AssociationThread[values, probabilities], newStates]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[op_ ? QuantumFrameworkOperatorQ] := With[{
    newOp = qmo["QuantumOperator"][op["QuantumOperator"]]
},
    QuantumMeasurementOperator[newOp, If[QuantumMeasurementOperatorQ[op], Union[qmo["Order"], op["Order"]], qmo["Order"]]]
]


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qm_QuantumMeasurement] := Module[{
    newMeasurements, newStates, oldValues, newValues, newProbabilities
},
    newMeasurements = qmo /@ qm["States"];
    newStates = Catenate[#["States"] & /@ newMeasurements];

    oldValues = qm["Outcomes"];

    newProbabilities = #["Probabilities"] & /@ newMeasurements;
    newValues = Keys[#][[All, 1]] & /@ newProbabilities;
    newValues = Catenate @ Table[
        Append[oldValues[[i]], newValues[[i, j]]], {i, Length[oldValues]}, {j, Length[newValues[[i]]]}];

    newProbabilities = Catenate @ MapThread[Times, {qm["ProbabilitiesList"], newProbabilities}];

    QuantumMeasurement[AssociationThread[newValues, newProbabilities], newStates]
]


(* equality *)

QuantumMeasurementOperator /: (qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) ==
    (qmo2_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) := qmo1["Matrix"] == qmo2["Matrix"]

