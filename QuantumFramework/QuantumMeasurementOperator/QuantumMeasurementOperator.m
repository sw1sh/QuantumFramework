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
    matrix, densityMatrix, partialTrace, values, projectors, probabilities, newStates, zeroProbabilities
},
    ConfirmAssert[Divisible[qs["OutputDimension"], qmo["InputDimension"]], "Operator dimension is not a multiple of state dimension"];


    (*matrix = qmo["Matrix"];*)

    partialTrace = ResourceFunction["MatrixPartialTrace"][#, Except[qmo["Order"]], qs["OutputDimensions"]] &;

    matrix = partialTrace[qmo[{"OrderedMatrix", qs["OutputQudits"]}]];

    densityMatrix = partialTrace[qs["Computational"]["NormalizedDensityMatrix"]];

    projectors = If[qmo["ProjectionQ"],
        projector @* Normalize /@ Eigenvectors[matrix],
        MatrixPower[#, 1 / 2] & /@ qmo["Tensor"]
    ];

    probabilities = Re @ Tr[densityMatrix . #] & /@ projectors;

    zeroProbabilities = Position[probabilities, 0];
    projectors = Delete[projectors, zeroProbabilities];
    probabilities = Delete[probabilities, zeroProbabilities];
    newStates = MapThread[
        QuantumState[
            ConjugateTranspose[#1] . densityMatrix . #1 / #2,
            QuantumBasis["Input" -> QuditBasis[], "Output" -> QuditBasis[qs["OutputDimensions"][[qmo["Order"]]] ]]
        ] &,
        {projectors, probabilities}
    ];
    values = If[qmo["ProjectionQ"], QuditBasis[qs["OutputDimensions"][[qmo["Order"]]]]["Names"] (*Eigenvalues[matrix]*), Range[First @ qmo["OutputDimensions"]]];
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

