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




(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] := Enclose @ Module[{
    matrix, values, projectors, probabilities, newStates, nonZeroProbabilities
},
    ConfirmAssert[Divisible[qs["OutputDimension"], qmo["InputDimension"]], "Operator dimension is not a multiple of state dimension"];
    matrix = qmo[{"OrderedMatrix", qs["Qudits"]}];
    projectors = If[qmo["ProjectionQ"],
        projector /@ Eigenvectors[matrix],
        MatrixPower[#, 1 / 2] & /@ qmo[{"OrderedTensor", qs["Qudits"]}]
    ];
    probabilities = Re @ Tr[qs["NormalizedDensityMatrix"] . #] & /@ projectors;
    nonZeroProbabilities = Position[probabilities, 0];
    projectors = Delete[projectors, nonZeroProbabilities];
    probabilities = Delete[probabilities, nonZeroProbabilities];
    newStates = MapThread[QuantumState[ConjugateTranspose[#1] . qs["NormalizedDensityMatrix"] . #1 / #2] &, {projectors, probabilities}];
    values = If[qmo["ProjectionQ"], Eigenvalues[matrix], Range[First @ qmo["OutputDimensions"]]];
    values = Catenate @ KeyValueMap[With[{v = #1, m = #2}, If[m > 1, Superscript[v, #] & /@ Range[m], {v}]] &, Counts[Delete[values, nonZeroProbabilities]]];
    QuantumMeasurement[AssociationThread[values, probabilities], newStates]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[op_ ? QuantumFrameworkOperatorQ] :=
    QuantumMeasurementOperator[qmo["QuantumOperator"][op]]


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

