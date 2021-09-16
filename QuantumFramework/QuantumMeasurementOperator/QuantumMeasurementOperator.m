Package["QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[op_]] := QuantumDiscreteOperatorQ[op]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)


QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumDiscreteOperator[qmo["DiscreteOperator"], args]]


QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumDiscreteOperatorQ], ___]] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumDiscreteOperator[args], QuantumDiscreteOperatorQ]]




(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qds_ ? QuantumDiscreteStateQ] := Module[{
    matrix, values, projectors, probabilities, newStates, nonZeroProbabilities
},
    matrix = qmo[{"OrderedMatrix", qds["Qudits"]}];
    projectors = If[qmo["ProjectionQ"],
        projector /@ Eigenvectors[matrix],
        qmo[{"OrderedTensor", qds["Qudits"]}]
    ];
    probabilities = N @ Re @ Tr[qds["DensityMatrix"] . #] & /@ projectors;
    nonZeroProbabilities = Position[probabilities, 0];
    projectors = Delete[projectors, nonZeroProbabilities];
    probabilities = Delete[probabilities, nonZeroProbabilities];
    newStates = MapThread[QuantumDiscreteState[ConjugateTranspose[#1] . qds["DensityMatrix"] . #1 / #2] &, {projectors, probabilities}];
    values = If[qmo["ProjectionQ"], Eigenvalues[matrix], Range[First @ qmo["OutputDimensions"]]];
    values = Ket /@ Delete[values, nonZeroProbabilities];
    QuantumMeasurement[AssociationThread[values, probabilities], newStates]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qdo_ ? QuantumDiscreteOperatorQ] :=
    QuantumMeasurementOperator[qmo["DiscreteOperator"][qdo]]


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

