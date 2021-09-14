Package["QuantumFramework`"]


QuantumMeasurementOperator["PauliX", args___] := QuantumMeasurementOperator[{"PauliX", 2}, args]
QuantumMeasurementOperator[{"PauliX", dimension_Integer}, args___] :=
 QuantumMeasurementOperator[SparseArray[({i_, j_} /; Mod[i - 1, dimension, 1] == j) -> 1, {dimension, dimension}], args]

(* TODO rewrite *)
QuantumMeasurementOperator["PauliY"] :=
 QuantumMeasurementOperator["PauliY", {1}]
QuantumMeasurementOperator["PauliY", picture_String] :=
 QuantumMeasurementOperator["PauliY", {1}, picture]
QuantumMeasurementOperator["PauliY", order_List] :=
 QuantumMeasurementOperator[Normal[SparseArray[{{1, 2} -> -I, {2, 1} -> I}]], order]
QuantumMeasurementOperator["PauliY", order_List, picture_String] :=
 QuantumMeasurementOperator[Normal[SparseArray[{{1, 2} -> -I, {2, 1} -> I}]], order, picture]
QuantumMeasurementOperator["PauliZ"] :=
 QuantumMeasurementOperator["PauliZ", {1}]
QuantumMeasurementOperator["PauliZ", order_List] :=
 QuantumMeasurementOperator[{"PauliZ", 2}, order]
QuantumMeasurementOperator["PauliZ", picture_String] :=
 QuantumMeasurementOperator["PauliZ", {1}, picture]
QuantumMeasurementOperator["PauliZ", order_List, picture_String] :=
 QuantumMeasurementOperator[{"PauliZ", 2}, order, picture]
QuantumMeasurementOperator[{"PauliZ", dimension_Integer}] :=
 QuantumMeasurementOperator[Normal[SparseArray[{j_, j_} :> Exp[(2 Pi I j / dimension) + I Pi], {dimension, dimension}]], dimension, {1}]
QuantumMeasurementOperator[{"PauliZ", dimension_Integer}, order_List] :=
 QuantumMeasurementOperator[Normal[SparseArray[{j_, j_} :> Exp[(2 Pi I j / dimension) + I Pi], {dimension, dimension}]], dimension, order]
QuantumMeasurementOperator[{"PauliZ", dimension_Integer}, picture_String] :=
 QuantumMeasurementOperator[Normal[SparseArray[{j_, j_} :> Exp[(2 Pi I j / dimension) + I Pi], {dimension, dimension}]], dimension, {1}, picture]
QuantumMeasurementOperator[{"PauliZ", dimension_Integer}, order_List, picture_String] :=
 QuantumMeasurementOperator[Normal[SparseArray[{j_, j_} :> Exp[(2 Pi I j / dimension) + I Pi], {dimension, dimension}]], dimension, order, picture]
QuantumMeasurementOperator["Hadamard"] :=
 QuantumMeasurementOperator["Hadamard", {1}]
QuantumMeasurementOperator["Hadamard", picture_String] :=
 QuantumMeasurementOperator["Hadamard", {1}, picture]
QuantumMeasurementOperator["Hadamard", order_List] :=
 QuantumMeasurementOperator[HadamardMatrix[2], order]
QuantumMeasurementOperator["Hadamard", order_List, picture_String] :=
 QuantumMeasurementOperator[HadamardMatrix[2], order, picture]
QuantumMeasurementOperator["RandomHermitian"] :=
 QuantumMeasurementOperator["RandomHermitian", {1}]
QuantumMeasurementOperator["RandomHermitian", order_List] :=
 QuantumMeasurementOperator[{"RandomHermitian", 2}, order]
QuantumMeasurementOperator["RandomHermitian", picture_String] :=
 QuantumMeasurementOperator["RandomHermitian", {1}, picture]
QuantumMeasurementOperator["RandomHermitian", order_List, picture_String] :=
 QuantumMeasurementOperator[{"RandomHermitian", 2}, order, picture]
QuantumMeasurementOperator[{"RandomHermitian", dimension_Integer}] :=
 Module[ {randomUnitary}, 
  randomUnitary = Orthogonalize[Table[RandomReal[NormalDistribution[0, 1 / 2]] + I RandomReal[NormalDistribution[0, 1 / 2]], dimension, dimension]];
  QuantumMeasurementOperator[randomUnitary + ConjugateTranspose[randomUnitary], dimension, {1}]
 ]
QuantumMeasurementOperator[{"RandomHermitian", dimension_Integer}, order_List] :=
 Module[ {randomUnitary}, 
  randomUnitary = Orthogonalize[Table[RandomReal[NormalDistribution[0, 1 / 2]] + I RandomReal[NormalDistribution[0, 1 / 2]], dimension ^ Length[order], dimension ^ Length[order]]];
  QuantumMeasurementOperator[randomUnitary + ConjugateTranspose[randomUnitary], dimension, order]
 ]
QuantumMeasurementOperator[{"RandomHermitian", dimension_Integer}, picture_String] :=
 Module[ {randomUnitary}, 
  randomUnitary = Orthogonalize[Table[RandomReal[NormalDistribution[0, 1 / 2]] + I RandomReal[NormalDistribution[0, 1 / 2]], dimension, dimension]];
  QuantumMeasurementOperator[randomUnitary + ConjugateTranspose[randomUnitary], dimension, {1}, picture]
 ]
QuantumMeasurementOperator[{"RandomHermitian", dimension_Integer}, order_List, picture_String] :=
 Module[ {randomUnitary}, 
  randomUnitary = Orthogonalize[Table[RandomReal[NormalDistribution[0, 1 / 2]] + I RandomReal[NormalDistribution[0, 1 / 2]], dimension ^ Length[order], dimension ^ Length[order]]];
  QuantumMeasurementOperator[randomUnitary + ConjugateTranspose[randomUnitary], dimension, order, picture]
 ]
QuantumMeasurementOperator["ComputationalBasis"] :=
 QuantumMeasurementOperator[{"ComputationalBasis", 2}]
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer}] :=
 QuantumMeasurementOperator[{"ComputationalBasis", dimension, Range[0, dimension - 1]}]
QuantumMeasurementOperator[{"ComputationalBasis", eigenvalues_List}] :=
 QuantumMeasurementOperator[{"ComputationalBasis", 2, eigenvalues}]
QuantumMeasurementOperator["ComputationalBasis", order_List] :=
 QuantumMeasurementOperator[{"ComputationalBasis", 2}, order]
QuantumMeasurementOperator["ComputationalBasis", picture_String] :=
 QuantumMeasurementOperator["ComputationalBasis", {1}, picture]
QuantumMeasurementOperator["ComputationalBasis", order_List, picture_String] :=
 QuantumMeasurementOperator[{"ComputationalBasis", 2}, order, picture]
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer}, order_List] :=
 QuantumMeasurementOperator[{"ComputationalBasis", dimension, Range[0, (dimension ^ Length[order]) - 1]}, order]
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer}, picture_String] :=
 QuantumMeasurementOperator[{"ComputationalBasis", dimension, Range[0, dimension - 1]}, {1}, picture]
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer}, order_List, picture_String] :=
 QuantumMeasurementOperator[{"ComputationalBasis", dimension, Range[0, (dimension ^ Length[order]) - 1]}, order, picture]
QuantumMeasurementOperator[{"ComputationalBasis", eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[{"ComputationalBasis", 2, eigenvalues}, order]
QuantumMeasurementOperator[{"ComputationalBasis", eigenvalues_List}, picture_String] :=
 QuantumMeasurementOperator[{"ComputationalBasis", 2, eigenvalues}, picture]
QuantumMeasurementOperator[{"ComputationalBasis", eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[{"ComputationalBasis", 2, eigenvalues}, order, picture]
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer, eigenvalues_List}] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ IdentityMatrix[dimension]}]], {1}]
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer, eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ IdentityMatrix[dimension]}]], order] /; (Length[order] == 1)
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer, eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ (Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[IdentityMatrix[dimension], Length[order]], List])}]], order] /; (Length[order] > 1)
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer, eigenvalues_List}, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ IdentityMatrix[dimension]}]], {1}, picture]
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer, eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ IdentityMatrix[dimension]}]], order, picture] /; (Length[order] == 1)
QuantumMeasurementOperator[{"ComputationalBasis", dimension_Integer, eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ (Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[IdentityMatrix[dimension], Length[order]], List])}]], order, picture] /; (Length[order] > 1)
QuantumMeasurementOperator["BellBasis"] :=
 QuantumMeasurementOperator["BellBasis", {1}]
QuantumMeasurementOperator["BellBasis", order_List] :=
 QuantumMeasurementOperator[{"BellBasis", Range[0, (4 ^ Length[order]) - 1]}, order]
QuantumMeasurementOperator["BellBasis", picture_String] :=
 QuantumMeasurementOperator["BellBasis", {1}, picture]
QuantumMeasurementOperator["BellBasis", order_List, picture_String] :=
 QuantumMeasurementOperator[{"BellBasis", Range[0, (4 ^ Length[order]) - 1]}, order, picture]
QuantumMeasurementOperator[{"BellBasis", eigenvalues_List}] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ((1 / Sqrt[2]) {{1, 0, 0, 1}, {0, 1, 1, 0}, {1, 0, 0, - 1}, {0, 1, - 1, 0}})}]], {1}]
QuantumMeasurementOperator[{"BellBasis", eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ((1 / Sqrt[2]) {{1, 0, 0, 1}, {0, 1, 1, 0}, {1, 0, 0, - 1}, {0, 1, - 1, 0}})}]], order] /; (Length[order] == 1)
QuantumMeasurementOperator[{"BellBasis", eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ (Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[(1 / Sqrt[2]) {{1, 0, 0, 1}, {0, 1, 1, 0}, {1, 0, 0, - 1}, {0, 1, - 1, 0}}, Length[order]], List])}]], order] /; (Length[order] > 1)
QuantumMeasurementOperator[{"BellBasis", eigenvalues_List}, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ((1 / Sqrt[2]) {{1, 0, 0, 1}, {0, 1, 1, 0}, {1, 0, 0, - 1}, {0, 1, - 1, 0}})}]], {1}, picture]
QuantumMeasurementOperator[{"BellBasis", eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ((1 / Sqrt[2]) {{1, 0, 0, 1}, {0, 1, 1, 0}, {1, 0, 0, - 1}, {0, 1, - 1, 0}})}]], order, picture] /; (Length[order] == 1)
QuantumMeasurementOperator[{"BellBasis", eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ (Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[(1 / Sqrt[2]) {{1, 0, 0, 1}, {0, 1, 1, 0}, {1, 0, 0, - 1}, {0, 1, - 1, 0}}, Length[order]], List])}]], order, picture] /; (Length[order] > 1)
QuantumMeasurementOperator["PauliBasis"] :=
 QuantumMeasurementOperator["PauliBasis", {1}]
QuantumMeasurementOperator["PauliBasis", order_List] :=
 QuantumMeasurementOperator[{"PauliBasis", Range[0, (2 ^ Length[order]) - 1]}, order]
QuantumMeasurementOperator["PauliBasis", picture_String] :=
 QuantumMeasurementOperator["PauliBasis", {1}, picture]
QuantumMeasurementOperator["PauliBasis", order_List, picture_String] :=
 QuantumMeasurementOperator[{"PauliBasis", Range[0, (2 ^ Length[order]) - 1]}, order, picture]
QuantumMeasurementOperator[{"PauliBasis", eigenvalues_List}] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ((1 / Sqrt[2]) {{1, 1}, {1, - 1}})}]], {1}]
QuantumMeasurementOperator[{"PauliBasis", eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ((1 / Sqrt[2]) {{1, 1}, {1, - 1}})}]], order] /; (Length[order] == 1)
QuantumMeasurementOperator[{"PauliBasis", eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ (Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[(1 / Sqrt[2]) {{1, 1}, {1, - 1}}, Length[order]], List])}]], order] /; (Length[order] > 1)
QuantumMeasurementOperator[{"PauliBasis", eigenvalues_List}, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ((1 / Sqrt[2]) {{1, 1}, {1, - 1}})}]], {1}, picture]
QuantumMeasurementOperator[{"PauliBasis", eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ((1 / Sqrt[2]) {{1, 1}, {1, - 1}})}]], order, picture] /; (Length[order] == 1)
QuantumMeasurementOperator[{"PauliBasis", eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ (Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[(1 / Sqrt[2]) {{1, 1}, {1, - 1}}, Length[order]], List])}]], order, picture] /; (Length[order] > 1)
QuantumMeasurementOperator["FourierBasis"] :=
 QuantumMeasurementOperator[{"FourierBasis", 2}]
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer}] :=
 QuantumMeasurementOperator[{"FourierBasis", dimension, Range[0, dimension - 1]}]
QuantumMeasurementOperator[{"FourierBasis", eigenvalues_List}] :=
 QuantumMeasurementOperator[{"FourierBasis", 2, eigenvalues}]
QuantumMeasurementOperator["FourierBasis", order_List] :=
 QuantumMeasurementOperator[{"FourierBasis", 2}, order]
QuantumMeasurementOperator["FourierBasis", picture_String] :=
 QuantumMeasurementOperator["FourierBasis", {1}, picture]
QuantumMeasurementOperator["FourierBasis", order_List, picture_String] :=
 QuantumMeasurementOperator[{"FourierBasis", 2}, order, picture]
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer}, order_List] :=
 QuantumMeasurementOperator[{"FourierBasis", dimension, Range[0, (dimension ^ Length[order]) - 1]}, order]
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer}, picture_String] :=
 QuantumMeasurementOperator[{"FourierBasis", dimension, Range[0, dimension - 1]}, {1}, picture]
QuantumMeasurementOperator[{"FourierBasis", eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[{"FourierBasis", 2, eigenvalues}, order]
QuantumMeasurementOperator[{"FourierBasis", eigenvalues_List}, picture_String] :=
 QuantumMeasurementOperator[{"FourierBasis", 2, eigenvalues}, picture]
QuantumMeasurementOperator[{"FourierBasis", eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[{"FourierBasis", 2, eigenvalues}, order, picture]
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer, eigenvalues_List}] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ConjugateTranspose[Partition[(1 / Sqrt[dimension]) (Exp[(2 Pi I (#[[2]] #[[1]])) / dimension] & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]), dimension]]}]], {1}]
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer, eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ConjugateTranspose[Partition[(1 / Sqrt[dimension]) (Exp[(2 Pi I (#[[2]] #[[1]])) / dimension] & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]), dimension]]}]], order] /; (Length[order] == 1)
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer, eigenvalues_List}, order_List] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ (Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[ConjugateTranspose[Partition[(1 / Sqrt[dimension]) (Exp[(2 Pi I (#[[2]] #[[1]])) / dimension] & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]), dimension]], Length[order]], List])}]], order] /; (Length[order] > 1)
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer, eigenvalues_List}, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ConjugateTranspose[Partition[(1 / Sqrt[dimension]) (Exp[(2 Pi I (#[[2]] #[[1]])) / dimension] & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]), dimension]]}]], {1}, picture]
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer, eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ ConjugateTranspose[Partition[(1 / Sqrt[dimension]) (Exp[(2 Pi I (#[[2]] #[[1]])) / dimension] & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]), dimension]]}]], order, picture] /; (Length[order] == 1)
QuantumMeasurementOperator[{"FourierBasis", dimension_Integer, eigenvalues_List}, order_List, picture_String] :=
 QuantumMeasurementOperator[Total[MapThread[#1 #2 &, {eigenvalues, ConjugateTranspose[{#}] . {#} & /@ (Flatten[Apply[KroneckerProduct, #]] & /@ Distribute[Table[ConjugateTranspose[Partition[(1 / Sqrt[dimension]) (Exp[(2 Pi I (#[[2]] #[[1]])) / dimension] & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]), dimension]], Length[order]], List])}]], order, picture] /; (Length[order] > 1)

