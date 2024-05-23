(* ::Package:: *)

Package["Wolfram`QuantumFramework`ExampleRepository`"]

PackageImport["Wolfram`QuantumFramework`"]

PackageExport["GradientDescent"]

PackageExport["QuantumNaturalGradientDescent"]

PackageExport["SPSRGradientValues"]

PackageExport["ASPSRGradientValues"]

PackageExport["FubiniStudyMetricTensor"]

PackageExport["FubiniStudyMetricTensorLayers"]

PackageExport["QuantumUnlockingMechanism"]

PackageExport["QuantumLockingMechanism"]

PackageExport["GraphToCircuit"]

PackageExport["LinearChainTeleportation"]


(* ::Section:: *)
(*Example specific functions *)


(* ::Subsection::Closed:: *)
(*Quantum Natural Gradient Descent*)


FubiniStudyMetricTensor[qstate_QuantumState, var_List]:=Module[{stateMatrix,result,derivatives},
	
	stateMatrix=qstate["Matrix"]//Normal;
	
	derivatives=Table[D[stateMatrix,i],{i,var}];
	
	result=Table[
			Re[ConjugateTranspose[derivatives[[i]]] . derivatives[[j]]]-(ConjugateTranspose[derivatives[[i]]] . stateMatrix)(ConjugateTranspose[stateMatrix] . derivatives[[j]]),
			{i,Length@derivatives},{j,Length@derivatives}
			];
	
	Flatten/@result
]


FubiniStudyMetricTensor[layers_List, parameters_List, initParameters_?(VectorQ[#,NumericQ]&)]:=Module[{probabilities,covariance,variance1,variance2},
	
	probabilities=Values[
	
					N[#[[2,2]][#[[2,1]]][#[[1]][AssociationThread[Rule[parameters,initParameters]]]]["Probabilities"]]&/@layers
					
					];
	
	covariance=(#[[4]]-(#[[3]]+#[[4]])*(#[[2]]+#[[4]]))&/@probabilities;
	
	variance1=(#*(1-#))&@Total[#[[{3,4}]]]&/@probabilities;
	
	variance2=(#*(1-#))&@Total[#[[{2,4}]]]&/@probabilities;
	
	BlockDiagonalMatrix[{{#[[1]],#[[2]]},{#[[2]],#[[3]]}}&/@Thread[Chop[{variance1,covariance,variance2},10^-8]]]
]


FubiniStudyMetricTensorLayers[qc_QuantumCircuitOperator, parameters_List]:=Module[{elements,input,layers},
	
	(*Re-generating Original QuantumCircuitOperator Inputs*)
	
		elements=DeleteCases[qc["Elements"],"Barrier"];
	
		input=#["Label"]->#["InputOrder"]&/@elements;

	(*Cleaning inputs (subscripts and innecesary brackets) to correctly build subcircuits*)
	
		input=input/.Subscript[x_String,y_String]:>x<>y/."CNOT"[___]:>"CNOT";

	(*Generating layers by splitting when parametric variables are found, dropping useless gates*)
	
		input=Most[SplitBy[input,FreeQ[#,Alternatives@@parameters]&]];

	(*Building layers*)
	
		layers=Table[input[[;;i]],{i,Range[2,Length@input,2]}];

	(*Changing last parametric Pauli gates from each layer to their correspondant QuantumMeasurementOperator*)
		
		layers=MapAt[#/.Rule[(pauli:("RX"|"RY"|"RZ"))[_],n_]:>QuantumMeasurementOperator[StringDelete[pauli,"R"],n]&,layers,{All,-1}];
	
	(*Output: QuantumState of first layer (parametrization is possible for QuamtumState from a Parametrized QuantumCircuit) and Measurements*)
	
		{QuantumCircuitOperator[Flatten[#[[;;-2]]],"Parameters"->parameters][],#[[-1]]}&/@layers
]

GraphToCircuit::invalidInput = "Inconsistent graph with measure qubit list";
GraphToCircuit::UnequalLength = "Incompatible sizes of measurement qubits and equatorial angles";
GraphToCircuit[g_Graph, measure_List/;AllTrue[measure,Internal`PositiveIntegerQ], angles_List]:=Module[{
	qubitsCircuit = Length[VertexList[g]] - Length[measure], 
	coords = Chop[GraphEmbedding[g]], 
	coordsAso, 
	visited = {},
	edgesNotMeasure,
	adjListEdges,
	gates = {}
	}, 
	If[Length[Union[coords[[All,2]]]] != qubitsCircuit,
		Message[GraphToCircuit::invalidInput];
		Return[$Failed]
	];
	If[Length[angles] !=Length[measure],
		Message[GraphToCircuit::UnequalLength];
		Return[$Failed]
	];

	coordsAso = AssociationThread[ReverseSort[Union[coords[[All,2]]]],Range[qubitsCircuit]];

	edgesNotMeasure = VertexDelete[g,measure] //EdgeList;

	Do[
	adjListEdges =  SortBy[
					Thread[{
					measure[[i]],
					Complement[AdjacencyList[g,measure[[i]]],visited]
					}
					],
						Abs[Subtract@@coords[[#,1]]]&];
	AppendTo[visited,measure[[i]]]; 
	gates = Join[
			gates,
			Thread[
				"CZ"->
			Lookup[coordsAso,coords[[List @@ #,2]]]& /@ Most[adjListEdges]
				]
			];
	gates =Join[
			gates,
			Thread[
				({"RZ"[angles[[i]]],"H"}/."RZ"[0]->Nothing)->
				Lookup[coordsAso,coords[[measure[[i]],2]]]
				]
			],
	{i,Length[measure]}
	];
	gates = Join[
				gates,
				Thread[
						"CZ"->
					Lookup[coordsAso,coords[[List @@ #,2]]]& /@ edgesNotMeasure
					]
			];
	QuantumCircuitOperator[gates][
				"Diagram",
				"WireLabels"->Table[
					{Placed["|+\[RightAngleBracket]",Left]},
					{i,qubitsCircuit}
					]
				]
]

LinearChainTeleportation[state_QuantumState, n_Integer?Positive] := Module[
	{
    graph = GridGraph[{n - 1, 1}], 
    measureState,
    correction,
    projected,
    q},

    q = QuantumCircuitOperator[
    	Join[
     	{Splice["+" -> # & /@ Range[2, n]]},
     	{"C", "1"} -> # & /@ Partition[Range[n], 2, 1]
     	 ],
    	"Cluster State"];

    measureState =  RandomChoice[Tuples[{0, 1}, n - 1]];

    projected = Fold[#1[#2] &,  QuantumBasis["X"]["BasisStates"][[measureState + 1]]]["Dagger"];

    correction = simplifyXH[measureState /. {0 -> "H", 1 -> Sequence @@ {"X", "H"}}];

   <|
   "FinalState" -> (Sqrt[2])^(n - 1) QuantumCircuitOperator[correction] @ (projected @ q[state]),

   "Circuit" -> QuantumCircuitOperator[{
       				q,

       				QuantumCircuitOperator[
        				Table[QuantumMeasurementOperator["X", {i}], {i, n - 1}],
                        "Measurement"],   
       		
       				QuantumCircuitOperator[
        				If[correction != {}, Thread[correction -> n], {"I" -> n}],
        	       		"Correction"]
       	          }]["Diagram"],

   "MeasurementGraph" -> Show[
     						HighlightGraph[
      							GridGraph[{n, 1}, VertexCoordinates -> Table[{0, n - i}, {i, n}],
       								VertexLabels -> "Name",
       								VertexSize -> 0.4],
      							 Range[n - 1]],

     						Graphics[
      							Style[Text[StringJoin["Measurements X basis:", ToString[measureState]], {2, 0}], 12]
      						     ]
     					]
   |>
  ]


(* ::Subsection::Closed:: *)
(*Stochastic Parameter Shift-Rule*)


(* ::Input::Initialization::Plain:: *)
Options[SPSRGradientValues]={
"Shift"->\[Pi]/4.,

"ParameterValues"->Subdivide[0,2\[Pi],50],

"RandomNumberCount"->10,

"MeasurementOperator"->QuantumOperator[{"PauliZ"->{1},"I"->{2}}]

};


(* ::Input::Initialization::Plain:: *)
SPSRGradientValues[generatorFunction_,pauli_,OptionsPattern[]]:=Module[{result,state,vector,rlist,\[Phi]value,rndlen,measurement,\[Theta]values},
	
	\[Theta]values=OptionValue["ParameterValues"];
		
	measurement=OptionValue["MeasurementOperator"]["Matrix"]//Normal;
	
		\[Phi]value=OptionValue["Shift"];
	
		rndlen=OptionValue["RandomNumberCount"];
	
		rlist=RandomReal[1,rndlen];

	result=Table[

				state=QuantumCircuitOperator[{
				
		Exp[I*(1.-s)*QuantumOperator[generatorFunction[\[Theta]]]],
		
				Exp[I*\[Phi]*pauli],
		
				Exp[I*s*QuantumOperator[generatorFunction[\[Theta]]]]
		
				},"Parameters"->{s,\[Phi]}][];
	
				Table[
					vector=state[<|s->sval,\[Phi]->shift|>]["StateVector"];
		
					Re[ConjugateTranspose[vector] . measurement . vector]
		
					,
					{shift,{\[Phi]value,-\[Phi]value}},{sval,rlist}
			],
		
			{\[Theta],\[Theta]values}
		];

		Thread[{\[Theta]values,Mean[(#[[1]]-#[[2]])]&/@result}]
]


(* ::Input::Initialization::Plain:: *)
Options[ASPSRGradientValues]={
"Shift"->\[Pi]/4.,

"ParameterValues"->Subdivide[0,2\[Pi],50],

"RandomNumberCount"->10,

"MeasurementOperator"->QuantumOperator[{"PauliZ"->{1},"I"->{2}}]

};


(* ::Input::Initialization::Plain:: *)
ASPSRGradientValues[generatorFunction_,pauli_,H_,OptionsPattern[]]:=Module[
{result,state,vector,rlist,\[Phi]value,rndlen,measurement,\[Theta]values},

	\[Theta]values=OptionValue["ParameterValues"];

	measurement=OptionValue["MeasurementOperator"]["Matrix"]//Normal;

	\[Phi]value=OptionValue["Shift"];

	rndlen=OptionValue["RandomNumberCount"];

	rlist=RandomReal[1,rndlen];

	result=Table[
				state=QuantumCircuitOperator[{
		
				Exp[I*(1.-s)*QuantumOperator[generatorFunction[\[Theta]]]],
		
				Exp[I*\[Pi]/4*(QuantumOperator[H]+shift*QuantumOperator[pauli])],
		
				Exp[I*s*QuantumOperator[generatorFunction[\[Theta]]]]
		
				},"Parameters"->{s}][];
			
				Table[
				vector=state[<|s->sval|>]["StateVector"];
	
				Re[ConjugateTranspose[vector] . measurement . vector],
	
			{sval,rlist}],

			{\[Theta],\[Theta]values},{shift,{\[Phi]value,-\[Phi]value}}
		];

			Thread[{\[Theta]values,Mean[(#[[1]]-#[[2]])]&/@result}]
]


(* ::Subsection:: *)
(*Quantum Locking Mechanism*)


QuantumLockingMechanism[key_String]:=Module[{pass,flipsigns, circuit},

	pass=IntegerString[ToCharacterCode[key],4,4];
	
	flipsigns=QuantumOperator[{
								{
								"C","FlipSign"[#,4]->(Range[StringLength@#]+1),{1}
								}
							}
				]&/@pass;

	circuit=QuantumCircuitOperator[{
								"H"->{1},
								#,
								"H"->{1},
								QuantumMeasurementOperator["Computational",{1}]
				}]&/@flipsigns

]


QuantumUnlockingMechanism[lock_List,key_String]:=Module[{result,pass,comb},
		
	pass=IntegerString[ToCharacterCode[key],4,4];

	If[!MatchQ[Length@lock,Length@pass],Return["Incorrect key, try again"]];

	result=#[[1]][
				QuantumState[
							<|Prepend[ToExpression[Characters[#[[2]]]],0]->1|>,
							Prepend[ConstantArray[4,StringLength@#[[2]]],2]
							]
				]["Mean"]&/@Transpose[{lock,pass}];

	If[
		MatchQ[result,{1..}],
			"Correct key",
			"Incorrect key, try again"
	]

]


(* ::Section::Closed:: *)
(*Gradient descent functions*)


Options[GradientDescent]={
	"Jacobian"->None,
	
	"MaxIterations"->50,
	
	"LearningRate"->0.8
}


GradientDescent[f_, init_ ? VectorQ, OptionsPattern[]]:=Module[{gradf,steps,\[Eta]},

	gradf=OptionValue["Jacobian"];
	
	steps=OptionValue["MaxIterations"];
	
	\[Eta]=OptionValue["LearningRate"];

	If[
		MatchQ[gradf,None],
		
			NestList[(#-\[Eta]*CheapGradient[f,Table[Symbol["\[Theta]"<>ToString[i]],{i,Length@init}],#])&,N@init,steps],
			
			NestList[(#-\[Eta]*gradf@@#)&,N@init,steps](*f->grad[f]*)
			
	]
]


Options[QuantumNaturalGradientDescent]={
	"Jacobian"->None,
	
	"MaxIterations"->50,
	
	"LearningRate"->0.8
}


QuantumNaturalGradientDescent[f_,init_ ? VectorQ , g_, OptionsPattern[]]:=Module[{gradf,steps,\[Eta]},
	
	gradf=OptionValue["Jacobian"];
	
	steps=OptionValue["MaxIterations"];
	
	\[Eta]=OptionValue["LearningRate"];
	
	If[
		MatchQ[gradf,None],
			
			NestList[(#-\[Eta] LinearSolve[g[#],CheapGradient[f,Table[Symbol["\[Theta]"<>ToString[i]],{i,Length@init}],#]])&,N@init,steps],
			
			NestList[(#-\[Eta] LinearSolve[g[#],gradf@@#])&,N@init,steps](*f->grad[f]*)
			
	]
]


CheapGradient[f_, vars_List, values_ ? VectorQ]:=Module[{permutedVars,nd},

		permutedVars=TakeDrop[#,{1}]&/@NestList[RotateLeft,Thread[vars->values],Length[vars]-1];
		
		centralFiniteDifference[f@@(vars/.#[[2]]),Sequence@@First@#[[1]]]&/@permutedVars
		
]


centralFiniteDifference[f_[vars__],var_,val_]:=With[{h=10.^-3},

			1/(2 h) ((f[vars]/.var->val+h)-(f[vars]/.var->val-h))
			
		]

simplifyXH[list_List] := ReplaceRepeated[list, {
  {a___, x_, x_, b___} :> {a, b}, 
  {a___, "H", "X", "H", b___} :> {a, "Z", b}, 
  {a___, "H", "Z", "H", b___} :> {a, "X", b}, 
  {a___, "X", "Z", "X", b___} :> {a, "Z", b}
  }]
