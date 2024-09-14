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

PackageExport["QuantumLinearSolve"]


(* ::Section:: *)
(*Example specific functions *)


(* ::Subsection:: *)
(*Quantum Natural Gradient Descent*)


FubiniStudyMetricTensor::param = "QuantumState parameters ill\[Dash]defined.";

Options[FubiniStudyMetricTensor]={"Parameters"->Automatic};

FubiniStudyMetricTensor[qstate:_QuantumState| (_List ? VectorQ),prop : _String | {__String} | All : "Result", opts:OptionsPattern[]]:=
Module[

	{var,stateVector,result,derivatives,output,cachedResults,reporter,matrix},

	output = Replace[prop, 
					All -> {"Result","Matrix","MatrixForm","Parameters","SparseArray"}
				];	

	cachedResults=<||>;	
	
	reporter[data_Association] := (
		PrependTo[cachedResults, data];  
		If[ContainsAll[Keys[cachedResults], Flatten[{output}]],
			Return[cachedResults[[output]],Module]
		];
		);		
									
	
	If[MatchQ[OptionValue["Parameters"],Automatic],
		var=qstate["Parameters"],
		var=OptionValue["Parameters"]
	];
	
	reporter[<|"Parameters"->var|>];
	
	If[!MatchQ[var,{_Symbol..}],
			Message[FubiniStudyMetricTensor::param];
			Return[$Failed]
	];
		
	stateVector=If[MatchQ[qstate,_QuantumState],
					Normal[qstate["StateVector"]],
					qstate
				];
	
	derivatives=Table[D[stateVector,i],{i,var}];
	
	result=Table[
			Re[ConjugateTranspose[derivatives[[i]]] . derivatives[[j]]]-(ConjugateTranspose[derivatives[[i]]] . stateVector)(ConjugateTranspose[stateVector] . derivatives[[j]]),
			{i,Length@derivatives},{j,Length@derivatives}
			];
	
	result=QuantumOperator[result,"Parameters"->var];
	
	reporter[<|"Result"->result,"SparseArray"->result["Matrix"]|>];
	
	matrix=result["Matrix"]//Normal//ComplexExpand//Simplify;
					
	reporter[<|"Matrix"->matrix,"MatrixForm"->MatrixForm[matrix]|>];
	
	
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


(* ::Subsection::Closed:: *)
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


(* ::Section:: *)
(*Gradient descent functions*)


QuantumNaturalGradientDescent::param = "Metric Tensor parameters ill\[Dash]defined.";

Options[QuantumNaturalGradientDescent]={
	
	"InitialPoint"->Automatic,
	
	"Gradient"->None,
	
	"MaxIterations"->50,
	
	"LearningRate"->0.8
	
};

QuantumNaturalGradientDescent[f_, g_QuantumOperator, OptionsPattern[]]:=Module[{p,gradf,init,steps,\[Eta],MetricTensorFunction},
	
	If[!MatchQ[g["Parameters"],{_Symbol..}],
		Message[QuantumNaturalGradientDescent::param];
		Return[$Failed]
	];
	
	If[VectorQ[OptionValue["InitialPoint"],NumericQ], 
		init=OptionValue["InitialPoint"],
		init=0.01*RandomVariate[NormalDistribution[],Length@g["Parameters"]]
	];
																																																		
	MetricTensorFunction[vect_ /; VectorQ[vect, NumericQ]]:=Normal[N[g[Sequence@@vect]["Matrix"]]];
	
	gradf=OptionValue["Gradient"];
	
	steps=OptionValue["MaxIterations"];
	
	\[Eta]=OptionValue["LearningRate"];
	
	If[
		MatchQ[gradf,None],
			
			NestList[(#-\[Eta] LinearSolve[MetricTensorFunction[#],CheapGradient[f,Table[Symbol["\[Theta]"<>ToString[i]],{i,Length@init}],#]])&,N@init,steps],
			
			NestList[(#-\[Eta] LinearSolve[MetricTensorFunction[#],gradf@@#])&,N@init,steps]
			
	]
]


(* ::Subsection:: *)
(*Auxiliar functions*)


CheapGradient[f_, vars_List, values_ ? VectorQ]:=Module[{permutedVars,nd},

		permutedVars=TakeDrop[#,{1}]&/@NestList[RotateLeft,Thread[vars->values],Length[vars]-1];
		
		centralFiniteDifference[f@@(vars/.#[[2]]),Sequence@@First@#[[1]]]&/@permutedVars
]


centralFiniteDifference[f_[vars__],var_,val_]:=With[{h=10.^-3},

			1/(2 h) ((f[vars]/.var->val+h)-(f[vars]/.var->val-h))
			
		]


Options[GradientDescent]={
	"Gradient"->None,
	
	"MaxIterations"->50,
	
	"LearningRate"->0.8
};
GradientDescent[f_, init_ ? VectorQ, OptionsPattern[]]:=Module[{gradf,steps,\[Eta]},

	gradf=OptionValue["Gradient"];
	
	steps=OptionValue["MaxIterations"];
	
	\[Eta]=OptionValue["LearningRate"];

	If[
		MatchQ[gradf,None],
		
			NestList[(#-\[Eta]*CheapGradient[f,Table[Symbol["\[Theta]"<>ToString[i]],{i,Length@init}],#])&,N@init,steps],
			
			NestList[(#-\[Eta]*gradf@@#)&,N@init,steps](*f->grad[f]*)
			
	]
]


(* ::Section:: *)
(*QuantumLinearSolver*)


QuantumLinearSolverCircuit[A_?MatrixQ,Ansatz_]:=Module[{pauliDecompose, multiplexer, ancillary,ansatzqubits,qc,parameters,state},

	parameters=Ansatz["Parameters"];
	
	pauliDecompose=QuantumOperator[A]["PauliDecompose"];
	
	multiplexer=QuantumCircuitOperator["Multiplexer"[Sequence@@Keys[pauliDecompose]]];
	
	ancillary=QuantumState[Sqrt@Values[pauliDecompose],"Label"->"\!\(\*SqrtBox[\(m\)]\)"];
	
	ansatzqubits=Range[ancillary["Qudits"]+1,multiplexer["Range"]];
	
	qc=N@QuantumCircuitOperator[
		{
		ancillary,
		Ansatz->ansatzqubits,
		multiplexer,
		SuperDagger[(ancillary["Conjugate"])]
		},
		"Parameters"->parameters
		];
	
	qc
	
	]


QuantumLinearSolve::error = "Global phase deviation exceeds threshold; possible errors."

Options[QuantumLinearSolve]=Join[{"Ansatz"->Automatic,"GlobalPhaseAccuracy"->10^-5},Options[NMinimize]]

QuantumLinearSolve[matrix_?MatrixQ, vector_?VectorQ/;!Mod[Log2[Length[vector]],1]==0, prop : _String | {__String} | All : "Result", opts:OptionsPattern[]]:=Module[
	{result,min,A,b},
	
	
	
	If[Dimensions[matrix]!={#,#}&@Length[vector],Message[QuantumLinearSolve::dim];Return[$Failed]];
	
	Progress`EvaluateWithProgress[
	
		min=Min@Select[Table[2^n,{n,1,10}],#>=Length[vector]&];
		
		A=ReplacePart[PadRight[matrix,{min,min},0],Table[{i,i}->1,{i,Length@vector+1,min}]];
		
		b=Flatten@Append[vector,ConstantArray[0,min-Length@vector]];
	
		,
				
		<|"Text" -> "Translating to \!\(\*SuperscriptBox[\(2\), \(n\)]\)\!\(\*SuperscriptBox[\(x2\), \(\(n\)\(\\\ \)\)]\)problem"|>
	
	];
	
	result=QuantumLinearSolve[A,b,prop,opts];
		
	If[MatchQ[prop,"Result"],result=Take[result,Length@vector]];
	
	If[MatchQ[result,_Association]&&KeyMemberQ[result,"Result"],result["Result"]=Take[result["Result"],Length@vector]];

	result				
]


QuantumLinearSolve::dim = "Vector and matrix dimension not compatible."

QuantumLinearSolve[matrix_?MatrixQ, vector_?VectorQ, prop : _String | {__String} | All : "Result", opts:OptionsPattern[]]:=Module[

	{A,b,cost,bstate,complexQ,\[Omega],v,var,output,plength,cachedResults,
	Ansatz,circuit,parameters,state,globalphase,QuantumDistanceCostFunction,optparameters,result, reporter},
	
	If[Dimensions[matrix]!={#,#}&@Length[vector],Message[QuantumLinearSolve::dim];Return[$Failed]];	
	
	$ModuleNumber=1;
	
	A = matrix/Norm[matrix];
	
	b = Normalize[vector];
																																																																																																																																																																																																									
	bstate = QuantumState[b];
	
	complexQ = !FreeQ[b,_Complex];

	output = Replace[prop, 
					All -> {"Result","Ansatz","CircuitOperator","GlobalPhase","OptimizedParameters","Parameters"}
				];
	
	cachedResults=<||>;
	
	reporter[data_Association] := (
			PrependTo[cachedResults, data];  
			If[ContainsAll[Keys[cachedResults], Flatten[{output}]],
				Return[cachedResults[[output]],Module]
			];
		);
			
 Progress`EvaluateWithProgress[		
	If[
		MatchQ[OptionValue["Ansatz"],Automatic], 
	
			plength = Length@vector;
			$ModuleNumber=1;
			
			If[complexQ,
				\[Omega] = Table[Unique[\[FormalOmega]],{i,2*plength}];
				(*If b is complex, then the QuantumState ansatz is the form of \[Sum]Subscript[\[Alpha], j]+Subscript[I\[Beta], j]|0\[Ellipsis]\[RightAngleBracket])*)		
				Ansatz = QuantumState[(#[[1]]+#[[2]]I)&@Partition[\[Omega],Length[\[Omega]]/2],"Label"->"Ansatz","Parameters"->\[Omega]]
				,
				\[Omega] = Table[Unique[\[FormalOmega]],{i,plength}];
				(*If b is real, then the QuantumState ansatz is the form of \[Sum]Subscript[\[Alpha], j]|0\[Ellipsis]\[RightAngleBracket])*)			
				Ansatz = QuantumState[\[Omega],"Label"->"Ansatz","Parameters"->\[Omega]]
			];
	
			, 
			
			Ansatz = OptionValue["Ansatz"];
			If[MatchQ[Ansatz,_QuantumCircuitOperator],Ansatz = Ansatz[]];
			\[Omega] = Ansatz["Parameters"];
			
		];
		,		
		<|"Text" -> "Ansatz initialization"|>
	];

	parameters=Ansatz["Parameters"];
	
	reporter[<|"Ansatz"->Ansatz,"Parameters"->parameters|>];


 Progress`EvaluateWithProgress[
 		 
		circuit = QuantumLinearSolverCircuit[A,Ansatz];
			
		state = circuit[];
		
	,
	
	<|"Text" -> "Variational circuit initialization", "ElapsedTime" -> Automatic|>
	
	];

	reporter[<|"CircuitOperator"->circuit|>];
	
	QuantumDistanceCostFunction[vect_ /; VectorQ[vect, RealValuedNumberQ]]:=QuantumDistance[bstate["Normalized"],state[AssociationThread[parameters->vect]]["Normalized"],"Fidelity"];


	
DynamicModule[{points},
	points={};
	Progress`EvaluateWithProgress[
		optparameters =
			Last[
			NMinimize[
				QuantumDistanceCostFunction[\[Omega]],
				\[Omega],
				FilterRules[{opts}, Options[NMinimize]],
				StepMonitor:>(AppendTo[points,cost=QuantumDistanceCostFunction[\[Omega]]])
				]
			],
			
			If[
				MatchQ[OptionValue[Method],Automatic],
				
				<|"Text" -> "Hybrid optimization procedure","ElapsedTime"->Automatic|>,
								
				<|"Text" -> Dynamic[
							ListLogPlot[points,
									PlotLabel->"Hybrid optimization procedure",
									FrameLabel->{"Step","Log[Cost function]"},
									PlotRange->{0,Log@First[points]},
									GridLines->Automatic,
									Frame->True,
									ImageSize->Small, 
									AspectRatio->1,
									PlotLegends->StringForm["Cost function\nminimization value:\n``",ScientificForm[Last[points]]],
									Joined->True
									]
								]								
				|>		
				
			]
		]
	];


	reporter[<|"OptimizedParameters"->optparameters|>];

 Progress`EvaluateWithProgress[

		globalphase=Divide@@(Extract[#,SparseArray[Chop[b]]["ExplicitPositions"]]&/@{state[<|optparameters|>]["AmplitudesList"],b});
	
		If[StandardDeviation[globalphase] > OptionValue["GlobalPhaseAccuracy"], Message[QuantumLinearSolve::error]];
	
		result=Ansatz[<|optparameters|>]["AmplitudesList"];
	
		result=1/Mean[globalphase]*result*Norm[vector]/Norm[matrix];

		,
		
		<|"Text" -> "Final normalization"|>
	
	];

	reporter[<|"Result"->result,"GlobalPhase"->Around[Mean[globalphase],StandardDeviation[globalphase]]|>]

]
