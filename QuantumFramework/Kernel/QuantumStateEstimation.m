(* ::Package:: *)

(* ::Title:: *)
(*Quantum State Estimation*)


(* ::Author:: *)
(*Mihai Vidrighin*)
(*First Released January 2023*)


Package["Wolfram`QuantumFramework`"]

PackageExport["MeasurementSimulation"]
PackageExport["QuantumStateEstimate"]
PackageExport["QuantumStateEstimation"]
PackageExport["QuantumStateSampler"]



(* ::Section:: *)
(*Public Symbols*)


MeasurementSimulation::usage = "MeasurementSimulation[_QuantumState, (list of _QuantumMeasurementOperator), counts (list or per measurement operator)]";

inversion::usage = "inversion[measurementResult]";

newState::usage = "newState[dimension]";

stateSamples::usage = "stateSamples[state, stepSize, numberOfSamples]";

logLikelihood::usage = "logLikelihood[_QuantumState, measurementResult]";

buresState::usage = "looks like a panel. Extract buresState['densityMatrix'] or buresState['state']";

QuantumStateEstimate::usage = "QuantumStateEstimate[measurementResults]";

QuantumStateEstimation::usage ="Use QuantumStateEstimation['properties']";


(* ::Chapter:: *)
(*Implementation*)

MeasurementResultQ[mr_Association] :=
	AllTrue[Keys[mr], QuantumMeasurementOperatorQ] &&
	With[{dims = Through[Keys[mr]["InputDimension"]]},
		Equal @@ dims &&
		AllTrue[Values[mr], VectorQ[#, IntegerQ] && Length[#] == First[dims] &]
	];
MeasurementResultQ[___] := False



(* ::Section::Closed:: *)
(*Measurement Simulation*)


MeasurementSimulation[state_ ? QuantumStateQ, measurementOperators_ ? (VectorQ[#, QuantumMeasurementOperatorQ] &), n_] :=
	AssociationMap[RandomVariate[MultinomialDistribution[n, #[state]["ProbabilitiesList"]]] &, measurementOperators]


(* ::Section::Closed:: *)
(*Inversion*)


inversion[measurementResult_ ? MeasurementResultQ] := Block[{
	measurementOutcomeOperators,  probabilities
},
	measurementOutcomeOperators = Join @@ Through[Keys[measurementResult]["POVMElements"]];
	probabilities = Normalize[#, Total] & /@ Values[measurementResult] // Flatten;
	inversion[measurementOutcomeOperators, probabilities]
]

inversion[operators_, probabilities_] := Block[{
	size, measurementMatrix, state
},
	size = Length[operators[[1]]];
	measurementMatrix = Flatten /@ operators // Normal;
	state = Transpose[Partition[PseudoInverse[measurementMatrix] . probabilities, size]] // Chop;
	<|"invertible" -> ! Chop[Det[ConjugateTranspose[measurementMatrix] . measurementMatrix]] == 0, "rho" -> QuantumState[state, size]|>
]


(* ::Section::Closed:: *)
(*Random Rotations*)


randomU[d_, stepSize_] :=
	MatrixPower[
		RandomVariate[CircularUnitaryMatrixDistribution[d]],
		RandomVariate[NormalDistribution[0, stepSize]]
	]

randomRotate[v_, stepSize_] := Normalize[v + RandomVariate[NormalDistribution[0, stepSize], Length[v]]]


(* ::Section::Closed:: *)
(*Bures Metric*)


logBuresVolumeFactor[rhos_] := Re @ Sum[Log[(rhos[[i]] - rhos[[j]]) ^ 2] - Log[rhos[[i]] + rhos[[j]]], {i, Length[rhos]}, {j, i - 1}]

Format[buresState[v_, u_]] := Panel["QuantumStateSample"]
buresState[v_, u_]["densityMatrix"] := u . DiagonalMatrix[v ^ 2] . ConjugateTranspose[u] // Chop
buresState[v_, u_]["state"] := QuantumState[buresState[v, u]["densityMatrix"]]

toBuresState[rho_] := Block[{
	d, u
},
	{d, u} = Eigensystem[rho];
	buresState[Sqrt[d] // Chop, Transpose[Normalize /@ u]]
]

max = Log[10 ^ -10];

newBuresVector[v_, stepSize_] := Block[{newV},
	newV = randomRotate[v, stepSize];
	(*Need to be careful about cases where the bures volume factor is 0*)
	If[	Re[logBuresVolumeFactor[newV ^ 2] - Max[logBuresVolumeFactor[v ^ 2], max]] > Log[RandomReal[]],
		newV,
		(*Else*)
		Sow[1, "rejected"];
		v
	]
]

newState[d_] := buresState[Normalize[RandomVariate[NormalDistribution[0, 1], d]], randomU[d, 1]]
newState[buresState[v_, u_], stepSize_] :=
	buresState[newBuresVector[v, stepSize], randomU[Length[v], stepSize] . u]

newState2[buresState[v_, u_], stepSize_] :=
	buresState[randomRotate[v, stepSize], randomU[Length[v], stepSize] . u]

stateSamples[state_, stepSize_, n_] := Block[{
	rejected, samples
},
	rejected = Total @ First[
		Reap[samples = NestList[newState[#, stepSize] &, state, n - 1];, "rejected"][[2]],
		{}
	];
	Sow[n / (n + rejected) // N, "acceptanceRatio"];
	samples
]


(* ::Section::Closed:: *)
(*Metropolis-Hastings*)


newMeasuredState[{state_, likelihood_}, {operators_, events_, stepSize_}] := Block[{
	new, newLikelihood
},
	new = newState[state, stepSize];
	newLikelihood = logLikelihood[new["densityMatrix"], operators, events];
	If[	Re[newLikelihood - likelihood] > Log[RandomReal[]],
		{new, newLikelihood},
		(*Else*)
		Sow[1, "rejected"];
		{state, likelihood}
	]
]

measuredStateSamples[state_, operators_, events_, stepSize_, n_] := Block[{
	likelihood, samples, rejected, stateBures
},
	stateBures = toBuresState[state];
	likelihood = logLikelihood[state, operators, events];
	rejected = Total @ First[
		Reap[
			samples = NestList[newMeasuredState[#, {operators, events, stepSize}] &, {stateBures, likelihood}, n - 1][[;;, 1]];,
			"rejected"
		][[2]],
		{}
	];
	Sow[n / (n + rejected) // N, "acceptanceRatio"];
	samples
]

Options[QuantumStateSampler] = {"StepSize" -> Automatic, "Burn" -> 0};

Format[qss : QuantumStateSampler[state_, operators_, events_, stepSize_]] :=
	Interpretation[QuantumStateSampler[Panel[StringTemplate["Dimension: ``, Counts: ``"][Length[state], Total[events] // Round]]], qss]
QuantumStateSampler[state_, operators_, events_, stepSize0_][n_, opt : OptionsPattern[]] := Block[{
	stepSize, burn
},
	stepSize = "StepSize" /. {opt, "StepSize" -> stepSize0};
	burn = "Burn" /. {opt, "Burn" -> 0};
	QuantumState[#["densityMatrix"], Length[state]] & /@ measuredStateSamples[state, operators, events, stepSize, n + burn][[burn + 1 ;;]]
]


(* ::Section:: *)
(*Max Likelihood*)


logLikelihood[state_ ? QuantumStateQ, measurementResult_ ? MeasurementResultQ] :=
	Re @ Chop @ Sum[Log[Values @ measurement[state]["Probabilities"]] . measurementResult[measurement], {measurement, Keys @ measurementResult}]

logLikelihood[rho_, measurementResult_ ? MeasurementResultQ] := Block[{
	operators, events
},
	operators = Join @@ Through[Keys[measurementResult]["POVMElements"]] // N;
	events = Values[measurementResult] // N // Flatten;
	logLikelihood[rho, operators, events]
]

logLikelihood[rho_, operators_, events_] := Block[{pick},
	(* Need to be careful about 0 Log[0] terms *)
	pick = Thread[events != 0];
	Log[Tr[rho . #] & /@ Pick[operators, pick]] . Pick[events, pick] // Chop // Re
]

Options[increaseLikelihood] = {"maxLikeStepFactors" -> {0.95, 1.05}, "maxLikeIterationLimit" -> 30};
increaseLikelihood[{state_, likelihood_, stepSize_}, operators_, events_, opt : OptionsPattern[]] :=
	increaseLikelihood[{state, likelihood, stepSize, 0}, operators, events, opt]

increaseLikelihood[{state_, likelihood_, stepSize_, iteration_}, operators_, events_, opt : OptionsPattern[]] := Block[{
	new, newLikelihood
},
	If[iteration == OptionValue["maxLikeIterationLimit"], Throw[state]];
	new = newState2[state, stepSize];
	Sow[<|"likelihood" -> likelihood, "stepSize" -> stepSize|>, "optimization"];
	newLikelihood = logLikelihood[new["densityMatrix"], operators, events];
	If[	newLikelihood > likelihood,
		{new, newLikelihood, stepSize OptionValue["maxLikeStepFactors"][[2]], 0},
		(*Else*)
		Sow[1, "rejected"];
		{state, likelihood, stepSize OptionValue["maxLikeStepFactors"][[1]], iteration + 1}
	]
]

filterOptions[opt___, function_] := FilterRules[{opt}, Options[function]]

Options[maximizeLikelihood] = Join[{"maxLikeIterations" -> 300, "maxLikeInitialStepSize" -> 0.5}, Options[increaseLikelihood]];
maximizeLikelihood[state_, operators_, events_, opt : OptionsPattern[]] := Block[{
	likelihood, options, maxLikelihoodState, rejected, n, stepSize
},
	likelihood = logLikelihood[state, operators, events];
	options = filterOptions[opt, increaseLikelihood];
	n = OptionValue["maxLikeIterations"];
	stepSize = OptionValue["maxLikeInitialStepSize"]/Length[state]^2;
	rejected = Total @ First[
		Reap[maxLikelihoodState =
			Catch[Nest[increaseLikelihood[#, operators, events, options] &, {toBuresState[state], likelihood, stepSize}, n - 1][[1]]];,
			"rejected"
		][[2]],
		{}
	];
	Sow[n / (n + rejected) // N, "acceptanceRatio"];
	maxLikelihoodState
]


(* ::Section:: *)
(* Main function *)


QuantumStateEstimate::normalization = "Measurement operators should be normalized";
QuantumStateEstimate::eventSize = "The number of measurement results should match the measurement operators";
QuantumStateEstimate::operatorSize = "The operators should all have the same dimension";

Options[QuantumStateEstimate] = Options[maximizeLikelihood];

QuantumStateEstimate[measurementResult_ ? MeasurementResultQ, opt : OptionsPattern[]] := Block[{
	operators, events, eventsPerMeasurement, size, probabilities, inversionResult, physicalInversion, inversionState,
	maxLikelihoodState, optimizationMeta, bayesianSampler, result, stepSize, startState, bayesianAcceptanceRatio
},
	operators = Through[Keys[measurementResult]["POVMElements"]] // N;
	events = Values[measurementResult] // N;
	eventsPerMeasurement = Mean[Total /@ events] // Round;

	(*Checks*)
	If[! And @@ Thread[Chop @* Total /@ operators == IdentityMatrix @* Length /@ operators[[;;, 1]]], Return[Message[QuantumStateEstimate::normalization]]];
	If[! And @@ Thread[Length /@ operators == Length /@ events], Return[Message[QuantumStateEstimate::eventSize]]];
	If[! Length[DeleteDuplicates[Dimensions /@ Flatten[operators, 1]]] == 1, Return[Message[QuantumStateEstimate::operatorSize]]];

	(*Inversion*)
	operators = Join @@ operators;
	probabilities = Flatten[Normalize[#, Total] & /@ events];
	events = Flatten[events];
	inversionResult = inversion[operators, probabilities];
	physicalInversion = inversionResult["rho"]["PhysicalQ"];
	inversionState = inversionResult["rho"]["Physical"]["DensityMatrix"];
	size = Length[inversionState];

	(*Max Likelihood*)
	optimizationMeta = <|Reap[maxLikelihoodState = maximizeLikelihood[inversionState, operators, events, filterOptions[opt, maximizeLikelihood]]["densityMatrix"];, _, Rule][[2]]|>;

	(*Bayesian Sampling*)
	stepSize = 4 / Sqrt[eventsPerMeasurement] / size ^ 2;
	bayesianAcceptanceRatio = Reap[startState = measuredStateSamples[maxLikelihoodState, operators, events, stepSize, 150][[-1]]["densityMatrix"]; "acceptanceRatio"][[2, 1, 1]];
	stepSize = stepSize (1 / 2 + bayesianAcceptanceRatio) ^ 2;
	bayesianAcceptanceRatio = Reap[startState = measuredStateSamples[maxLikelihoodState, operators, events, stepSize, 150][[-1]]["densityMatrix"]; "acceptanceRatio"][[2, 1, 1]];
	bayesianSampler = QuantumStateSampler[startState, operators, events, stepSize];

	(*Output*)
	result = <|
		"Invertible" -> inversionResult["invertible"],
		"PhysicalInversion" -> physicalInversion,
		"InversionState" -> QuantumState[inversionState],
		"MaximumLikelihoodState" -> QuantumState[maxLikelihoodState],
		"MaximumLikelihoodAcceptanceRatio" -> optimizationMeta["acceptanceRatio"][[1]],
		"MaximumLikelihoodConvergence" -> (# - Max[#] & @ optimizationMeta["optimization"][[;;, "likelihood"]]),
		"MaximumLikelihoodConvergenceStepSize" -> optimizationMeta["optimization"][[;;, "stepSize"]],
		"BayesianAcceptanceRatio" -> bayesianAcceptanceRatio,
		"BayesianSampler" -> bayesianSampler,
		"TotalCounts" -> Round[Total[events]],
		"CountsPerMeasurement" -> eventsPerMeasurement,
		"Dimension" -> size,
		"MetropolisStepSize" -> stepSize,
		"MeasurementOutcomes" -> Length[operators]
	|>;
	QuantumStateEstimation[Merge[Last] @ {result, "Properties" -> Keys[result]}]
]



(* ::Section:: *)
(* Formatting *)

$QuantumStateEstimationIcon = Show[Image[CompressedData["
1:eJzsvWWQXFe2tnljZn7Mz4n4Iub3zHzf7du37+02MzMzsyXLJBllZpRZZpTd
ZkZZkkG2ZYu5pCpJVSpmTsbKzMrKNetZ++ysrJIsk+y2u7UjdiWfyjxnv3ut
d+H/O/HKEy/63/7t3/7tmv9T/5w44YYDpk6dcNNJ/5c+OOWKay69+IoLJx1x
xbUXXnzh1N0n/u/65JM6l+v8P3SKSPSfaZZKJZsjIyPRYrEYLQ4P2y2Pef6n
HsuOUTF/znG2zT/sHJB/gqFrVnTt2vyhwXuSyaSEQiHp7++Xrq4u6ejokN6e
HhkcGJBoNCpDQ0M/+n9yq3jZGj9j2/j9jRGdpT/aZOjatLm512PRaKmmpqb0
8ccfl554/PHSDddfX5o4YULp1FNOKR1z9NGlIw47rHTYIYe4eeihpcN1lh8f
cmjp6COPLJ1w/PGlM884ozRl8uTSvffcU3rl5ZdL87/7rqRYKhUKhU3+p+LE
vg+3/+jzs21utZmTP8hgj96cjOD5+vp6ee/dd+XmG2+UU08+WXSd2zzphBPk
kosukjvvuF2ef/YZ+eD99+TbeV/JsqWLZU3VKllfs1YaNtZKc2O91NdtkNUr
l9v8bt7X8uXnc+Tdd96SJx5/VK6/9ho579xz5OijjpRDDjpIFD9y0aRJ+trj
smDBAolEIpv9rtvkyh9+5P/RX+CHhtdjKgf60dy5c+W2W26RE447Tg479BA5
64wz7PE7b78pSxYvlI263mPRkOSyKSkWhmQ4n5VUMiaJWFiGMgnJpGKSSkQl
GQ/bc5Fwv84BKeTSdp9bPlcq5mVkeEjS+tmuznbD1exZM+XxR6fLlMmXyvHH
HiMqf+SCiRPlxRkzDKub+/7bsPKHHL9bfLCumH7k83nDxNVXXSUHH3iQHHXk
EXLj9debTNhYVysD/b0S13WeiEekt7tDuUSPhAZ6ZaCvW/p7OvW5dunubDNZ
wezpapfO9hZpb2vWdd+mn43Kl198IQvmf6e4SUh/b5d06mu8p621Sbo6WmWw
v1uxFDJ8FXIZGVLsDer/qFq1QuXT03K+yphDDz5IVI+Tp596SlpbWsb8pm04
+cON3x0+xvPd5uZmefCBB0Q5gxxz1FEy7Z67TT4MDvTpmo5IaLBP+nQts35Z
y431dTa7da9va2nS2Wjru0Nf6+lqU7x02drn/VGVE2H9fDoZlba2VsPfF4qR
cGhQsdAnjQ11usYbpUNx0t/bKYP62b6eDjsOs09xyLGQN8iiqMqfjbXr5IP3
3pHJl1ykOD5QJk28QObMniPKWcq/aRtO/jDjd4OP8fJi+bJlMvnSS2WfvfaS
KXr79dwvDQ+FXFb6+3ps3YOHvh7Wbbetc9YnciKVCEs8Omhrl9f97WB/j70n
nYjo+3vteeRBSh93dXXI7NmzbIbDIdOnUqZ7hexzne2thimO06dY6Q7w2N7W
ZLe8Bp7gM516P6lybGPdenny8cfkuGOOkaOOOEJeeP55/Q2x8m/chpPf/fiH
44P1UYkL+O5ZZ54pB+y3n9xy802yfl216jMpiUVCZdnQ3trs1r3iwvQnblX3
QX9infYoRpAZrH9kRDwyaDMa6jMdic+AH+7H9PWY3i8UcrJy5QqpqanW71M0
7oKsYYIP8MT7kRscE9wk9HNMjhWLDNjkf6C71a6vtu8CTvh+n3z0gZxx+mmy
v/6uRx5+WGKxaPk3/xi79LbxDxn/UHxUrouVK1bIeeecIwcecIA8+sjDsq5m
relQcIPmpnqp1325TXHB2h7s77U1y3oNDbi1y5rt7+1w+pNih3XLc7zuMOHX
ut7vdXwEnamzvVkxVyvpVEI5Tk6GhwvKK7Kmt/HeSMgdm8n6j4Tcd+LWMGHP
97vb4D1w/rD+35Ymx3MG+jr1+UGb2MYmnHuu7L/vfsZRvK9l/D6xbfwuxj8E
H5Uco7u7W6Yq50ZePPjA/VK7YZ10q67T3obO0mYcuwc9X3HB2mOvNp1I1yB6
kdOBdO2F+1R+tBpXgCP06j6PDtTd0WKzp9Pdtrc26rrdqPpZg63fd995W449
+mipWr1KcdhguhEY7Ao+Z5/tajW5wTH7etrt/7Q2NygOO0yODaicGjTcdpex
CGZ7gu/j9DqHt4TKJeTVnNmfmi0a29fMmTPL52abLPldjd8cH5XXf8YLL8i+
e+8t1117raxdUyW9Pd3S1FhvfBuegE7E2kJPSiqniAW6kuMOnbaG0f/bW5ts
vTc11kmXygN4dET377ju64nogN0iB0IDTt6wltHFwoq9zz+bbb4SZAjH6zO5
0qGvt6iu1Gv6ErKAGRl0jzkm2HCv90gIXASyKRTINC+3kE/og736frAeDp5P
K5+HM7395utyyEEHyrkqO5ubm+y8bJMlv5vxm+Gj8prX1dXJKSedbPv21199
pbp4RNfvgO216DWxgC9wi35ie7HeggP2/w7FBPu55w9p+Ljuz6zXpMqSoXTM
btmzwUQ4wEpc13V40K11XuNzq1cuMR9GaBD55N6PPOrt5vhd9rl0HA4fsvt8
HpzALULBcflfqfig4SYaBgMOL9z2drUGMqfN4VrljdO5ugxr2AFamxvltltu
lt122UWeefqZ8jnbJkv+4eM3wcdIxXV+6cUXZacddpD7pt2r/KJf10mvYYI1
w3pHf0K3Z49H/2lu3CgdugfDF1i7rMWMrutsMlKeQ+movtZlaxdbkmFC78ej
/ZJJ6Xv09ViUtc3aH9DHEeMEw8NpWb50oeGD9/NaUtd5GJmgxwMTUcNMgKdk
WHLZuB0T7PDeqJdLAVex76ffiZlUXPFct+l2rSZzIoNetvQEtrVO+71J1RWX
LFogRx95hOpdp1hMGGObjesfOn51fPg9MBqJyITzz5cD999fvvt2nmQzKdX1
dU8NOQ7NfowNCix42y3yBO6LvZa1ydpjL2e6NRi2Nc3a7upoNg7Cns0aLAzF
JZ+N2Z7OWuY44Cinz/GYz/HzFy2YJ8cdc7Qdf1A/O5SJGZ6QTxGVBYVcwvDk
ZQ/rHV2J7wRWeG82FTVZBD9y+ltXILMG7HuCIT5nvhLTDTvsO3icgBG3P/Sa
Xnn3nbfL7rvtJh999JGdO/CxDSP/kPGr4sNjo2r1atlHecYVl19mvgtsRfBv
9PGQro9WlRPoTp2q//f3ddm6SsVDJk9Yh+zD6C/gA3nBWnR8161DcBHRPT+f
HcWEX8us65yuef8az9txdF2XillZtPBbkx88juhxeH8+l5S44mdA8RJVXOUU
a4V80t4zoP8THJms0O8EnjL6vJdTDisR+w3GQ/Q3eDzwmXQyUrYHI3eQY96u
Zjqk7gn4Gud+Mcd8P7ffdlv5fG7jJL/5+FXwUck13nn7bdl5xx3ltVdf0XWX
VXkxYHZa1kKvztbAvx023jto9qhk4FsAK+g44CGT9Ht4b5l3gJUC+k4yZHs0
8gGZAlbgxeg1TtfCbuTtv52md/Ee1irxiuCDx8iLrs5Wh43+TnsMHhxudY9X
ToKellYsJBQb4ASZZ3rXkMoZw0rE5A04yegE3/AMYlXQsfjeJgeTkeC3Op5k
/F4ncovzEVG52tRQJ6efeoqcduqp+n+dv2QbJ/lNx1bHR6Uu8NCDDxrnXLZ0
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
6i1gA0YnK40EsYhBzQVkB/kc0Sj5DIOWF+RrJDi7Vbz8JVtbm3U/eMn8ROjJ
7GMzXnhBmpuaNrv+v2/PAD+VMoKBfoVe4THg8cO+CTcayoJtfW0Yu9Oww0bF
4N2ZhP5uvU65WbNtDSfPOk9ie+1v9qbI//qLxUxFvP8bmRDIBtOPVDYMKl5C
ioWYvj+770FSPHeClO57QFSQSm5tlTRWr5Zvli6SGS/NkBt33V1O33kXOVjx
gE9hF537KyaO0HnibrvLNfr8BXvtLZNVV5104SQ5+/TTZMK551gsBT10kBsX
TDhfbrvlZrn37rvMxop/Ab3oCeUPrAP0bdY5069t9HnsqZyvzXFmcPR9XPrX
ml5X47ujS2FbrKmutuuylfpeGz7+/tJLxtPQpenn4erqhsz3gQ/E1WP3+IgY
9yDu0Gy8xtFdjTfiET0+PPcAD/QeoFZmhjoNZsvNuFpvFosYt5okzmbrfB7U
64DbOj1/7O/sU/n93rvvWtwNOi+x2k8+8bjtH3DvMWt3HFaw0fj7Hh/wSOQT
7/Xn9OknnpAD99lHf5+r8T4eaaVYVIZV5x565TVJTb1eorq/IwtMLiAf/rK9
szUFsiGyu2JB9aaQyoZBYkD0fXHVq7KKl6JycLn6WpHnXpDQxx9L/byv5Zuv
vpQnX3pBrrn9NjlZ9/69dd/eHVuqXv/9Dj5ITthpZzlz193kfJ1TdF6q84Ld
95AJO+wok3bbQy5QXF90+WVylmIDO8i0u++0PN95X38p9+h9ZAf1gT6fM0tO
OuEEwwE21C+++MJimzwuKtcizxGvsTn54Pfx3xobYBJ9jrVLzCJ6nr/uW2kY
PrBHo0uQd+r7pLk+wq5nOfFXPk8Q/m11pHWtgw/q6WLPov7CyGbwUSrnB2ZN
7oQGu226PEBX65C43VG/ufMJOlvWaHwV/sHxvBwfx5zZs2TavffIypUrjC/i
X1pTVbWJLQs84P8Yjw/s+88+9ZR+b6fbvf7GG7L7fvtJTyLhznVBeU5nl+S/
+lqy0x+T5IQLJbb/waYfIRscj96pzKMjyAXFRUjxElKZEVE8JFQuDG2/qxSV
f5dUthRuvk3CzzwrtW+/LbPffF0ee+pJufzGG+QY1Xf2DGKQ9lS5cLji9kTl
CuedfZbpRJcyVU+a8tftZKLi4WzFxTn63rMUN5O230GuVbnxzNNPyMX6vrtu
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
wjrQ9YGtixxRfFzU3UCOmH9YsZXRPRYdKedjgAP/mbdpgQH0K+IcnZ8ku0mv
w+GgVrwdB/3KZEbO7oOZETtGWBpVF8JPabXmR0ZjB4eyUTnrzNNl7z33MN0G
rr5qxUKzS9UqBwcPldhowd5bv07X8FJpVXw0bFgrXW0NMvniC+Wpxx+xczeE
rCDeP9xrewdYgKcXLN4wZDoo/GvJou+sVyv6Z3PDBpMbK5c5joENFx1u1Yql
itOl1keQ+PWXZsywunL0s/4+varSRvXyy84ONmfOnPL13YaNMeMn4aOyDiW5
mjfecL3VSW9rbba4rI72VrNpgRHskNSkthyqoO/taL56pCxH4O3YNvEVu55t
PRaPYrWEFAdM+iTwGXBCTCtxW86G5TCAnPBxKpU9RySIjaVmcM5iiNOmW/F+
dHtitwZ622XmR+9YjoXLjx80O0CUXljKh6g1hQy57pqrlO9eIO+/+4ZxC/Qn
w0RjMHUNg4n25jrVf5apLqQ6kMoR5MfDD0yTyZdcZDY093sHg3gqx8mjKqfg
HZmAk330/juyw3bbydNPPqbHVr1t7Wo7JvWqrS+O6lnL9T49NqnrvqZqtcz7
+ivD8Scff2yyY3N6VWWM4X3TppnfD32A8S9so9rS+En4YHi/IfsPvtuXXnrR
YnzJn8Inxy1xWfAR5AkyxOy4Qe33tMeI6lzEwlvPKd1HCxU+RNaPs/e7vT8b
xGmBm35dz+hC5A6Bk2Kw5/M+ly+RKHMS8y1aDYiB/7+9c/GusrzS+P/T6nRm
rZnpWg7epsvpmjWtrVWoNxSRO1JFvKCoBcG2oAXrjFIdBBUHkYttx6lCgFxP
Qi4n95CEEBJykpjbSU4SEkDHzDvv73nf9/RMDGIgkMT59lrfOiffuRjD93x7
P3s/e2/1U+CT8Amc57387yd7E2bnju36vuA7+C5yT/SZvP3WG65OaLFx97y7
lI/q6jwtfLRYf8HRymGv4yZ2yFpMFBfmmdKifFNTWap7/74Pdps7qbFcGPSz
K1x9w2lGBhxWNdtqzLz+2quKXz/av9fyl0ZxjuqKMvkOOHjc74Gq9DXA4uOF
0udSh9y8efNF+wLDrg30Isyi+qfbblOPFBbx8IvapPGBBS5CfoT73J+tf2Z+
YfdnHeLq+BKnXTyjGiL+Q5w6AyPqF2FGDfum7HUfcprkujg0cyDl4hDiMDjI
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
nIivqNeR51208CHx9Vtvvsm89uo26xtqhRF2N8Iz6ix/qq+rM9kW6xt++Uvx
afr2d+zYIV9BLBVytRP1wYKNrVu3SvtVGIvp7/NllKO6ljbt+Ag2HieJtjb1
SP+jjbeZy/Ta735nTjY2qA5/dnjQ+o129b3jU8gLuz3UoZ7itF0p+rR8nb4j
0ap5XeSOOdhb2Xr6lJ6jneR1votdi+CBnzl4D9gDh8RacGN+H/DB3sKXNr5o
9u39QLv9yEVVVsQVQ7E3+j/ef98sszEjnAE+jR8IuMjM1U40x5D3PPXkkxZ7
N+scFuVvr7nNGHwE+x/LSzLjLnZN0Ltw7z33KL/DvOidb79t6i2XpaeXejxz
Ic5YnwFPQd+V7O1Kx2DSDNtrm3M9XR1+Vpfrk6eGTw5Z7/Wfcb2Prv+RvQm9
1r/gi3jvFxdGzepHfyE/AD7YOcNuDfbCUtMrL49rPsJz655VPZSdG+hojx45
Il0ImsKL4SLkqBJeY8gO5jvvuEM+BItyVNNiMw4fwcb7E4z8DvVkrku0eosX
PWze+v12U1pabOP4hOKwPsvp2f1Mjhh/0q14iTmonTqnPYq9XcqRse8n4Cgc
5APAEu+hx4tHvgdNzNnhlPVj21STQy+4aOFC88Sax827u3aZNY8/rv1lcIsV
y5eb3bt3m/r6enFrsDFRHDUeG/AQPsOem+XLlqX9xViEjemyGYuPYAEn4/no
CRvXM9OMebHcz7lfr3pkpXnj9X8zn3zyX6aqqkJ7EwcGkto9OGxjMnJizNdm
PwM7sfqUH+v5Pwfn+sKjfT97awcHB0y79U3sgoZnwJXByHXf+Y65/rvfVf/3
0zYW2mPjJ3bJEBvhK+gJ57q/1PzboDEsLCxUHo+6eLAofzutNuPxkWlcKxPF
GalUyhTZa4s9F9y72ZfEPZjrdsGDD9h7+2qzZfNvzPbtr9v7+nvm4IH95kjW
YZObQ12iUJqOWCzf5OXm6PwfDh5QLRsssB95+dIl0iTSp45+nF5u+jDIQS20
+GT2AX6CGgX8AjyMr2Fcao4hOyTQGO7auTP9/xrlqKbdZhU+Mi1g5WL3V+7f
zJU9eOCAdlk8u/YZYYddnOgkie3pIcLvwLfJKxEf3Wljt/ss12FO8+pHHzMv
bdpksbLLZGVl6VqGD/E5cEFNj/eiDRy050Pu6VKYGK8xfOvNN83fWp+Udfiw
fveIa8wYm7X4yLRQb/w6vIw33kcPEfVJ9kgxs/yb5k5XrVwp/vOLVaukg3pu
3TrlouEM5J4uhY1MjSH9GtR8aqqr9d1RjmpG2bcCHxNZJmbC8U1jlok+ywGe
eI1cM/225HfxJRwvb9miObbfBBvEYJ2Wi5P7hc/Dy7HIb8w4+9bi45tY0Ghk
Hl9n4fo9fOiQ+d711+vaZk7nCus3iOfgH6cvsWuDejmxFfHdg/Pny3dlfndk
M8r+X+NjshZiN/JN+A/qf/S1okW/VJ6Kz4Ah5kGjR3z6qae+8r2RzTiL8DEJ
C/4FXTm5rHhZWboWfqn8LVzjiOX4aEWIz8L3RTmqGW0RPiZp4XpmJjS5WM2E
vsiOpkyN4fvW11A3+ejgQX0+0hjOCovwMUkLPIFeYfaRUbv42l0bFhtodtGm
E4thUY5q1liEj0lawAfcg1oKfRvjeQc/Uztndyy6E/gG57EIG7PKInxM0gKX
jsfj5keWg4CDiTSG7DdDs4vel/o+FuWoZp1F+JikBc4wYOMm+v+o0YOHwDWo
ZbD/j9ceWbEijYkoRzUrLcLHZVjAyLy5c83+/fvF0ZntSawVKyhQPZzelWAR
NmatRfi4DAs+AW7xm1//WjlesEHvBzkq+maxSGM46y3Cx2VYwAf6+mVLlohz
ox1Gf0svVOZ7IpvVBj7GomNyh7329Zibmzv287lzx17atGlszg03jNXV1uq8
xcu0/47RMSUHy1wjm6SFmImY6q+uu076eLRXWOQ3vnUWi47LO0ZGRope3rKl
7Ny5c/rZYmPaf6fomNIj538BoZNkxQ==
"], "Byte", ColorSpace -> "RGB", ImageResolution -> {300, 300}, ImageSize -> {46.0859375, Automatic}, Interleaving -> True], ImageSize -> Dynamic[{Automatic, 4 CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]}]];

QuantumStateEstimation /: MakeBoxes[obj : QuantumStateEstimation[info_Association], form_] := Block[{
	above, below
},
	above = {
		{
			BoxForm`SummaryItem[{"Dimension: ", info["Dimension"]}],
			BoxForm`SummaryItem[{"Possible Outcomes: ", info["MeasurementOutcomes"]}]
		}, {
			BoxForm`SummaryItem[{"Invertible: ", info["Invertible"]}],
			BoxForm`SummaryItem[{"Counts: ", info["CountsPerMeasurement"]}]
		}
	};
	below = {
		BoxForm`SummaryItem[{"Physical Inversion: ", info["PhysicalInversion"]}],
		BoxForm`SummaryItem[{"Total Counts: ", info["TotalCounts"]}],
		BoxForm`SummaryItem[{"Maximum Likelihood Acceptance Ratio: ", info["MaximumLikelihoodAcceptanceRatio"]}],
		BoxForm`SummaryItem[{"Sampler Acceptance Ratio: ", info["BayesianAcceptanceRatio"]}],
		BoxForm`SummaryItem[{"Sampler stepSize: ", info["MetropolisStepSize"]}]
	};
	BoxForm`ArrangeSummaryBox[
		QuantumStateEstimation,
		obj,
		$QuantumStateEstimationIcon,
		above,
		below,
		form,
		"Interpretable" -> True
	]
];

QuantumStateEstimation[info_Association][key_] := info[key]

