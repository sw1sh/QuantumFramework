Package["QuantumFramework`"]

PackageScope["basisElementNameLength"]
PackageScope["normalBasisElementName"]
PackageScope["symbolicTensorQ"]



basisElementNameLength[name : _TensorProduct | _CircleTimes | _List] := Length @ name

basisElementNameLength[_] := 1


normalBasisElementName[name : _TensorProduct | _CircleTimes | _List] := List @@ name

normalBasisElementName[name_] := {name}


symbolicTensorQ[a_] := MatchQ[a, _Symbol] || TensorQ[a] && AnyTrue[Level[a, {-1}], MatchQ[_Symbol]]

