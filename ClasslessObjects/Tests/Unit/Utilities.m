(* ::Package:: *)


BeginPackage["ClasslessObjects`Tests`Unit`Utilities`"]


(* ::Section:: *)
(*Usage messages*)


Unprotect[declareMockObject]
ClearAll[declareMockObject]

declareMockObject::usage =
"\
declareMockObject[sym] \
assigns True to ObjectQ[sym] and Object to Super[sym].\

declareMockObject[sym, super] \
assigns True to ObjectQ[sym] and super to Super[sym]."


Unprotect[inheritanceDownValues]
ClearAll[inheritanceDownValues]

inheritanceDownValues::usage =
"\
inheritanceDownValues[child, parent] \
returns list containing two down value rules that let child inherit members \
from parent."


Unprotect[setAlteringUpValues]
ClearAll[setAlteringUpValues]

setAlteringUpValues::usage =
"\
setAlteringUpValues[obj] \
returns list containing three up value rules that override behavior of Set, \
SetDelayed and Upset when acted on obj."


(* ::Section:: *)
(*Implementation*)
	

Begin["`Private`"]


Needs["ClasslessObjects`"]


(* ::Subsection:: *)
(*declareMockObject*)


declareMockObject[obj_] := (
	ClearAll[obj];
	
	ObjectQ[obj] ^= True;
)


declareMockObject[obj_, super_] := (
	ClearAll[obj];
	
	ObjectQ[obj] ^= True;
	Super[obj] ^= super;
)


(* ::Subsection:: *)
(*inheritanceDownValues*)


inheritanceDownValues[child_, parent_] := {
	HoldPattern[HoldPattern][
		HoldPattern[child][HoldPattern[Pattern][x_, Blank[]]]
	] :>
		parent[x_, child]
	,
	HoldPattern[HoldPattern][HoldPattern[child][
		HoldPattern[Pattern][x_, Blank[]],
		HoldPattern[Pattern][self_, Blank[]]]
	] :>
		parent[x_, self_]
}


(* ::Subsection:: *)
(*setAlteringUpValues*)


setAlteringUpValues[obj_] := {
	HoldPattern[HoldPattern][HoldPattern[Set][
		HoldPattern[obj][HoldPattern[Pattern][lhs_, Blank[]]],
		HoldPattern[Pattern][rhs_, Blank[]]
	]] :>
		ClasslessObjects`Private`setMember[obj, lhs_, rhs_]
	,
	HoldPattern[HoldPattern][HoldPattern[SetDelayed][
		HoldPattern[obj][HoldPattern[Pattern][lhs_, Blank[]]],
		HoldPattern[Pattern][rhs_, Blank[]]
	]] :>
		ClasslessObjects`Private`bindMember[obj, lhs_, rhs_]
	,
	HoldPattern[HoldPattern][HoldPattern[Unset][
		HoldPattern[obj][HoldPattern[Pattern][lhs_, Blank[]]]
	]] :>
		ClasslessObjects`Private`unsetMember[obj, lhs_]
}


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
