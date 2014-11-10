(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`ObjectQ`", {"MUnit`"}]


Get["ClasslessObjects`"]
Get["ClasslessObjects`Tests`Unit`Utilities`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


TestMatch[
	ObjectQ[]
	,
	HoldPattern @ ObjectQ[]
	,
	Message[ObjectQ::argx, ObjectQ, 0, 1]
	,
	TestID -> "no args"
]


TestMatch[
	ObjectQ[sym1, sym2]
	,
	HoldPattern @ ObjectQ[sym1, sym2]
	,
	Message[ObjectQ::argx, ObjectQ, 2, 1]
	,
	TestID -> "2 args"
]


(* ::Subsection:: *)
(*Correct arguments*)


Test[
	ObjectQ[sym]
	,
	False
	,
	TestID -> "non-object"
]


declareMockObject[obj]

Test[
	ObjectQ[obj]
	,
	True
	,
	TestID -> "object"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
