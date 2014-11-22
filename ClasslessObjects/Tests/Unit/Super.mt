(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`Super`", {"MUnit`"}]


Get["ClasslessObjects`"]
Get["ClasslessObjects`Tests`Unit`Utilities`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


TestMatch[
	Super[]
	,
	HoldPattern @ Super[]
	,
	Message[Super::argx, Super, 0]
	,
	TestID -> "no args"
]


TestMatch[
	Super[sym]
	,
	HoldPattern @ Super[sym]
	,
	Message[Super::object, 1, Super[sym]]
	,
	TestID -> "1 arg: non-object"
]


TestMatch[
	Super[sym1, sym2]
	,
	HoldPattern @ Super[sym1, sym2]
	,
	Message[Super::argx, Super, 2]
	,
	TestID -> "2 args"
]


(* ::Subsection:: *)
(*Correct arguments*)


declareMockObject[obj]

Test[
	Super[obj]
	,
	Object
	,
	TestID -> "non-inheriting object"
]


declareMockObject[parent]
declareMockObject[child, parent]

Test[
	Super[child]
	,
	parent
	,
	TestID -> "inheriting object"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
