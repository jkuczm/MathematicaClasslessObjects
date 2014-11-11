(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Integration`ObjectDeclaration`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*No inheritance*)


DeclareObject[obj]


Test[
	ObjectQ[obj]
	,
	True
	,
	TestID -> "No inheritance: ObjectQ"
]
Test[
	Super[obj]
	,
	Object
	,
	TestID -> "No inheritance: Super"
]


(* ::Subsection:: *)
(*With inheritance*)


DeclareObject[parent]
DeclareObject[child, parent]


Test[
	ObjectQ[parent]
	,
	True
	,
	TestID -> "With inheritance: parent: ObjectQ"
]
Test[
	Super[parent]
	,
	Object
	,
	TestID -> "With inheritance: parent: Super"
]


Test[
	ObjectQ[child]
	,
	True
	,
	TestID -> "With inheritance: child: ObjectQ"
]
Test[
	Super[child]
	,
	parent
	,
	TestID -> "With inheritance: child: Super"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
