(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`getInheritanceChain`", {"MUnit`"}]


Get["ClasslessObjects`"]
Get["ClasslessObjects`Tests`Unit`Utilities`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


declareMockObject[obj1]
declareMockObject[obj2, obj1]
declareMockObject[obj3, obj2]
declareMockObject[obj4, obj3]

declareMockObject[nonAncestor]


(* ::Section:: *)
(*Tests*)


Test[
	getInheritanceChain[obj4]
	,
	{obj4, obj3, obj2, obj1, Object}
	,
	TestID -> "all ancestors"
]


Test[
	getInheritanceChain[obj4, Object]
	,
	{obj4, obj3, obj2, obj1, Object}
	,
	TestID -> "ancestors up to Object"
]


Test[
	getInheritanceChain[obj4, obj2]
	,
	{obj4, obj3, obj2}
	,
	TestID -> "ancestors up to certain ancestor"
]


Test[
	getInheritanceChain[obj4, nonAncestor]
	,
	{obj4, obj3, obj2, obj1, Object}
	,
	TestID -> "ancestors up to non-ancestor object"
]


Test[
	getInheritanceChain[obj4, obj4]
	,
	{obj4}
	,
	TestID -> "ancestors up to self"
]


Test[
	getInheritanceChain[Object]
	,
	{Object}
	,
	TestID -> "ancestors of Object"
]


Test[
	getInheritanceChain[Object, Object]
	,
	{Object}
	,
	TestID -> "ancestors of Object up to Object"
]


Test[
	getInheritanceChain[Object, nonAncestor]
	,
	{Object}
	,
	TestID -> "ancestors of Object up to non-ancestor object"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
