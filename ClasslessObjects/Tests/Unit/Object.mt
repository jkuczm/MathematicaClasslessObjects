(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`Object`", {"MUnit`"}]


Get["ClasslessObjects`"]
Get["ClasslessObjects`Tests`Unit`Utilities`"]


Scan[declareMockObject, {obj1, obj2, obj3}]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


TestMatch[
	Object[]
	,
	HoldPattern @ Object[]
	,
	Message[Object::argrx, Object, 0, 2]
	,
	TestID -> "no args"
]


TestMatch[
	Object[obj1]
	,
	HoldPattern @ Object[obj1]
	,
	Message[Object::argr, Object, 2]
	,
	TestID -> "1 arg"
]


TestMatch[
	Object[obj1, obj2, obj3]
	,
	HoldPattern @ Object[obj1, obj2, obj3]
	,
	Message[Object::argrx, Object, 3, 2]
	,
	TestID -> "3 args"
]


(* ::Subsection:: *)
(*Correct arguments*)


declareMockObject[grandParent]
declareMockObject[parent, grandParent]
declareMockObject[child, parent]

Test[
	Object["memberCall", child]
	,
	$Failed
	,
	Message[Object::objectMember,
		child, "memberCall", {parent, grandParent, Object}
	]
	,
	TestID -> "default member call"
]


(* ::Subsection:: *)
(*Properties*)


Test[
	ObjectQ[Object]
	,
	True
	,
	TestID -> "Properties: ObjectQ"
]


Test[
	Super[Object]
	,
	Object
	,
	TestID -> "Properties: Super"
]


(* ::Subsection:: *)
(*Messages*)


Test[
	ToString @ StringForm[Object::objectMember,
		HoldForm[child],
		HoldForm[f[arg1, arg2]],
		HoldForm[{parent, grandParent, Object}]
	]
	,
	"The call child@f[arg1, arg2] didn't match any of defined member patterns \
in child nor in its ancestors: {parent, grandParent, Object}."
	,
	TestID -> "objectMember"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
