(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Acceptance`ProtectedObjects`", {"MUnit`"}]


Get["ClasslessObjects`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Set*)


Module[
	{obj, member, value}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj@member = value
		]
		,
		value
		,
		Message[Set::write, obj, obj@member]
		,
		TestID -> "Set: non-inheritable member"
	]
]


Module[
	{obj, member, value}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj[member, _] = value
		]
		,
		value
		,
		Message[Set::write, obj, obj[member, _]]
		,
		TestID -> "Set: inheritable-only member"
	]
]


Module[
	{obj, member, value}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		obj@member = value
		,
		value
		,
		Message[Set::write, obj, obj@member]
		,
		TestID -> "Set: inheritable member"
	]
]


(* ::Subsection:: *)
(*SetDelayed*)


Module[
	{obj, member, value}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj@member := value
		]
		,
		$Failed
		,
		Message[SetDelayed::write, obj, obj@member]
		,
		TestID -> "SetDelayed: non-inheritable member"
	]
]


Module[
	{obj, member, value}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj[member, _] := value
		]
		,
		$Failed
		,
		Message[SetDelayed::write, obj, obj[member, _]]
		,
		TestID -> "SetDelayed: inheritable-only member"
	]
]


Module[
	{obj, member, value}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		obj@member := value
		,
		$Failed
		,
		Message[SetDelayed::write, obj, obj@member]
		,
		TestID -> "SetDelayed: inheritable member"
	]
]


(* ::Subsection:: *)
(*Unset*)


Module[
	{obj, member}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj@member =.
		]
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID -> "Unset: non-inheritable member"
	]
]


Module[
	{obj, member}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj[member, _] =.
		]
		,
		$Failed
		,
		Message[Unset::write, obj, obj[member, _]]
		,
		TestID -> "Unset: inheritable-only member"
	]
]


Module[
	{obj, member}
	,
	DeclareObject[obj];
	Protect[obj];
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::write, obj, obj@member]
		,
		TestID -> "Unset: inheritable member"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
