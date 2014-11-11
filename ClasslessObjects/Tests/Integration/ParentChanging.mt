(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Integration`ParentChanging`", {"MUnit`"}]


Get["ClasslessObjects`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Start with no inheritance*)


Module[
	{child, newParent}
	,
	DeclareObject[child];
	
	DeclareObject[newParent];
	newParent@member = "newParentMember";
	
	SetSuper[child, newParent];
	
	
	Test[
		Super[child]
		,
		newParent
		,
		TestID -> "Start with no inheritance: Super"
	];
	
	Test[
		child@member
		,
		"newParentMember"
		,
		TestID -> "Start with no inheritance: member inheritance"
	];
]


(* ::Subsection:: *)
(*Start with inheritance*)


Module[
	{oldParent, child, newParent}
	,
	DeclareObject[oldParent];
	oldParent@member = "oldParentMember";
	
	DeclareObject[child, oldParent];
	
	DeclareObject[newParent];
	newParent@member = "newParentMember";
	
	SetSuper[child, newParent];
	
	
	Test[
		Super[child]
		,
		newParent
		,
		TestID -> "Start with inheritance: Super"
	];
	
	Test[
		child@member
		,
		"newParentMember"
		,
		TestID -> "Start with inheritance: member inheritance"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
