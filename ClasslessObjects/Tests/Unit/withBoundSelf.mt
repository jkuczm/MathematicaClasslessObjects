(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`withBoundSelf`", {"MUnit`"}]


Get["ClasslessObjects`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsubsection:: *)
(*Returned value*)


Module[
	{obj, body}
	,
	ObjectQ[obj] ^= True;
	
	Test[
		withBoundSelf[obj, body]
		,
		body
		,
		TestID -> "Returned value"
	];
]


(* ::Subsubsection:: *)
(*$self assignment*)


Module[
	{obj, tmp}
	,
	ObjectQ[obj] ^= True;
	
	Test[
		withBoundSelf[obj, tmp = $self]
		,
		obj
		,
		TestID -> "$self assignment: withBoundSelf evaluation"
	];
	
	Test[
		tmp
		,
		obj
		,
		TestID -> "$self assignment: $self value"
	];
]


(* ::Subsubsection:: *)
(*Set altering up value*)


Module[
	{obj, member, value, $log = {}}
	,
	ObjectQ[obj] ^= True;
	obj /: (obj[lhs_] = rhs_) := AppendTo[$log, Hold[obj[lhs], rhs]];
	
	Test[
		withBoundSelf[obj, $self@member = value]
		,
		{Hold[obj[member], value]}
		,
		TestID -> "Set altering up value: withBoundSelf evaluation"
	];
	
	Test[
		$log
		,
		{Hold[obj[member], value]}
		,
		TestID -> "Set altering up value: object Set log"
	];
]


(* ::Subsubsection:: *)
(*SetDelayed altering up value*)


Module[
	{obj, member, value, $log = {}}
	,
	ObjectQ[obj] ^= True;
	obj /: (obj[lhs_] := rhs_) := AppendTo[$log, Hold[obj[lhs], rhs]];
	
	Test[
		withBoundSelf[obj, $self@member := value]
		,
		{Hold[obj[member], value]}
		,
		TestID -> "SetDelayed altering up value: withBoundSelf evaluation"
	];
	
	Test[
		$log
		,
		{Hold[obj[member], value]}
		,
		TestID -> "SetDelayed altering up value: object SetDelayed log"
	];
]


(* ::Subsubsection:: *)
(*Unset altering up value*)


Module[
	{obj, member, $log = {}}
	,
	ObjectQ[obj] ^= True;
	obj /: (obj[lhs_] =.) := AppendTo[$log, Hold[obj[lhs]]];
	
	Test[
		withBoundSelf[obj, $self@member =.]
		,
		{Hold[obj[member]]}
		,
		TestID -> "Set altering up value: withBoundSelf evaluation"
	];
	
	Test[
		$log
		,
		{Hold[obj[member]]}
		,
		TestID -> "Set altering up value: object Set log"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
