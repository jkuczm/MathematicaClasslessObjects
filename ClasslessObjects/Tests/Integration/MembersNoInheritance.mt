(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Integration`MembersNoInheritance`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Non-existing member*)


Module[
	{obj, member}
	,
	DeclareObject[obj];
	
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember, obj, member, {Object}]
		,
		TestID -> "Non-existing member: call symbol"
	]
]
Module[
	{obj, otherObj, member}
	,
	DeclareObject[obj];
	DeclareObject[otherObj];
	
	
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-existing member: call symbol: explicit self"
	]
]


Module[
	{obj, member, arg}
	,
	DeclareObject[obj];
	
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[arg], {Object}]
		,
		TestID -> "Non-existing member: call non-atomic expression"
	]
]
Module[
	{obj, otherObj, member, arg}
	,
	DeclareObject[obj];
	DeclareObject[otherObj];
	
	
	Test[
		obj[member[arg], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg], {Object}]
		,
		TestID ->
			"Non-existing member: call non-atomic expression: explicit self"
	]
]


Module[
	{obj, member}
	,
	DeclareObject[obj];
	
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID -> "Non-existing member: unset symbol"
	]
]


Module[
	{obj, member, arg}
	,
	DeclareObject[obj];
	
	
	Test[
		obj@member[arg] =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member[arg], obj]
		,
		TestID -> "Non-existing member: unset non-atomic expression"
	]
]


(* ::Subsection:: *)
(*Unbound member*)


Module[
	{obj, otherObj, member, value}
	,
	DeclareObject[obj];
	DeclareObject[otherObj];
	
	
	Test[
		obj@member = value
		,
		value
		,
		TestID -> "Unbound member: set"
	];
	
	Test[
		obj@member
		,
		value
		,
		TestID -> "Unbound member: call existing"
	];
	Test[
		obj[member, otherObj]
		,
		value
		,
		TestID -> "Unbound member: call existing: explicit self"
	];
	
	Test[
		obj@member =.
		,
		Null
		,
		TestID -> "Unbound member: unset"
	];
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember, obj, member, {Object}]
		,
		TestID -> "Unbound member: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound member: call unset: explicit self"
	];
]


(* ::Subsection:: *)
(*Unbound member with $self*)


Module[
	{obj, otherObj, member, value}
	,
	DeclareObject[obj];
	DeclareObject[otherObj];
	
	
	Test[
		obj@member = {value, $self}
		,
		{value, $self}
		,
		TestID -> "Unbound member with $self: set"
	];
	
	TestMatch[
		obj@member
		,
		HoldPattern[{value, $self}]
		,
		TestID -> "Unbound member with $self: call existing"
	];
	TestMatch[
		obj[member, otherObj]
		,
		HoldPattern[{value, $self}]
		,
		TestID -> "Unbound member with $self: call existing: explicit self"
	];
	
	Test[
		obj@member =.
		,
		Null
		,
		TestID -> "Unbound member with $self: unset"
	];
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember, obj, member, {Object}]
		,
		TestID -> "Unbound member with $self: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound member with $self: call unset: explicit self"
	];
]


(* ::Subsection:: *)
(*Bound member*)


Module[
	{obj, otherObj, member, value, arg}
	,
	DeclareObject[obj];
	DeclareObject[otherObj];
	
	
	Test[
		obj@member[i_Symbol] := {value, i}
		,
		Null
		,
		TestID -> "Bound member: set"
	];
	
	Test[
		obj@member[arg]
		,
		{value, arg}
		,
		TestID -> "Bound member: call existing"
	];
	Test[
		obj[member[arg], otherObj]
		,
		{value, arg}
		,
		TestID -> "Bound member: call existing: explicit self"
	];
	
	Test[
		obj@member["test string"]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member["test string"], {Object}]
		,
		TestID -> "Bound member: call non-existing with head same as existing"
	];
	Test[
		obj[member["test string"], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			otherObj, member["test string"], {Object}
		]
		,
		TestID -> "Bound member: \
call non-existing with head same as existing: explicit self"
	];
	
	Test[
		obj@member[i_Symbol] =.
		,
		Null
		,
		TestID -> "Bound member: unset"
	];
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[arg], {Object}]
		,
		TestID -> "Bound member: call unset"
	];
	Test[
		obj[member[arg], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg], {Object}]
		,
		TestID -> "Bound member: call unset: explicit self"
	];
]


(* ::Subsection:: *)
(*Bound member with $self*)


Module[
	{obj, otherObj, member, value, arg}
	,
	DeclareObject[obj];
	DeclareObject[otherObj];
	
	Test[
		obj@member[] := {value, $self}
		,
		Null
		,
		TestID -> "Bound member with $self: set"
	];
	
	Test[
		obj@member[]
		,
		{value, obj}
		,
		TestID -> "Bound member with $self: call existing"
	];
	Test[
		obj[member[], otherObj]
		,
		{value, otherObj}
		,
		TestID -> "Bound member with $self: call existing: explicit self"
	];
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[arg], {Object}]
		,
		TestID -> "Bound member with $self: \
call non-existing with head same as existing"
	];
	Test[
		obj[member[arg], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg], {Object}]
		,
		TestID -> "Bound member with $self: \
call non-existing with head same as existing: explicit self"
	];
	
	Test[
		obj@member[] =.
		,
		Null
		,
		TestID -> "Bound member with $self: unset"
	];
	
	Test[
		obj@member[]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[], {Object}]
		,
		TestID -> "Bound member with $self: call unset"
	];
	Test[
		obj[member[], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[], {Object}]
		,
		TestID -> "Bound member with $self: call unset: explicit self"
	];
]


(* ::Subsection:: *)
(*Non-inheritable member*)


Module[
	{obj, otherObj, member, value}
	,
	DeclareObject[obj];
	DeclareObject[otherObj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj@member = value
		]
		,
		value
		,
		TestID -> "Non-inheritable member: set"
	];
	
	Test[
		obj@member
		,
		value
		,
		TestID -> "Non-inheritable member: call existing"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable member: call existing: explicit self"
	];
	
	Test[
		obj@member =.
		,
		Null
		,
		TestID -> "Non-inheritable member: unset"
	];
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember, obj, member, {Object}]
		,
		TestID -> "Non-inheritable member: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable member: call unset: explicit self"
	];
]


(* ::Subsection:: *)
(*Inheritable-only member*)


Module[
	{obj, otherObj, member, value}
	,
	DeclareObject[obj];
	DeclareObject[otherObj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj[member[], self_] := {value, self}
		]
		,
		Null
		,
		TestID -> "Inheritable-only member: set"
	];
	
	Test[
		obj@member[]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[], {Object}]
		,
		TestID -> "Inheritable-only member: call existing"
	];
	Test[
		obj[member[], otherObj]
		,
		{value, otherObj}
		,
		TestID -> "Inheritable-only member: call existing: explicit self"
	];
	
	Test[
		obj@member[] =.
		,
		Null
		,
		TestID -> "Inheritable-only member: unset"
	];
	
	Test[
		obj@member[]
		,
		$Failed
		,
		Message[Object::objectMember, obj, member[], {Object}]
		,
		TestID -> "Inheritable-only member: call unset"
	];
	Test[
		obj[member[], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[], {Object}]
		,
		TestID -> "Inheritable-only member: call unset: explicit self"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
