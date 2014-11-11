(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Integration`MembersNoInheritance`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


DeclareObject[obj]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Non-existing member*)


Test[
	obj@field
	,
	$Failed
	,
	Message[Object::objectMember, obj, field, {Object}]
	,
	TestID -> "Non-existing member: call symbol"
]


Test[
	obj@addSome[5]
	,
	$Failed
	,
	Message[Object::objectMember, obj, addSome[5], {Object}]
	,
	TestID -> "Non-existing member: call non-atomic expression"
]


Test[
	obj@field =.
	,
	$Failed
	,
	Message[Unset::norep, obj[field], obj]
	,
	TestID -> "Non-existing member: unset symbol"
]


Test[
	obj@addSome[5] =.
	,
	$Failed
	,
	Message[Unset::norep, obj[addSome[5]], obj]
	,
	TestID -> "Non-existing member: unset non-atomic expression"
]


(* ::Subsection:: *)
(*Unbound member*)


Test[
	obj@field = 5
	,
	5
	,
	TestID -> "Unbound member: set"
]


Test[
	obj@field
	,
	5
	,
	TestID -> "Unbound member: call existing"
]


Test[
	obj@field =.
	,
	Null
	,
	TestID -> "Unbound member: unset"
]


Test[
	obj@field
	,
	$Failed
	,
	Message[Object::objectMember, obj, field, {Object}]
	,
	TestID -> "Unbound member: call unset"
]


(* ::Subsection:: *)
(*Unbound member with $self*)


Test[
	obj@self = $self
	,
	$self
	,
	TestID -> "Unbound member with $self: set"
]


TestMatch[
	obj@self
	,
	HoldPattern[$self]
	,
	TestID -> "Unbound member with $self: call existing"
]


Test[
	obj@self =.
	,
	Null
	,
	TestID -> "Unbound member with $self: unset"
]


Test[
	obj@self
	,
	$Failed
	,
	Message[Object::objectMember, obj, self, {Object}]
	,
	TestID -> "Unbound member with $self: call unset"
]


(* ::Subsection:: *)
(*Bound member*)


Test[
	obj@addSome[i_Integer] := i + 2
	,
	Null
	,
	TestID -> "Bound member: set"
]


Test[
	obj@addSome[5]
	,
	7
	,
	TestID -> "Bound member: call existing"
]


Test[
	obj@addSome["test string"]
	,
	$Failed
	,
	Message[Object::objectMember, obj, addSome["test string"], {Object}]
	,
	TestID -> "Bound member: call non-existing with head same as existing"
]


Test[
	obj@addSome[i_Integer] =.
	,
	Null
	,
	TestID -> "Bound member: unset"
]


Test[
	obj@addSome[5]
	,
	$Failed
	,
	Message[Object::objectMember, obj, addSome[5], {Object}]
	,
	TestID -> "Bound member: call unset"
]


(* ::Subsection:: *)
(*Bound member with $self*)


Test[
	obj@getSelf[] := $self
	,
	Null
	,
	TestID -> "Bound member with $self: set"
]


Test[
	obj@getSelf[]
	,
	obj
	,
	TestID -> "Bound member with $self: call existing"
]


Test[
	obj@getSelf[5]
	,
	$Failed
	,
	Message[Object::objectMember, obj, getSelf[5], {Object}]
	,
	TestID ->
		"Bound member with $self: call non-existing with head same as existing"
]


Test[
	obj@getSelf[] =.
	,
	Null
	,
	TestID -> "Bound member with $self: unset"
]


Test[
	obj@getSelf[]
	,
	$Failed
	,
	Message[Object::objectMember, obj, getSelf[], {Object}]
	,
	TestID -> "Bound member with $self: call unset"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
