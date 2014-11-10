(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Acceptance`MembersTwoLevelInheritance`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


DeclareObject[grandParent]
grandParent@field = 4
grandParent@self = {$self}
grandParent@addSome[i_Integer] := i + 3
grandParent@getSelf[] := {$self}

DeclareObject[parent, grandParent]

DeclareObject[obj, parent]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Non-existing member*)


Test[
	obj@field
	,
	4
	,
	TestID -> "Non-existing member: unbound on grandParent: call"
]

Test[
	obj@field =.
	,
	$Failed
	,
	Message[Unset::norep, obj[field], obj]
	,
	TestID -> "Non-existing member: unbound on grandParent: unset"
]


Test[
	obj@self
	,
	{$self}
	,
	TestID -> "Non-existing member: unbound with $self on grandParent: call"
]

Test[
	obj@self =.
	,
	$Failed
	,
	Message[Unset::norep, obj[self], obj]
	,
	TestID -> "Non-existing member: unbound with $self on grandParent: unset"
]


Test[
	obj@addSome[5]
	,
	8
	,
	TestID -> "Non-existing member: bound on grandParent: call"
]

Test[
	obj@addSome[5] =.
	,
	$Failed
	,
	Message[Unset::norep, obj[addSome[5]], obj]
	,
	TestID -> "Non-existing member: bound on grandParent: unset"
]


Test[
	obj@getSelf[]
	,
	{obj}
	,
	TestID -> "Non-existing member: bound with $self on grandParent: call"
]

Test[
	obj@getSelf[] =.
	,
	$Failed
	,
	Message[Unset::norep, obj[getSelf[]], obj]
	,
	TestID -> "Non-existing member: bound with $self on grandParent: unset"
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
	4
	,
	TestID -> "Unbound member: call unset"
]


grandParent@field =.

Test[
	obj@field
	,
	$Failed
	,
	Message[Object::objectMember, obj, field, {parent, grandParent, Object}]
	,
	TestID -> "Unbound member: call unset from self and grandParent"
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
	{$self}
	,
	TestID -> "Unbound member with $self: call unset"
]


grandParent@self =.

Test[
	obj@self
	,
	$Failed
	,
	Message[Object::objectMember, obj, self, {parent, grandParent, Object}]
	,
	TestID -> "Unbound member with $self: call unset from self and grandParent"
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
	Message[Object::objectMember,
		obj, addSome["test string"], {parent, grandParent, Object}
	]
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
	8
	,
	TestID -> "Bound member: call unset"
]


Test[
	obj@addSome[x_Real] := x + 5.1
	,
	Null
	,
	TestID -> "Bound member: set with head same as existing on grandParent"
]


Test[
	obj@addSome[1.5]
	,
	6.6
	,
	TestID ->
		"Bound member: call existing with head same as existing on grandParent"
]


grandParent@addSome[i_Integer] =.

Test[
	obj@addSome[5]
	,
	$Failed
	,
	Message[Object::objectMember,
		obj, addSome[5], {parent, grandParent, Object}
	]
	,
	TestID -> "Bound member: call unset from self and grandParent"
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
	Message[Object::objectMember,
		obj, getSelf[5], {parent, grandParent, Object}
	]
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
	{obj}
	,
	TestID -> "Bound member with $self: call unset"
]


Test[
	obj@getSelf[x_] := {$self, x}
	,
	Null
	,
	TestID -> "Bound member with $self: \
set with head same as existing on grandParent"
]


Test[
	obj@getSelf[10]
	,
	{obj, 10}
	,
	TestID -> "Bound member with $self: \
call existing with head same as existing on grandParent"
]


grandParent@getSelf[] =.

Test[
	obj@getSelf[]
	,
	$Failed
	,
	Message[Object::objectMember,
		obj, getSelf[], {parent, grandParent, Object}
	]
	,
	TestID -> "Bound member with $self: call unset from self and grandParent"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
