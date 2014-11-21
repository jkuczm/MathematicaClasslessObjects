(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Integration`MembersTwoLevelInheritance`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


setUpObjects[obj_, parent_, grandParent_] := (
	DeclareObject[grandParent];
	DeclareObject[parent, grandParent];
	DeclareObject[obj, parent];
)

setUpObjects[obj_, parent_, grandParent_, otherObj_] := (
	setUpObjects[obj, parent, grandParent];
	DeclareObject[otherObj];
)


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Non-existing member*)


Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member = grandParentValue;
	
	Test[
		obj@member
		,
		grandParentValue
		,
		TestID -> "Non-existing member: unbound on grandParent: call"
	]
]
Module[
	{obj, parent, grandParent, otherObj, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member = grandParentValue;
	
	Test[
		obj[member, otherObj]
		,
		grandParentValue
		,
		TestID ->
			"Non-existing member: unbound on grandParent: call: explicit self"
	]
]

Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member = grandParentValue;
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID -> "Non-existing member: unbound on grandParent: unset"
	]
]


Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member = {grandParentValue, $self};
	
	Test[
		obj@member
		,
		{grandParentValue, $self}
		,
		TestID ->
			"Non-existing member: unbound with $self on grandParent: call"
	]
]
Module[
	{obj, parent, grandParent, otherObj, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member = {grandParentValue, $self};
	
	Test[
		obj[member, otherObj]
		,
		{grandParentValue, $self}
		,
		TestID -> "Non-existing member: unbound with $self on grandParent: \
call: explicit self"
	]
]

Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member = {grandParentValue, $self};
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID ->
			"Non-existing member: unbound with $self on grandParent: unset"
	]
]


Module[
	{obj, parent, grandParent, member, grandParentValue, arg}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member[i_Symbol] := {grandParentValue, i};
	
	Test[
		obj@member[arg]
		,
		{grandParentValue, arg}
		,
		TestID -> "Non-existing member: bound on grandParent: call"
	]
]
Module[
	{obj, parent, grandParent, otherObj, member, grandParentValue, arg}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member[i_Symbol] := {grandParentValue, i};
	
	Test[
		obj[member[arg], otherObj]
		,
		{grandParentValue, arg}
		,
		TestID ->
			"Non-existing member: bound on grandParent: call: explicit self"
	]
]

Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member[i_Symbol] := {grandParentValue, i};
	
	Test[
		obj@member[i_Symbol] =.
		,
		$Failed
		,
		Message[Unset::norep,
			obj@member[HoldPattern[Pattern][i, HoldPattern[Blank][Symbol]]],
			obj
		]
		,
		TestID -> "Non-existing member: bound on grandParent: unset"
	]
]


Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member[] := {grandParentValue, $self};
	
	Test[
		obj@member[]
		,
		{grandParentValue, obj}
		,
		TestID -> "Non-existing member: bound with $self on grandParent: call"
	]
]
Module[
	{obj, parent, grandParent, otherObj, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member[] := {grandParentValue, $self};
	
	Test[
		obj[member[], otherObj]
		,
		{grandParentValue, otherObj}
		,
		TestID -> "Non-existing member: bound with $self on grandParent: \
call: explicit self"
	]
]

Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member[] := {grandParentValue, $self};
	
	Test[
		obj@member[] =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member[], obj]
		,
		TestID -> "Non-existing member: bound with $self on grandParent: unset"
	]
]


Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	WithOrdinaryObjectSet[grandParent,
		grandParent@member = grandParentValue
	];
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member, {parent, grandParent, Object}
		]
		,
		TestID -> "Non-existing member: non-inheritable on grandParent: call"
	]
]
Module[
	{obj, parent, grandParent, otherObj, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	WithOrdinaryObjectSet[grandParent,
		grandParent@member = grandParentValue
	];
	
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-existing member: non-inheritable on grandParent: call: \
explicit self"
	]
]

Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	WithOrdinaryObjectSet[grandParent,
		grandParent@member = grandParentValue
	];
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID -> "Non-existing member: non-inheritable on grandParent: unset"
	]
]


Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	WithOrdinaryObjectSet[grandParent,
		grandParent[member[], self_] := {grandParentValue, self}
	];
	
	Test[
		obj@member[]
		,
		{grandParentValue, obj}
		,
		TestID -> "Non-existing member: inheritable-only on grandParent: call"
	]
]
Module[
	{obj, parent, grandParent, otherObj, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	WithOrdinaryObjectSet[grandParent,
		grandParent[member[], self_] := {grandParentValue, self}
	];
	
	Test[
		obj[member[], otherObj]
		,
		{grandParentValue, otherObj}
		,
		TestID -> "Non-existing member: inheritable-only on grandParent: \
call: explicit self"
	]
]

Module[
	{obj, parent, grandParent, member, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	WithOrdinaryObjectSet[grandParent,
		grandParent[member[], self_] := {grandParentValue, self}
	];
	
	Test[
		obj@member[] =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member[], obj]
		,
		TestID -> "Non-existing member: inheritable-only on grandParent: unset"
	]
]


Module[
	{obj, parent, grandParent, member, parentValue, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member = grandParentValue;
	WithOrdinaryObjectSet[parent,
		parent@member = parentValue
	];
	
	Test[
		obj@member
		,
		grandParentValue
		,
		TestID -> "Non-existing member: \
non-inheritable on parent, inheritable on grandParent: call"
	]
]
Module[
	{obj, parent, grandParent, otherObj, member, parentValue, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member = grandParentValue;
	WithOrdinaryObjectSet[parent,
		parent@member = parentValue
	];
	
	Test[
		obj[member, otherObj]
		,
		grandParentValue
		,
		TestID -> "Non-existing member: \
non-inheritable on parent, inheritable on grandParent: call: explicit self"
	]
]

Module[
	{obj, parent, grandParent, member, parentValue, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member = grandParentValue;
	WithOrdinaryObjectSet[parent,
		parent@member = parentValue
	];
	
	Test[
		obj@member =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member, obj]
		,
		TestID -> "Non-existing member: \
non-inheritable on parent, inheritable on grandParent: unset"
	]
]


Module[
	{obj, parent, grandParent, member, parentValue, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member = grandParentValue;
	WithOrdinaryObjectSet[parent,
		parent[member[], self_] := {parentValue, self}
	];
	
	Test[
		obj@member[]
		,
		{parentValue, obj}
		,
		TestID -> "Non-existing member: \
inheritable-only on parent, inheritable on grandParent: call"
	]
]
Module[
	{obj, parent, grandParent, otherObj, member, parentValue, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member = grandParentValue;
	WithOrdinaryObjectSet[parent,
		parent[member[], self_] := {parentValue, self}
	];
	
	Test[
		obj[member[], otherObj]
		,
		{parentValue, otherObj}
		,
		TestID -> "Non-existing member: \
inheritable-only on parent, inheritable on grandParent: call: explicit self"
	]
]

Module[
	{obj, parent, grandParent, member, parentValue, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent];
	grandParent@member = grandParentValue;
	WithOrdinaryObjectSet[parent,
		parent[member[], self_] := {parentValue, self}
	];
	
	Test[
		obj@member[] =.
		,
		$Failed
		,
		Message[Unset::norep, obj@member[], obj]
		,
		TestID -> "Non-existing member: \
inheritable-only on parent, inheritable on grandParent: unset"
	]
]


(* ::Subsection:: *)
(*Unbound member*)


Module[
	{obj, parent, grandParent, otherObj, member, value, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member = grandParentValue;
	
	
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
		grandParentValue
		,
		TestID -> "Unbound member: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		grandParentValue
		,
		TestID -> "Unbound member: call unset: explicit self"
	];
	
	
	grandParent@member =.;
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member, {parent, grandParent, Object}
		]
		,
		TestID -> "Unbound member: call unset from self and grandParent"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound member: call unset from self and grandParent: \
explicit self"
	];
]


(* ::Subsection:: *)
(*Unbound member with $self*)


Module[
	{obj, parent, grandParent, otherObj, member, value, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member = {grandParentValue, $self};
	
	
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
		{grandParentValue, $self}
		,
		TestID -> "Unbound member with $self: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		{grandParentValue, $self}
		,
		TestID -> "Unbound member with $self: call unset: explicit self"
	];
	
	
	grandParent@member =.;
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member, {parent, grandParent, Object}
		]
		,
		TestID ->
			"Unbound member with $self: call unset from self and grandParent"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound member with $self: \
call unset from self and grandParent: explicit self"
	];
]


(* ::Subsection:: *)
(*Bound member*)


Module[
	{
		obj, parent, grandParent, otherObj,
		member, value, newValue, grandParentValue, arg
	}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member[i_Symbol] := {grandParentValue, i};
	
	
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
		Message[Object::objectMember,
			obj, member["test string"], {parent, grandParent, Object}
		]
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
		{grandParentValue, arg}
		,
		TestID -> "Bound member: call unset"
	];
	Test[
		obj[member[arg], otherObj]
		,
		{grandParentValue, arg}
		,
		TestID -> "Bound member: call unset: explicit self"
	];
	
	Test[
		obj@member[x_Real] := {newValue, x}
		,
		Null
		,
		TestID -> "Bound member: set with head same as existing on grandParent"
	];
	
	Test[
		obj@member[1.5]
		,
		{newValue, 1.5}
		,
		TestID -> "Bound member: \
call existing with head same as existing on grandParent"
	];
	Test[
		obj[member[1.5], otherObj]
		,
		{newValue, 1.5}
		,
		TestID -> "Bound member: \
call existing with head same as existing on grandParent: explicit self"
	];
	
	
	grandParent@member[i_Symbol] =.;
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member[arg], {parent, grandParent, Object}
		]
		,
		TestID -> "Bound member: call unset from self and grandParent"
	];
	Test[
		obj[member[arg], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg], {Object}]
		,
		TestID ->
			"Bound member: call unset from self and grandParent: explicit self"
	];
]


(* ::Subsection:: *)
(*Bound member with $self*)


Module[
	{obj, parent, grandParent, otherObj, member, value, grandParentValue, arg}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member[] := {grandParentValue, $self};
	
	
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
		Message[Object::objectMember,
			obj, member[arg], {parent, grandParent, Object}
		]
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
		{grandParentValue, obj}
		,
		TestID -> "Bound member with $self: call unset"
	];
	Test[
		obj[member[], otherObj]
		,
		{grandParentValue, otherObj}
		,
		TestID -> "Bound member with $self: call unset: explicit self"
	];
	
	Test[
		obj@member[x_] := {value, $self, x}
		,
		Null
		,
		TestID -> "Bound member with $self: \
set with head same as existing on grandParent"
	];
	
	Test[
		obj@member[arg]
		,
		{value, obj, arg}
		,
		TestID -> "Bound member with $self: \
call existing with head same as existing on grandParent"
	];
	Test[
		obj[member[arg], otherObj]
		,
		{value, otherObj, arg}
		,
		TestID -> "Bound member with $self: \
call existing with head same as existing on grandParent: explicit self"
	];
	
	
	grandParent@member[] =.;
	
	Test[
		obj@member[]
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member[], {parent, grandParent, Object}
		]
		,
		TestID ->
			"Bound member with $self: call unset from self and grandParent"
	];
	Test[
		obj[member[], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[], {Object}]
		,
		TestID -> "Bound member with $self: \
call unset from self and grandParent: explicit self"
	];
]


(* ::Subsection:: *)
(*Non-inheritable member*)


Module[
	{obj, parent, grandParent, otherObj, member, value, grandParentValue}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member = grandParentValue;
	
	
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
		grandParentValue
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
		grandParentValue
		,
		TestID -> "Non-inheritable member: call unset"
	];
	Test[
		obj[member, otherObj]
		,
		grandParentValue
		,
		TestID -> "Non-inheritable member: call unset: explicit self"
	];
	
	
	grandParent@member =.;
	
	Test[
		obj@member
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member, {parent, grandParent, Object}
		]
		,
		TestID ->
			"Non-inheritable member: call unset from self and grandParent"
	];
	Test[
		obj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable member: \
call unset from self and grandParent: explicit self"
	];
]


(* ::Subsection:: *)
(*Inheritable-only member*)


Module[
	{obj, parent, grandParent, otherObj, member, value, grandParentValue, arg}
	,
	setUpObjects[obj, parent, grandParent, otherObj];
	grandParent@member[] := {grandParentValue, $self};
	
	
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
		{grandParentValue, obj}
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
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member[arg], {parent, grandParent, Object}
		]
		,
		TestID -> "Inheritable-only member: \
call non-existing with head same as existing"
	];
	Test[
		obj[member[arg], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg], {Object}]
		,
		TestID -> "Inheritable-only member: \
call non-existing with head same as existing: explicit self"
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
		{grandParentValue, obj}
		,
		TestID -> "Inheritable-only member: call unset"
	];
	Test[
		obj[member[], otherObj]
		,
		{grandParentValue, otherObj}
		,
		TestID -> "Inheritable-only member: call unset: explicit self"
	];
	
	Test[
		WithOrdinaryObjectSet[obj,
			obj[member[x_], self_] := {value, self, x}
		]
		,
		Null
		,
		TestID -> "Inheritable-only member: \
set with head same as existing on grandParent"
	];
	
	Test[
		obj@member[arg]
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member[arg], {parent, grandParent, Object}
		]
		,
		TestID -> "Inheritable-only member: \
call existing with head same as existing on grandParent"
	];
	Test[
		obj[member[arg], otherObj]
		,
		{value, otherObj, arg}
		,
		TestID -> "Inheritable-only member: \
call existing with head same as existing on grandParent: explicit self"
	];
	
	
	grandParent@member[] =.;
	
	Test[
		obj@member[]
		,
		$Failed
		,
		Message[Object::objectMember,
			obj, member[], {parent, grandParent, Object}
		]
		,
		TestID ->
			"Inheritable-only member: call unset from self and grandParent"
	];
	Test[
		obj[member[], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[], {Object}]
		,
		TestID -> "Inheritable-only member: \
call unset from self and grandParent: explicit self"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
