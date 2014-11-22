(* ::Package:: *)

BeginPackage["ClasslessObjects`"]


(* ::Section:: *)
(*Usage messages*)


Unprotect[ObjectQ]
ClearAll[ObjectQ]

ObjectQ::usage =
"\
ObjectQ[x] \
returns True if x is an object and False otherwise."


Unprotect[Super]
ClearAll[Super]

Super::usage =
"\
Super[obj] \
returns parent of given object obj."


Unprotect[SetSuper]
ClearAll[SetSuper]

SetSuper::usage =
"\
SetSuper[obj, super] \
sets object super as parent of object obj."


Unprotect[DeclareObject]
ClearAll[DeclareObject]

DeclareObject::usage =
"\
DeclareObject[sym] \
declares symbol sym as object.\

DeclareObject[sym, super] \
declares symbol sym as object and sets super as it's parent."


Unprotect[WithOrdinaryObjectSet]
ClearAll[WithOrdinaryObjectSet]

WithOrdinaryObjectSet::usage =
"\
WithOrdinaryObjectSet[obj, body] \
evaluates body with switched off set-altering definitions of object obj, \
i.e. definitions changing behavior of Set, SetDelayed and Unset used on obj. \
Returns result of body evaluation.\

WithOrdinaryObjectSet[{obj1, obj2, ...}, body] \
switches off set-altering definitions of all given objects obji."


Unprotect[$self]
ClearAll[$self]

$self::usage =
"\
$self \
is a variable that, in the scope of bound member, is equal to object on \
which that member was called."


Unprotect[Object]
ClearAll[Object]

Object::usage =
"\
Object \
is an ancestor of all objects."


(* ::Section:: *)
(*Implementation*)
	

Begin["`Private`"]


(* ::Subsection:: *)
(*Private symbols usage*)


ClearAll[fixArgumentsNumber]

fixArgumentsNumber::usage =
"\
fixArgumentsNumber[sym, argNo] \
assign, to symbol sym, down value printing message informing about required \
number of arguments if number of arguments is different than argNo."


ClearAll[getInheritanceChain]

getInheritanceChain::usage =
"\
getInheritanceChain[obj] \
returns list of all ancestors of given object obj.\

getInheritanceChain[obj, ances] \
returns list of ancestors of given object obj up to object ances if ances is \
one of obj ancestors. Otherwise returns list of all ancestors."


ClearAll[withBoundSelf]

withBoundSelf::usage =
"\
withBoundSelf[self, body] \
evaluates body with $self variable representing given object self. \
Returns result of body evaluation."


ClearAll[setMember]

setMember::usage =
"\
setMember[obj, lhs, rhs] \
sets two down value definitions on obj ordinary definition: obj[lhs] and \
inheritable definition obj[lhs, self_] that allows objects inheriting from \
obj to call this member."


ClearAll[bindMember]

bindMember::usage =
"\
bindMember[obj, lhs, rhs] \
sets ordinary and inheritable down value definitions of lhs with right hand \
side rhs wrapped in Block providing $self variable."


ClearAll[unsetMember]

unsetMember::usage =
"\
unsetMember[obj, member] \
unsets ordinary: obj[member] and inheritable: obj[member, self_] definitions."


(* ::Subsection:: *)
(*General messages*)


general::object = "Object expected at position `1` in `2`."

general::objects =
"Object or non-empty list of objects expected at position `1` in `2`. \
Argument contained following non-object(s): `3`."

general::nonObject = "Non-object expected at position `1` in `2`."


(* ::Subsection:: *)
(*fixArgumentsNumber*)


fixArgumentsNumber[sym_Symbol, argNo_Integer] := (
	If[argNo === 1,
		sym[args___] := "nothing" /;
			With[
				{givenArgsNo = Length[{args}]}
				,
				If[givenArgsNo =!= 1,
					Message[sym::argx, HoldForm[sym], HoldForm[givenArgsNo]]
				]
			];
	(* else *),
		sym[args___] := "nothing" /;
			With[
				{givenArgsNo = Length[{args}]}
				,
				Switch[givenArgsNo,
					argNo,
						False,
					1,
						Message[sym::argr, HoldForm[sym], HoldForm[argNo]],
					_,
						Message[sym::argrx,
							HoldForm[sym],
							HoldForm[givenArgsNo],
							HoldForm[argNo]
						]
				]
			];
	];
	
	SyntaxInformation[sym] = {"ArgumentsPattern" -> ConstantArray[_, argNo]};
)


(* ::Subsection:: *)
(*ObjectQ*)


ObjectQ[_] = False

fixArgumentsNumber[ObjectQ, 1]


(* ::Subsection:: *)
(*Super*)


Super::object = general::object


Super[_?ObjectQ] = Object

Super[arg_ /; !ObjectQ[arg]] := "nothing" /;
	Message[Super::object, HoldForm[1], HoldForm[Super[arg]]]

fixArgumentsNumber[Super, 1]


(* ::Subsection:: *)
(*getInheritanceChain*)


(*
	Using ObjectQ[ances] condition because MMA 8 chokes on
	ances:_?ObjectQ:Object
*)
getInheritanceChain[obj_?ObjectQ, ances_:Object] /; ObjectQ[ances] :=
	NestWhileList[Super, obj, !MatchQ[#, ances | Object] &]


(* ::Subsection:: *)
(*Object*)


Object::objectMember =
"The call `1`@`2` didn't match any of defined member patterns in `1` nor in \
its ancestors: `3`."


SetAttributes[Object, HoldFirst]


Object[memberCall_, self_] :=
	With[
		{ancestors = Rest[getInheritanceChain[self]]}
		,
		Message[Object::objectMember,
			HoldForm[self], HoldForm[memberCall], HoldForm[ancestors]
		];
		
		$Failed
	]

fixArgumentsNumber[Object, 2]


ObjectQ[Object] ^= True


(* ::Subsection:: *)
(*WithOrdinaryObjectSet*)


WithOrdinaryObjectSet::objects = general::objects


SetAttributes[WithOrdinaryObjectSet, HoldRest]


WithOrdinaryObjectSet[obj_?ObjectQ, body_] :=
	WithOrdinaryObjectSet[{obj}, body]

WithOrdinaryObjectSet[objs:{__?ObjectQ}, body_] :=
	Module[
		{protected, wrapper}
		,
		protected = Unprotect @@ objs;
		(*
			All "set altering" definitions are attached to objects via
			UpValues, override them temporarily.
		*)
		Scan[
			(
				UpValues[#] =
					Replace[
						UpValues[#]
						,
						RuleDelayed[
							Verbatim[HoldPattern][
								lhs:(_Set | _SetDelayed | _Unset)
							]
							,
							val_
						] :>
							RuleDelayed[HoldPattern[wrapper[lhs]], val]
						,
						{1}
					]
			)&
			,
			objs
		];
		Protect @@ protected;
		
		CheckAll[
			body
			,
			Function[
				{result, heldFlowControl}
				,
				Unprotect @@ protected;
				Scan[
					(UpValues[#] = UpValues[#] /. wrapper[lhs_] :> lhs)&,
					objs
				];
				Protect @@ protected;
				
				ReleaseHold[heldFlowControl];
				
				result
			]
		]
	]
	
WithOrdinaryObjectSet[arg1:{__} /; MemberQ[arg1, _?(!ObjectQ[#]&)], arg2_] :=
	"nothing" /;
		Message[WithOrdinaryObjectSet::objects,
			HoldForm[1],
			HoldForm[WithOrdinaryObjectSet[arg1, arg2]],
			HoldForm[Evaluate[DeleteCases[arg1, _?ObjectQ]]]
		]
	
WithOrdinaryObjectSet[arg1:Except[{__}] /; !ObjectQ[arg1], arg2_] :=
	"nothing" /;
		Message[WithOrdinaryObjectSet::objects,
			HoldForm[1],
			HoldForm[WithOrdinaryObjectSet[arg1, arg2]],
			HoldForm[arg1]
		]

fixArgumentsNumber[WithOrdinaryObjectSet, 2]


(* ::Subsection:: *)
(*SetSuper*)


SetSuper::object = general::object

SetSuper::cyclic =
"Object `1` can't inherit from object `2`, since it would lead to \
inheritance cycle `3`."


SetSuper[obj_?ObjectQ, super_?ObjectQ] := (
	With[
		{inheritanceChain = getInheritanceChain[super, obj]}
		,
		If[Last[inheritanceChain] === obj,
			Message[SetSuper::cyclic,
				HoldForm[obj], HoldForm[super], HoldForm[inheritanceChain]
			];
			Return[$Failed]
		]
	];
	
	If[super === Object,
		Quiet[
			obj /: Super[obj] =.
			,
			TagUnset::norep
		]
	(* else *),
		Super[obj] ^= super
	];
	
	WithOrdinaryObjectSet[obj,
		(* Delegate any undefinded member call to parent object. *)
		obj[x_, self_:obj] := super[x, self]
	]
)
	
SetSuper[arg1_ /; !ObjectQ[arg1], arg2_] := "nothing" /;
	Message[SetSuper::object, HoldForm[1], HoldForm[SetSuper[arg1, arg2]]]

SetSuper[arg1_, arg2_ /; !ObjectQ[arg2]] := "nothing" /;
	Message[SetSuper::object, HoldForm[2], HoldForm[SetSuper[arg1, arg2]]]

fixArgumentsNumber[SetSuper, 2]


(* ::Subsection:: *)
(*withBoundSelf*)


SetAttributes[withBoundSelf, HoldRest]


withBoundSelf[self_?ObjectQ, body_] :=
	Block[
		{$self = self}
		,
		$self /: Set[$self[lhs_], rhs_] := Set[self[lhs], rhs];
		$self /: SetDelayed[$self[lhs_], rhs_] := SetDelayed[self[lhs], rhs];
		$self /: Unset[$self[lhs_]] := Unset[self[lhs]];
		
		body
	]


(* ::Subsection:: *)
(*setMember*)


SetAttributes[setMember, HoldRest]


setMember[obj_?ObjectQ, lhs_, rhs_] :=
	WithOrdinaryObjectSet[obj,
		obj[lhs, _:obj] = rhs
	]


(* ::Subsection:: *)
(*bindMember*)


SetAttributes[bindMember, HoldRest]


bindMember[obj_?ObjectQ, lhs_, rhs_] :=
	WithOrdinaryObjectSet[obj,
		obj[lhs, self_:obj] := withBoundSelf[self, rhs]
	]


(* ::Subsection:: *)
(*unsetMember*)


SetAttributes[unsetMember, HoldRest]


unsetMember[obj_?ObjectQ, lhs_] :=
	Module[
		{norepNo = 0, countNorep, results}
		,
		If[MemberQ[Attributes[obj], Protected],
			Message[Unset::write, HoldForm[obj], HoldForm[obj@lhs]];
			Return[$Failed]
		];
		
		countNorep =
			Function[expr, Check[expr, norepNo++, Unset::norep], HoldAll];
		
		Quiet[
			WithOrdinaryObjectSet[obj,
				results = {
					(* Non-inheritable member. *)
					countNorep[obj[lhs] =.]
					,
					(* Inheritable-only unbound member. *)
					countNorep[obj[lhs, _] =.]
					,
					(* Inheritable-only bound member. *)
					countNorep[obj[lhs, self_] =.]
					,
					(* Inheritable unbound member. *)
					countNorep[obj[lhs, _:obj] =.]
					,
					(* Inheritable bound member. *)
					countNorep[obj[lhs, self_:obj] =.]
				}
			];
			,
			Unset::norep
		];
			
		If[norepNo === 5,
			(*	All Upset evaluations printed Unset::norep,
				so given member was not defined on given object. *)
			Message[Unset::norep, HoldForm[obj@lhs], HoldForm[obj]]
		];
		
		If[!MemberQ[results, Null],
			(* All Upset evaluations failed so return $Failed. *)
			$Failed
		]
	]


(* ::Subsection:: *)
(*DeclareObject*)


DeclareObject::object = general::object

DeclareObject::nonObject = general::nonObject


(*
	Using ObjectQ[super] condition because MMA 8 chokes on
	super:_?ObjectQ:Object
*)
DeclareObject[obj_Symbol /; !ObjectQ[obj], super:_:Object] /; ObjectQ[super] :=
	(
		ClearAll[obj];
		
		ObjectQ[obj] ^= True;
		SetSuper[obj, super];
		
		(* Make members created with Set automatically inheritable. *)
		obj /: Set[obj[lhs_], rhs_] := (setMember[obj, lhs, rhs]);
		
		(* Make members created with SetDelayed automatically bound. *)
		obj /: SetDelayed[obj[lhs_], rhs_] := (bindMember[obj, lhs, rhs]);
		
		(* Make sure unset removes ordinary and inheritable definitions. *)
		obj /: Unset[obj[lhs_]] := (unsetMember[obj, lhs]);
	)

DeclareObject[arg1:Except[_Symbol], Repeated[_, {0, 1}]] := "nothing" /;
	Message[DeclareObject::sym, HoldForm[arg1], HoldForm[1]]

DeclareObject[arg1_?ObjectQ, rest:Repeated[_, {0, 1}]] := "nothing" /;
	Message[DeclareObject::nonObject,
		HoldForm[1], HoldForm[DeclareObject[arg1, rest]]
	]

DeclareObject[arg1_, arg2_ /; !ObjectQ[arg2]] := "nothing" /;
	Message[DeclareObject::object, HoldForm[2], HoldForm[DeclareObject[arg1, arg2]]]

DeclareObject[args___ /; !MatchQ[Length[{args}], 1 | 2]] := "nothing" /;
	Message[DeclareObject::argt,
		HoldForm[DeclareObject],
		HoldForm[Evaluate @ Length[{args}]],
		HoldForm[1],
		HoldForm[2]
	]


SyntaxInformation[DeclareObject] = {"ArgumentsPattern" -> {_, _.}};


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
