

section  

universe CN

constant delegate : Type CN 
constant human : Type CN
constant report : Type CN

axiom delegate2human : delegate → human


Parameter delegate      : CN.   (* noun *)
Axiom delegate2human    : delegate -> human.
Coercion delegate2human : delegate >-> human.

Axiom report2object    : report -> object.
Coercion report2object : report >-> object.

Parameter the_report : report.  (* the specific object *)

Parameter Irish  : human -> Prop.   (* adjective *)
Record Irish_delegate : CN := { (* modified noun *)
  d       :> delegate;
  d_irish : Irish d;
}.

Parameter on_time : forall {A : CN}, (A -> Prop) -> (A -> Prop). (* adverb *)
Parameter finish : object -> human -> Prop.           (* verb *)

Check @some Irish_delegate : (Irish_delegate -> Prop) -> Prop.
Check on_time (finish the_report) : human -> Prop.

Theorem Ex25 : @some Irish_delegate (on_time (finish the_report))
               -> @some delegate (on_time (finish the_report)).
Proof.
  intros [* H]. eexists. apply H.
Qed.


