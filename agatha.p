thf(x_decl,type,x : $tType).
thf(e_decl,type,e : $i).

thf(a_q_decl,type,a_q:                       x > (x > $o) > (x > $o) > $o).
thf(every_q_decl,type,every_q:               x > (x > $o) > (x > $o) > $o).
thf(some_q_decl,type,some_q:                 x > (x > $o) > (x > $o) > $o).
thf(the_q_decl,type,the_q:                   x > (x > $o) > (x > $o) > $o).
thf(proper_q_decl,type,proper_q:             x > (x > $o) > (x > $o) > $o).
thf(pronoun_q_decl,type,pronoun_q:           x > (x > $o) > (x > $o) > $o).
thf(udef_q_decl,type,udef_q:                 x > (x > $o) > (x > $o) > $o).
thf(def_explicit_q_decl,type,def_explicit_q: x > (x > $o) > (x > $o) > $o).
thf(no_q_decl,type,no_q:                     x > (x > $o) > (x > $o) > $o).
thf(colon_p_namely,type,colon_p_namely:      e > ($o) > ($o) > $o).

thf(named_people_decl,type,named_people: x > $o).
thf(place_decl,type,place: x > $o).
thf(other_object_decl,type,other_object: x > $o).

thf(a_q_decl,definition,
   a_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(every_q_decl,definition,
  every_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) & (Body @ X))))).
thf(udef_q_decl,definition,
  udef_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(no_q_decl,definition,
  no_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) & ~(Body @ X))))).
thf(some_q_decl,definition,
  some_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] :  ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(pronoun_q_decl,definition, 
  pronoun_q = ( ^[X : x, Rstr : (x > $o), Body : (x > $o)] : (? [X : x] : ((Rstr @ X) & (Body @ X))))).

thf(the_q_decl,definition,
  the_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : (((Rstr @ Y) & (Body @ Y)) => (Y = X))))))).
thf(proper_q_decl,definition,
  % proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X))) ))).
  proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : (((Rstr @ Y) & (Body @ Y)) => (Y = X))))))).
thf(def_explicit_q_decl,definition,
  def_explicit_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)))))).
thf(colon_p_namely,definition,
  colon_p_namely = (^[Event : e,Pred1 : ($o),Pred2: ($o)] : Pred2)).

include('thf-outputs/sentences.p').
% include('id-decls.p').

thf(except_p,definition,
  except_p = (^[Event : e,Included : x,Excluded : x] : (~(Included = Excluded)))).
thf(be_v_id,definition,
  be_v_id = (^[Event : e,Obj1 : x,Obj2 : x] : (Obj1 = Obj2))).
thf(neg,definition,
  neg = (^[Event : e,Cond : $o] : (~Cond))).
thf(never_a_1,definition,
  never_a_1 = (^[Cond : $o] : (~Cond))).
thf(compound,definition,
  compound = (^[Event : e,Obj1 : x, Obj2: x] : (Obj1 = Obj2))).
thf(implicit_conj,definition,
  implicit_conj = (^[Out : x,A : x, B : x] : ((Out = A) | (Out = B)))).
thf(and_c_x,definition,
  and_c_x = (^[Out : x,A : x, B : x] : ((Out = A) | (Out = B)))).

%thf(butler_n_1,definition,
%  butler_n_1 = (^[Butler : x] : $true)).
%thf(aunt_n_of,definition,
%  aunt_n_of = (^[Aunt : x] : $true)).
%thf(hate_v_1,definition,
%  hate_v_1 = (^[Event : e,Hater : x,Hated : x] : $true)).
%thf(card,definition,
%  card = (^[Event : e, Obj : x, Name: name] : $true)).
%thf(generic_entity,definition,
%  generic_entity = (^[Obj : x] : $true)).

thf(id_Agatha_named, axiom, ?[X : x] : (named @ X @ id_Agatha)).
thf(id_Aunt_named, axiom, ?[X : x] : (named @ X @ id_Aunt)).
thf(id_Charles_named, axiom, ?[X : x] : (named @ X @ id_Charles)).
thf(id_Dreadbury_named, axiom, ?[X : x] : (named @ X @ id_Dreadbury)).
thf(id_Mansion_named, axiom, ?[X : x] : (named @ X @ id_Mansion)).

thf(assert_same, axiom, ![X : x,Y : x,Name : name] : ( ((named @ X @ Name) & (named @ Y @ Name)) => (X = Y) )).
thf(assert_different, axiom, ![X : x,Y : x,NX : name, NY: name] : ( ((named @ X @ NX) & (named @ Y @ NY) & ~(NX = NY)) => (~(X = Y)) )).

thf(named_people,definition,
  named_people = (^[X : x] : ((named @ X @ id_Agatha) | (named @ X @ id_Charles)))).

thf(place,definition,
  place = (^[X : x] : ((named @ X @ id_Mansion) | (named @ X @ id_Dreadbury)))).

thf(other_object,definition,
  other_object = (^[X : x] : (named @ X @ id_1))).

thf(butler_n_1, axiom, ?[X : x] : ( ((butler_n_1 @ X) & (![Y : x] : ((butler_n_1 @ Y) => (X = Y)))) & (~(named_people @ X)) & (person @ X))).

thf(person,definition,
  person = (^[X : x] : (((butler_n_1 @ X) | (named_people @ X)) & ~((place @ X) | (other_object @ X)) ))).

thf(conj,conjecture,
  ?[
    S0_X29 : x, S0_X23 : x, S0_X16 : x, S0_X10 : x, S0_X3 : x,
    S1_X46 : x, S1_X38 : x, S1_X32 : x, S1_X24 : x, S1_X19 : x, S1_X14 : x, S1_X8 : x, S1_X3 : x,
    S2_X34 : x, S2_X28 : x, S2_X16 : x, S2_X10 : x, S2_X3 : x,
    S3_X22 : x, S3_X17 : x, S3_X9 : x, S3_X3 : x,
    S4_X15 : x, S4_X9 : x, S4_X3 : x,
    S5_X25 : x, S5_X19 : x, S5_X8 : x, S5_X3 : x,
    S6_X19 : x, S6_X14 : x, S6_X8 : x, S6_X3 : x,
    S7_X8 : x, S7_X3 : x,
    S8_X10 : x,S8_X3 : x
   ] : (
           (s0_root @ S0_X29 @ S0_X23 @ S0_X16 @ S0_X10 @ S0_X3) &
           (s1_root @ S1_X46 @ S1_X38 @ S1_X32 @ S1_X24 @ S1_X19 @ S1_X14 @ S1_X8 @ S1_X3) &
           (s2_root @ S2_X34 @ S2_X28 @ S2_X16 @ S2_X10 @ S2_X3) &
           (s3_root @ S3_X22 @ S3_X17 @ S3_X9 @ S3_X3) &
           (s4_root @ S4_X15 @ S4_X9 @ S4_X3) &
           (s5_root @ S5_X25 @ S5_X19 @ S5_X8 @ S5_X3) &
           (s6_root @ S6_X19 @ S6_X14 @ S6_X8 @ S6_X3) &
           (s7_root @ S7_X8 @ S7_X3) &
           (s8_root @ S8_X10 @ S8_X3) &
           ((S0_X23 = S1_X8) & (S0_X23 = S3_X17) & (S0_X23 = S5_X19) & (S0_X23 = S6_X14) & (S0_X23 = S8_X3)) & % Agatha
           ((S1_X19 = S4_X15) & (S1_X19 = S5_X3) & (S1_X19 = S6_X3) & (S1_X19 = S8_X10)) & % butler_n_1 
           (S1_X24 = S3_X3) & % Charles
           (S0_X16 = S1_X38) & % Dreadbury
           (S0_X10 = S1_X32) & % Mansion
           (S0_X23 = S0_X3)
     )).



