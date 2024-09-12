thf(x_decl,type,x : $tType).
thf(e_decl,type,e : $tType).
thf(int_to_e_decl,type,int_to_e: $int > e).

thf(a_q_decl,axiom, 
  a_q = ( ^[X : x, Rstr : (x > $o), Body : (x > $o)] : (? [X : x] : ((Rstr @ X) & (Body @ X))))).
thf(every_q_decl,axiom,
  every_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x, Rstr : (x > $o), Body : (x > $o)] : ((Rstr @ X) & (Body @ X))))).
thf(udef_q_decl,axiom,
  udef_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x, Rstr : (x > $o), Body : (x > $o)] : ((Rstr @ X) & (Body @ X))))).
thf(some_q_decl,axiom,
  some_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] :  ( ?[X : x, Rstr : (x > $o), Body : (x > $o)] : ((Rstr @ X) & (Body @ X))))).
thf(the_q_decl,axiom,
  the_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x, Rstr : (x > $o), Body: (x > $o)] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y)  & (Body @ Y))))))).
thf(def_explicit_q_decl,axiom,
  def_explicit_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x, Rstr : (x > $o), Body: (x > $o)] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y)  & (Body @ Y))))))).
thf(proper_q_decl,axiom,
  proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x, Rstr : (x > $o), Body: (x > $o)] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y)  & (Body @ Y))))))).
thf(pronoun_q_decl,axiom, 
  pronoun_q = ( ^[X : x, Rstr : (x > $o), Body : (x > $o)] : (? [X : x] : ((Rstr @ X) & (Body @ X))))).
thf(no_q_decl,axiom,
  no_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x, Rstr : (x > $o), Body : (x > $o)] : ~((Rstr @ X) & (Body @ X))))).
thf(never_a_1,axiom, 
  never_a1 = ( ^[Cond : (x > $o)] : (! [X : x] : ~(Cond @ X)))).
thf(neg,axiom, 
  neg = ( ^[Event : e,Cond : $o] : ~Cond)).
thf(colon_p_namely,axiom,
  colon_p_namely = (^[Event : e,Pred1 : $o,Pred2: $o] : Pred2)).

include('thf-outputs/sentences-trim.p').
% include('id-decls.p').

thf(agatha_ids, axiom,
    ((s0_id_Agatha = s1_id_Agatha) &
     (s0_id_Agatha = s3_id_Agatha) &
     (s0_id_Agatha = s4_id_Agatha) &
     (s0_id_Agatha = s5_id_Agatha) &
     (s0_id_Agatha = s6_id_Agatha) &
     (s0_id_Agatha = s8_id_Agatha) &
     (s0_id_Agatha = s9_id_Agatha) &
     (s0_id_Agatha = s0_id_Aunt) &
     (s0_id_Agatha = s5_id_Aunt))).

thf(charles_ids, axiom,
    (s1_id_Charles = s3_id_Charles)).

thf(mansion_ids, axiom,
    ((s0_id_Dreadbury = s0_id_Mansion) &
     (s0_id_Dreadbury = s1_id_Dreadbury) &
     (s0_id_Dreadbury = s1_id_Mansion))).

thf(named_match,axiom, ![X : x, Y : x, N : name] : (((named @ X @ N) & (named @ Y @ N)) => (X = Y))).
thf(named_not_match,axiom, ![X : x, Y : x, N : name] : ((~(named @ X @ N) & ~(named @ Y @ N)) => ~(X = Y))).

% thf(person,axiom,
%  person = ( ^[X : x] : (named(X,s0_id_Agatha) | named(X,s1_id_Charles) | butler_n_1(X)) )).

thf(s9_conjecture,conjecture,?[X : x] : s9_root(X,X)).
% thf(myconj,conjecture,?[X : x] : (person(X) => named(X,s0_id_Agatha))).
