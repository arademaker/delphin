thf(x_decl,type,x : $tType).
thf(e_decl,type,e : $tType).
thf(int_to_e_decl,type,int_to_e: $int > e).

thf(person,axiom,
  person = ( ^[X : x] : (named(X,s0_id_Agatha) | named(X,s1_id_Charles) | butler_n_1(X)) )).

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

% thf(s9_conjecture,conjecture,?[X : x] : s9_root(X,X)).
thf(myconj,conjecture,?[X : x] : (person(X) => named(X,s0_id_Agatha))).
