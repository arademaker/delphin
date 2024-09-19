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
thf(no_q_in_decl,type,no_q_in:               ($o) > ($o) > $o).
thf(colon_p_namely,type,colon_p_namely:      e > ($o) > ($o) > $o).

thf(udef_in_q_decl,type,udef_in_q:           ($o) > ($o) > $o).
thf(proper_in_q_decl,type,proper_in_q:       ($o) > ($o) > $o).

thf(named_people_decl,type,named_people: x > $o).
thf(place_decl,type,place: x > $o).
thf(other_object_decl,type,other_object: x > $o).

thf(a_q_decl,definition,
   a_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(every_q_decl,definition,
  every_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) <=> (Body @ X))))).
thf(udef_q_decl,definition,
  udef_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(udef_in_q_decl,definition,
  udef_in_q = (^[Rstr : ($o), Body : ($o)] : ( ((Rstr) & (Body))))).
thf(no_q_decl,definition,
  no_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) => ~(Body @ X))))).
thf(no_q_in_decl,definition,
  no_q_in = (^[Rstr : ($o), Body : ($o)] : ( ((Rstr) => ~(Body))))).
thf(some_q_decl,definition,
  some_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] :  ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(pronoun_q_decl,definition, 
  pronoun_q = ( ^[X : x, Rstr : (x > $o), Body : (x > $o)] : (? [X : x] : ((Rstr @ X) & (Body @ X))))).

thf(the_q_decl,definition,
  the_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)) & (! [Y : x] : (((Rstr @ Y)) => ((Y = X) & (Body @ Y)))))))).
  %the_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : (((Rstr @ Y) & (Body @ Y)) => (Y = X))))))).
thf(proper_q_decl,definition,
   proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) => (Body @ X))))).
thf(proper_in_q_decl,definition,
   proper_in_q = (^[Rstr : ($o), Body : ($o)] : ((Rstr) & (Body)))).
thf(def_explicit_q_decl,definition,
  def_explicit_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X) & (Body @ X)))))).
thf(colon_p_namely,definition,
  colon_p_namely = (^[Event : e,Pred1 : ($o),Pred2: ($o)] : Pred2)).

include('thf-outputs/sentences.p').
% include('id-decls.p').

thf(except_p,definition,
  except_p = (^[Event : e,Included : x,Excluded : x] : ~(Included = Excluded))).
thf(be_v_id,definition,
  be_v_id = (^[Event : e,Obj1 : x,Obj2 : x] : (Obj1 = Obj2))).
thf(neg,definition,
  neg = (^[Event : e,Cond : $o] : (~Cond))).
thf(never_a_1,definition,
  never_a_1 = (^[Cond : $o] : (~Cond))).
thf(implicit_conj,definition,
  implicit_conj = (^[Out : x,A : x, B : x] : ((Out = A) | (Out = B)))).
thf(and_c_x,definition,
  and_c_x = (^[Out : x,A : x, B : x] : ((Out = A) | (Out = B)))).

thf(card,definition,
  card = (^[Event : e, Obj : x, Name: name] : $true)).
thf(generic_entity,definition,
  generic_entity = (^[Obj : x] : $true)).
thf(pron,definition,
  pron = (^[Pron : x] : $true)).
thf(therein_p_dir,definition,
  therein_p_dir = (^[Event1 : e, Event2 : e] : $true)).
thf(only_a_1,definition,
  only_a_1 = (^[Event : e, X : x] : $true)).
thf(and_c_e,definition,
  and_c_e = (^[EventOut : e,Event1 : e,Event2 : e] : $true)).
thf(poss,definition,
  poss = (^[Event : e, Possessed : x, Possessor : x] : $true)).
thf(always_a_1,definition,
  always_a_1 = (^[Event : e] : $true)).
thf(compound,definition,
  compound = (^[Event : e,Obj1 : x, Obj2: x] : $true)).

%thf(more_comp,definition,
%  more_comp = (^[Event1 : e, Event2 : e, X : x] : $true)).
%thf(rich_a_in,definition,
%  rich_a_in = (^[Event : e, X : x] : $true)).

%thf(killer_n_1,definition,
%  killer_n_1 = (^[X : x] : $true)).

%thf(in_p_loc,definition,
%  in_p_loc = (^[Event1 : e, Event2 : e, X : x] : $true)).
%thf(live_v_1,definition,
%  live_v_1 = (^[Event : e, X : x] : $true)).
%thf(kill_v_1,definition,
%  kill_v_1 = (^[Event : e, Killer : x, Killed : x] : $true)).
%thf(hate_v_1,definition,
%  hate_v_1 = (^[Event : e,Hater : x,Hated : x] : $true)).

thf(victim_n_of,definition,
  victim_n_of = (^[X : x] : (person @ X))).

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
  other_object = (^[X : x] : $false)).

thf(aunt_n_of,definition,
  aunt_n_of = (^[X : x] : (named @ X @ id_Agatha))).

thf(person,definition,
  person = (^[X : x] : (((butler_n_1 @ X) | (named_people @ X)) & ~((place @ X) | (other_object @ X)) ))).

thf(people_n_of,definition,
  people_n_of = (^[X : x] : (person @ X))).

thf(butler_exists, axiom, ?[X : x] : ( ((butler_n_1 @ X) & (![Y : x] : ((butler_n_1 @ Y) => (X = Y)))) & (~(named_people @ X)) & (person @ X))).

thf(person_exists, axiom, ?[X : x] : ((person @ X))).

thf(s0_root_alt_decl,type,s0_root_alt: x > x > x > x > x > $o).
thf(s0_root_alt,definition,
  s0_root_alt =
  (^[S0_X29 : x, S0_X23: x, S0_X16 : x, S0_X10 : x, S0_X3 : x] :
    (s0_root @ S0_X29 @ S0_X23 @ S0_X16 @ S0_X10 @ S0_X3))).

thf(live_kill,type,live_kill: x > x > $o).
thf(live_kill,definition,
  live_kill = (^[Killer : x,Agatha : x] : ((person @ Killer) & (live_v_1 @ s1_e30 @ Killer) & (kill_v_1 @ s0_e2 @ Killer @ Agatha)))).

% "Someone who lives in Dreadbury Mansion killed Aunt Agatha."
thf(s0_root_alt_x,axiom,
  % S0_X29: Aunt
  % S0_X23: Agatha
  % S0_X16: Dreadbury
  % S0_X10: Mansion
  % S0_X3:  Killer
  (?[S0_X29 : x, S0_X23: x, S0_X16 : x, S0_X10 : x] : 
    ((named @ S0_X29 @ id_Aunt) &
     (named @ S0_X23 @ id_Agatha) &
     (named @ S0_X10 @ id_Mansion) &
     (named @ S0_X16 @ id_Dreadbury) & 
     (?[S0_X3 : x] : ((live_kill @ S0_X3 @ S0_X23)  =>
                       (![Y : x] : ((live_kill @ Y @ S0_X23) => ((Y = S0_X3))))))))).

thf(s0_root_alt,axiom,
  (?[S0_X29 : x, S0_X23: x, S0_X16 : x, S0_X10 : x] : 
    (?[S0_X3 : x]  : ((person @ S0_X3) & (kill_v_1 @ s0_e2 @ S0_X3 @ S0_X23))))).

% "Agatha, the butler, and Charles live in Dreadbury Mansion, and are the only people who live therein."
thf(s1_root_alt_decl,type,s1_root_alt: x > x > x > x > x > x > x > x > $o).
thf(s1_root_alt,definition,
  s1_root_alt = 
  (^[S1_X46 : x,S1_X38 : x,S1_X32 : x,S1_X24 : x,S1_X19 : x,S1_X14 : x,S1_X8 : x,S1_X3 : x] :
    (%(named @ S1_X8 @ id_Agatha) &
     %(named @ S1_X24 @ id_Charles) &
     %(butler_n_1 @ S1_X19) &

     %(named @ S1_X38 @ id_Dreadbury) &
     (named @ S1_X32 @ id_Mansion) &

     (person @ S1_X3) & % (S1_X24 | S1_X8 | S1_X19) &
     % (be_v_id @ s1_e45 @ S1_X3 @ S1_X46) &
 
     (live_v_1 @ s1_e30 @ S1_X3) 
     %(people_n_of @ S1_X46) &
     %(only_a_1 @ s1_e51 @ S1_X46) &
     %(in_p_loc @ s1_e31 @ s1_e30 @ S1_X32) &
     %(live_v_1 @ s1_e53 @ S1_X46) &
     %(and_c_e @ s1_e2 @ s1_e30 @ s1_e45)
   ))).

thf(s1_root_alt_axiom,axiom,
   (?[S1_X46 : x,S1_X38 : x,S1_X32 : x,S1_X24 : x,S1_X19 : x,S1_X14 : x,S1_X8 : x] : 
    ((named @ S1_X38 @ id_Dreadbury) &
     (named @ S1_X32 @ id_Mansion) &
     (![S1_X3 : x] : 
        (((person @ S1_X3) => 
           ((live_v_1 @ s1_e30 @ S1_X3) &
            (live_v_1 @ s1_e53 @ S1_X46)))))))).
 
% "A killer always hates his victim, and is never richer than his victim.",
thf(s2_root_alt_decl,type,s2_root_alt: x > x > x > x > x > $o).
thf(s2_root_alt,definition,
  s2_root_alt = 
  (^[S2_X34 : x, S2_X28 : x, S2_X16 : x, S2_X10 : x, S2_X3 : x] : 
    (
     (killer_n_1 @ S2_X3) &
     (victim_n_of @ S2_X28) &
     (victim_n_of @ S2_X10) &
     (S2_X28 = S2_X10) &
     (pron @ S2_X16) &
     (pron @ S2_X34) &
     (hate_v_1 @ s4_e2 @ S2_X3 @ S2_X10) &
     (~((more_comp @ s5_e18 @ s5_e16 @ S2_X28) & (rich_a_in @ s5_e16 @ S2_X3)))
    ))).

thf(s2_root_alt_axiom,axiom,
  (?[S2_X34 : x, S2_X28 : x, S2_X16 : x, S2_X10 : x, S2_X3 : x] : (s2_root_alt @ S2_X34 @ S2_X28 @ S2_X16 @ S2_X10 @ S2_X3))).

% "Charles hates nobody that Aunt Agatha hates."
thf(s3_root_alt_decl,type,s3_root_alt: x > x > x > x > $o).
thf(s3_root_alt_definition,definition,
   s3_root_alt = 
   (^[S3_X20 : x, S3_X15 : x, S3_X9 : x, S3_X3 : x] : 
    ((named @ S3_X15 @ id_Agatha) &
     (named @ S3_X3 @ id_Charles) &
     (aunt_n_of @ S3_X20) &
     ((person @ S3_X9) => (s3_root @ S3_X20 @ S3_X15 @ S3_X9 @ S3_X3))))).

thf(s3_root_axiom, axiom, 
   (?[S3_X20 : x, S3_X15 : x, S3_X3 : x] :
    ((named @ S3_X15 @ id_Agatha) &
     (named @ S3_X3 @ id_Charles) &
     (aunt_n_of @ S3_X20) &
     (![S3_X9 : x] : ((person @ S3_X9) => ((hate_v_1 @ s4_e2 @ S3_X15 @ S3_X9) => ~(hate_v_1 @ s4_e2 @ S3_X3 @ S3_X9))))))).

% "Agatha hates everyone except the butler."
thf(s4_root_alt_decl,type,s4_root_alt: x > x > x > $o).
thf(s4_root_alt,definition,
  s4_root_alt = 
  (^[S4_X15 : x,S4_X9 : x,S4_X3 : x] : 
       (((person @ S4_X9) & ~(S4_X9 = S4_X15)) <=> (hate_v_1 @ s4_e2 @ S4_X3 @ S4_X9)))).

thf(s4_root_axiom,axiom,
    (?[S4_X15 : x, S4_X3 : x] : 
       ((butler_n_1 @ S4_X15) &
        (named @ S4_X3 @ id_Agatha) &
        (![S4_X9 : x] : (((person @ S4_X9) & ~(S4_X9 = S4_X15)) <=> (hate_v_1 @ s4_e2 @ S4_X3 @ S4_X9)))))).

%thf(s4_root_axiom,axiom, (![S4_X9 : x] : (every_q @ S4_X9 @ (^[X : x] : ?[S4_X15 : x] : (s4_h16 @ S4_X15 @ X)) @ (^[X : x] : ?[S4_X3 : x] : (s4_h4 @ S4_X3 @ X))))).
%thf(s4_root_axiom,axiom, (?[S4_X3 : x, S4_X15 : x] : (![S4_X9 : x] : (every_q @ S4_X9 @ (s4_h16 @ S4_X15) @ (s4_h4 @ S4_X3))))).
%thf(s4_root_axiom, axiom, (?[S4_X3 : x, S4_X15 : x, S4_X9 : x] : (s4_root @ S4_X3 @ S4_X15 @ S4_X9))).

% "The butler hates everyone not richer than Aunt Agatha."
thf(s5_root_alt_decl,type,s5_root_alt: x > x > x > x > $o).
thf(s5_root_alt,definition,
  s5_root_alt = 
   (^[S5_X25 : x,S5_X19 : x,S5_X8 : x,S5_X3 : x] :
    ((aunt_n_of @ S5_X25) &
     (named @ S5_X19 @ id_Agatha) &
     (butler_n_1 @ S5_X3) &
     ((person @ S5_X8) =>
       ((~((more_comp @ s5_e18 @ s5_e16 @ S5_X19) & (rich_a_in @ s5_e16 @ S5_X8))) => (hate_v_1 @ s4_e2 @ S5_X3 @ S5_X8)))
   ))).

thf(s5_root_alt,axiom,
   (?[S5_X25 : x,S5_X19 : x,S5_X3 : x] :
     (![S5_X8 : x] : (s5_root_alt @ S5_X25 @ S5_X19 @ S5_X8 @ S5_X3)))).

% "The butler hates everyone Aunt Agatha hates."
thf(s6_root_alt_decl,type,s6_root_alt: x > x > x > x > $o).
thf(s6_root_alt,definition,
  s6_root_alt = 
  (^[S6_X19 : x,S6_X14 : x,S6_X8 : x,S6_X3 : x] : 
    ((aunt_n_of @ S6_X19) &
     (named @ S6_X14 @ id_Agatha) &
     (butler_n_1 @ S6_X3) &
     ((person @ S6_X8) => ((hate_v_1 @ s4_e2 @ S6_X14 @ S6_X8) => (hate_v_1 @ s4_e2 @ S6_X3 @ S6_X8)))))).

thf(s6_root_alt,axiom,
  % S6_X19: Aunt
  % S6_X14: Agatha
  % S6_X8: Hated
  % S6_X3: Butler
  (?[S6_X19 : x,S6_X14 : x,S6_X3 : x] :
    (![S6_X8 : x] : (s6_root_alt @ S6_X19 @ S6_X14 @ S6_X8 @ S6_X3)))).

thf(s7_root_alt_decl,type,s7_root_alt: x > x > $o).
thf(s7_root_alt,definition,
  (![S7_X8 : x] : ((person @ S7_X8) => ~(![S7_X3 : x] : ((person @ S7_X3) => (hate_v_1 @ s4_e2 @ S7_X8 @ S7_X3)))))).
  %s7_root_alt = (^[S7_X8 : x, S7_X3 : x]: (![S7_X8 : x] : (~(![S7_X3 : x] : ((hate_v_1 @ s4_e2 @ S7_X8 @ S7_X3))))))).
 
thf(s7_root_alt_axiom,axiom,
  (?[S7_X3 : x, S7_X8 : x] : (s7_root_alt @ S7_X3 @ S7_X8))).

thf(s8_root_alt_decl,type,s8_root_alt: x > x > $o).
thf(s8_root_alt,definition,
  s8_root_alt = (^[S8_X10 : x, S8_X3 : x] : 
    ((named @ S8_X3 @ id_Agatha) &
     (butler_n_1 @ S8_X10) &
     ~ (S8_X3 = S8_X10)))).

% "Agatha is not the butler."
thf(s8_root_alt,axiom,
  (?[S8_X10 : x , S8_X3 : x] : (s8_root_alt @ S8_X10 @ S8_X3))).

% "Agatha hates everyone except the butler."
thf(agatha_hate_check_decl,type,agatha_hate_check: x > x > x > x > $o).
thf(agatha_hate_check, definition, 
  agatha_hate_check = (^[S1_X24 : x, S4_X3 :x , S4_X9 : x, S4_X15 : x] :
                        ((butler_n_1 @ S4_X15) &
                         (named_people @ S4_X9) &
                         ~(named_people @ S4_X15) &
                         %(named_people @ S4_X15) &

                         (hate_v_1 @ s4_e2 @ S4_X3 @ S1_X24) &
                         (hate_v_1 @ s4_e2 @ S4_X3 @ S4_X3) &
                         ~(hate_v_1 @ s4_e2 @ S4_X3 @ S4_X15)
  ))).

% "Charles hates nobody that Aunt Agatha hates." -- was "no one"
thf(charles_hate_check_decl,type,charles_hate_check: x > x > x > x > $o).
thf(charles_hate_check, definition, 
  charles_hate_check = (^[S3_X3 : x, S3_X9 : x, S3_X15 : x, S3_X20 : x] :  % Charles, Hated, Agatha, aunt_n_of
                          (~((hate_v_1 @ s4_e2 @ S3_X3 @ S3_X9)))
                         )
  ).

thf(conj,conjecture,
  ?[
    S0_X29 : x, S0_X23 : x, S0_X16 : x, S0_X10 : x, S0_X3 : x,
    S1_X46 : x, S1_X38 : x, S1_X32 : x, S1_X24 : x, S1_X19 : x, S1_X14 : x, S1_X8 : x, S1_X3 : x,
    S2_X34 : x, S2_X28 : x, S2_X16 : x, S2_X10 : x, S2_X3 : x,
    S3_X20 : x, S3_X15 : x, S3_X9 : x, S3_X3 : x,
    S4_X15 : x, S4_X9 : x, S4_X3 : x,
    S5_X25 : x, S5_X19 : x, S5_X8 : x, S5_X3 : x,
    S6_X19 : x, S6_X14 : x, S6_X8 : x, S6_X3 : x,
    S7_X8 : x, S7_X3 : x,
    S8_X10 : x,S8_X3 : x
   ] : (
           (named @ S4_X3 @ id_Agatha) & 
           (butler_n_1 @ S4_X15) &
           (named @ S1_X24 @ id_Charles) &
           (aunt_n_of @ S3_X20) &
           (named @ S0_X29 @ id_Aunt) &
           (named @ S0_X16 @ id_Dreadbury) &
           (named @ S0_X10 @ id_Mansion) &
 
           ((S0_X23 = S1_X8) & (S0_X23 = S3_X15) & (S0_X23 = S4_X3) & (S0_X23 = S5_X19) & (S0_X23 = S6_X14) & (S0_X23 = S8_X3)) & % Agatha
           ((S1_X19 = S4_X15) & (S1_X19 = S5_X3) & (S1_X19 = S6_X3) & (S1_X19 = S8_X10)) & % butler_n_1 
           (S1_X24 = S3_X3) & % Charles
           (S0_X16 = S1_X38) & % Dreadbury
           (S0_X10 = S1_X32) & % Mansion
           (S3_X20 = S6_X19) & % aunt_n_of
           (S0_X29 = S5_X25) & % Aunt

           %(s0_root_alt @ S0_X29 @ S0_X23 @ S0_X16 @ S0_X10 @ S0_X3) &
           %(s1_root_alt @ S1_X46 @ S1_X38 @ S1_X32 @ S1_X24 @ S1_X19 @ S1_X14 @ S1_X8 @ S1_X3) &
           %(s2_root_alt @ S2_X34 @ S2_X28 @ S2_X16 @ S2_X10 @ S2_X3) &
           %(s3_root_alt @ S3_X20 @ S3_X15 @ S3_X9 @ S3_X3) &
           %(s4_root_alt @ S4_X15 @ S4_X9 @ S4_X3) &
           %(s5_root_alt @ S5_X25 @ S5_X19 @ S5_X8 @ S5_X3) &
           %(s6_root_alt @ S6_X19 @ S6_X14 @ S6_X8 @ S6_X3) &
           %(s7_root_alt @ S7_X8 @ S7_X3) &
           %(s8_root_alt @ S8_X10 @ S8_X3) &

           % "Someone who lives in Dreadbury Mansion killed Aunt Agatha." -- 0
           %((live_v_1 @ s1_e30 @ S0_X3) & (kill_v_1 @ s0_e2 @ S0_X3 @ S0_X23)) &

           % "Agatha, the butler, and Charles live in Dreadbury Mansion, and are the only people who live therein." -- 1
           (live_v_1 @ s1_e30 @ S1_X8) & 
           (live_v_1 @ s1_e30 @ S1_X24) & 
           (live_v_1 @ s1_e30 @ S1_X19) & 

           % "A killer always hates his victim, and is never richer than his victim." -- 2

           % "Charles hates nobody that Aunt Agatha hates." -- 3
           ~(hate_v_1 @ s4_e2 @ S3_X3 @ S3_X3) &
           ~(hate_v_1 @ s4_e2 @ S3_X3 @ S3_X15) &

           % "Agatha hates everyone except the butler." -- 4
           (hate_v_1 @ s4_e2 @ S4_X3 @ S1_X24) &
           (hate_v_1 @ s4_e2 @ S4_X3 @ S4_X3) &
           ~(hate_v_1 @ s4_e2 @ S4_X3 @ S4_X15) &

           % "The butler hates everyone not richer than Aunt Agatha." -- 5

           % "The butler hates everyone Aunt Agatha hates." -- 6
           (hate_v_1 @ s4_e2 @ S4_X15 @ S1_X24) &
           (hate_v_1 @ s4_e2 @ S4_X15 @ S4_X3) &
           ~(hate_v_1 @ s4_e2 @ S4_X15 @ S4_X15) &

           % No one hates everyone.  -- 7
           ~((hate_v_1 @ s4_e2 @ S4_X15 @ S1_X24) & (hate_v_1 @ s4_e2 @ S4_X15 @ S4_X3) & (hate_v_1 @ s4_e2 @ S4_X15 @ S4_X15)) &
           ~((hate_v_1 @ s4_e2 @ S1_X24 @ S1_X24) & (hate_v_1 @ s4_e2 @ S1_X24 @ S4_X3) & (hate_v_1 @ s4_e2 @ S1_X24 @ S4_X15)) &
           ~((hate_v_1 @ s4_e2 @ S4_X3 @ S1_X24) & (hate_v_1 @ s4_e2 @ S4_X3 @ S4_X3) & (hate_v_1 @ s4_e2 @ S4_X3 @ S4_X15)) &
  
           % "Agatha is not the butler." -- 8
           ~(S4_X3 = S4_X15) 
            
           %(kill_v_1 @ s0_e2 @ S0_X23 @ S0_X23) 
           
     )).


