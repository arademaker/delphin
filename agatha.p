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
  every_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) <=> (Body @ X))))).
thf(udef_q_decl,definition,
  udef_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(no_q_decl,definition,
  no_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) => ~(Body @ X))))).
thf(some_q_decl,definition,
  some_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] :  ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(pronoun_q_decl,definition, 
  pronoun_q = ( ^[X : x, Rstr : (x > $o), Body : (x > $o)] : (? [X : x] : ((Rstr @ X) & (Body @ X))))).

thf(the_q_decl,definition,
  the_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)) & (! [Y : x] : (((Rstr @ Y)) => ((Y = X) & (Body @ Y)))))))).
  %the_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : (((Rstr @ Y) & (Body @ Y)) => (Y = X))))))).
thf(proper_q_decl,definition,
  proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) => (Body @ X))))).
% thf(proper_q_decl,definition,
  % proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (Rstr @ X) & (Body @ X)))).
  % proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
%thf(def_explicit_q_decl,definition,
%  def_explicit_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : (((Rstr @ Y) & (Body @ Y)) => (Y = X))))))).
thf(def_explicit_q_decl,definition,
  def_explicit_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)))))).
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
%thf(hate_v_1,definition,
%  hate_v_1 = (^[Event : e,Hater : x,Hated : x] : $true)).
thf(pron,definition,
  pron = (^[Pron : x] : $true)).
thf(in_p_loc,definition,
  in_p_loc = (^[Event1 : e, Event2 : e, X : x] : $true)).
thf(live_v_1,definition,
  live_v_1 = (^[Event : e, X : x] : $true)).
thf(kill_v_1,definition,
  kill_v_1 = (^[Event : e, Killer : x, Killed : x] : $true)).
thf(therein_p_dir,definition,
  therein_p_dir = (^[Event1 : e, Event2 : e] : $true)).
thf(only_a_1,definition,
  only_a_1 = (^[Event : e, X : x] : $true)).
thf(and_c_e,definition,
  and_c_e = (^[EventOut : e,Event1 : e,Event2 : e] : $true)).
thf(poss,definition,
  poss = (^[Event : e, Possessed : x, Possessor : x] : $true)).
thf(more_comp,definition,
  more_comp = (^[Event1 : e, Event2 : e, X : x] : $true)).
thf(rich_a_in,definition,
  rich_a_in = (^[Event : e, X : x] : $true)).
thf(always_a_1,definition,
  always_a_1 = (^[Event : e] : $true)).
thf(killer_n_1,definition,
  killer_n_1 = (^[X : x] : $true)).
thf(compound,definition,
  compound = (^[Event : e,Obj1 : x, Obj2: x] : $true)).

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
  aunt_n_of = (^[X : x] : (person @ X))).

thf(person,definition,
  person = (^[X : x] : (((butler_n_1 @ X) | (named_people @ X)) & ~((place @ X) | (other_object @ X)) ))).

thf(people_n_of,definition,
  people_n_of = (^[X : x] : (person @ X))).

thf(butler_exists, axiom, ?[X : x] : ( ((butler_n_1 @ X) & (![Y : x] : ((butler_n_1 @ Y) => (X = Y)))) & (~(named_people @ X)) & (person @ X))).

thf(person_exists, axiom, ?[X : x] : ((person @ X))).

%thf(agatha_hate,axiom, (![S4_X9 : x] : (every_q @ S4_X9 @ (^[X : x] : ?[S4_X15 : x] : (s4_h16 @ S4_X15 @ X)) @ (^[X : x] : ?[S4_X3 : x] : (s4_h4 @ S4_X3 @ X))))).
thf(agatha_hate,axiom, (?[S4_X3 : x] : (?[S4_X9 : x] : (every_q @ S4_X9 @ (^[X : x] : ?[S4_X15 : x] : (s4_h16 @ S4_X15 @ X)) @ (s4_h4 @ S4_X3))))).
%thf(agatha_hate,axiom, (?[S4_X3 : x, S4_X15 : x] : (![S4_X9 : x] : (every_q @ S4_X9 @ (s4_h16 @ S4_X15) @ (s4_h4 @ S4_X3))))).
%thf(agatha_hate, axiom, (?[S4_X3 : x, S4_X15 : x, S4_X9 : x] : (s4_root @ S4_X3 @ S4_X15 @ S4_X9))).

thf(charles_hate, axiom, (?[S3_X22 : x, S3_X17 : x, S3_X9 : x, S3_X3 : x] : (s3_root @ S3_X22 @ S3_X17 @ S3_X9 @ S3_X3))).

% "Agatha hates everyone except the butler."
thf(agatha_hate_check_decl,type,agatha_hate_check: x > x > x > x > $o).
thf(agatha_hate_check, definition, 
  agatha_hate_check = (^[S1_X24 : x, S4_X3 :x , S4_X9 : x, S4_X15 : x] :
                        ((butler_n_1 @ S4_X15) &
                         (named_people @ S4_X9) &
                         ~(named_people @ S4_X15) &
                         %(named_people @ S4_X15) 

                         (hate_v_1 @ s4_e2 @ S4_X3 @ S1_X24) &
                         (hate_v_1 @ s4_e2 @ S4_X3 @ S4_X3) &
                         ~(hate_v_1 @ s4_e2 @ S4_X3 @ S4_X15)
  ))).

% "Charles hates nobody that Aunt Agatha hates." -- was "no one"
thf(charles_hate_check_decl,type,charles_hate_check: x > x > x > x > $o).
thf(charles_hate_check, definition, 
  charles_hate_check = (^[S3_X3, S3_X9, S3_X17, S3_X22] :  % Charles, Hated, Agatha, aunt_of
                         (?[AgathaHates : x] : 
                            ((named @ S3_X3 @ id_Charles) &
                             (named_people @ AgathaHates) 
                             % ~((hate_v_1 @ s3_e2 @ S3_X3 @ AgathaHates))
                            )
                         ))
  ).

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
           ((S0_X23 = S1_X8) & (S0_X23 = S3_X17) & (S0_X23 = S5_X19) & (S0_X23 = S6_X14) & (S0_X23 = S8_X3)) & % Agatha
           ((S1_X19 = S4_X15) & (S1_X19 = S5_X3) & (S1_X19 = S6_X3) & (S1_X19 = S8_X10)) & % butler_n_1 
           (S1_X24 = S3_X3) & % Charles
           (S0_X16 = S1_X38) & % Dreadbury
           (S0_X10 = S1_X32) & % Mansion

           (named @ S1_X24 @ id_Charles) &

           % "Someone who lives in Dreadbury Mansion killed Aunt Agatha."
           %(s0_root @ S0_X29 @ S0_X23 @ S0_X16 @ S0_X10 @ S0_X3) &

           % "Agatha, the butler, and Charles live in Dreadbury Mansion, and are the only people who live therein."
           %(s1_root @ S1_X46 @ S1_X38 @ S1_X32 @ S1_X24 @ S1_X19 @ S1_X14 @ S1_X8 @ S1_X3) &

           % "A killer always hates his victim, and is never richer than his victim."
           % (s2_root @ S2_X34 @ S2_X28 @ S2_X16 @ S2_X10 @ S2_X3) 

           % "Charles hates nobody that Aunt Agatha hates." -- was "no one"
           % (s3_root @ S3_X22 @ S3_X17 @ S3_X9 @ S3_X3) &

           % "Agatha hates everyone except the butler."
           (s4_root @ S4_X15 @ S4_X9 @ S4_X3) &

           % "The butler hates everyone not richer than Aunt Agatha."
           % (s5_root @ S5_X25 @ S5_X19 @ S5_X8 @ S5_X3) &

           % "The butler hates everyone Aunt Agatha hates."
           % (s6_root @ S6_X19 @ S6_X14 @ S6_X8 @ S6_X3) &

           % "No one hates everyone."
           % (s7_root @ S7_X8 @ S7_X3) &

           % "Agatha is not the butler."
           % (s8_root @ S8_X10 @ S8_X3) 
   
           (agatha_hate_check @ S1_X24 @ S4_X3 @ S4_X9 @ S4_X15)
           % (charles_hate_check @ S3_X3 @ S3_X9 @ S3_X17 @ S3_X22)

     )).



