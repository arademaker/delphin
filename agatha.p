thf(x_decl,type,x : $tType).
thf(e_decl,type,e : $tType).
thf(int_to_e_decl,type,int_to_e: $int > e).

include('thf-outputs/sentences-trim.p').

thf(a_q_decl,axiom, 
  a_q = ( ^[X : x, Rstr : (x > $o), Body : (x > $o)] : (? [X : x] : ((Rstr @ X) & (Body @ X))))).
thf(every_q_decl,axiom,
  every_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x, Rstr : (x > $o), Body : (x > $o)] : ((Rstr @ X) & (Body @ X))))).
thf(udef_q_decl,axiom,
  udef_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x, Rstr : (x > $o), Body : (x > $o)] : ((Rstr @ X) & (Body @ X))))).
thf(some_q_decl,axiom,
  some_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] :  ( ?[X : x, Rstr : (x > $o), Body : (x > $o)] : ((Rstr @ X) & (Body @ X))))).
thf(the_q_decl,axiom,
  the_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x, Rstr : (x > $o), Body: (x > $o)] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y) & (Body @ Y) & (Y = X))))))).
thf(def_explicit_q_decl,axiom,
  def_explicit_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x, Rstr : (x > $o), Body: (x > $o)] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y)  & (Body @ Y) & (Y = X))))))).
thf(proper_q_decl,axiom,
  proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x, Rstr : (x > $o), Body: (x > $o)] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y)  & (Body @ Y) & (Y = X))))))).
thf(pronoun_q_decl,axiom, 
  pronoun_q = ( ^[X : x, Rstr : (x > $o), Body : (x > $o)] : (? [X : x] : ((Rstr @ X) & (Body @ X))))).
thf(no_q_decl,axiom,
  no_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x, Rstr : (x > $o), Body : (x > $o)] : ~((Rstr @ X) & (Body @ X))))).
thf(never_a_1,axiom, 
  never_a_1 = ( ^[Cond : ($o)] : ~(Cond))).
thf(neg,axiom, 
  neg = ( ^[Event : e,Cond : $o] : ~Cond)).
thf(colon_p_namely,axiom,
  colon_p_namely = (^[Event : e,Pred1 : $o,Pred2: $o] : Pred2)).

% include('id-decls.p').

thf(agatha_ids, axiom,
    ((s0_id_Agatha = s1_id_Agatha) &
     (s0_id_Agatha = s3_id_Agatha) &
     (s0_id_Agatha = s4_id_Agatha) &
     (s0_id_Agatha = s5_id_Agatha) &
     (s0_id_Agatha = s6_id_Agatha) &
     (s0_id_Agatha = s8_id_Agatha) &
     (s0_id_Agatha = s9_id_Agatha))).

thf(aunt_ids, axiom, ((s0_id_Aunt = s5_id_Aunt))).

thf(charles_ids, axiom, (s1_id_Charles = s3_id_Charles)).

thf(dreadbury_ids, axiom, (s0_id_Dreadbury = s1_id_Dreadbury)).

thf(mansion_ids, axiom, (s0_id_Mansion = s1_id_Mansion)).

thf(id_difference, axiom, ~((s0_id_Agatha = s0_id_Aunt) | (s0_id_Agatha = s1_id_Charles) | (s0_id_Agatha = s0_id_Dreadbury) | (s0_id_Agatha = s0_id_Mansion)  |
                            (s0_id_Aunt = s1_id_Charles) | (s0_id_Aunt = s0_id_Dreadbury) | (s0_id_Aunt = s0_id_Mansion) |
                            (s1_id_Charles = s0_id_Dreadbury) | (s1_id_Charles = s0_id_Mansion) |
                            (s0_id_Dreadbury = s0_id_Mansion))).

thf(s0_id_Agatha_named, axiom, ?[X : x] : (named @ X @ s0_id_Agatha)).
thf(s0_id_Aunt_named, axiom, ?[X : x] : (named @ X @ s0_id_Aunt)).
thf(s1_id_Charles_named, axiom, ?[X : x] : (named @ X @ s1_id_Charles)).
thf(s0_id_Dreadbury_named, axiom, ?[X : x] : (named @ X @ s0_id_Dreadbury)).
thf(s0_id_Mansion_named, axiom, ?[X : x] : (named @ X @ s0_id_Mansion)).

thf(assert_same, axiom, ![X : x,Y : x,Name : name] : ( ((named @ X @ Name) & (named @ Y @ Name)) => (X = Y) )).
thf(assert_different, axiom, ![X : x,Y : x,NX : name, NY: name] : ( ((named @ X @ NX) & (named @ Y @ NY) & ~(NX = NY)) => (~(X = Y)) )).

thf(person,axiom,
  person = (^[X : x] : ((butler_n_1 @ X) | (named @ X @ s0_id_Agatha) | (named @ X @ s1_id_Charles)))).
        
thf(combined_decl,type,combined: x > x > x > x > x >   
                                 x > x > x > x > x > x > x > x >
                                 x > x > x > x > x >
                                 x > x > x > x >
                                 x > x > x >
                                 x > x > x > x >
                                 x > x > x > x >
                                 x > x >
                                 x > x >
                                 % x > x > 
                                 $o).
thf(combined,axiom,
  combined = (^[S0_X29 : x, S0_X23 : x, S0_X16 : x, S0_X10 : x, S0_X3 : x,
                S1_X46 : x, S1_X38 : x, S1_X32 : x, S1_X24 : x, S1_X19 : x, S1_X14 : x, S1_X8 : x, S1_X3 : x,
                S2_X34 : x, S2_X28 : x, S2_X16 : x, S2_X10 : x, S2_X3 : x,
                S3_X22 : x, S3_X17 : x, S3_X9 : x, S3_X3 : x,
                S4_X15 : x, S4_X9 : x, S4_X3 : x,
                S5_X25 : x, S5_X19 : x, S5_X8 : x, S5_X3 : x,
                S6_X19 : x, S6_X14 : x, S6_X8 : x, S6_X3 : x,
                S7_X8 : x, S7_X3 : x,
                S8_X10 : x,S8_X3 : x
              %  S9_X20 : x,S9_X13 : x
                ] :
             ((s0_root @ S0_X29 @ S0_X23 @ S0_X16 @ S0_X10 @ S0_X3) & % X29=aunt_n_of @ X23=Agatha @ X16=Dreadbury @ X10=Mansion @ X3=personX
              (s1_root @ S1_X46 @ S1_X38 @ S1_X32 @ S1_X24 @ S1_X19 @ S1_X14 @ S1_X8 @ S1_X3) & % X46=people_n_of @ X38=Dreadbury @ X32=Mansion @ X24=Charles @ X19=butler_n_1 @ X14=Charles+butler @ X8=Agatha @ X3=Agatha+X14
              (s2_root @ S2_X34 @ S2_X28 @ S2_X16 @ S2_X10 @ S2_X3) & % X34=pron @ X28=victim_n_of @ X16=pron @ X10=victim_n_of @ X3=rich_a_in
              (s3_root @ S3_X22 @ S3_X17 @ S3_X9 @ S3_X3) & % X22=aunt_n_of @ X17=hater/Agatha @ X9=hated @ X3=Charles
              (s4_root @ S4_X15 @ S4_X9 @ S4_X3) & % X15=butler_n_1 @ X9=hated @ X3=hater
              (s5_root @ S5_X25 @ S5_X19 @ S5_X8 @ S5_X3) & % X25=Aunt @ X19=Agatha @ X8=rich_a_in @ X3=butler_n_1
              (s6_root @ S6_X19 @ S6_X14 @ S6_X8 @ S6_X3) & % X19=aunt_n_of @ X14=Agatha @ X8=hated @ X3=butler_n_1
              (s7_root @ S7_X8 @ S7_X3) & % X8=hated @ X3=hater
              (s8_root @ S8_X10 @ S8_X3) & % X10=butler_n_1 @ X3=Agatha
              %(s9_root @ S9_X20 @ S9_X13) & % X20=killed, X13=killer/Agatha
              ((S0_X23 = S1_X8) & (S0_X23 = S3_X17) & (S0_X23 = S5_X19) & (S0_X23 = S6_X14) & (S0_X23 = S8_X3)) & % (S0_X23 = S9_X13) & % Agatha
             
              (S0_X16 = S1_X38) & % Dreadbury
              (S0_X10 = S1_X32) & % Mansion

              (S1_X24 = S3_X3) & % Charles

              ((S1_X19 = S4_X15) & (S1_X19 = S5_X3) & (S1_X19 = S6_X3) & (S1_X19 = S8_X10)) % butler_n_1 
             ))).

thf(combined_conj,conjecture,
  ?[S0_X29 : x, S0_X23 : x, S0_X16 : x, S0_X10 : x, S0_X3 : x,
    S1_X46 : x, S1_X38 : x, S1_X32 : x, S1_X24 : x, S1_X19 : x, S1_X14 : x, S1_X8 : x, S1_X3 : x,
    S2_X34 : x, S2_X28 : x, S2_X16 : x, S2_X10 : x, S2_X3 : x,
    S3_X22 : x, S3_X17 : x, S3_X9 : x, S3_X3 : x,
    S4_X15 : x, S4_X9 : x, S4_X3 : x,
    S5_X25 : x, S5_X19 : x, S5_X8 : x, S5_X3 : x,
    S6_X19 : x, S6_X14 : x, S6_X8 : x, S6_X3 : x,
    S7_X8 : x, S7_X3 : x,
    S8_X10 : x,S8_X3 : x
    % S9_X20 : x,S9_X13 : x
      ] :
      ((combined @
               S0_X29 @ S0_X23 @ S0_X16 @ S0_X10 @ S0_X3 @
               S1_X46 @ S1_X38 @ S1_X32 @ S1_X24 @ S1_X19 @ S1_X14 @ S1_X8 @ S1_X3 @
               S2_X34 @ S2_X28 @ S2_X16 @ S2_X10 @ S2_X3 @
               S3_X22 @ S3_X17 @ S3_X9 @ S3_X3 @
               S4_X15 @ S4_X9 @ S4_X3 @
               S5_X25 @ S5_X19 @ S5_X8 @ S5_X3 @
               S6_X19 @ S6_X14 @ S6_X8 @ S6_X3 @
               S7_X8 @ S7_X3 @
               S8_X10 @ S8_X3 
               % @ S9_X20 @ S9_X13
               ) & (S0_X23 = S0_X3))
       ).

