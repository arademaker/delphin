thf(x_decl,type,x : $tType).
thf(e_decl,type,e : $tType).
thf(int_to_e_decl,type,int_to_e: $int > e).

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

thf(a_q_decl,axiom,
   a_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(every_q_decl,axiom,
  every_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) & (Body @ X))))).
thf(udef_q_decl,axiom,
  udef_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ((Rstr @ X) & (Body @ X))))).
thf(no_q_decl,axiom,
  no_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ![X : x] : ~((Rstr @ X) & (Body @ X))))).
thf(some_q_decl,axiom,
  some_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] :  ( ?[X : x] : ((Rstr @ X) & (Body @ X))))).
thf(pronoun_q_decl,axiom, 
  pronoun_q = ( ^[X : x, Rstr : (x > $o), Body : (x > $o)] : (? [X : x] : ((Rstr @ X) & (Body @ X))))).
thf(the_q_decl,axiom,
  the_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y)  & (Body @ Y) & (Y = X))))))).
thf(def_explicit_q_decl,axiom,
  def_explicit_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y)  & (Body @ Y) & (Y = X))))))).
thf(proper_q_decl,axiom,
  proper_q = (^[X : x, Rstr : (x > $o), Body : (x > $o)] : ( ?[X : x] : (((Rstr @ X)  & (Body @ X)) & (! [Y : x] : ((Rstr @ Y)  & (Body @ Y) & (Y = X))))))).
thf(colon_p_namely,axiom,
  colon_p_namely = (^[Event : e,Pred1 : ($o),Pred2: ($o)] : Pred2)).


include('thf-outputs/sentences.p').
% include('id-decls.p').

thf(id_Agatha_named, axiom, ?[X : x] : (named @ X @ id_Agatha)).
thf(id_Aunt_named, axiom, ?[X : x] : (named @ X @ id_Aunt)).
thf(id_Charles_named, axiom, ?[X : x] : (named @ X @ id_Charles)).
thf(id_Dreadbury_named, axiom, ?[X : x] : (named @ X @ id_Dreadbury)).
thf(id_Mansion_named, axiom, ?[X : x] : (named @ X @ id_Mansion)).

thf(assert_same, axiom, ![X : x,Y : x,Name : name] : ( ((named @ X @ Name) & (named @ Y @ Name)) => (X = Y) )).
thf(assert_different, axiom, ![X : x,Y : x,NX : name, NY: name] : ( ((named @ X @ NX) & (named @ Y @ NY) & ~(NX = NY)) => (~(X = Y)) )).

thf(person,definition,
  person = (^[X : x] : ((butler_n_1 @ X) | (named @ X @ id_Agatha) | (named @ X @ id_Charles)))).
        
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
thf(combined,definition,
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


%thf(compare_different_decl,type,compare_different: x > x > $o).
%thf(compare_different,definition,
%  compare_different = (^[A : x, B : x] : ((named @ A @ id_Agatha) & (named @ B @ id_Charles)))).

%thf(compare_conj,conjecture, ?[A : x,B : x] : ((compare_different @ A @ B) & ~(A = B))).

