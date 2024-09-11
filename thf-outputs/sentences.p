thf(x_decl,type,x : $tType).
thf(e_decl,type,e : $tType).
thf(name_decl,type,name : $i).
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
thf(never_a_1_decl,type,never_a_1:           ($o) > $o).
thf(neg_decl,type,neg:                       e > ($o) > $o).
thf(colon_p_namely,type,colon_p_namely:      e > ($o) > ($o) > $o).

thf(therein_p_dir_decl,type,therein_p_dir: e > e > $o).
thf(live_v_1_decl,type,live_v_1: e > x > $o).
thf(people_n_of_decl,type,people_n_of: x > $o).
thf(vicitm_n_of_decl,type,victim_n_of: x > $o).
thf(only_a_1_decl,type,only_a_1: e > x > $o).
thf(named_decl,type,named: x > name > $o).
thf(and_c_x_decl,type,and_c_x: x > x > x > $o).
thf(and_c_e_decl,type,and_c_e: e > e > e > $o).
thf(butler_n_1_decl,type,butler_n_1: x > $o).
thf(killer_n_1_decl,type,killer_n_1: x > $o).
thf(implicit_conj_decl,type,implicit_conj: x > x > x > $o).
thf(be_v_id_decl,type,be_v_id: e > x > x > $o).
thf(in_p_loc_decl,type,in_p_loc: e > e > x > $o).
thf(compound_decl,type,compound: e > x > x > $o).
thf(person_decl,type,person: x > $o).
thf(kill_v_1_decl,type,kill_v_1: e > x > x > $o).
thf(hate_v_1_decl,type,hate_v_1: e > x > x > $o).
thf(pron_decl,type,pron: x > $o).
thf(poss,type,poss: e > x > x > $o).
thf(more_comp,type,more_comp: e > e > x > $o).
thf(rich_a_in,type,rich_a_in: e > x > $o).
thf(always_a_1,type,always_a_1: e > $o).
thf(aunt_n_of,type,aunt_n_of: x > $o).
thf(card,type,card: e > x > name > $o).
thf(generic_entity,type,generic_entity: x > $o).
thf(except_p,type,except_p: e > x > x > $o).
thf(therefore_a_1,type,therefore_a_1: ($o) > $o).
thf(unknown,type,unknown: e > $o).

thf(s0_e8_decl,type,(s0_e8 : e)).
thf(s0_e9_decl,type,(s0_e9 : e)).
thf(s0_e15_decl,type,(s0_e15 : e)).
thf(s0_e2_decl,type,(s0_e2 : e)).
thf(s0_e28_decl,type,(s0_e28 : e)).

thf(s0_e8_value,axiom,(s0_e8 = (int_to_e @ 8))).
thf(s0_e9_value,axiom,(s0_e9 = (int_to_e @ 9))).
thf(s0_e15_value,axiom,(s0_e15 = (int_to_e @ 15))).
thf(s0_e2_value,axiom,(s0_e2 = (int_to_e @ 2))).
thf(s0_e28_value,axiom,(s0_e28 = (int_to_e @ 28))).

thf(s0_id_Agatha_decl,type,s0_id_Agatha: name).
thf(s0_id_Aunt_decl,type,s0_id_Aunt: name).
thf(s0_id_Dreadbury_decl,type,s0_id_Dreadbury: name).
thf(s0_id_Mansion_decl,type,s0_id_Mansion: name).

thf(s0_h33_decl,type,s0_h33: x > $o).
thf(s0_root_decl,type,s0_root: x > x > x > x > x > $o).
thf(s0_h27_decl,type,s0_h27: x > x > $o).
thf(s0_h24_decl,type,s0_h24: x > x > x > x > $o).
thf(s0_h20_decl,type,s0_h20: x > $o).
thf(s0_h17_decl,type,s0_h17: x > x > $o).
thf(s0_h14_decl,type,s0_h14: x > x > $o).
thf(s0_h11_decl,type,s0_h11: x > x > x > x > x > $o).
thf(s0_h5_decl,type,s0_h5: x > x > x > $o).
thf(s0_h4_decl,type,s0_h4: x > x > $o).
thf(s0_h1_decl,type,s0_h1: x > x > $o).

thf(s0_h33,axiom,
   s0_h33 = ( ^ [S0_X29 : x] : (named @ S0_X29 @ s0_id_Aunt))).
thf(s0_root,axiom,
   s0_root = ( ^ [S0_X29 : x,S0_X23 : x,S0_X16 : x,S0_X10 : x,S0_X3 : x] : (proper_q @ S0_X29 @ s0_h33 @ (s0_h11 @ S0_X23 @ S0_X16 @ S0_X10 @ S0_X3)))).
thf(s0_h27,axiom,
   s0_h27 = ( ^ [S0_X29 : x,S0_X23 : x] : ((named @ S0_X23 @ s0_id_Agatha) & (compound @ s0_e28 @ S0_X23 @ S0_X29)))).
thf(s0_h24,axiom,
   s0_h24 = ( ^ [S0_X29 : x,S0_X23 : x,S0_X3 : x,S0_X10 : x] : (proper_q @ S0_X23 @ (s0_h27 @ S0_X29) @ (s0_h5 @ S0_X10 @ S0_X3)))).
thf(s0_h20,axiom,
   s0_h20 = ( ^ [S0_X16 : x] : (named @ S0_X16 @ s0_id_Dreadbury))).
thf(s0_h17,axiom,
   s0_h17 = ( ^ [S0_X16 : x,S0_X10 : x] : (proper_q @ S0_X16 @ s0_h20 @ (s0_h14 @ S0_X10)))).
thf(s0_h14,axiom,
   s0_h14 = ( ^ [S0_X10 : x,S0_X16 : x] : ((named @ S0_X10 @ s0_id_Mansion) & (compound @ s0_e15 @ S0_X10 @ S0_X16)))).
thf(s0_h11,axiom,
   s0_h11 = ( ^ [S0_X23 : x,S0_X16 : x,S0_X10 : x,S0_X3 : x,S0_X29 : x] : (proper_q @ S0_X10 @ (s0_h17 @ S0_X16) @ (s0_h24 @ S0_X29 @ S0_X23 @ S0_X3)))).
thf(s0_h5,axiom,
   s0_h5 = ( ^ [S0_X10 : x,S0_X3 : x,S0_X23 : x] : (some_q @ S0_X3 @ (s0_h4 @ S0_X10) @ (s0_h1 @ S0_X23)))).
thf(s0_h4,axiom,
   s0_h4 = ( ^ [S0_X10 : x,S0_X3 : x] : ((in_p_loc @ s0_e9 @ s0_e8 @ S0_X10) & (live_v_1 @ s0_e8 @ S0_X3) & (person @ S0_X3)))).
thf(s0_h1,axiom,
   s0_h1 = ( ^ [S0_X23 : x,S0_X3 : x] : (kill_v_1 @ s0_e2 @ S0_X3 @ S0_X23))).
thf(s1_e30_decl,type,(s1_e30 : e)).
thf(s1_e31_decl,type,(s1_e31 : e)).
thf(s1_e37_decl,type,(s1_e37 : e)).
thf(s1_e45_decl,type,(s1_e45 : e)).
thf(s1_e2_decl,type,(s1_e2 : e)).
thf(s1_e51_decl,type,(s1_e51 : e)).
thf(s1_e53_decl,type,(s1_e53 : e)).
thf(s1_e54_decl,type,(s1_e54 : e)).

thf(s1_e30_value,axiom,(s1_e30 = (int_to_e @ 30))).
thf(s1_e31_value,axiom,(s1_e31 = (int_to_e @ 31))).
thf(s1_e37_value,axiom,(s1_e37 = (int_to_e @ 37))).
thf(s1_e45_value,axiom,(s1_e45 = (int_to_e @ 45))).
thf(s1_e2_value,axiom,(s1_e2 = (int_to_e @ 2))).
thf(s1_e51_value,axiom,(s1_e51 = (int_to_e @ 51))).
thf(s1_e53_value,axiom,(s1_e53 = (int_to_e @ 53))).
thf(s1_e54_value,axiom,(s1_e54 = (int_to_e @ 54))).

thf(s1_id_Agatha_decl,type,s1_id_Agatha: name).
thf(s1_id_Charles_decl,type,s1_id_Charles: name).
thf(s1_id_Dreadbury_decl,type,s1_id_Dreadbury: name).
thf(s1_id_Mansion_decl,type,s1_id_Mansion: name).

thf(s1_h50_decl,type,s1_h50: x > $o).
thf(s1_h47_decl,type,s1_h47: x > x > x > x > x > $o).
thf(s1_h42_decl,type,s1_h42: x > $o).
thf(s1_h39_decl,type,s1_h39: x > x > $o).
thf(s1_h36_decl,type,s1_h36: x > x > $o).
thf(s1_root_decl,type,s1_root: x > x > x > x > x > x > x > x > $o).
thf(s1_h28_decl,type,s1_h28: x > $o).
thf(s1_h25_decl,type,s1_h25: x > x > x > $o).
thf(s1_h23_decl,type,s1_h23: x > x > x > $o).
thf(s1_h22_decl,type,s1_h22: x > $o).
thf(s1_h18_decl,type,s1_h18: x > x > x > $o).
thf(s1_h17_decl,type,s1_h17: x > x > x > $o).
thf(s1_h13_decl,type,s1_h13: x > x > x > x > x > x > x > $o).
thf(s1_h11_decl,type,s1_h11: x > $o).
thf(s1_h7_decl,type,s1_h7: x > x > x > $o).
thf(s1_h4_decl,type,s1_h4: x > x > x > x > x > $o).
thf(s1_h1_decl,type,s1_h1: x > x > x > $o).

thf(s1_h50,axiom,
   s1_h50 = ( ^ [S1_X46 : x] : ((therein_p_dir @ s1_e54 @ s1_e53) & (live_v_1 @ s1_e53 @ S1_X46) & (people_n_of @ S1_X46) & (only_a_1 @ s1_e51 @ S1_X46)))).
thf(s1_h47,axiom,
   s1_h47 = ( ^ [S1_X46 : x,S1_X32 : x,S1_X8 : x,S1_X3 : x,S1_X14 : x] : (the_q @ S1_X46 @ s1_h50 @ (s1_h4 @ S1_X32 @ S1_X14 @ S1_X8 @ S1_X3)))).
thf(s1_h42,axiom,
   s1_h42 = ( ^ [S1_X38 : x] : (named @ S1_X38 @ s1_id_Dreadbury))).
thf(s1_h39,axiom,
   s1_h39 = ( ^ [S1_X38 : x,S1_X32 : x] : (proper_q @ S1_X38 @ s1_h42 @ (s1_h36 @ S1_X32)))).
thf(s1_h36,axiom,
   s1_h36 = ( ^ [S1_X32 : x,S1_X38 : x] : ((named @ S1_X32 @ s1_id_Mansion) & (compound @ s1_e37 @ S1_X32 @ S1_X38)))).
thf(s1_root,axiom,
   s1_root = ( ^ [S1_X46 : x,S1_X38 : x,S1_X32 : x,S1_X24 : x,S1_X19 : x,S1_X14 : x,S1_X8 : x,S1_X3 : x] : (proper_q @ S1_X32 @ (s1_h39 @ S1_X38) @ (s1_h13 @ S1_X46 @ S1_X24 @ S1_X19 @ S1_X14 @ S1_X8 @ S1_X3)))).
thf(s1_h28,axiom,
   s1_h28 = ( ^ [S1_X24 : x] : (named @ S1_X24 @ s1_id_Charles))).
thf(s1_h25,axiom,
   s1_h25 = ( ^ [S1_X24 : x,S1_X19 : x,S1_X14 : x] : (proper_q @ S1_X24 @ s1_h28 @ (s1_h18 @ S1_X19 @ S1_X14)))).
thf(s1_h23,axiom,
   s1_h23 = ( ^ [S1_X24 : x,S1_X14 : x,S1_X19 : x] : (and_c_x @ S1_X14 @ S1_X19 @ S1_X24))).
thf(s1_h22,axiom,
   s1_h22 = ( ^ [S1_X19 : x] : (butler_n_1 @ S1_X19))).
thf(s1_h18,axiom,
   s1_h18 = ( ^ [S1_X19 : x,S1_X14 : x,S1_X24 : x] : (the_q @ S1_X19 @ s1_h22 @ (s1_h23 @ S1_X24 @ S1_X14)))).
thf(s1_h17,axiom,
   s1_h17 = ( ^ [S1_X14 : x,S1_X3 : x,S1_X8 : x] : (implicit_conj @ S1_X3 @ S1_X8 @ S1_X14))).
thf(s1_h13,axiom,
   s1_h13 = ( ^ [S1_X46 : x,S1_X24 : x,S1_X19 : x,S1_X14 : x,S1_X8 : x,S1_X3 : x,S1_X32 : x] : (udef_q @ S1_X14 @ (s1_h25 @ S1_X24 @ S1_X19) @ (s1_h47 @ S1_X46 @ S1_X32 @ S1_X8 @ S1_X3)))).
thf(s1_h11,axiom,
   s1_h11 = ( ^ [S1_X8 : x] : (named @ S1_X8 @ s1_id_Agatha))).
thf(s1_h7,axiom,
   s1_h7 = ( ^ [S1_X14 : x,S1_X8 : x,S1_X3 : x] : (proper_q @ S1_X8 @ s1_h11 @ (s1_h17 @ S1_X14 @ S1_X3)))).
thf(s1_h4,axiom,
   s1_h4 = ( ^ [S1_X32 : x,S1_X14 : x,S1_X8 : x,S1_X3 : x,S1_X46 : x] : (udef_q @ S1_X3 @ (s1_h7 @ S1_X14 @ S1_X8) @ (s1_h1 @ S1_X46 @ S1_X32)))).
thf(s1_h1,axiom,
   s1_h1 = ( ^ [S1_X46 : x,S1_X32 : x,S1_X3 : x] : ((be_v_id @ s1_e45 @ S1_X3 @ S1_X46) & (and_c_e @ s1_e2 @ s1_e30 @ s1_e45) & (in_p_loc @ s1_e31 @ s1_e30 @ S1_X32) & (live_v_1 @ s1_e30 @ S1_X3)))).
thf(s2_e9_decl,type,(s2_e9 : e)).
thf(s2_e15_decl,type,(s2_e15 : e)).
thf(s2_e22_decl,type,(s2_e22 : e)).
thf(s2_e2_decl,type,(s2_e2 : e)).
thf(s2_e27_decl,type,(s2_e27 : e)).
thf(s2_e33_decl,type,(s2_e33 : e)).

thf(s2_e9_value,axiom,(s2_e9 = (int_to_e @ 9))).
thf(s2_e15_value,axiom,(s2_e15 = (int_to_e @ 15))).
thf(s2_e22_value,axiom,(s2_e22 = (int_to_e @ 22))).
thf(s2_e2_value,axiom,(s2_e2 = (int_to_e @ 2))).
thf(s2_e27_value,axiom,(s2_e27 = (int_to_e @ 27))).
thf(s2_e33_value,axiom,(s2_e33 = (int_to_e @ 33))).


thf(s2_h38_decl,type,s2_h38: x > $o).
thf(s2_h35_decl,type,s2_h35: x > x > x > x > $o).
thf(s2_h32_decl,type,s2_h32: x > x > $o).
thf(s2_h29_decl,type,s2_h29: x > x > x > $o).
thf(s2_h25_decl,type,s2_h25: x > x > $o).
thf(s2_h20_decl,type,s2_h20: x > $o).
thf(s2_h17_decl,type,s2_h17: x > x > $o).
thf(s2_h14_decl,type,s2_h14: x > x > $o).
thf(s2_root_decl,type,s2_root: x > x > x > x > x > $o).
thf(s2_h7_decl,type,s2_h7: x > $o).
thf(s2_h4_decl,type,s2_h4: x > x > x > x > $o).
thf(s2_h1_decl,type,s2_h1: x > x > x > x > $o).

thf(s2_h38,axiom,
   s2_h38 = ( ^ [S2_X34 : x] : (pron @ S2_X34))).
thf(s2_h35,axiom,
   s2_h35 = ( ^ [S2_X34 : x,S2_X28 : x,S2_X3 : x,S2_X10 : x] : (pronoun_q @ S2_X34 @ s2_h38 @ (s2_h4 @ S2_X28 @ S2_X10 @ S2_X3)))).
thf(s2_h32,axiom,
   s2_h32 = ( ^ [S2_X34 : x,S2_X28 : x] : ((victim_n_of @ S2_X28) & (poss @ s2_e33 @ S2_X28 @ S2_X34)))).
thf(s2_h29,axiom,
   s2_h29 = ( ^ [S2_X34 : x,S2_X28 : x,S2_X3 : x] : (def_explicit_q @ S2_X28 @ (s2_h32 @ S2_X34) @ (s2_h25 @ S2_X3)))).
thf(s2_h25,axiom,
   s2_h25 = ( ^ [S2_X3 : x,S2_X28 : x] : ((more_comp @ s2_e27 @ s2_e22 @ S2_X28) & (rich_a_in @ s2_e22 @ S2_X3)))).
thf(s2_h20,axiom,
   s2_h20 = ( ^ [S2_X16 : x] : (pron @ S2_X16))).
thf(s2_h17,axiom,
   s2_h17 = ( ^ [S2_X16 : x,S2_X10 : x] : (pronoun_q @ S2_X16 @ s2_h20 @ (s2_h14 @ S2_X10)))).
thf(s2_h14,axiom,
   s2_h14 = ( ^ [S2_X10 : x,S2_X16 : x] : ((victim_n_of @ S2_X10) & (poss @ s2_e15 @ S2_X10 @ S2_X16)))).
thf(s2_root,axiom,
   s2_root = ( ^ [S2_X34 : x,S2_X28 : x,S2_X16 : x,S2_X10 : x,S2_X3 : x] : (def_explicit_q @ S2_X10 @ (s2_h17 @ S2_X16) @ (s2_h35 @ S2_X34 @ S2_X28 @ S2_X3)))).
thf(s2_h7,axiom,
   s2_h7 = ( ^ [S2_X3 : x] : (killer_n_1 @ S2_X3))).
thf(s2_h4,axiom,
   s2_h4 = ( ^ [S2_X28 : x,S2_X10 : x,S2_X3 : x,S2_X34 : x] : (a_q @ S2_X3 @ s2_h7 @ (s2_h1 @ S2_X34 @ S2_X28 @ S2_X10)))).
thf(s2_h1,axiom,
   s2_h1 = ( ^ [S2_X34 : x,S2_X28 : x,S2_X10 : x,S2_X3 : x] : ((never_a_1 @ (s2_h29 @ S2_X34 @ S2_X28 @ S2_X3)) & (and_c_e @ s2_e2 @ s2_e9 @ s2_e22) & (hate_v_1 @ s2_e9 @ S2_X3 @ S2_X10) & (always_a_1 @ s2_e9)))).
thf(s3_e2_decl,type,(s3_e2 : e)).
thf(s3_e15_decl,type,(s3_e15 : e)).
thf(s3_e21_decl,type,(s3_e21 : e)).
thf(s3_e29_decl,type,(s3_e29 : e)).

thf(s3_e2_value,axiom,(s3_e2 = (int_to_e @ 2))).
thf(s3_e15_value,axiom,(s3_e15 = (int_to_e @ 15))).
thf(s3_e21_value,axiom,(s3_e21 = (int_to_e @ 21))).
thf(s3_e29_value,axiom,(s3_e29 = (int_to_e @ 29))).

thf(s3_id_1_decl,type,s3_id_1: name).
thf(s3_id_Agatha_decl,type,s3_id_Agatha: name).
thf(s3_id_Charles_decl,type,s3_id_Charles: name).

thf(s3_h26_decl,type,s3_h26: x > $o).
thf(s3_h23_decl,type,s3_h23: x > x > x > $o).
thf(s3_h20_decl,type,s3_h20: x > x > $o).
thf(s3_h16_decl,type,s3_h16: x > x > x > $o).
thf(s3_h13_decl,type,s3_h13: x > x > $o).
thf(s3_root_decl,type,s3_root: x > x > x > x > $o).
thf(s3_h7_decl,type,s3_h7: x > $o).
thf(s3_h4_decl,type,s3_h4: x > x > $o).
thf(s3_h1_decl,type,s3_h1: x > x > $o).

thf(s3_h26,axiom,
   s3_h26 = ( ^ [S3_X22 : x] : (aunt_n_of @ S3_X22))).
thf(s3_h23,axiom,
   s3_h23 = ( ^ [S3_X22 : x,S3_X17 : x,S3_X9 : x] : (udef_q @ S3_X22 @ s3_h26 @ (s3_h16 @ S3_X17 @ S3_X9)))).
thf(s3_h20,axiom,
   s3_h20 = ( ^ [S3_X22 : x,S3_X17 : x] : ((named @ S3_X17 @ s3_id_Agatha) & (compound @ s3_e21 @ S3_X17 @ S3_X22)))).
thf(s3_h16,axiom,
   s3_h16 = ( ^ [S3_X17 : x,S3_X9 : x,S3_X22 : x] : (proper_q @ S3_X17 @ (s3_h20 @ S3_X22) @ (s3_h13 @ S3_X9)))).
thf(s3_h13,axiom,
   s3_h13 = ( ^ [S3_X9 : x,S3_X17 : x] : ((hate_v_1 @ s3_e29 @ S3_X17 @ S3_X9) & (card @ s3_e15 @ S3_X9 @ s3_id_1) & (generic_entity @ S3_X9)))).
thf(s3_root,axiom,
   s3_root = ( ^ [S3_X22 : x,S3_X17 : x,S3_X9 : x,S3_X3 : x] : (no_q @ S3_X9 @ (s3_h23 @ S3_X22 @ S3_X17) @ (s3_h4 @ S3_X3)))).
thf(s3_h7,axiom,
   s3_h7 = ( ^ [S3_X3 : x] : (named @ S3_X3 @ s3_id_Charles))).
thf(s3_h4,axiom,
   s3_h4 = ( ^ [S3_X3 : x,S3_X9 : x] : (proper_q @ S3_X3 @ s3_h7 @ (s3_h1 @ S3_X9)))).
thf(s3_h1,axiom,
   s3_h1 = ( ^ [S3_X9 : x,S3_X3 : x] : (hate_v_1 @ s3_e2 @ S3_X3 @ S3_X9))).
thf(s4_e2_decl,type,(s4_e2 : e)).
thf(s4_e14_decl,type,(s4_e14 : e)).

thf(s4_e2_value,axiom,(s4_e2 = (int_to_e @ 2))).
thf(s4_e14_value,axiom,(s4_e14 = (int_to_e @ 14))).

thf(s4_id_Agatha_decl,type,s4_id_Agatha: name).

thf(s4_h19_decl,type,s4_h19: x > $o).
thf(s4_h16_decl,type,s4_h16: x > x > $o).
thf(s4_root_decl,type,s4_root: x > x > x > $o).
thf(s4_h10_decl,type,s4_h10: x > x > $o).
thf(s4_h7_decl,type,s4_h7: x > $o).
thf(s4_h4_decl,type,s4_h4: x > x > $o).
thf(s4_h1_decl,type,s4_h1: x > x > $o).

thf(s4_h19,axiom,
   s4_h19 = ( ^ [S4_X15 : x] : (butler_n_1 @ S4_X15))).
thf(s4_h16,axiom,
   s4_h16 = ( ^ [S4_X15 : x,S4_X9 : x] : (the_q @ S4_X15 @ s4_h19 @ (s4_h10 @ S4_X9)))).
thf(s4_root,axiom,
   s4_root = ( ^ [S4_X15 : x,S4_X9 : x,S4_X3 : x] : (every_q @ S4_X9 @ (s4_h16 @ S4_X15) @ (s4_h4 @ S4_X3)))).
thf(s4_h10,axiom,
   s4_h10 = ( ^ [S4_X9 : x,S4_X15 : x] : ((except_p @ s4_e14 @ S4_X9 @ S4_X15) & (person @ S4_X9)))).
thf(s4_h7,axiom,
   s4_h7 = ( ^ [S4_X3 : x] : (named @ S4_X3 @ s4_id_Agatha))).
thf(s4_h4,axiom,
   s4_h4 = ( ^ [S4_X3 : x,S4_X9 : x] : (proper_q @ S4_X3 @ s4_h7 @ (s4_h1 @ S4_X9)))).
thf(s4_h1,axiom,
   s4_h1 = ( ^ [S4_X9 : x,S4_X3 : x] : (hate_v_1 @ s4_e2 @ S4_X3 @ S4_X9))).
thf(s5_e2_decl,type,(s5_e2 : e)).
thf(s5_e13_decl,type,(s5_e13 : e)).
thf(s5_e16_decl,type,(s5_e16 : e)).
thf(s5_e18_decl,type,(s5_e18 : e)).
thf(s5_e24_decl,type,(s5_e24 : e)).

thf(s5_e2_value,axiom,(s5_e2 = (int_to_e @ 2))).
thf(s5_e13_value,axiom,(s5_e13 = (int_to_e @ 13))).
thf(s5_e16_value,axiom,(s5_e16 = (int_to_e @ 16))).
thf(s5_e18_value,axiom,(s5_e18 = (int_to_e @ 18))).
thf(s5_e24_value,axiom,(s5_e24 = (int_to_e @ 24))).

thf(s5_id_Agatha_decl,type,s5_id_Agatha: name).
thf(s5_id_Aunt_decl,type,s5_id_Aunt: name).

thf(s5_h29_decl,type,s5_h29: x > $o).
thf(s5_h26_decl,type,s5_h26: x > x > $o).
thf(s5_h23_decl,type,s5_h23: x > x > $o).
thf(s5_h20_decl,type,s5_h20: x > x > x > $o).
thf(s5_h15_decl,type,s5_h15: x > x > $o).
thf(s5_root_decl,type,s5_root: x > x > x > x > $o).
thf(s5_h9_decl,type,s5_h9: x > x > x > $o).
thf(s5_h7_decl,type,s5_h7: x > $o).
thf(s5_h4_decl,type,s5_h4: x > x > $o).
thf(s5_h1_decl,type,s5_h1: x > x > $o).

thf(s5_h29,axiom,
   s5_h29 = ( ^ [S5_X25 : x] : (named @ S5_X25 @ s5_id_Aunt))).
thf(s5_h26,axiom,
   s5_h26 = ( ^ [S5_X25 : x,S5_X19 : x] : (proper_q @ S5_X25 @ s5_h29 @ (s5_h23 @ S5_X19)))).
thf(s5_h23,axiom,
   s5_h23 = ( ^ [S5_X19 : x,S5_X25 : x] : ((named @ S5_X19 @ s5_id_Agatha) & (compound @ s5_e24 @ S5_X19 @ S5_X25)))).
thf(s5_h20,axiom,
   s5_h20 = ( ^ [S5_X25 : x,S5_X19 : x,S5_X8 : x] : (proper_q @ S5_X19 @ (s5_h26 @ S5_X25) @ (s5_h15 @ S5_X8)))).
thf(s5_h15,axiom,
   s5_h15 = ( ^ [S5_X8 : x,S5_X19 : x] : ((more_comp @ s5_e18 @ s5_e16 @ S5_X19) & (rich_a_in @ s5_e16 @ S5_X8)))).
thf(s5_root,axiom,
   s5_root = ( ^ [S5_X25 : x,S5_X19 : x,S5_X8 : x,S5_X3 : x] : (every_q @ S5_X8 @ (s5_h9 @ S5_X25 @ S5_X19) @ (s5_h4 @ S5_X3)))).
thf(s5_h9,axiom,
   s5_h9 = ( ^ [S5_X25 : x,S5_X19 : x,S5_X8 : x] : ((neg @ s5_e13 @ (s5_h20 @ S5_X25 @ S5_X19 @ S5_X8)) & (person @ S5_X8)))).
thf(s5_h7,axiom,
   s5_h7 = ( ^ [S5_X3 : x] : (butler_n_1 @ S5_X3))).
thf(s5_h4,axiom,
   s5_h4 = ( ^ [S5_X3 : x,S5_X8 : x] : (the_q @ S5_X3 @ s5_h7 @ (s5_h1 @ S5_X8)))).
thf(s5_h1,axiom,
   s5_h1 = ( ^ [S5_X8 : x,S5_X3 : x] : (hate_v_1 @ s5_e2 @ S5_X3 @ S5_X8))).
thf(s6_e2_decl,type,(s6_e2 : e)).
thf(s6_e18_decl,type,(s6_e18 : e)).
thf(s6_e26_decl,type,(s6_e26 : e)).

thf(s6_e2_value,axiom,(s6_e2 = (int_to_e @ 2))).
thf(s6_e18_value,axiom,(s6_e18 = (int_to_e @ 18))).
thf(s6_e26_value,axiom,(s6_e26 = (int_to_e @ 26))).

thf(s6_id_Agatha_decl,type,s6_id_Agatha: name).

thf(s6_h23_decl,type,s6_h23: x > $o).
thf(s6_h20_decl,type,s6_h20: x > x > x > $o).
thf(s6_h17_decl,type,s6_h17: x > x > $o).
thf(s6_h13_decl,type,s6_h13: x > x > x > $o).
thf(s6_root_decl,type,s6_root: x > x > x > x > $o).
thf(s6_h9_decl,type,s6_h9: x > x > $o).
thf(s6_h7_decl,type,s6_h7: x > $o).
thf(s6_h4_decl,type,s6_h4: x > x > $o).
thf(s6_h1_decl,type,s6_h1: x > x > $o).

thf(s6_h23,axiom,
   s6_h23 = ( ^ [S6_X19 : x] : (aunt_n_of @ S6_X19))).
thf(s6_h20,axiom,
   s6_h20 = ( ^ [S6_X19 : x,S6_X14 : x,S6_X8 : x] : (udef_q @ S6_X19 @ s6_h23 @ (s6_h13 @ S6_X14 @ S6_X8)))).
thf(s6_h17,axiom,
   s6_h17 = ( ^ [S6_X19 : x,S6_X14 : x] : ((named @ S6_X14 @ s6_id_Agatha) & (compound @ s6_e18 @ S6_X14 @ S6_X19)))).
thf(s6_h13,axiom,
   s6_h13 = ( ^ [S6_X14 : x,S6_X8 : x,S6_X19 : x] : (proper_q @ S6_X14 @ (s6_h17 @ S6_X19) @ (s6_h9 @ S6_X8)))).
thf(s6_root,axiom,
   s6_root = ( ^ [S6_X19 : x,S6_X14 : x,S6_X8 : x,S6_X3 : x] : (every_q @ S6_X8 @ (s6_h20 @ S6_X19 @ S6_X14) @ (s6_h4 @ S6_X3)))).
thf(s6_h9,axiom,
   s6_h9 = ( ^ [S6_X8 : x,S6_X14 : x] : ((hate_v_1 @ s6_e26 @ S6_X14 @ S6_X8) & (person @ S6_X8)))).
thf(s6_h7,axiom,
   s6_h7 = ( ^ [S6_X3 : x] : (butler_n_1 @ S6_X3))).
thf(s6_h4,axiom,
   s6_h4 = ( ^ [S6_X3 : x,S6_X8 : x] : (the_q @ S6_X3 @ s6_h7 @ (s6_h1 @ S6_X8)))).
thf(s6_h1,axiom,
   s6_h1 = ( ^ [S6_X8 : x,S6_X3 : x] : (hate_v_1 @ s6_e2 @ S6_X3 @ S6_X8))).
thf(s7_e2_decl,type,(s7_e2 : e)).

thf(s7_e2_value,axiom,(s7_e2 = (int_to_e @ 2))).


thf(s7_root_decl,type,s7_root: x > x > $o).
thf(s7_h9_decl,type,s7_h9: x > $o).
thf(s7_h5_decl,type,s7_h5: x > x > $o).
thf(s7_h4_decl,type,s7_h4: x > $o).
thf(s7_h1_decl,type,s7_h1: x > x > $o).

thf(s7_root,axiom,
   s7_root = ( ^ [S7_X8 : x,S7_X3 : x] : (every_q @ S7_X8 @ s7_h9 @ (s7_h5 @ S7_X3)))).
thf(s7_h9,axiom,
   s7_h9 = ( ^ [S7_X8 : x] : (person @ S7_X8))).
thf(s7_h5,axiom,
   s7_h5 = ( ^ [S7_X3 : x,S7_X8 : x] : (no_q @ S7_X3 @ s7_h4 @ (s7_h1 @ S7_X8)))).
thf(s7_h4,axiom,
   s7_h4 = ( ^ [S7_X3 : x] : (person @ S7_X3))).
thf(s7_h1,axiom,
   s7_h1 = ( ^ [S7_X8 : x,S7_X3 : x] : (hate_v_1 @ s7_e2 @ S7_X3 @ S7_X8))).
thf(s8_e2_decl,type,(s8_e2 : e)).
thf(s8_e11_decl,type,(s8_e11 : e)).

thf(s8_e2_value,axiom,(s8_e2 = (int_to_e @ 2))).
thf(s8_e11_value,axiom,(s8_e11 = (int_to_e @ 11))).

thf(s8_id_Agatha_decl,type,s8_id_Agatha: name).

thf(s8_h16_decl,type,s8_h16: x > $o).
thf(s8_h13_decl,type,s8_h13: x > x > $o).
thf(s8_h9_decl,type,s8_h9: x > x > $o).
thf(s8_h7_decl,type,s8_h7: x > $o).
thf(s8_h4_decl,type,s8_h4: x > x > $o).
thf(s8_root_decl,type,s8_root: x > x > $o).

thf(s8_h16,axiom,
   s8_h16 = ( ^ [S8_X10 : x] : (butler_n_1 @ S8_X10))).
thf(s8_h13,axiom,
   s8_h13 = ( ^ [S8_X10 : x,S8_X3 : x] : (the_q @ S8_X10 @ s8_h16 @ (s8_h4 @ S8_X3)))).
thf(s8_h9,axiom,
   s8_h9 = ( ^ [S8_X10 : x,S8_X3 : x] : (be_v_id @ s8_e2 @ S8_X3 @ S8_X10))).
thf(s8_h7,axiom,
   s8_h7 = ( ^ [S8_X3 : x] : (named @ S8_X3 @ s8_id_Agatha))).
thf(s8_h4,axiom,
   s8_h4 = ( ^ [S8_X3 : x,S8_X10 : x] : (proper_q @ S8_X3 @ s8_h7 @ (s8_h9 @ S8_X10)))).
thf(s8_root,axiom,
   s8_root = ( ^ [S8_X10 : x,S8_X3 : x] : (neg @ s8_e11 @ (s8_h13 @ S8_X10 @ S8_X3)))).
thf(s9_e2_decl,type,(s9_e2 : e)).
thf(s9_e9_decl,type,(s9_e9 : e)).
thf(s9_e19_decl,type,(s9_e19 : e)).

thf(s9_e2_value,axiom,(s9_e2 = (int_to_e @ 2))).
thf(s9_e9_value,axiom,(s9_e9 = (int_to_e @ 9))).
thf(s9_e19_value,axiom,(s9_e19 = (int_to_e @ 19))).

thf(s9_id_Agatha_decl,type,s9_id_Agatha: name).

thf(s9_h22_decl,type,s9_h22: x > x > $o).
thf(s9_h21_decl,type,s9_h21: x > $o).
thf(s9_h18_decl,type,s9_h18: x > x > $o).
thf(s9_h16_decl,type,s9_h16: x > $o).
thf(s9_root_decl,type,s9_root: x > x > $o).
thf(s9_h6_decl,type,s9_h6: $o).
thf(s9_h4_decl,type,s9_h4: $o).
thf(s9_h1_decl,type,s9_h1: x > x > $o).

thf(s9_h22,axiom,
   s9_h22 = ( ^ [S9_X20 : x,S9_X13 : x] : (pronoun_q @ S9_X20 @ s9_h21 @ (s9_h18 @ S9_X13)))).
thf(s9_h21,axiom,
   s9_h21 = ( ^ [S9_X20 : x] : (pron @ S9_X20))).
thf(s9_h18,axiom,
   s9_h18 = ( ^ [S9_X13 : x,S9_X20 : x] : (kill_v_1 @ s9_e19 @ S9_X13 @ S9_X20))).
thf(s9_h16,axiom,
   s9_h16 = ( ^ [S9_X13 : x] : (named @ S9_X13 @ s9_id_Agatha))).
thf(s9_root,axiom,
   s9_root = ( ^ [S9_X20 : x,S9_X13 : x] : (proper_q @ S9_X13 @ s9_h16 @ (s9_h1 @ S9_X20)))).
thf(s9_h6,axiom,
   s9_h6 = ((therefore_a_1 @ s9_h4))).
thf(s9_h4,axiom,
   s9_h4 = ((unknown @ s9_e2))).
thf(s9_h1,axiom,
   s9_h1 = ( ^ [S9_X20 : x,S9_X13 : x] : (colon_p_namely @ s9_e9 @ s9_h6 @ (s9_h22 @ S9_X20 @ S9_X13)))).
