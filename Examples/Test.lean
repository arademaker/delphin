

/-
   Examples from
   https://github.com/delph-in/docs/wiki/MatrixMrsTestSuiteEn
-/

/-
SENT: It rained.

[  LTOP: h0
  INDEX: e2 [ e SF: prop TENSE: past MOOD: indicative PROG: - PERF: - ]
   RELS: < [ _rain_v_1<3:9> LBL: h1 ARG0: e2 ] >
  HCONS: < h0 qeq h1 >
  ICONS: < > ]
-/

section TFF
 variable (e : Type)
 variable (_rain_v_1 : e → Prop)

 #check ∃ e2, _rain_v_1 e2
end TFF

/-
SENT: Abrams barked.
[ LTOP: h0
 INDEX: e2 [ e SF: prop TENSE: past MOOD: indicative PROG: - PERF: - ]
  RELS: < [ proper_q<0:6>   LBL: h4 ARG0: x3 [ x PERS: 3 NUM: sg IND: + ] RSTR: h5 BODY: h6 ]
          [ named<0:6>      LBL: h7 CARG: "Abrams" ARG0: x3 ]
          [ _bark_v_1<7:13> LBL: h1 ARG0: e2 ARG1: x3 ] >
 HCONS: < h0 qeq h1 h5 qeq h7 >
 ICONS: < > ]
-/

section TFF
 variable (e : Type) (x : Type)
 variable (_bark_v_1 : e → x → Prop)
 variable (named : String → x → Prop) /- FOL can't do that -/

 #check ∃ e2 : e, ∃ x3 : x, _bark_v_1 e2 x3 ∧ named "Abrams" x3
end TFF

section MTT
 universe CN

 variable (_bark_v_1 : Type CN → Prop)
 variable (named : String → Type CN → Type CN)

 axiom s : ∃ h7 : (Σ x3 : Type CN, named "Abrams" x3), _bark_v_1 h7.1
end MTT


/-
SENT: The window opened.
[ LTOP: h0
INDEX: e2 [ e SF: prop TENSE: past MOOD: indicative PROG: - PERF: - ]
RELS: <
 [ _the_q<0:3>       LBL: h4 ARG0: x3 [ x PERS: 3 NUM: sg IND: + ] RSTR: h5 BODY: h6 ]
 [ _window_n_1<4:10> LBL: h7 ARG0: x3 ]
 [ _open_v_1<11:17>  LBL: h1 ARG0: e2 ARG1: x3 ] >
HCONS: < h0 qeq h1 h5 qeq h7 >
ICONS: < > ]
-/

section TFF
 /- the quantifier 'the' is implicited converted to ∃, we use event
    variables (Davidson's semantics) -/

 variable (x : Type) (e : Type)
 variable (_window_n_1 : x → Prop)
 variable (_open_v_1: e → x → Prop)

 #check ∃ x3 e2, _window_n_1 x3 ∧ _open_v_1 e2 x3
end TFF

section MTT
 universe CN

 variable (Thing : Type CN)

 variable (_the_q : ∀ a : Type CN, (a → Prop) → Prop)
 variable (_window_n_1 : Type CN)
 variable (_open_v_1 : Thing → Prop)

 axiom mh : _window_n_1 → Thing
 variable (agent : Thing)

 -- _window_n_1 < Thing
 instance : Coe _window_n_1 Thing where
  coe w := agent

 def toThing : _window_n_1 → Thing
  | w => agent

 #check ∃ w : _window_n_1, _open_v_1 (toThing w)

 #check _the_q (coe _window_n_1) _open_v_1
end MTT



/-
SENT: The dog arrived barking.
[ LTOP: h0
 INDEX: e2 [ e SF: prop TENSE: past MOOD: indicative PROG: - PERF: - ]
  RELS: < [ _the_q<0:3>       LBL: h4 ARG0: x3 [ x PERS: 3 NUM: sg IND: + ] RSTR: h5 BODY: h6 ]
          [ _dog_n_1<4:7>     LBL: h7 ARG0: x3 ]
          [ _arrive_v_1<8:15> LBL: h8 ARG0: e2 ARG1: x3 ]
          [ subord<16:24>     LBL: h1 ARG0: e9 [ e SF: prop ] ARG1: h10 ARG2: h11 ]
          [ _bark_v_1<16:23>  LBL: h12 ARG0: e13 [ e SF: prop TENSE: untensed MOOD: indicative PROG: + PERF: - ] ARG1: i14 ] >
HCONS: < h0 qeq h1 h5 qeq h7 h10 qeq h8 h11 qeq h12 >
ICONS: < > ]
-/

section FOL
 /- 'the' and 'subord' can't be represented, uninstantiated variable
    is not different from the other ones, no explicit connection
    between the events -/

 variable (u : Type)
 variable (_dog_n_1 : u → Prop)
 variable (_bark_v_1 : u → u → Prop)
 variable (_arrive_v_1 : u → u → Prop)

 #check  ∃ e2 e13 i14 x, _dog_n_1 x ∧ _arrive_v_1 e2 x ∧ _bark_v_1 e13 i14
end FOL

section THF
  /- 'the' is not represented, implicited converted to ∃ -/

 variable (e : Type) (x : Type) (i : Type)
 variable (_dog_n_1 : x → Prop)
 variable (_arrive_v_1 : e → x → Prop)
 variable (subord : e → Prop → Prop → Prop)
 variable (_bark_v_1 : e → i → Prop)  /- how to deal with i type -/

 #check ∃ e9, subord e9 (∃ x3, _dog_n_1 x3 ∧ (∃ e2, _arrive_v_1 e2 x3)) (∃ e13 i14, _bark_v_1 e13 i14)
end THF



/-
SENT: She noticed a jaguar speeding up on the highway
[ LTOP: h0
INDEX: e2 [ e SF: prop TENSE: past MOOD: indicative PROG: - PERF: - ]
RELS: <
 [ pron<0:3>          LBL: h4 ARG0: x3 [ x PERS: 3 NUM: sg GEND: f IND: + PT: std ] ]
 [ pronoun_q<0:3>     LBL: h5 ARG0: x3 RSTR: h6 BODY: h7 ]
 [ _notice_v_1<4:11>  LBL: h1 ARG0: e2 ARG1: x3 ARG2: x8 [ x PERS: 3 NUM: sg IND: + ] ]
 [ _a_q<12:13>        LBL: h9 ARG0: x8 RSTR: h10 BODY: h11 ]
 [ _jaguar_n_1<14:20> LBL: h12 ARG0: x8 ]
 [ _speed_v_up<21:29> LBL: h12 ARG0: e13 [ e SF: prop TENSE: untensed MOOD: indicative PROG: + PERF: - ] ARG1: x8 ]
 [ _on_p_state<33:35> LBL: h12 ARG0: e14 [ e SF: prop TENSE: untensed MOOD: indicative PROG: - PERF: - ]
                               ARG1: e13 ARG2: x15 [ x PERS: 3 NUM: sg IND: + ] ]
 [ _the_q<36:39>       LBL: h16 ARG0: x15 RSTR: h17 BODY: h18 ]
 [ _highway_n_1<40:47> LBL: h19 ARG0: x15 ] >
HCONS: < h0 qeq h1 h6 qeq h4 h10 qeq h12 h17 qeq h19 >
ICONS: < > ]

 (h0 (h1 ?))
 (h5  x3  (h6 (h4 ?)) (h7 ?))
 (h9  x8  (h10 (h12 ?)) (h11 ?))
 (h16 x15 (h17 (h19 ?)) (h18 ?))

 h4  = pron x3
 h1  = _notice_v_1 e2 x3 x8
 h12 = _jaguar_n_1 x8 ∧ _speed_v_up e13 x8 ∧ _on_p_state e4 e13 x15
 h19 = _highway_n_1 x15

 (h5  x3  (h6 (h4 ?)) (h7 (h16 x15 (h17 (h19 ?)) (h18 (h9  x8  (h10 (h12 ?)) (h11 (h0 (h1 ?))))))))
-/

section TFF
 variable (e : Type) (x : Type)
 variable (pron : x → Prop)
 variable (_notice_v_1 : e → x → x → Prop)
 variable (_jaguar_n_1 : x → Prop)
 variable (_speed_v_up : e → x → Prop)
 variable (_on_p_state : e → e → x → Prop)
 variable (_highway_n_1 : x → Prop)

 #check ∃ e4 e13 e2,
  ∃ x3, (pron x3) ∧
   (∃ x15, (_highway_n_1 x15) ∧
    (∃ x8, (_jaguar_n_1 x8 ∧ _speed_v_up e13 x8 ∧ _on_p_state e4 e13 x15) ∧ (_notice_v_1 e2 x3 x8)))

 -- CNF encoding ?
 -- prepare guys with Logic backgroud
 -- I have thing combine semantic structure sentence with UKB (!!)
end TFF

section MTT
  universe CN

  variable (pron : Type CN)
  variable (_jaguar_n_1 : Type CN)
  variable (_highway_n_1 : Type CN)
  variable (_speed_v_up : _jaguar_n_1 → Prop)
  variable (_notice_v_1 : pron → _jaguar_n_1 → Prop)
  variable (_on_p_state : ∀ {x y : Type CN}, y → (x → Prop) → (x → Prop))


  #check ∃ z : (∃ x8 : _jaguar_n_1, (∃ x15 : _highway_n_1, (_on_p_state x15 _speed_v_up) x8)), ∃ x3 : pron, _notice_v_1 x3 z.1

end MTT


/-
SENT: Any farmer who owns a donkey beats it
[ LTOP: h0
 INDEX: e2 [ e SF: prop TENSE: pres MOOD: indicative PROG: - PERF: - ]
  RELS: < [ _any_q<0:3>        LBL:  h4 ARG0: x3 RSTR: h5 BODY: h6 ]
          [ _farmer_n_1<4:10>  LBL:  h7 ARG0: x3 ]
          [ _own_v_1<15:19>    LBL:  h7 ARG0: e8 ARG1: x3 ARG2: x9 ]
          [ _a_q<20:21>        LBL: h10 ARG0: x9 RSTR: h11 BODY: h12 ]
          [ _donkey_n_1<22:28> LBL: h13 ARG0: x9 ]
          [ _beat_v_to<29:34>  LBL:  h1 ARG0: e2 ARG1: x3 ARG2: x14 ]
          [ pron<35:37>        LBL: h15 ARG0: x14 ]
          [ pronoun_q<35:37>   LBL: h16 ARG0: x14 RSTR: h17 BODY: h18 ] >
HCONS: < h0 qeq h1 h5 qeq h7 h11 qeq h13 h17 qeq h15 >
ICONS: < > ]

h1: _beat_v_to x3 x14

h4: any_q x3                                     any_q v P Q  =>  ∀ v, P → Q
  h5 = h7: _farmer_n_1 x3 ∧ _own_v_1 e8 x3 x9
  h6: ?                                          h1=h18 and h6=h16  or h6=h1 and h18=h4

h10: a_q x9                                      a_q v P Q  =>  ∃ v, P ∧ Q
  h11 = h13: _donkey_n_1 x9
  h12: h4                                        because x9 used in h5

h16: pronoun_q x14                               pronoun_q v P Q  =>  ∃ v, P ∧ Q
  h17 = h15: pron x14
  h18: ?                                         h1=h18 and h6=h16  or h6=h1 and h18=h4
-/

section FOF
 variable (u : Type)
 variable (_farmer_n_1 : u → Prop)
 variable (_own_v_1 : u → u → u → Prop)
 variable (_donkey_n_1 : u → Prop)
 variable (_beat_v_to : u → u → u → Prop)
 variable (pron : u → Prop)

 #check ∃ e8 e2, (∃ x9, (_donkey_n_1 x9) ∧ (∀ x3, (_farmer_n_1 x3 ∧ _own_v_1 e8 x3 x9) → (∃ x14, pron x14 ∧ _beat_v_to e2 x3 x14)))
end FOF



/-
SENT: A man walks.
[ LTOP: h0
INDEX: e2 [ e SF: prop TENSE: pres MOOD: indicative PROG: - PERF: - ]
RELS: < [ _a_q<0:1>       LBL: h4 ARG0: x3 [ x PERS: 3 NUM: sg IND: + ] RSTR: h5 BODY: h6 ]
        [ _man_n_1<2:5>   LBL: h7 ARG0: x3 ]
        [ _walk_v_1<6:11> LBL: h1 ARG0: e2 ARG1: x3 ] >
HCONS: < h0 qeq h1 h5 qeq h7 >
ICONS: < > ]
-/

section MTT

  universe CN

  variable (_man_n_1 : Type CN)
  variable (_walk_v_1 : _man_n_1 → Type)

  #check Σ x3 : _man_n_1, _walk_v_1 x3

end MTT


/-
SENT: Some Irish delegates finished the survey on time.
[ LTOP: h0
INDEX: e2 [ e SF: prop TENSE: past MOOD: indicative PROG: - PERF: - ]
RELS: <
 [ _some_q<0:4>         LBL: h4 ARG0: x3 [ x PERS: 3 NUM: pl IND: + ] RSTR: h5 BODY: h6 ]
 [ _irish_a_1<5:10>     LBL: h7 ARG0: e8 [ e SF: prop TENSE: untensed MOOD: indicative PROG: bool PERF: - ] ARG1: x3 ]
 [ _delegate_n_1<11:20> LBL: h7 ARG0: x3 ]
 [ _finish_v_1<21:29>   LBL: h1 ARG0: e2 ARG1: x3 ARG2: x9 [ x PERS: 3 NUM: sg IND: + ] ]
 [ _the_q<30:33>        LBL: h10 ARG0: x9 RSTR: h11 BODY: h12 ]
 [ _survey_n_1<34:40>   LBL: h13 ARG0: x9 ]
 [ _on+time_a_1<41:48>  LBL: h1 ARG0: e14 [ e SF: prop TENSE: untensed MOOD: indicative PROG: - PERF: - ] ARG1: e2 ] >
HCONS: < h0 qeq h1 h5 qeq h7 h11 qeq h13 >
ICONS: < > ]
-/


/-
SENT: Corrosion prevented continuous contact.
[ LTOP: h0
INDEX: e2 [ e SF: prop TENSE: past MOOD: indicative PROG: - PERF: - ]
RELS: <
 [ udef_q<0:9>            LBL: h4 ARG0: x3 [ x PERS: 3 NUM: sg GEND: n IND: - ] RSTR: h5 BODY: h6 ]
 [ _corrosion_n_1<0:9>    LBL: h7 ARG0: x3 ]
 [ _prevent_v_from<10:19> LBL: h1 ARG0: e2 ARG1: x3 ARG2: x8 [ x PERS: 3 NUM: sg ] ]
 [ udef_q<20:39>          LBL: h9 ARG0: x8 RSTR: h10 BODY: h11 ]
 [ _continuous_a_1<20:30> LBL: h12 ARG0: e13 [ e SF: prop TENSE: untensed MOOD: indicative PROG: bool PERF: - ] ARG1: x8 ]
 [ _contact_n_1<31:38>    LBL: h12 ARG0: x8 ] >
HCONS: < h0 qeq h1 h5 qeq h7 h10 qeq h12 >
ICONS: < > ]
-/

section MTT
  universe CN

  variable (_corrosion_n_1 : Type CN)
  variable (_contact_n_1 : Type CN)
  variable (_prevent_v_from : _corrosion_n_1 → _contact_n_1 → Prop)
  variable (_continuous_a_1 : _contact_n_1 → Prop)

  #check ∃ h₉ : (∃ x8, _continuous_a_1 x8), ∃ x3, _prevent_v_from x3 h₉.1

end MTT
